#![feature(box_patterns)]
#![feature(rustc_private)]
#![feature(if_let_guard)]
#![feature(let_else)]
#![warn(clippy::if_not_else)]
#![warn(elided_lifetimes_in_paths)]

use std::borrow::Cow;
use std::env;
use std::fmt::{Write};
use std::path::Path;
use std::{io, mem};

use rustc_const_eval::interpret::{get_slice_bytes, ConstValue};
use rustc_data_structures::fx::{FxIndexMap};

use rustc_driver::Callbacks;
use rustc_interface::interface::Compiler;
use rustc_interface::Queries;
use rustc_middle::mir::mono::{CodegenUnit, MonoItem};
use rustc_middle::mir::pretty::write_mir_fn;
use rustc_middle::mir::{
    self, BinOp::*, Constant, ConstantKind, HasLocalDecls, Operand, Place, Rvalue, Terminator,
};

use rustc_middle::ty::{
    self, Instance, ParamEnv, Ty, TyCtxt, TypeAndMut,
};

use rustc_span::{sym, Symbol};

use rustc_target::spec::abi::Abi;

extern crate rustc_const_eval;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_span;
extern crate rustc_target;

struct Mir2c;

struct State<'tcx> {
    tcx: TyCtxt<'tcx>,
    h: String,
    slice_types: FxIndexMap<Ty<'tcx>, usize>,
    slices: usize,
    s: String,
}

// for debugging use only, dumps MIR when dropped. Must forget.
struct DumpMirGuard<'a, 'tcx>(TyCtxt<'tcx>, &'a mir::Body<'tcx>);

impl<'a, 'tcx> Drop for DumpMirGuard<'a, 'tcx> {
    fn drop(&mut self) {
        let stderr = io::stderr();
        let _ = write_mir_fn(self.0, self.1, &mut |_, _| Ok(()), &mut stderr.lock());
    }
}

impl<'tcx> State<'tcx> {
    fn new(tcx: TyCtxt<'tcx>) -> Self {
        let mut state = State {
            tcx,
            h: String::new(),
            slice_types: FxIndexMap::default(),
            slices: 0,
            s: String::new(),
        };
        state.header();
        state
    }

    fn header(&mut self) {
        self.h.push_str(include_str!("header.c"));
    }

    fn build(mut self) -> String {
        let mut header = mem::take(&mut self.h);
        while !self.slice_types.is_empty() {
            for (ty, n) in mem::take(&mut self.slice_types) {
                write!(
                    header,
                    "struct __slice_{n} {{{}* ptr;size_t len;}};",
                    self.ty(ty)
                )
                .unwrap();
            }
        }
        header.push_str(&self.s);
        header
    }

    fn slice_n(&mut self, elem: Ty<'tcx>) -> usize {
        *self.slice_types.entry(elem).or_insert_with(|| {
            let prev = self.slices;
            self.slices += 1;
            prev
        })
    }

    #[must_use]
    fn ty(&mut self, ty: Ty<'tcx>) -> Cow<'static, str> {
        match ty.kind() {
            ty::Tuple(l) if l.is_empty() => "__unitty".into(),
            ty::Uint(ty::UintTy::U8) => "unsigned char".into(),
            ty::Int(ty::IntTy::I8) => "char".into(),
            ty::Int(ty::IntTy::I32) => "int32_t".into(),
            ty::Int(ty::IntTy::Isize) => "int".into(), // TODO sus
            ty::Ref(_, ty, _) if let ty::Slice(elem) = ty.kind() => {
                format!("struct __slice_{}", self.slice_n(elem)).into()
            }
            ty::RawPtr(TypeAndMut { ty, .. }) if let ty::Slice(elem) = ty.kind() => {
                format!("struct __slice_{}", self.slice_n(elem)).into()
            }
            ty::RawPtr(TypeAndMut { ty, mutbl: _ }) => {
                format!("{}*", self.ty(ty)).into()
            }
            ty::Never => "char".into(), // TODO ??
            ty => unimplemented!("{ty:?}"),
        }
    }

    fn konst(&mut self, konst: &ty::Const<'tcx>) {
        if konst.ty.is_integral() {
            let int = konst.val.try_to_scalar_int().unwrap();
            write!(self.s, "{}", int.to_bits(int.size()).unwrap()).unwrap();
        } else {
            if let ty::Ref(_, ty, _) = konst.ty.kind() {
                if let ty::Slice(elem) = ty.kind() {
                    if !matches!(elem.kind(), ty::Uint(ty::UintTy::U8)) {
                        // figure out elements length
                        unimplemented!("{elem:?}");
                    }

                    if let ty::ConstKind::Value(cv) = konst.val {
                        let ConstValue::Slice { data, .. } = &cv else { panic!() };
                        let bytes = get_slice_bytes(&self.tcx, cv);
                        if !data.relocations().is_empty() {
                            unimplemented!();
                        }

                        let len = data.len();
                        let name = format!("_allocation_{len}");
                        write!(self.h, "unsigned char {name}[{}] = {{", self.h.len()).unwrap();

                        for (n, byte) in bytes.iter().enumerate() {
                            if n != 0 {
                                self.h.push(',');
                            }

                            write!(self.h, "{byte}").unwrap();
                        }

                        self.h.push_str("};");

                        let n = self.slice_n(elem);

                        write!(self.s, "(struct __slice_{n}){{.ptr = {name},.len = {len}}}").unwrap();
                        return;
                    }
                }
            }
            unimplemented!("{konst:?}");
        }
    }

    fn operand(&mut self, op: &Operand<'tcx>) {
        match op {
            Operand::Constant(box mir::Constant { literal, .. }) => match literal {
                ConstantKind::Ty(konst) => self.konst(konst),
                ConstantKind::Val(_, _) => todo!(),
            },
            Operand::Copy(p) => {
                if !p.projection.is_empty() {
                    unimplemented!();
                }
                write!(self.s, "{:?}", p.local).unwrap();
            }
            Operand::Move(p) => {
                // TODO distinguish move with copy
                if !p.projection.is_empty() {
                    unimplemented!();
                }
                write!(self.s, "{:?}", p.local).unwrap();
            }
        }
    }

    fn rvalue(&mut self, mir: &mir::Body<'tcx>, rvalue: &Rvalue<'tcx>) {
        match rvalue {
            Rvalue::Use(op) => {
                self.operand(op);
            }
            Rvalue::Cast(mir::CastKind::Misc, op, ty) if let ty::RawPtr(TypeAndMut { ty: pointee, .. }) = ty.kind() => {
                let Operand::Move(pl) = op else { unimplemented!() };
                let local = pl.as_local().unwrap();
                
                if let ty::RawPtr(TypeAndMut { ty: pointee_from, .. }) = mir.local_decls()[local].ty.kind() {
                    if let ty::Slice(elem) = pointee_from.kind() {
                        // we have a wrapper struct for the slice ref type. We retrive the pointer from the slice struct
                        assert_eq!(pointee, elem);
                        self.operand(op);
                        self.s.push_str(".ptr");
                    } else {
                        // normal pointer cast
                        self.s.push_str("((");
                        let ty = self.ty(ty);
                        self.s.push_str(&ty);
                        self.s.push(')');
                        self.operand(op);
                        self.s.push(')');
                    }
                } else {
                    todo!()
                }
            }
            Rvalue::AddressOf(_, Place { local, projection })
                if projection.len() == 1 && matches!(projection[0], mir::ProjectionElem::Deref) => {
                // reference to raw pointer. do nothing in C.
                write!(self.s, "{local:?}").unwrap();
            }
            Rvalue::BinaryOp(binop, box (op, op1)) => {
                self.operand(op);
                self.s.push_str(match binop {
                    // TODO overflow behavior
                    Add => "+",
                    Sub => "-",
                    Mul => "*",
                    Div => "/",
                    Rem => "%",
                    BitXor => "^",
                    BitAnd => "&",
                    BitOr => "|",
                    Shl => "<<",
                    Shr => ">>",
                    Eq => "==",
                    Lt => "<",
                    Le => "<=",
                    Ne => "!=",
                    Ge => ">=",
                    Gt => ">",
                    Offset => unimplemented!(),
                });
                self.operand(op1);
            }
            r => unimplemented!("{r:?}"),
        }
    }

    fn instance_header(&mut self, instance: &Instance<'tcx>) -> Option<mir::Body<'tcx>> {
        let Some(def_id) = instance.def.def_id_if_not_guaranteed_local_codegen() else { return None };
        let tcx = self.tcx;

        if tcx.has_attr(def_id, Symbol::intern("mir2c_do_not_codegen")) {
            return None;
        }

        let mir: mir::Body<'_> = instance.subst_mir_and_normalize_erasing_regions(
            tcx,
            ty::ParamEnv::reveal_all(),
            self.tcx.instance_mir(instance.def).clone(),
        );

        let locals = mir.local_decls();
        let ty = self.ty(mir.return_ty());
        write!(self.s, "{ty} {}(", tcx.symbol_name(*instance)).unwrap();
        for (l, local) in locals.iter_enumerated().skip(1).take(mir.arg_count) {
            if l.as_usize() != 1 {
                self.s.push(',');
            }
            let ty = self.ty(local.ty);
            write!(self.s, "{ty} {l:?}").unwrap();
        }
        self.s.push(')');

        Some(mir)
    }

    fn terminator(&mut self, mir: &mir::Body<'tcx>, terminator: &Terminator<'tcx>) {
        let tcx = self.tcx;
        match &terminator.kind {
            mir::TerminatorKind::Return => {
                if mir.return_ty().is_unit() {
                    self.s.push_str("return;");
                } else {
                    self.s.push_str("return _0;");
                }
            }

            mir::TerminatorKind::Goto { target } => {
                write!(self.s, "goto {target:?};").unwrap();
            }

            mir::TerminatorKind::Call {
                func: Operand::Constant(box Constant { literal: ConstantKind::Ty(konst), .. }),
                args,
                destination,
                ..
            } if let &ty::FnDef(def_id, substs) = konst.ty.kind() => {
                match tcx.fn_sig(def_id).abi() {
                    Abi::Rust | Abi::C { unwind: false } => {}
                    Abi::RustIntrinsic => {
                        // handle intrinsic here
                        match tcx.item_name(def_id) {
                            sym::black_box | sym::unlikely | sym::likely => {
                                assert_eq!(1, args.len());

                                if let Some((destplace, destbb)) = destination {
                                    if !destplace.projection.is_empty() {
                                        unimplemented!();
                                    }
                                    write!(self.s, "{:?} = ", destplace.local).unwrap();
                                    self.operand(args.iter().next().unwrap());
                                    write!(self.s, ";goto {destbb:?};").unwrap();
                                    return;
                                } else {
                                    unimplemented!();
                                }
                            }
                            sym => unimplemented!("rust intrinsic: {sym}"),
                        }
                    }
                    abi => unimplemented!("{abi:?}")
                }
                let instance = tcx.resolve_instance(ParamEnv::reveal_all().and((def_id, substs))).unwrap().unwrap();
                let sym = tcx.symbol_name(instance);
                if let Some((destplace, _)) = destination {
                    if !destplace.projection.is_empty() {
                        unimplemented!();
                    }
                    write!(self.s, "{:?} = ", destplace.local).unwrap();
                }
                write!(self.s, "{sym}(").unwrap();
                for (n, op) in args.iter().enumerate() {
                    if n != 0 {
                        self.s.push(',');
                    }
                    self.operand(op);
                }
                self.s.push_str(");");
                if let Some((_, destbb)) = destination {
                    write!(self.s, "goto {destbb:?};").unwrap();
                }
            }

            g => unimplemented!("{g:?}"),
        }
    }

    fn instance(&mut self, instance: &Instance<'tcx>) {
        let tcx = self.tcx;
        let Some(mir) = self.instance_header(instance) else { return };

        self.s.push('{');

        let locals = mir.local_decls();
        for (local, local_decl) in locals
            .iter_enumerated()
            .take(1)
            .chain(locals.iter_enumerated().skip(mir.arg_count + 1))
        {
            let ty = self.ty(local_decl.ty);
            write!(self.s, "{ty} {local:?};").unwrap();
        }

        let guard = DumpMirGuard(tcx, &mir);

        for (b, bb) in mir.basic_blocks().iter_enumerated() {
            write!(self.s, "{b:?}: {{").unwrap();
            for stmt in &bb.statements {
                match &stmt.kind {
                    mir::StatementKind::Assign(box (place, rvalue)) => {
                        if !place.projection.is_empty() {
                            unimplemented!();
                        }

                        write!(self.s, "{:?} = ", place.local).unwrap();
                        self.rvalue(&mir, rvalue);
                        self.s.push(';');
                    }
                    mir::StatementKind::StorageDead(_) | mir::StatementKind::StorageLive(_) => {}
                    stmt => unimplemented!("{stmt:?}"),
                }
            }

            self.terminator(&mir, bb.terminator());

            self.s.push('}');
        }

        self.s.push('}');
        mem::forget(guard);
    }

    fn prototype(&mut self, item: &MonoItem<'tcx>) {
        if let MonoItem::Fn(instance) = item {
            if self.instance_header(instance).is_none() {
                return;
            };
            self.s.push(';');
        }
    }

    fn define(&mut self, item: &MonoItem<'tcx>) {
        match item {
            MonoItem::Fn(instance) => self.instance(instance),
            MonoItem::Static(_) => todo!(),
            MonoItem::GlobalAsm(_) => todo!(),
        }
    }

    fn process(mut self) -> String {
        let (_, cgus): (_, &[CodegenUnit<'_>]) = self.tcx.collect_and_partition_mono_items(());

        // To allow randomized definition order, prototype each function before defining them.
        for cgu in cgus {
            for mono in cgu.items().keys() {
                self.prototype(mono)
            }
        }

        for cgu in cgus {
            for mono in cgu.items().keys() {
                self.define(mono)
            }
        }

        self.build()
    }
}

impl Callbacks for Mir2c {
    fn after_analysis<'tcx>(
        &mut self,
        _compiler: &Compiler,
        queries: &'tcx Queries<'tcx>,
    ) -> rustc_driver::Compilation {
        queries.global_ctxt().unwrap().peek_mut().enter(|tcx| {
            println!("{}", State::new(tcx).process());
        });

        rustc_driver::Compilation::Stop
    }
}

fn main() {
    rustc_driver::init_rustc_env_logger();
    let mut args: Vec<_> = env::args().collect();
    let wrapper_mode =
        args.get(1).map(Path::new).and_then(Path::file_stem) == Some("rustc".as_ref());

    if wrapper_mode {
        // we still want to be able to invoke it normally though
        args.remove(1);
    }

    rustc_driver::RunCompiler::new(&args, &mut Mir2c)
        .run()
        .unwrap();
}
