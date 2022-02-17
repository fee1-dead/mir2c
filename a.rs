#![feature(bench_black_box, register_attr, rustc_private)]
#![register_attr(mir2c_do_not_codegen)]
#![no_std]
#![no_main]

extern crate libc;

use core::hint::black_box;

struct S<T>(T);

#[no_mangle]
pub extern "C" fn main(_argc: isize, _argv: *const *const i8) -> isize {
    const HELLO: &'static [u8] = b"Hello, world!\n\0";
    match black_box(1) {
        1 => unsafe {
            libc::printf(HELLO.as_ptr() as *const _);
        },
        _ => {}
    }

    4567 * black_box(123)
}

#[panic_handler]
#[mir2c_do_not_codegen]
fn panic_handler(_: &core::panic::PanicInfo) -> ! {
    loop {}
}
