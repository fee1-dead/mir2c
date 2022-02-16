#![feature(bench_black_box, register_attr, rustc_private)]
#![register_attr(mir2c_do_not_codegen)]
#![no_std]
#![no_main]

extern crate libc;

#[no_mangle]
pub extern "C" fn main(_argc: isize, _argv: *const *const i8) -> isize {
    // Since we are passing a C string the final null character is mandatory
    const HELLO: &'static [u8] = b"Hello, world!\n\0";
    unsafe {
        libc::printf(HELLO.as_ptr() as *const _);
    }
    4567 * core::hint::black_box(123)
}

#[panic_handler]
#[mir2c_do_not_codegen]
fn panic_handler(_: &core::panic::PanicInfo) -> ! {
    loop {}
}
