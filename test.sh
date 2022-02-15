#!/bin/bash
set -e
set -o pipefail

cargo run \
--release \
-- --sysroot \
`rustc --print sysroot` \
-C panic=abort \
-O \
a.rs \
--extern libc=/home/beef/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib/liblibc-84f70e829012df9d.rlib \
| tee >(gcc -x c -)