#!/bin/bash
set -e
set -o pipefail

# https://stackoverflow.com/a/69816610/13854774

cargo run \
--bin mir2c \
--release \
-- --sysroot \
`rustc --print sysroot` \
-C panic=abort \
-O \
a.rs \
--extern libc=`ls \`rustc --print sysroot\`/lib/rustlib/\`rustc -vV | sed -n 's|host: ||p'\`/lib/liblibc* | head -1` \
-Z mir-opt-level=5 \
| tee >(clang -x c -)