#!/usr/bin/env bash
# Regenerate .f90 sources from .fypp templates. Generated files are checked
# in so plain `fpm build` works without fypp present.
set -euo pipefail
cd "$(dirname "$0")/.."
for tpl in src/*.fypp; do
   out="${tpl%.fypp}.f90"
   fypp "$tpl" "$out"
   echo "fypp: $tpl -> $out"
done

# schema-request compiles one type the way `capnp convert` does. The
# capnproto sources are not installed with the binary, so the build is
# skipped when they cannot be found. capnpf then falls back to
# `capnp compile`.
if command -v g++ >/dev/null 2>&1 && command -v capnp >/dev/null 2>&1; then
   prefix="$(dirname "$(dirname "$(command -v capnp)")")"
   src=""
   if [[ -n "${CAPNP_SRC:-}" && -f "${CAPNP_SRC}/capnp/compiler/module-loader.c++" ]]; then
      src="${CAPNP_SRC}"
   elif [[ -f "${prefix}/../src/capnproto-c++-1.4.0/src/capnp/compiler/module-loader.c++" ]]; then
      src="${prefix}/../src/capnproto-c++-1.4.0/src"
   fi
   if [[ -n "$src" && -f "${prefix}/include/capnp/schema.capnp.h" ]]; then
      mkdir -p build
      g++ -std=c++17 -O2 -I"$src" -I"${prefix}/include" -c \
         "$src/capnp/compiler/module-loader.c++" -o build/module-loader.o
      g++ -std=c++17 -O2 -I"$src" -I"${prefix}/include" \
         tools/schema-request.cpp build/module-loader.o \
         -L"${prefix}/lib" -Wl,-rpath,"${prefix}/lib" \
         -lcapnpc -lcapnp -lkj -pthread -o build/schema-request
      echo "schema-request: build/schema-request"
   else
      echo "schema-request: capnproto sources not found, skipped"
   fi
fi
