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
