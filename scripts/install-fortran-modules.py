#!/usr/bin/env python3
"""Install the library's .mod files.

Meson compiles Fortran modules into the target's private directory and
passes its own -J, so a second -J is not possible (gfortran allows one).
A subproject consumer never notices, because meson tracks that directory
itself; an installed package must carry the files, or `use capnp` cannot
resolve.
"""
import glob
import os
import shutil
import sys

build_root = os.environ["MESON_BUILD_ROOT"]
destdir = os.environ.get("MESON_INSTALL_DESTDIR_PREFIX") or os.environ["MESON_INSTALL_PREFIX"]
target = sys.argv[1] if len(sys.argv) > 1 else "include/capnp-fortran"

out = os.path.join(destdir, target)
os.makedirs(out, exist_ok=True)

mods = sorted(glob.glob(os.path.join(build_root, "libcapnp_fortran*.p", "*.mod")))
if not mods:
    sys.exit("no .mod files found under the capnp_fortran target directory")
for m in mods:
    shutil.copy2(m, out)
print(f"installed {len(mods)} Fortran modules to {out}")
