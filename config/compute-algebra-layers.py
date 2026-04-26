#!/usr/bin/env python3
"""
Compute algebra constructor layer assignments by isolated compilation.

OpenAxiom algebra constructors are compiled in layers: all constructors
within a layer may be compiled in parallel, but layer N depends on all
constructors in layers 0..N-1 being compiled first.  This script
determines the correct layer assignment for every constructor by
repeatedly attempting isolated compilations against a controlled set
of previously-compiled FASLs.

Algorithm
---------
1. Clean the staging directory (the directory the SPAD compiler uses
   to resolve inter-constructor references at compile time) so that
   it contains NO stale FASLs.

2. For layer 0, the candidates are all .spad files in the algebra
   build directory.  No prior-layer FASLs are staged; only strap-2
   outputs are available.

3. For layer N > 0, stage exactly the FASLs from layers 0..N-1
   (read from the L*_passed.txt result files), then try compiling
   each constructor that failed in the previous layer.

4. Constructors that compile successfully are assigned to the current
   layer; those that fail are deferred to the next round.

5. Repeat until convergence (no new constructors compile) or the
   layer limit (50) is reached.

6. After each layer, update cmake/OpenAxiomAlgebraLayers.cmake with
   the new assignments.

Prerequisites
-------------
This script must be run from the repository root after a successful
build of the ``all-algstrap`` target (i.e., strap-0, strap-1, and
strap-2 are complete, and all .spad files have been extracted).

Usage: python config/compute-algebra-layers.py <build_dir> [start_layer]
           --target-triple <triple> [options]

Arguments:
  build_dir      Path to the CMake binary directory (e.g. build-debug-sbcl)
  start_layer    Layer number to start from (default: 0)

Required options:
  --target-triple  Target triple (e.g. AMD64-pc-windows,
                   x86_64-unknown-linux-gnu).  This is the value of
                   OA_TARGET_TRIPLE from the CMake configuration and
                   determines the subdirectory layout under the build dir.

Options:
  --output-dir   Directory for L*_passed.txt / L*_failed.txt files.
                 Default: <build_dir>/layer-results
  --clean        Remove all prior layer result files before starting.

Example:
  python config/compute-algebra-layers.py build-debug-sbcl 0 \\
      --target-triple AMD64-pc-windows --clean
"""

from __future__ import annotations

import argparse
import os
import re
import shutil
import subprocess
import sys
from collections import Counter

# -- Path to the CMake file that defines algebra layer variables.
#    Relative to the repository root (the script's expected working directory).
CMAKE_FILE: str = "cmake/OpenAxiomAlgebraLayers.cmake"

# -- Maximum number of layers before the script gives up.
MAX_LAYERS: int = 50

# ---------------------------------------------------------------------------
# Global path variables, initialized by setup_paths().
# ---------------------------------------------------------------------------

# -- Absolute path to the CMake binary directory.
BUILD_DIR: str = ""

# -- Path to the algebra subdirectory of the build dir, containing
#    extracted .spad files, initdb.fasl, strap-0/1/2 directories,
#    and per-constructor .NRLIB output directories.
ALGEBRA_DIR: str = ""

# -- Path to the staging directory where the SPAD compiler looks for
#    already-compiled algebra FASLs during compilation.  Located at
#    <build_dir>/<target_triple>/algebra/.  This directory is cleaned
#    and selectively populated before each layer computation to ensure
#    only the intended FASLs are visible.
STAGING_DIR: str = ""

# -- Path to the open-axiom driver executable.
DRIVER: str = ""

# -- Path to the interpsys Lisp image executable, passed to the driver
#    via --execpath.
INTERPSYS: str = ""

# -- Path to the system directory (<build_dir>/<target_triple>/),
#    passed to the driver via --system.
SYSTEM: str = ""

# -- Path to the initial constructor database (initdb.fasl), produced
#    by the initdb build step from all .spad files.
INITDB: str = ""

# -- Path to the system algebra database directory (src/share/algebra/),
#    containing .daase files with pre-built constructor metadata.
SYSDB: str = ""

# -- Path to the strap-2 directory, used as the --strap argument.
#    Strap-2 is the final bootstrap stage; constructors in the layer
#    compilation can import types from strap-2 without needing them
#    to be in the staging directory.
STRAP2: str = ""

# -- Directory where per-layer result files (L*_passed.txt,
#    L*_failed.txt) are written.
OUTPUT_DIR: str = ""


def setup_paths(build_dir: str, output_dir: str | None, target_triple: str) -> None:
    """Initialize all global path variables from the command-line arguments.

    Parameters
    ----------
    build_dir : str
        Path to the CMake binary directory.
    output_dir : str or None
        Explicit output directory for result files, or None to use
        the default (<build_dir>/layer-results).
    target_triple : str
        The OA_TARGET_TRIPLE value (e.g. "AMD64-pc-windows").
    """
    global BUILD_DIR, ALGEBRA_DIR, STAGING_DIR, DRIVER, INTERPSYS
    global SYSTEM, INITDB, SYSDB, STRAP2, OUTPUT_DIR

    BUILD_DIR = os.path.abspath(build_dir)
    ALGEBRA_DIR = os.path.join(BUILD_DIR, "algebra")
    STAGING_DIR = os.path.join(BUILD_DIR, target_triple, "algebra")
    exe_ext = ".exe" if sys.platform == "win32" else ""
    DRIVER = os.path.join(BUILD_DIR, "src", "open-axiom" + exe_ext)
    INTERPSYS = os.path.join(BUILD_DIR, "interp", "interpsys" + exe_ext)
    SYSTEM = os.path.join(BUILD_DIR, target_triple)
    INITDB = os.path.join(ALGEBRA_DIR, "initdb.fasl")
    SYSDB = os.path.abspath(os.path.join("src", "share", "algebra"))
    STRAP2 = os.path.join(ALGEBRA_DIR, "strap-2")
    OUTPUT_DIR = (os.path.abspath(output_dir) if output_dir
                  else os.path.join(BUILD_DIR, "layer-results"))
    os.makedirs(OUTPUT_DIR, exist_ok=True)

    print(f"Target triple   : {target_triple}")


def layer_passed_file(n: int) -> str:
    """Return the path to the file listing constructors that passed in layer N.

    Each file contains one constructor abbreviation per line.
    """
    return os.path.join(OUTPUT_DIR, f"L{n}_passed.txt")


def layer_failed_file(n: int) -> str:
    """Return the path to the file listing constructors that failed in layer N.

    These constructors become the candidates for layer N+1.
    """
    return os.path.join(OUTPUT_DIR, f"L{n}_failed.txt")


def clean_staging_dir() -> int:
    """Remove every .fasl file from the staging directory.

    This is the critical step that prevents stale FASLs from masking
    real inter-constructor dependencies.  Without this, a constructor
    may appear compilable because a FASL from a prior (unrelated) build
    run is still present, even though that constructor's true dependency
    has not yet been staged for the current layer computation.

    Returns the number of files removed.
    """
    removed = 0
    if os.path.isdir(STAGING_DIR):
        for f in os.listdir(STAGING_DIR):
            if f.endswith(".fasl"):
                os.remove(os.path.join(STAGING_DIR, f))
                removed += 1
    print(f"  Cleaned {removed} stale FASLs from staging directory")
    return removed


def stage_fasls(passed_file: str) -> int:
    """Copy compiled FASLs for passed constructors into the staging directory.

    For each constructor abbreviation listed in ``passed_file``, copies
    both the main FASL (<CTOR>.NRLIB/code.fasl -> <CTOR>.fasl) and, if
    present, the default-package FASL (<CTOR>-.NRLIB/code.fasl ->
    <CTOR>-.fasl).  Default packages are companion packages automatically
    generated when a category is compiled; they carry the "-" suffix.

    Parameters
    ----------
    passed_file : str
        Path to a text file with one constructor abbreviation per line.

    Returns the number of FASL files copied.
    """
    passed = open(passed_file).read().split()
    copied = 0
    for ctor in passed:
        for name in [ctor, ctor + "-"]:
            src = os.path.join(ALGEBRA_DIR, f"{name}.NRLIB", "code.fasl")
            if os.path.exists(src):
                dst = os.path.join(STAGING_DIR, f"{name}.fasl")
                shutil.copy2(src, dst)
                copied += 1
    print(f"  Staged {copied} FASLs from {os.path.basename(passed_file)}")
    return copied


def compile_one(ctor: str) -> tuple[bool, str]:
    """Attempt to compile a single constructor in isolation.

    Invokes the open-axiom driver with the SPAD compiler to compile the
    extracted .spad file for the given constructor abbreviation.  The
    compilation uses:
      - --strap=strap-2 (bootstrap types are always available)
      - --system=<target_triple dir> (staging directory for layer FASLs)
      - --optimize=3 (match the production build settings)

    Parameters
    ----------
    ctor : str
        Constructor abbreviation (e.g. "INT", "SETCAT", "ANY1").

    Returns
    -------
    (success, reason) : (bool, str)
        success is True if the compiler exited with code 0.
        reason is a short description on failure ("timeout", "no .spad file",
        or an exception message); empty string on success.
    """
    spad_file = os.path.join(ALGEBRA_DIR, f"{ctor}.spad")
    if not os.path.exists(spad_file):
        return False, "no .spad file"
    cmd = [
        DRIVER, f"--execpath={INTERPSYS}", f"--system={SYSTEM}",
        f"--initial-db={INITDB}", f"--sysdb={SYSDB}",
        "--system-algebra", "--compile", f"--strap={STRAP2}",
        "--optimize=3", spad_file
    ]
    try:
        result = subprocess.run(cmd, capture_output=True, text=True,
                                timeout=120, cwd=ALGEBRA_DIR)
        return result.returncode == 0, ""
    except subprocess.TimeoutExpired:
        return False, "timeout"
    except Exception as e:
        return False, str(e)


def get_all_constructors() -> list[str]:
    """Return a sorted list of all constructor abbreviations.

    Scans the algebra build directory for .spad files and extracts the
    abbreviation (filename without extension).  This is the initial
    candidate set for layer 0.
    """
    ctors = []
    for f in sorted(os.listdir(ALGEBRA_DIR)):
        if f.endswith(".spad"):
            ctors.append(f[:-5])
    return ctors


def compute_layer(layer_num: int) -> tuple[list[str], list[str]]:
    """Compute which constructors can be compiled at layer N.

    This is the core of the algorithm:

    1. Clean the staging directory (remove all .fasl files).
    2. Stage FASLs from layers 0..N-1 (for layer 0, nothing is staged).
    3. Determine the candidate list:
       - Layer 0: all constructors (from .spad files).
       - Layer N>0: constructors that failed in layer N-1.
    4. Try compiling each candidate.  Successful ones are recorded in
       L<N>_passed.txt; failures go to L<N>_failed.txt.

    Parameters
    ----------
    layer_num : int
        The layer number to compute.

    Returns
    -------
    (passed, failed) : (list[str], list[str])
        Lists of constructor abbreviations that passed or failed.
    """
    print(f"\n{'=' * 60}")
    print(f"Computing L{layer_num}")
    print(f"{'=' * 60}")

    clean_staging_dir()

    if layer_num > 0:
        print(f"Staging L0..L{layer_num-1} FASLs to {STAGING_DIR}")
        total_staged = 0
        for i in range(layer_num):
            total_staged += stage_fasls(layer_passed_file(i))
        print(f"  Total staged: {total_staged} FASLs")
    else:
        print("Layer 0: no prior FASLs to stage (strap-2 only)")

    # Determine candidates.
    if layer_num == 0:
        candidates = get_all_constructors()
    else:
        prev_failed = layer_failed_file(layer_num - 1)
        candidates = open(prev_failed).read().split()

    print(f"\nConstructors to test: {len(candidates)}")
    sys.stdout.flush()

    passed = []
    failed = []
    for i, ctor in enumerate(candidates):
        ok, reason = compile_one(ctor)
        status = "OK" if ok else f"FAIL ({reason})" if reason else "FAIL"
        print(f"[{i+1:4d}/{len(candidates)}] {ctor:20s} {status}")
        sys.stdout.flush()
        if ok:
            passed.append(ctor)
        else:
            failed.append(ctor)

    print(f"\nL{layer_num}: {len(passed)} passed, {len(failed)} failed")

    with open(layer_passed_file(layer_num), "w") as f:
        f.write('\n'.join(passed))
    with open(layer_failed_file(layer_num), "w") as f:
        f.write('\n'.join(failed))

    return passed, failed


def update_cmake(layer_num: int) -> None:
    """Update cmake/OpenAxiomAlgebraLayers.cmake with the results of layer N.

    Reads the L<N>_passed.txt file and rewrites the corresponding
    ``set(oa_algebra_layer_<N> ...)`` block in the CMake file.  If the
    layer variable does not yet exist, it is created before the
    default-packages section.

    Constructors that were previously in layer N but are NOT in the new
    passed list ("displaced" constructors) are moved to layer N+1.
    Constructors newly placed in layer N are removed from any higher
    layer they may have been assigned to, preventing duplicates.

    After writing, the function verifies that no constructor appears
    in more than one layer and prints a per-layer summary.

    Parameters
    ----------
    layer_num : int
        The layer number whose results should be written to CMake.
    """
    passed_file = layer_passed_file(layer_num)
    passed = set(open(passed_file).read().split())
    if not passed:
        print(f"No constructors passed for L{layer_num}, skipping CMake update")
        return

    print(f"\nUpdating CMake layer {layer_num} with {len(passed)} constructors")

    with open(CMAKE_FILE) as f:
        content = f.read()

    # Find the existing set(oa_algebra_layer_<N> ...) block.
    layer_match = re.search(
        r'set\(oa_algebra_layer_' + str(layer_num) + r'\n(.*?)\n\)',
        content, re.DOTALL
    )
    if not layer_match:
        # Layer variable does not exist yet; create it before the
        # default-packages section.
        insert_marker = (
            '# ===========================================================================\n'
            '# Known default-package producers'
        )
        if insert_marker not in content:
            print(f"WARNING: Could not find insertion point for layer {layer_num}")
            return
        new_section = (
            f"\n\n# ---------------------------------------------------------------------------\n"
            f"# Layer {layer_num} -- Constructors compilable with L0..L{layer_num-1} FASLs staged\n"
            f"# ---------------------------------------------------------------------------\n\n"
            f"set(oa_algebra_layer_{layer_num}\n\n)\n\n"
        )
        content = content.replace(insert_marker, new_section + insert_marker)
        layer_match = re.search(
            r'set\(oa_algebra_layer_' + str(layer_num) + r'\n(.*?)\n\)',
            content, re.DOTALL
        )
        if not layer_match:
            print(f"WARNING: Still could not find oa_algebra_layer_{layer_num}")
            return

    # Determine which constructors were previously in this layer but
    # are no longer (they need to be moved to the next layer).
    old_body = layer_match.group(1)
    old_ctors = set()
    for line in old_body.split('\n'):
        s = line.strip()
        if s and not s.startswith('#'):
            old_ctors.update(s.split())

    displaced = old_ctors - passed
    if displaced:
        print(f"  Displaced from layer {layer_num}: {len(displaced)} -> {sorted(displaced)}")

    # Format the new layer body: sorted, 8 constructors per line.
    sorted_passed = sorted(passed)
    lines = []
    chunk = []
    for ctor in sorted_passed:
        chunk.append(ctor)
        if len(chunk) >= 8:
            lines.append("  " + " ".join(chunk))
            chunk = []
    if chunk:
        lines.append("  " + " ".join(chunk))

    new_body = "\n".join(lines)
    old_full = layer_match.group(0)
    new_full = f"set(oa_algebra_layer_{layer_num}\n{new_body}\n)"
    content = content.replace(old_full, new_full)

    # Remove newly-placed constructors from any higher layer, and
    # add displaced constructors to layer N+1.
    for lnum in range(layer_num + 1, MAX_LAYERS):
        pattern = re.compile(
            r'(set\(oa_algebra_layer_' + str(lnum) + r'\n)(.*?)(\n\))',
            re.DOTALL
        )
        match = pattern.search(content)
        if not match:
            continue

        body = match.group(2)
        new_lines = []
        for line in body.split('\n'):
            stripped = line.strip()
            if not stripped or stripped.startswith('#'):
                new_lines.append(line)
                continue
            ctors = stripped.split()
            remaining = [c for c in ctors if c not in passed]
            if remaining:
                new_lines.append("  " + " ".join(remaining))

        if lnum == layer_num + 1 and displaced:
            new_lines.append("  " + " ".join(sorted(displaced)))

        new_body_l = "\n".join(new_lines)
        new_section = match.group(1) + new_body_l + match.group(3)
        content = content[:match.start()] + new_section + content[match.end():]

    # Update the layer's comment header.
    old_comment_pattern = re.compile(
        r'# Layer ' + str(layer_num) + r' -- .*'
    )
    content = old_comment_pattern.sub(
        f'# Layer {layer_num} -- Constructors compilable with L0..L{layer_num-1} FASLs staged',
        content
    )

    with open(CMAKE_FILE, 'w') as f:
        f.write(content)

    # Verification pass: count constructors per layer and check for
    # duplicates across all layers.
    total = 0
    all_ctors = []
    for n in range(MAX_LAYERS):
        m = re.search(
            r'set\(oa_algebra_layer_' + str(n) + r'\n(.*?)\n\)',
            content, re.DOTALL
        )
        if m:
            ctors = []
            for line in m.group(1).split('\n'):
                s = line.strip()
                if s and not s.startswith('#'):
                    ctors.extend(s.split())
            total += len(ctors)
            all_ctors.extend(ctors)
            if ctors:
                print(f"  L{n:2d}: {len(ctors):4d}")

    print(f"  Total: {total}")
    dupes = {k: v for k, v in Counter(all_ctors).items() if v > 1}
    if dupes:
        print(f"  WARNING: Duplicates: {dupes}")
    else:
        print(f"  No duplicates. OK.")


def main() -> None:
    """Entry point: parse arguments, then iterate layers to convergence."""
    parser = argparse.ArgumentParser(
        description="Compute algebra layer assignments by isolated compilation.")
    parser.add_argument("build_dir",
        help="Path to the CMake binary directory (e.g. build-debug-sbcl)")
    parser.add_argument("start_layer", nargs="?", type=int, default=0,
        help="Layer number to start from (default: 0)")
    parser.add_argument("--output-dir",
        help="Directory for L*_passed/failed.txt files "
             "(default: <build_dir>/layer-results)")
    parser.add_argument("--target-triple", required=True,
        help="Target triple, e.g. AMD64-pc-windows, "
             "x86_64-unknown-linux-gnu")
    parser.add_argument("--clean", action="store_true",
        help="Remove all prior layer result files before starting")
    args = parser.parse_args()

    setup_paths(args.build_dir, args.output_dir, args.target_triple)

    # Optionally remove prior results to guarantee a fresh computation.
    if args.clean:
        if os.path.isdir(OUTPUT_DIR):
            for f in os.listdir(OUTPUT_DIR):
                if f.startswith("L") and f.endswith(".txt"):
                    os.remove(os.path.join(OUTPUT_DIR, f))
            print(f"Cleaned prior results from {OUTPUT_DIR}")

    print(f"Build directory : {BUILD_DIR}")
    print(f"Output directory: {OUTPUT_DIR}")
    print(f"Staging directory: {STAGING_DIR}")

    # Ensure the staging algebra directory is completely empty before
    # starting -- remove ALL files, not just .fasl, to guarantee no
    # stale artifacts from prior builds influence the computation.
    if os.path.isdir(STAGING_DIR):
        removed = 0
        for f in os.listdir(STAGING_DIR):
            p = os.path.join(STAGING_DIR, f)
            if os.path.isfile(p):
                os.remove(p)
                removed += 1
            elif os.path.isdir(p):
                shutil.rmtree(p)
                removed += 1
        print(f"Wiped {removed} entries from staging directory")
    else:
        os.makedirs(STAGING_DIR, exist_ok=True)
        print(f"Created staging directory")

    start = args.start_layer
    layer = start

    # If the start layer's results already exist, skip straight to
    # updating CMake and continue from the next layer.
    passed_file = layer_passed_file(start)
    if os.path.exists(passed_file):
        update_cmake(layer)
    else:
        # For layer > 0, we need the previous layer's failed list
        # as input.  For layer 0, get_all_constructors() provides
        # the initial candidate set.
        if layer > 0:
            prev_failed = layer_failed_file(start - 1)
            if not os.path.exists(prev_failed):
                print(f"ERROR: Neither {passed_file} nor {prev_failed} exist.")
                sys.exit(1)
            remaining = len(open(prev_failed).read().split())
            if remaining == 0:
                print(f"\nAll constructors placed! Convergence at L{start-1}.")
                return

        passed, failed = compute_layer(layer)
        if len(passed) == 0:
            print(f"\nNo new constructors passed at L{layer}. Convergence reached.")
            print(f"Remaining {len(failed)} constructors cannot be compiled.")
            for c in failed:
                print(f"  {c}")
            return
        update_cmake(layer)

    # Main loop: compute successive layers until convergence.
    while True:
        layer += 1
        if layer > MAX_LAYERS:
            print(f"\nReached layer {MAX_LAYERS} limit. Stopping.")
            break

        prev_failed = layer_failed_file(layer - 1)
        remaining = len(open(prev_failed).read().split())
        if remaining == 0:
            print(f"\nAll constructors placed! Convergence at L{layer-1}.")
            break

        passed, failed = compute_layer(layer)

        if len(passed) == 0:
            print(f"\nNo new constructors passed at L{layer}. Convergence reached.")
            print(f"Remaining {len(failed)} constructors cannot be compiled.")
            print(f"\nFinal unresolved ({len(failed)}):")
            for c in failed:
                print(f"  {c}")
            break

        update_cmake(layer)
        print(f"\nCumulative placed: L0-L{layer}")

    # Print final summary across all layers.
    print(f"\n{'=' * 60}")
    print("FINAL LAYER SUMMARY")
    print(f"{'=' * 60}")
    total_placed = 0
    for i in range(min(layer + 1, MAX_LAYERS + 1)):
        pf = layer_passed_file(i)
        if os.path.exists(pf):
            n = len(open(pf).read().split())
            total_placed += n
            print(f"  L{i:2d}: {n:4d} constructors")
    ff = layer_failed_file(layer)
    if os.path.exists(ff):
        nf = len(open(ff).read().split())
        print(f"  Unresolved: {nf}")
    print(f"  Total placed: {total_placed}")


if __name__ == "__main__":
    main()
