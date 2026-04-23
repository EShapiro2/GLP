#!/bin/bash
# Map vs Association List Benchmark
# Compares Dart HashMap against GLP association list across 5 operations.
# Tests at 1K and 100K entry sizes.

DART=${DART:-$(which dart 2>/dev/null || echo "/home/user/dart-sdk/bin/dart")}
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GLP_DIR="$SCRIPT_DIR/../.."
GLP_RUNTIME="$GLP_DIR/glp_runtime"
MAP_TEST="$SCRIPT_DIR"

cd "$GLP_RUNTIME"

# Compile REPL snapshot
REPL_SNAPSHOT=".dart_tool/repl.dill"
if [ ! -f "$REPL_SNAPSHOT" ] || [ -n "$(find lib bin -name '*.dart' -newer "$REPL_SNAPSHOT" 2>/dev/null | head -1)" ]; then
    echo "Compiling REPL snapshot..."
    mkdir -p .dart_tool
    $DART compile kernel -o "$REPL_SNAPSHOT" bin/glp_repl.dart 2>/dev/null || true
fi
REPL="${REPL_SNAPSHOT:-bin/glp_repl.dart}"

TIMEOUT_SEC=${TIMEOUT_SEC:-60}
OPS=${OPS:-100}

# Measure wall-clock time for a GLP query. Returns seconds or "TIMEOUT".
measure() {
    python3 -c "
import subprocess, time, sys
start = time.perf_counter()
try:
    proc = subprocess.run(
        ['$DART', 'run', '$REPL'],
        input=sys.stdin.read(),
        capture_output=True, text=True,
        timeout=$TIMEOUT_SEC
    )
    elapsed = time.perf_counter() - start
    print(f'{elapsed:.3f}')
except subprocess.TimeoutExpired:
    print('TIMEOUT')
" <<HEREDOC
$MAP_TEST/test_map_vs_list.glp
$1
:quit
HEREDOC
}

# Run a benchmark: measure, subtract build baseline, format result.
# Usage: run_bench "query" "build_baseline_time"
# Prints the isolated operation time (or TIMEOUT).
run_bench() {
    local raw build
    raw=$(measure "$1")
    build="$2"
    if [ "$raw" = "TIMEOUT" ]; then
        echo "TIMEOUT"
    else
        python3 -c "print(f'{max(float(\"$raw\") - float(\"$build\"), 0.001):.3f}')"
    fi
}

# Format ratio between list and map times
format_ratio() {
    local map_t="$1" list_t="$2"
    if [ "$list_t" = "TIMEOUT" ]; then
        echo "map wins"
    elif [ "$map_t" = "TIMEOUT" ]; then
        echo "list wins"
    else
        python3 -c "
m = float('$map_t')
l = float('$list_t')
if m < 0.002 and l < 0.002:
    print('tied')
elif m < 0.002:
    print(f'map {l/max(m,0.001):,.0f}x faster')
elif l < 0.002:
    print(f'list {m/max(l,0.001):,.0f}x faster')
elif l / m > 1.5:
    print(f'map {l/m:,.0f}x faster')
elif m / l > 1.5:
    print(f'list {m/l:,.0f}x faster')
else:
    print('tied')
"
    fi
}

# Print a result row
print_row() {
    local label="$1" map_t="$2" list_t="$3" ratio="$4"
    local map_s list_s
    if [ "$map_t" = "TIMEOUT" ]; then map_s="TIMEOUT"; else map_s="${map_t}s"; fi
    if [ "$list_t" = "TIMEOUT" ]; then list_s="TIMEOUT"; else list_s="${list_t}s"; fi
    printf "  %-18s map %-10s list %-12s %s\n" "$label" "$map_s" "$list_s" "$ratio"
}

run_size() {
    local N=$1
    local K=$OPS
    local label="${2:-$N}"

    echo "=============================================================="
    echo "  ${label} entries"
    echo "=============================================================="

    # Build baselines
    local build_m build_l
    build_m=$(measure "build_map_only($N, X).")
    build_l=$(measure "build_list_only($N, X).")

    local bm_s bl_s
    if [ "$build_m" = "TIMEOUT" ]; then bm_s="TIMEOUT"; else bm_s="${build_m}s"; fi
    if [ "$build_l" = "TIMEOUT" ]; then bl_s="TIMEOUT"; else bl_s="${build_l}s"; fi
    printf "\n  %-18s map %-10s list %s\n\n" "BUILD" "$bm_s" "$bl_s"

    # Skip operation benchmarks if build itself timed out
    if [ "$build_m" = "TIMEOUT" ]; then
        echo "  (map build timed out — skipping operations)"
        echo ""
        return
    fi

    # GET x${K}
    local gm gl gr
    gm=$(run_bench "bench_map_get($N, $K, X)." "$build_m")
    gl=$(run_bench "bench_list_get($N, $K, X)." "$build_l")
    gr=$(format_ratio "$gm" "$gl")
    print_row "GET x${K}" "$gm" "$gl" "$gr"

    # CONTAINS x${K}
    local cm cl cr
    cm=$(run_bench "bench_map_has($N, $K, X)." "$build_m")
    cl=$(run_bench "bench_list_has($N, $K, X)." "$build_l")
    cr=$(format_ratio "$cm" "$cl")
    print_row "CONTAINS x${K}" "$cm" "$cl" "$cr"

    # PUT x${K}
    local pm pl pr
    pm=$(run_bench "bench_map_put($N, $K, X)." "$build_m")
    pl=$(run_bench "bench_list_put($N, $K, X)." "$build_l")
    pr=$(format_ratio "$pm" "$pl")
    print_row "PUT x${K}" "$pm" "$pl" "$pr"

    # REMOVE x${K}
    local rm rl rr
    rm=$(run_bench "bench_map_remove($N, $K, X)." "$build_m")
    rl=$(run_bench "bench_list_remove($N, $K, X)." "$build_l")
    rr=$(format_ratio "$rm" "$rl")
    print_row "REMOVE x${K}" "$rm" "$rl" "$rr"

    # KEYS x${K}
    local km kl kr_
    km=$(run_bench "bench_map_keys($N, $K, X)." "$build_m")
    kl=$(run_bench "bench_list_keys($N, $K, X)." "$build_l")
    kr_=$(format_ratio "$km" "$kl")
    print_row "KEYS x${K}" "$km" "$kl" "$kr_"

    echo ""
}

echo ""
echo "  Map vs Association List Benchmark"
echo "  ops per test: $OPS   timeout: ${TIMEOUT_SEC}s"
echo ""

run_size 1000 "1K"
run_size 100000 "100K"
