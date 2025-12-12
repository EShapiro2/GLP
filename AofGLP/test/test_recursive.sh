#!/bin/bash
# test_recursive.sh - Test recursive examples (arithmetic, lists, structures)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/test_helper.sh"

echo "=========================================="
echo "Testing: Recursive Examples"
echo "=========================================="

# Arithmetic Trees
echo ""
echo "--- Arithmetic Trees ---"
test_compile "$GLP_ROOT/AofGLP/recursive/arithmetic_trees/factorial.glp" "factorial.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/arithmetic_trees/fibonacci.glp" "fibonacci.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/arithmetic_trees/gcd_integer.glp" "gcd_integer.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/arithmetic_trees/exp.glp" "exp.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/arithmetic_trees/ackermann.glp" "ackermann.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/arithmetic_trees/hanoi.glp" "hanoi.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/arithmetic_trees/min.glp" "min.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/arithmetic_trees/natural_numbers.glp" "natural_numbers.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/arithmetic_trees/plus.glp" "plus.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/arithmetic_trees/primes.glp" "primes.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/arithmetic_trees/sum_list.glp" "sum_list.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/arithmetic_trees/times.glp" "times.glp compiles"

# List Processing
echo ""
echo "--- List Processing ---"
test_compile "$GLP_ROOT/AofGLP/recursive/list_processing/append.glp" "append.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/list_processing/copy.glp" "copy.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/list_processing/delete.glp" "delete.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/list_processing/is_list.glp" "is_list.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/list_processing/length.glp" "length.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/list_processing/member.glp" "member.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/list_processing/nth.glp" "nth.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/list_processing/prefix.glp" "prefix.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/list_processing/reverse.glp" "reverse.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/list_processing/translate.glp" "translate.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/list_processing/maxlist.glp" "maxlist.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/list_processing/inner_product.glp" "inner_product.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/list_processing/inner_product_iter.glp" "inner_product_iter.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/list_processing/polygon_area.glp" "polygon_area.glp compiles"

# Sorting
echo ""
echo "--- Sorting ---"
test_compile "$GLP_ROOT/AofGLP/recursive/list_processing/bubble_sort.glp" "bubble_sort.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/list_processing/insertion_sort.glp" "insertion_sort.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/list_processing/merge_ordered.glp" "merge_ordered.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/list_processing/merge_sort.glp" "merge_sort.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/list_processing/quicksort.glp" "quicksort.glp compiles"

# Structure Processing
echo ""
echo "--- Structure Processing ---"
test_compile "$GLP_ROOT/AofGLP/recursive/structure_processing/ancestor.glp" "ancestor.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/structure_processing/binary_tree.glp" "binary_tree.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/structure_processing/distribute_nonground.glp" "distribute_nonground.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/structure_processing/heapify.glp" "heapify.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/structure_processing/substitute.glp" "substitute.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/structure_processing/traversals.glp" "traversals.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/structure_processing/observe.glp" "observe.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/structure_processing/observe_minimal.glp" "observe_minimal.glp compiles"
test_compile "$GLP_ROOT/AofGLP/recursive/structure_processing/observe_play.glp" "observe_play.glp compiles"

print_summary "Recursive Examples"
exit $?
