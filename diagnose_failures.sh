#!/bin/bash
# Diagnose first 5 failing tests with detailed error messages
cd /Users/udi/Grassroots/GLP/glp_runtime
REPL="bin/glp_repl.dart"

echo "=========================================="
echo "DIAGNOSIS BATCH 1"
echo "=========================================="
echo ""

# Test 1: sum_list
echo "TEST 1: sum_list.glp"
echo "test/../programs/typed_book/recursive/arithmetic_trees/sum_list.glp
:quit" | dart run "$REPL" 2>&1 | tail -30
echo ""
echo "===================="
echo ""

# Test 2: bubble_sort
echo "TEST 2: bubble_sort.glp"
echo "test/../programs/typed_book/recursive/list_processing/bubble_sort.glp
:quit" | dart run "$REPL" 2>&1 | tail -30
echo ""
echo "===================="
echo ""

# Test 3: dl_append
echo "TEST 3: dl_append.glp"
echo "test/../programs/typed_book/recursive/list_processing/dl_append.glp
:quit" | dart run "$REPL" 2>&1 | tail -30
echo ""
echo "===================="
echo ""

# Test 4: maxlist
echo "TEST 4: maxlist.glp"
echo "test/../programs/typed_book/recursive/list_processing/maxlist.glp
:quit" | dart run "$REPL" 2>&1 | tail -30
echo ""
echo "===================="
echo ""

# Test 5: member
echo "TEST 5: member.glp"
echo "test/../programs/typed_book/recursive/list_processing/member.glp
:quit" | dart run "$REPL" 2>&1 | tail -30
echo ""
