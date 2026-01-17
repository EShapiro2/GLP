#!/bin/bash
# Test if underscore is actually being rejected

cd /Users/udi/Grassroots/GLP/glp_runtime

echo "Testing bubble_sort.glp..."
dart run bin/check_types.dart ../programs/typed_book/recursive/list_processing/bubble_sort.glp

echo ""
echo "Testing cooperative.glp..."  
dart run bin/check_types.dart ../programs/typed_book/streams/producers_consumers/cooperative.glp

echo ""
echo "Testing sum_list.glp..."
dart run bin/check_types.dart ../programs/typed_book/recursive/arithmetic_trees/sum_list.glp
