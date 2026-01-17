#!/bin/bash
cd /Users/udi/GLP/glp_runtime
dart test test/analysis/type_checker/ 2>&1 | tee /Users/udi/GLP/type_test_output.txt
echo "Output saved to /Users/udi/GLP/type_test_output.txt"
