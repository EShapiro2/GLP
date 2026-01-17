#!/bin/bash
# GLP Type Checker REPL Test Suite
# Output: /Users/udi/Grassroots/GLP/test_output/typed_repl_output.txt
cd /Users/udi/Grassroots/GLP
mkdir -p test_output
chmod +x ./test/run_typechecker_repl_tests.sh
./test/run_typechecker_repl_tests.sh > /Users/udi/Grassroots/GLP/test_output/typed_repl_output.txt 2>&1
echo "Done. Output saved to /Users/udi/Grassroots/GLP/test_output/typed_repl_output.txt"
