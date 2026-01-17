#!/bin/bash
# GLP Main REPL Test Suite
# Output: /Users/udi/Grassroots/GLP/test_output/main_repl_output.txt
cd /Users/udi/Grassroots/GLP
mkdir -p test_output
chmod +x ./test/full_run_repl_tests.sh
./test/full_run_repl_tests.sh > /Users/udi/Grassroots/GLP/test_output/main_repl_output.txt 2>&1
echo "Done. Output saved to /Users/udi/Grassroots/GLP/test_output/main_repl_output.txt"
