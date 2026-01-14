#!/bin/bash
cd /Users/udi/GLP
chmod +x ./test/full_run_repl_tests.sh
./test/full_run_repl_tests.sh 2>&1 | tee /tmp/main_repl_test_output.txt
echo ""
echo "Output saved to /tmp/main_repl_test_output.txt"
