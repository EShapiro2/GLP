#!/bin/bash
cd /Users/udi/GLP
chmod +x ./test/full_run_repl_tests.sh
./test/full_run_repl_tests.sh > /tmp/main_repl_test_output.txt 2>&1
echo "Done. Output saved to /tmp/main_repl_test_output.txt"
