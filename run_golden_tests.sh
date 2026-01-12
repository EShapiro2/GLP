#!/bin/bash
cd /Users/udi/GLP/glp_runtime
dart test test/golden_test.dart 2>&1 | tee /Users/udi/GLP/golden_test_output.txt
