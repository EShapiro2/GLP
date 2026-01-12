#!/bin/bash
#
# run_typechecker_tests.sh
#
# Run all typechecker tests in positive/ and negative/ directories.
# Positive tests should pass (exit 0), negative tests should fail (exit 1).

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
POSITIVE_DIR="$PROJECT_DIR/test/programs/typechecker/positive"
NEGATIVE_DIR="$PROJECT_DIR/test/programs/typechecker/negative"

pass=0
fail=0
total=0

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "=========================================="
echo "GLP Type Checker Test Suite"
echo "=========================================="
echo ""

# Run positive tests (should pass)
echo "--- Positive Tests (should pass) ---"
if [ -d "$POSITIVE_DIR" ] && [ "$(ls -A "$POSITIVE_DIR"/*.glp 2>/dev/null)" ]; then
  for f in "$POSITIVE_DIR"/*.glp; do
    ((total++))
    filename=$(basename "$f")
    
    if result=$(cd "$PROJECT_DIR" && dart run bin/check_types.dart "$f" 2>&1); then
      echo -e "${GREEN}✓ PASS${NC}: $filename"
      ((pass++))
    else
      echo -e "${RED}✗ FAIL${NC}: $filename (expected pass)"
      echo "  Output: $result"
      ((fail++))
    fi
  done
else
  echo -e "${YELLOW}No positive tests found in $POSITIVE_DIR${NC}"
fi
echo ""

# Run negative tests (should fail)
echo "--- Negative Tests (should fail) ---"
if [ -d "$NEGATIVE_DIR" ] && [ "$(ls -A "$NEGATIVE_DIR"/*.glp 2>/dev/null)" ]; then
  for f in "$NEGATIVE_DIR"/*.glp; do
    ((total++))
    filename=$(basename "$f")
    
    if result=$(cd "$PROJECT_DIR" && dart run bin/check_types.dart "$f" 2>&1); then
      echo -e "${RED}✗ FAIL${NC}: $filename (expected fail, but passed)"
      ((fail++))
    else
      # Check that it failed for type reasons (exit code 1), not parse error (exit code 2)
      exit_code=$?
      if [ $exit_code -eq 1 ]; then
        echo -e "${GREEN}✓ PASS${NC}: $filename (correctly rejected)"
        ((pass++))
      else
        echo -e "${YELLOW}? WARN${NC}: $filename (failed with exit code $exit_code - parse error?)"
        echo "  Output: $result"
        ((fail++))
      fi
    fi
  done
else
  echo -e "${YELLOW}No negative tests found in $NEGATIVE_DIR${NC}"
fi
echo ""

# Summary
echo "=========================================="
echo "Summary: $pass/$total passed"
if [ $fail -gt 0 ]; then
  echo -e "${RED}$fail test(s) failed${NC}"
  exit 1
else
  echo -e "${GREEN}All tests passed!${NC}"
  exit 0
fi
