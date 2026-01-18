#!/bin/bash
# Test all paper examples from Appendix A
cd /Users/udi/Grassroots/GLP/glp_runtime

echo "Testing Paper Examples (Appendix A)"
echo "===================================="

PASS=0
FAIL=0

for f in ../programs/paper/*.glp; do
    echo ""
    echo "=== $(basename $f) ==="
    OUTPUT=$(dart run bin/check_types.dart "$f" 2>&1)
    echo "$OUTPUT"
    
    if echo "$OUTPUT" | grep -q "well-typed"; then
        PASS=$((PASS + 1))
    else
        FAIL=$((FAIL + 1))
    fi
done

echo ""
echo "===================================="
echo "Summary: $PASS passed, $FAIL failed"
