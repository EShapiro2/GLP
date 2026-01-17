#!/bin/bash
# Run type checker on all typed_book programs
cd /Users/udi/Grassroots/GLP/glp_runtime

OUTPUT_FILE="/private/tmp/typed_book_results.txt"

echo "Type Checker Results - $(date)" > "$OUTPUT_FILE"
echo "======================================" >> "$OUTPUT_FILE"

# Find all .glp files in typed_book (excluding .disabled files)
find ../programs/typed_book -name "*.glp" | sort | while read f; do
    echo "" >> "$OUTPUT_FILE"
    echo "=== $f ===" >> "$OUTPUT_FILE"
    dart run bin/check_types.dart "$f" 2>&1 | grep -v "declared but not defined" >> "$OUTPUT_FILE"
done

echo "" >> "$OUTPUT_FILE"
echo "======================================" >> "$OUTPUT_FILE"
echo "Summary:" >> "$OUTPUT_FILE"
grep -c "well-typed" "$OUTPUT_FILE" | xargs echo "  Passed:" >> "$OUTPUT_FILE"
grep -c "Type Errors" "$OUTPUT_FILE" | xargs echo "  Failed:" >> "$OUTPUT_FILE"

echo "Done. Results in $OUTPUT_FILE"
