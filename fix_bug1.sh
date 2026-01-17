#!/bin/bash
# Fix Bug 1: Atoms are strings per paper Section 4.1

TARGET="/Users/udi/Grassroots/GLP/glp_runtime/lib/analysis/type_checker/well_typed_term.dart"

# Create backup
cp "$TARGET" "$TARGET.bak"

# Apply fix - change the last return statement in _pathStepToLeafTerm
# FROM: return LeafTerm.constant(value, mode: step.mode);
# TO: return LeafTerm.stringConstant(value, mode: step.mode);

sed -i '' 's/\/\/ Otherwise it'"'"'s an atom\/constant$/\/\/ Fix Bug 1: Atoms (unquoted identifiers) are strings per paper Section 4.1\n    \/\/ Paper line 49: "String ::= ... ; a ; b ; ... ; foo ; bar ; ..."\n    \/\/ Therefore atoms like '"'"'user'"'"', '"'"'net'"'"', '"'"'foo'"'"' etc. are strings/' "$TARGET"

sed -i '' 's/return LeafTerm\.constant(value, mode: step\.mode);$/return LeafTerm.stringConstant(value, mode: step.mode);/' "$TARGET"

echo "Bug 1 fix applied to $TARGET"
echo "Backup saved as $TARGET.bak"
