#!/bin/bash
cd /Users/udi/Grassroots/GLP/glp_runtime
echo "Testing fixed union_alias_basic.glp:"
dart run bin/glpc.dart --type-check test/programs/moded_types/valid/union_alias_basic.glp 2>&1
