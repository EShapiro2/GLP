// test/analysis/type_checker/test_helpers.dart
//
// Helper functions for type checker tests

import 'package:glp_runtime/analysis/type_checker/type_checker.dart';

/// Parse and type-check GLP source code
///
/// Uses the unified parser to handle type definitions, procedure declarations,
/// and clauses in a single pass.
TypeCheckResult checkTypes(String source) {
  return checkSource(source);
}
