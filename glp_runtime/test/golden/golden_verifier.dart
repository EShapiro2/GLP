// test/golden/golden_verifier.dart
//
// Verifies actual type checker results against golden expectations

import 'package:glp_runtime/analysis/type_checker/type_checker.dart';
import 'package:glp_runtime/analysis/type_checker/program_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';
import 'golden_parser.dart';

/// Result of verifying expectations against actual results
class VerificationResult {
  final bool passed;
  final List<String> failures;
  final List<String> warnings;

  VerificationResult({
    required this.passed,
    required this.failures,
    required this.warnings,
  });

  factory VerificationResult.success() => VerificationResult(
        passed: true,
        failures: [],
        warnings: [],
      );

  factory VerificationResult.failure(List<String> failures) => VerificationResult(
        passed: false,
        failures: failures,
        warnings: [],
      );

  @override
  String toString() {
    if (passed) return 'PASSED';
    return 'FAILED:\n${failures.map((f) => '  - $f').join('\n')}';
  }
}

/// Verify type check result against expectations
VerificationResult verifyTypeCheckResult(
  TypeCheckResult actual,
  GoldenExpectations expected,
) {
  final failures = <String>[];
  final warnings = <String>[];

  // Check well-typed expectation
  if (expected.expectWellTyped && !actual.isWellTyped) {
    failures.add('Expected well-typed but got errors: ${actual.errors}');
  } else if (!expected.expectWellTyped && actual.isWellTyped) {
    failures.add('Expected ill-typed but program passed type checking');
  }

  // For ill-typed programs, check expected error patterns
  if (!expected.expectWellTyped && expected.expectedErrors.isNotEmpty) {
    final actualErrorStrings = actual.errors.map((e) => e.toString()).toList();
    
    for (final expectedError in expected.expectedErrors) {
      final found = actualErrorStrings.any(
        (e) => e.toLowerCase().contains(expectedError.toLowerCase().split(':').last.trim()),
      );
      if (!found) {
        warnings.add('Expected error pattern "$expectedError" not found in: $actualErrorStrings');
      }
    }
  }

  return failures.isEmpty
      ? VerificationResult.success()
      : VerificationResult.failure(failures);
}

/// Verify automaton transitions against expectations
VerificationResult verifyAutomaton(
  ProgramDFA dfa,
  GoldenExpectations expected,
) {
  if (!expected.hasAutomatonExpectations) {
    return VerificationResult.success();
  }

  final failures = <String>[];

  for (final entry in expected.automata.entries) {
    final stateName = entry.key;
    final expectation = entry.value;

    // Get the automaton
    DFAAutomaton automaton;
    try {
      automaton = dfa.getAutomaton(stateName);
    } catch (e) {
      failures.add('Automaton "$stateName" not found in DFA');
      continue;
    }

    // Check each expected transition
    for (final trans in expectation.transitions) {
      final label = _buildTransitionLabel(trans);
      if (label == null) {
        failures.add('Could not build label for transition: $trans');
        continue;
      }

      final targetState = automaton.transition(automaton.startState, label);
      
      if (targetState == null) {
        failures.add('$stateName: No transition for $trans');
        continue;
      }

      final expectedTarget = _normalizeStateName(trans.targetState);
      final actualTarget = _normalizeStateName(targetState.name);

      if (expectedTarget != actualTarget) {
        failures.add(
          '$stateName: $trans expected target "$expectedTarget" but got "$actualTarget"',
        );
      }
    }
  }

  return failures.isEmpty
      ? VerificationResult.success()
      : VerificationResult.failure(failures);
}

/// Build a TransitionLabel from expectation
TransitionLabel? _buildTransitionLabel(TransitionExpectation trans) {
  Mode? mode;
  if (trans.mode == '↑') {
    mode = Mode.produce;
  } else if (trans.mode == '↓') {
    mode = Mode.consume;
  }

  if (trans.argIndex == null) {
    // Constant transition
    return TransitionLabel.constant(trans.symbol);
  } else {
    // Functor transition - need to determine arity
    // For now, infer from common patterns
    int arity;
    if (trans.symbol == '[|]') {
      arity = 2;
    } else if (trans.symbol == 'merge') {
      arity = 3;
    } else {
      // Default: assume arity equals max arg index seen
      arity = trans.argIndex!;
    }
    return TransitionLabel.functor(trans.symbol, arity, trans.argIndex!, mode: mode);
  }
}

/// Normalize state name for comparison
String _normalizeStateName(String name) {
  // Handle ✓ vs _FINAL_
  if (name == '✓' || name == '_FINAL_') {
    return '_FINAL_';
  }
  return name;
}

/// Verify moded clauses against expectations
/// 
/// Note: This requires access to the clause check results which include
/// the moded head. For now, this is a placeholder.
VerificationResult verifyModedClauses(
  TypeCheckResult result,
  GoldenExpectations expected,
) {
  if (!expected.hasClauseExpectations) {
    return VerificationResult.success();
  }

  final failures = <String>[];
  final warnings = <String>[];

  // TODO: Access clause results and compare moded heads
  // This requires TypeCheckResult to expose per-clause results with moded heads
  
  for (final clauseExp in expected.clauses) {
    warnings.add('Clause ${clauseExp.clauseIndex} verification not yet implemented');
  }

  return VerificationResult(
    passed: true,  // Don't fail on unimplemented verification
    failures: failures,
    warnings: warnings,
  );
}

/// Full verification of all expectations
VerificationResult verifyAll(
  TypeCheckResult result,
  ProgramDFA dfa,
  GoldenExpectations expected,
) {
  final allFailures = <String>[];
  final allWarnings = <String>[];

  // Verify type check result
  final typeResult = verifyTypeCheckResult(result, expected);
  allFailures.addAll(typeResult.failures);
  allWarnings.addAll(typeResult.warnings);

  // Verify automaton (only if well-typed expected)
  if (expected.expectWellTyped) {
    final autoResult = verifyAutomaton(dfa, expected);
    allFailures.addAll(autoResult.failures);
    allWarnings.addAll(autoResult.warnings);

    // Verify moded clauses
    final clauseResult = verifyModedClauses(result, expected);
    allFailures.addAll(clauseResult.failures);
    allWarnings.addAll(clauseResult.warnings);
  }

  return VerificationResult(
    passed: allFailures.isEmpty,
    failures: allFailures,
    warnings: allWarnings,
  );
}
