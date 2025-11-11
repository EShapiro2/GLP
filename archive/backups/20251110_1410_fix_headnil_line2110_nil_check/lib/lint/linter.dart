import '../bytecode/opcodes.dart';
import '../bytecode/runner.dart';

class LintIssue {
  final String code;
  final String message;
  final int index; // op index in program
  LintIssue(this.code, this.message, this.index);
  @override
  String toString() => '[$code] @op#$index: $message';
}

class LintResult {
  final List<LintIssue> issues;
  LintResult(this.issues);
  bool get ok => issues.isEmpty;
}

class Linter {
  /// Lints a single predicate/program.
  ///
  /// Checks (subset of compiler contract, semantics-preserving):
  ///  - BODY_BEFORE_COMMIT: No body ops (BodySet*) before Commit in a clause.
  ///  - ILLEGAL_PRECOMMIT_OP: Only head/guard ops allowed pre-commit within clause.
  ///  - SUSPEND_ONCE_AT_END: Exactly one predicate-level SuspendEnd, after clauses.
  LintResult lint(BytecodeProgram p) {
    final issues = <LintIssue>[];

    var inClause = false;
    var inBody = false;
    var seenSuspendEnd = false;
    var suspendEndIndex = -1;

    bool isHeadGuardOp(Op op) =>
        op is ClauseTry ||
        op is HeadBindWriter ||
        op is GuardNeedReader ||
        op is GuardFail ||
        op is UnionSiAndGoto ||
        op is ResetAndGoto ||
        op is Label;

    for (var i = 0; i < p.ops.length; i++) {
      final op = p.ops[i];

      if (op is Label) continue;

      // After a final SuspendEnd, no new clauses are allowed.
      if (seenSuspendEnd) {
        if (op is ClauseTry) {
          issues.add(LintIssue(
              'SUSPEND_ONCE_AT_END',
              'ClauseTry found after SuspendEnd; end-of-predicate suspend must be last predicate-level action.',
              i));
        }
        continue;
      }

      // Predicate level (not inside a clause)
      if (!inClause) {
        if (op is ClauseTry) {
          inClause = true;
          inBody = false;
          continue;
        }
        if (op is SuspendEnd) {
          seenSuspendEnd = true;
          suspendEndIndex = i;
          continue;
        }
        // Other ops are illegal at predicate level.
        issues.add(LintIssue(
            'ILLEGAL_PRECOMMIT_OP',
            'Operation not allowed at predicate level outside a clause: ${op.runtimeType}',
            i));
        continue;
      }

      // Inside a clause:
      if (!inBody) {
        // Head/Guard phase
        if (op is Commit) {
          inBody = true;
          continue;
        }
        if (op is UnionSiAndGoto || op is ResetAndGoto) {
          // Clause ends; next clause (if any) must start with ClauseTry.
          inClause = false;
          inBody = false;
          continue;
        }
        if (op is SuspendEnd) {
          // Predicate-level suspend encountered (after finishing clauses)
          seenSuspendEnd = true;
          suspendEndIndex = i;
          inClause = false;
          inBody = false;
          continue;
        }
        // Body ops before commit are illegal
        if (op is BodySetConst || op is BodySetStructConstArgs || op is Proceed) {
          issues.add(LintIssue('BODY_BEFORE_COMMIT',
              'Body operation ${op.runtimeType} appears before Commit in this clause.', i));
          continue;
        }
        // Only head/guard ops are allowed pre-commit
        if (!isHeadGuardOp(op)) {
          issues.add(LintIssue('ILLEGAL_PRECOMMIT_OP',
              'Pre-commit operation not permitted in head/guards: ${op.runtimeType}', i));
          continue;
        }
      } else {
        // Body phase
        if (op is UnionSiAndGoto || op is ResetAndGoto) {
          issues.add(LintIssue('ILLEGAL_BODY_OP',
              'Control-flow back to clause scan inside body: ${op.runtimeType}', i));
          continue;
        }
        if (op is SuspendEnd) {
          issues.add(LintIssue('ILLEGAL_BODY_OP',
              'SuspendEnd cannot appear in a body; it is predicate-level after all clauses.', i));
          seenSuspendEnd = true;
          suspendEndIndex = i;
          inClause = false;
          inBody = false;
          continue;
        }
        if (op is Commit) {
          issues.add(LintIssue('REDUNDANT_COMMIT',
              'Commit encountered in body; compiler should not emit this.', i));
          continue;
        }
        // BodySet* and Proceed are allowed here.
      }
    }

    // Ensure SuspendEnd occurs at most once
    final suspendCount = p.ops.whereType<SuspendEnd>().length;
    if (suspendCount > 1) {
      issues.add(LintIssue('SUSPEND_ONCE_AT_END',
          'Multiple SuspendEnd opcodes found ($suspendCount). Expect a single final suspend per predicate.',
          suspendEndIndex >= 0 ? suspendEndIndex : 0));
    }

    return LintResult(issues);
  }
}
