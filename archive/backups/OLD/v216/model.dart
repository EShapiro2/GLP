library glp_bytecode_v216_model;

/// Minimal term/value model for executing v2.16 bytecode.
sealed class Term {
  const Term();
}

/// Writer variable (value is set at COMMIT when σ̂w is applied).
final class Writer extends Term {
  Writer([this.value]);
  String? value;
  @override
  String toString() => 'Writer(${value ?? "?"})';
}

/// Reader variable (may be unbound; suspension uses references to these).
final class Reader extends Term {
  Reader([this.value]);
  String? value;
  bool get isBound => value != null;
  @override
  String toString() => isBound ? 'Reader($value)' : 'Reader(?)';
}

/// Ground constant.
final class Const extends Term {
  const Const(this.symbol);
  final String symbol;
  @override
  String toString() => "Const('$symbol')";
}

/// Execution outcome of a predicate call (unit goal).
enum VmOutcome { success, fail, suspend }

/// Call frame mutated while running a predicate (unit goal).
final class CallFrame {
  CallFrame(this.predicate, this.args);

  final String predicate;
  final List<Term> args;

  /// U: goal-accumulated blockers (set of Reader references).
  final Set<Reader> blockersU = <Reader>{};

  /// Si: clause-local blockers (reset by ClauseTry).
  final Set<Reader> blockersSi = <Reader>{};

  /// Whether we've committed to a clause (entering BODY).
  bool committed = false;

  /// σ̂w: tentative writer substitution computed in HEAD/GUARDS,
  /// applied atomically at COMMIT.
  final Map<int, String> pendingWrites = <int, String>{};
}
