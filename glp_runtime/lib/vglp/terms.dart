// glp_runtime/lib/vglp/terms.dart
//
// What the two compilations of this directory share over GLP's terms: the
// walk that tells whether a variable occurs in a term, and the names fresh
// against a clause's own variables.

import '../compiler/ast.dart';

/// Whether some variable of [t] satisfies [test]: a variable by itself, a
/// structure by its arguments, a list cell by its head and its tail.
bool variableOccurs(Term t, bool Function(VarTerm v) test) {
  if (t is VarTerm) return test(t);
  if (t is StructTerm) return t.args.any((a) => variableOccurs(a, test));
  if (t is ListTerm) {
    return (t.head != null && variableOccurs(t.head!, test)) ||
        (t.tail != null && variableOccurs(t.tail!, test));
  }
  return false;
}

/// Names the compilation adds are made fresh against the clause's own
/// variables, so that a program using `Med` or `S1` itself still compiles.
class FreshNames {
  final Set<String> _taken = {};

  FreshNames(Clause c) {
    void scanTerm(Term t) {
      if (t is VarTerm) _taken.add(t.name);
      if (t is StructTerm) t.args.forEach(scanTerm);
      if (t is ListTerm) {
        if (t.head != null) scanTerm(t.head!);
        if (t.tail != null) scanTerm(t.tail!);
      }
    }

    c.head.args.forEach(scanTerm);
    for (final g in c.guards ?? const <Guard>[]) {
      g.args.forEach(scanTerm);
    }
    for (final g in c.body ?? const <Goal>[]) {
      g.args.forEach(scanTerm);
    }
    if (c.elseBranch != null) {
      for (final g in c.elseBranch!.body) {
        g.args.forEach(scanTerm);
      }
      c.elseBranch!.answer.forEach(scanTerm);
    }
    if (c.volitionGuard != null) {
      for (final q in c.volitionGuard!.question) {
        if (q.writer != null) _taken.add(q.writer!.name);
        if (q.value != null) scanTerm(q.value!);
      }
      for (final v in c.volitionGuard!.context) {
        _taken.add(v.name);
      }
    }
  }

  /// [stem] if it is fresh, else the first of stem1, stem2, ... that is.
  String fresh(String stem) {
    if (!_taken.contains(stem)) {
      _taken.add(stem);
      return stem;
    }
    var n = 1;
    while (_taken.contains('$stem$n')) {
      n++;
    }
    _taken.add('$stem$n');
    return '$stem$n';
  }
}
