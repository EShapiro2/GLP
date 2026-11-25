/// Tests for the body kernel `'_string_to_term'`/2, behind string_to_term/2:
/// T is the ground term the reader reads from the string S, the converse of
/// term_to_string/2 (Udi, 2026-11-25 22:52 UTC, approving it for Chess's
/// positions as text, Audit #157; GLP-Spec appendix-guards, "Text").
library;

import 'package:glp_runtime/runtime/body_kernels.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:test/test.dart';

/// Runs [name]/2 on [input] in [rt]; the result and the term assigned, null
/// where it aborted.
(BodyKernelResult, Term?) _run(GlpRuntime rt, String name, Term input) {
  final kernel = rt.bodyKernels.lookup(name, 2);
  expect(kernel, isNotNull, reason: "'$name'/2 should be registered");
  final (w, _) = rt.heap.allocateVariable();
  final r = kernel!(rt, [input, VarRef(w)]);
  if (r != BodyKernelResult.success) return (r, null);
  return (r, rt.heap.getValue(w));
}

/// The text term_to_string gives of [t].
String _text(GlpRuntime rt, Term t) {
  final (_, s) = _run(rt, '_term_to_string', t);
  return (s as ConstTerm).value as String;
}

void main() {
  group("'_string_to_term'/2", () {
    test('reads back what term_to_string prints: a position of nested lists',
        () {
      final rt = GlpRuntime();
      Term l(List<Term> xs) => xs.reversed
          .fold<Term>(ConstTerm(nil), (t, h) => StructTerm('.', [h, t]));
      final pos = StructTerm('pos', [
        l([l([ConstTerm(1), ConstTerm(2)]), l([ConstTerm(3)])]),
        ConstTerm('w'),
        StructTerm('castle', [ConstTerm('k'), ConstTerm('q')]),
        ConstTerm(-7),
        ConstTerm(2.5),
      ]);
      final s = _text(rt, pos);
      final (r, t) = _run(rt, '_string_to_term', ConstTerm(s));
      expect(r, BodyKernelResult.success);
      expect(_text(rt, t!), s);
    });

    test('a list with a tail, and the empty list', () {
      final rt = GlpRuntime();
      final (r, t) = _run(rt, '_string_to_term', ConstTerm('[a, b | c]'));
      expect(r, BodyKernelResult.success);
      expect(_text(rt, t!), '[a, b | c]');
      final (r2, t2) = _run(rt, '_string_to_term', ConstTerm(nil));
      expect(r2, BodyKernelResult.success);
      expect((t2 as ConstTerm).value, nil);
    });

    test('aborts on a text with a variable', () {
      final (r, _) = _run(GlpRuntime(), '_string_to_term', ConstTerm('f(X)'));
      expect(r, isNot(BodyKernelResult.success));
    });

    test('aborts on a text that is not one term', () {
      final rt = GlpRuntime();
      for (final s in ['a), w(b', 'f(', 'a) :- b, w(c']) {
        final (r, _) = _run(rt, '_string_to_term', ConstTerm(s));
        expect(r, isNot(BodyKernelResult.success), reason: s);
      }
    });

    test('aborts on a number', () {
      final (r, _) = _run(GlpRuntime(), '_string_to_term', ConstTerm(3));
      expect(r, isNot(BodyKernelResult.success));
    });
  });
}
