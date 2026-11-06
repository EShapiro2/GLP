/// Tests for the body kernels `'_string_concat'`/2 and `'_term_to_string'`/2,
/// behind the kernel predicates string_concat/2 and term_to_string/2.
///
/// Spec: GLP-Spec appendix-guards.tex, "Text (string_concat/2,
/// term_to_string/2)" (GLP-ICLP-2026 ca3c8e5): "string_concat(Ss, S) assigns S
/// the string of the characters of the strings of the list Ss, one after
/// another; the empty list contributes the characters []", its kernel
/// aborting "where an element is not a string"; and "term_to_string(T, S)
/// assigns S a text that the reader reads as the ground term T ... so that a
/// term has one text at every agent".  Each text is read back here by the
/// reader (Lexer, Parser) and compared with the term, node by node.
library;

import 'package:glp_runtime/compiler/ast.dart' as ast;
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/runtime/body_kernels.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:test/test.dart';

Term _list(List<Term> items, [Term? tail]) {
  Term t = tail ?? ConstTerm(nil);
  for (var i = items.length - 1; i >= 0; i--) {
    t = StructTerm('.', [items[i], t]);
  }
  return t;
}

/// The kernel [name]/2 on [input], in a fresh runtime: its result and the
/// string it assigns, null where it aborted.
(BodyKernelResult, String?) _run(String name, Term input) {
  final rt = GlpRuntime();
  final kernel = rt.bodyKernels.lookup(name, 2);
  expect(kernel, isNotNull, reason: '$name/2 should be registered');
  final (w, _) = rt.heap.allocateVariable();
  final r = kernel!(rt, [input, VarRef(w)]);
  if (r != BodyKernelResult.success) return (r, null);
  final v = rt.heap.getValue(w);
  expect(v, isA<ConstTerm>());
  return (r, (v as ConstTerm).value as String);
}

String _text(Term t) {
  final (r, s) = _run('_term_to_string', t);
  expect(r, BodyKernelResult.success);
  return s!;
}

/// The term the reader reads [text] as, the argument of a unit clause.
ast.Term _read(String text) => Parser(Lexer('t($text).').tokenize())
    .parseModule()
    .procedures
    .single
    .clauses
    .single
    .head
    .args
    .single;

/// Whether the term [a] read by the reader is the runtime term [b].
bool _same(ast.Term? a, Term b) {
  if (a == null) return b is ConstTerm && b.value == nil;
  if (a is ast.ListTerm) {
    if (a.isNil) return b is ConstTerm && b.value == nil;
    if (b is! StructTerm || b.functor != '.' || b.args.length != 2) return false;
    return _same(a.head, b.args[0]) && _same(a.tail, b.args[1]);
  }
  if (a is ast.ConstTerm) {
    if (b is! ConstTerm) return false;
    final av = a.value, bv = b.value;
    return av.runtimeType == bv.runtimeType && av == bv;
  }
  if (a is ast.StructTerm) {
    if (a.args.isEmpty) return b is ConstTerm && b.value == a.functor;
    if (b is! StructTerm ||
        b.functor != a.functor ||
        b.args.length != a.args.length) {
      return false;
    }
    for (var i = 0; i < a.args.length; i++) {
      if (!_same(a.args[i], b.args[i])) return false;
    }
    return true;
  }
  return false;
}

void main() {
  group("'_string_concat'/2", () {
    test("['Menu', '?'] gives 'Menu?'", () {
      expect(_run('_string_concat', _list([ConstTerm('Menu'), ConstTerm('?')])),
          (BodyKernelResult.success, 'Menu?'));
    });
    test('[] gives the empty string', () {
      expect(_run('_string_concat', ConstTerm(nil)),
          (BodyKernelResult.success, ''));
    });
    test('one string gives that string', () {
      expect(_run('_string_concat', _list([ConstTerm('Carol')])),
          (BodyKernelResult.success, 'Carol'));
    });
    test('an element [] contributes the characters []', () {
      expect(
          _run('_string_concat', _list([ConstTerm('X = '), ConstTerm(nil)])),
          (BodyKernelResult.success, 'X = []'));
    });
    test('aborts where an element is a number', () {
      expect(_run('_string_concat', _list([ConstTerm('a'), ConstTerm(42)])).$1,
          BodyKernelResult.abort);
      expect(_run('_string_concat', _list([ConstTerm(2.5)])).$1,
          BodyKernelResult.abort);
    });
    test('aborts where an element is a structure', () {
      expect(
          _run('_string_concat',
              _list([StructTerm('f', [ConstTerm('a')])])).$1,
          BodyKernelResult.abort);
    });
  });

  group("'_term_to_string'/2: the text reads back as the term", () {
    final cases = <String, (Term, String)>{
      'Carol': (ConstTerm('Carol'), "'Carol'"),
      'G': (ConstTerm('G'), "'G'"),
      '+': (ConstTerm('+'), "'+'"),
      "'42'": (ConstTerm('42'), "'42'"),
      'Flood on Main Street':
          (ConstTerm('Flood on Main Street'), "'Flood on Main Street'"),
      'an atom': (ConstTerm('alice'), 'alice'),
      'the empty list': (ConstTerm(nil), '[]'),
      'a structure': (
        StructTerm('utterance', [
          ConstTerm('Flood on Main Street'),
          StructTerm('f', [ConstTerm('G'), StructTerm('+', [ConstTerm(1)])])
        ]),
        "utterance('Flood on Main Street', f('G', +(1)))"
      ),
      'a list': (
        _list([ConstTerm('Carol'), ConstTerm('G'), ConstTerm(42)]),
        "['Carol', 'G', 42]"
      ),
      'a list with a tail': (
        _list([ConstTerm('a'), ConstTerm('B')], ConstTerm('C')),
        "[a, 'B' | 'C']"
      ),
      'a list with a structure tail': (
        _list([ConstTerm(1)], StructTerm('f', [ConstTerm(-3)])),
        '[1 | f(-3)]'
      ),
      'an integer': (ConstTerm(42), '42'),
      'a negative integer': (ConstTerm(-7), '-7'),
      'a real': (ConstTerm(2.5), '2.5'),
      'a real that is whole': (ConstTerm(1.0), '1.0'),
      'a negative real': (ConstTerm(-2.5), '-2.5'),
      'a small real': (ConstTerm(0.0000001), '0.0000001'),
      'a large real': (ConstTerm(1e21), '1000000000000000000000.0'),
      'a large real with digits': (ConstTerm(1.25e22), '12500000000000000000000.0'),
      'a negative argument': (StructTerm('neg', [ConstTerm(-7)]), 'neg(-7)'),
    };
    for (final c in cases.entries) {
      test('${c.key} is ${c.value.$2} and reads back', () {
        final text = _text(c.value.$1);
        expect(text, c.value.$2);
        expect(_same(_read(text), c.value.$1), isTrue,
            reason: '$text read back is not the term');
      });
    }
    test('1 and 1.0 have two texts', () {
      expect(_text(ConstTerm(1)), isNot(_text(ConstTerm(1.0))));
    });
    test('a real that is not finite aborts', () {
      expect(_run('_term_to_string', ConstTerm(double.infinity)).$1,
          BodyKernelResult.abort);
      expect(_run('_term_to_string', ConstTerm(double.nan)).$1,
          BodyKernelResult.abort);
    });
  });
}
