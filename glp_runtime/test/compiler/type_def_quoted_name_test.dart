// glp_runtime/test/compiler/type_def_quoted_name_test.dart
//
// In a type definition a quoted name is a constant or a functor, never a type
// name (GLP #3 Cowork, 2026-10-11 12:59 UTC, "21:56").  GLP-Spec
// appendix-lp.tex, Definition "Logic Programs Syntax": a term is a variable,
// "identifiers beginning with uppercase", a constant, "numbers, strings, or
// the empty list", or a compound term f(T1, ..., Tn); a quoted name is a
// string, and so `'Odd one'(Integer)` is a structure, whatever its first
// letter.  A type name, a parameterised one among them, is written as a
// variable is, unquoted.  Until 2026-10-11 the reader took a structure whose
// functor began with a capital for a parameterised type reference, quoted or
// not: `T ::= 'Odd one'(Integer).` with `p('Odd one'(1)).` was refused,
// "Head of p is not well-typed: Inconsistent path: No transition for Odd
// one(1,1):↓ from state T?" (Code #7, 2026-10-10 21:56 UTC; measured on
// GLP-vparse 22af6536, tmp/str7vc-probe-quoted-type.txt).

import 'dart:io';

import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:test/test.dart';

/// The alternatives of the one type definition of [source].
List<TypeExpr> _alternatives(String source) =>
    Parser(Lexer(source).tokenize()).parseModule().typeDefs.single.alternatives;

/// GLP's case, the clause's argument written [arg].
String _case(String arg) => '''
T ::= 'Odd one'(Integer).
procedure p(T).
p('Odd one'($arg)).
''';

void main() {
  group('the reader', () {
    test('a quoted name before "(" is a structure\'s functor, its first '
        'letter a capital or not', () {
      final alt = _alternatives("T ::= 'Odd one'(Integer).").single;
      expect(alt, isA<StructAlt>());
      expect((alt as StructAlt).functor, 'Odd one');
      expect(alt.args.single, isA<TypeRef>());
      expect((alt.args.single as TypeRef).name, 'Integer');
    });

    test('a quoted capitalised name, with arguments or alone, is a structure '
        'or a constant, the name of a type or not', () {
      final alts = _alternatives(
          "T ::= 'Odd'(Integer) ; 'Stream'(Integer) ; 'X' ; 'Stream'.");
      expect([for (final a in alts) a.runtimeType],
          [StructAlt, StructAlt, ConstantAlt, ConstantAlt]);
      expect((alts[0] as StructAlt).functor, 'Odd');
      expect((alts[1] as StructAlt).functor, 'Stream');
      expect((alts[2] as ConstantAlt).value, 'X');
      expect((alts[3] as ConstantAlt).value, 'Stream');
    });

    test('a quoted name ending in "?" is a functor, not a dual', () {
      final alt = _alternatives("T ::= 'Odd?'(Integer).").single;
      expect(alt, isA<StructAlt>());
      expect((alt as StructAlt).functor, 'Odd?');
    });

    test('an unquoted capitalised name before "(" is still the parameterised '
        'type, its dual marked', () {
      final alts = _alternatives(
          'T ::= f(Stream(Integer)) ; g(Stream(Integer)?) ; h(Stream?(Integer)).');
      final refs = [
        for (final a in alts) (a as StructAlt).args.single as TypeRef
      ];
      expect([for (final r in refs) r.name], ['Stream', 'Stream', 'Stream']);
      expect([for (final r in refs) r.isInput], [false, true, true]);
      expect([for (final r in refs) r.typeArgs.single.toString()],
          ['Integer', 'Integer', 'Integer']);
    });
  });

  group('GLP\'s case', () {
    late GlpEngine engine;
    setUp(() => engine = GlpEngine(
        rootSelfGlpPath: File('../programs/self.glp').absolute.path));

    test("T ::= 'Odd one'(Integer) with p('Odd one'(1)) loads and runs",
        () async {
      expect(engine.loadSource(_case('1')), isTrue);
      final r = await engine.runGoal('p(X).');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      final x = r.bindings['X'] as rt.StructTerm;
      expect(x.functor, 'Odd one');
      expect((x.args.single as rt.ConstTerm).value, 1);
    });

    test("the structure's argument is checked as its type says: "
        "p('Odd one'(x)) does not load", () {
      expect(() => engine.loadSource(_case('x')), throwsA(anything));
    });
  });
}
