/// A doubled quote inside a quoted name.  GLP-Spec appendix-lp.tex, Definition
/// "Logic Programs Syntax": "We employ standard LP notions"; standard LP
/// syntax doubles the quote inside a quoted name, `'it''s'`, as well as
/// escaping it, `'it\'s'`, and the reader accepts both, as the one string
/// `it's` (GLP #3 Cowork, 2026-10-04 15:19 UTC, NOTED).  Until 2026-10-07
/// `'it''s'` read as the two names `it` and `s`, a syntax error in a term.
library;

import 'dart:io';

import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/token.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:test/test.dart';

final _root = File('../programs/self.glp').absolute.path;

/// The values of the arguments of `t(...)` as the reader reads [args].
List<Object?> _values(String args) => [
      for (final a in Parser(Lexer('t($args).').tokenize())
          .parse()
          .procedures
          .single
          .clauses
          .single
          .head
          .args)
        (a as ConstTerm).value
    ];

void main() {
  test("'it''s' is one name, it's", () {
    final tokens = Lexer("'it''s'").tokenize();
    expect(tokens.map((t) => t.type),
        [TokenType.ATOM, TokenType.EOF]);
    expect(tokens.first.lexeme, "it's");
  });

  test('the doubled and the escaped quote read as the one string', () {
    expect(_values(r"'it''s', 'it\'s'"), ["it's", "it's"]);
  });

  test('doubled quotes at either end and in a row', () {
    expect(_values("'''', '''a', 'a''', 'a''''b', ''"),
        ["'", "'a", "a'", "a''b", '']);
  });

  test('a doubled quote is a quoted name\'s only: two names apart stay two',
      () {
    expect(_values("'a', 'b'"), ['a', 'b']);
  });

  test("a goal reads 'it''s' as it's, =?= 'it\\'s'", () async {
    final e = GlpEngine(rootSelfGlpPath: _root);
    expect(
        e.loadSource('''
exported procedure eq(_?, _?, Constant).
eq(X, Y, yes) :- X? =?= Y? | true.
eq(_, _, no) :- otherwise | true.
''', filename: 'doubled_quote.glp'),
        isTrue);
    final r = await e.runGoal(r"eq('it''s', 'it\'s', R), X = 'it''s'");
    expect(r.error, isNull);
    expect(r.status, ExecutionStatus.succeeded);
    expect((r.bindings['R'] as rt.ConstTerm).value, 'yes');
    expect((r.bindings['X'] as rt.ConstTerm).value, "it's");
  });
}
