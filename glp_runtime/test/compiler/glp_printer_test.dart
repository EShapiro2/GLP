/// The printer prints the anonymous variable as it is written: `_`, and `_?`.
///
/// Until 2026-10-02 GlpPrinter printed every anonymous variable `_`, so a
/// head's `_?` came out as a writer (GLP Cowork's task of 2026-10-02 08:40
/// UTC, F); it prints each as written.  Since 2026-10-14 an anonymous reader
/// is not permitted in a clause (GLP-Spec glp.tex, Remark "Anonymous
/// Variables", 297db75; TGLP "Anonymous variables", 2424f4c): "a clause
/// writes every writer it holds, closing a stream it does not extend".  So the
/// clauses below that load and are hashed close their stream with [] and write
/// their Handle, and a clause printed with `_?` is refused on load as its
/// source is (GLP, 2026-10-14 09:35 UTC, item 2).
library;

import 'dart:io';

import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/glp_printer.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/wire/flattening.dart';
import 'package:test/test.dart';

const _rootSelf = '../programs/self.glp';

const _handle = 'Handle ::= withdraw.\n';
const _decl = 'procedure p(Integer?, Handle, Stream(Integer)).\n';

/// p's clauses as they were until 2026-10-14: `_?` at the produced Handle
/// position, `_` at the consumed Integer position.
const _anonClauses = 'p(0, _?, []).\np(_, withdraw, []).\n';

/// p's clauses: each writes the Handle it holds and closes its stream with
/// [], `_` at the consumed Integer position.
const _clauses = 'p(0, withdraw, []).\np(_, withdraw, []).\n';

/// The clauses of [source] as GlpPrinter prints them.
List<String> _printed(String source) {
  final module = Parser(Lexer(source).tokenize()).parseModule();
  final printer = GlpPrinter();
  return [
    for (final p in module.procedures)
      for (final c in p.clauses) printer.printClause(c)
  ];
}

GlpEngine _engine() =>
    GlpEngine(rootSelfGlpPath: File(_rootSelf).absolute.path);

void main() {
  test('the anonymous variable prints as written, _ and _?', () {
    expect(GlpPrinter().printTerm(UnderscoreTerm(1, 1)), '_');
    expect(GlpPrinter().printTerm(UnderscoreTerm(1, 1, isReader: true)), '_?');
  });

  test('a clause prints with its anonymous reader as written', () {
    expect(_printed('$_handle$_decl$_anonClauses'),
        ['p(0, _?, []).', 'p(_, withdraw, []).']);
  });

  test('the printed `_?` is refused on load, as the source is', () {
    final printed = _printed('$_handle$_decl$_anonClauses').join('\n');
    for (final text in [_anonClauses, printed]) {
      expect(
          () => _engine().loadSource('$_handle$_decl$text\n'),
          throwsA(predicate((Object e) => '$e'.contains(
              '_? (anonymous reader) is not permitted in program clauses'))));
    }
  });

  test('a clause closing with [] prints as written', () {
    expect(_printed('$_handle$_decl$_clauses'),
        ['p(0, withdraw, []).', 'p(_, withdraw, []).']);
  });

  test('the printed clauses load, as the source does', () {
    final printed = _printed('$_handle$_decl$_clauses').join('\n');
    expect(_engine().loadSource('$_handle$_decl$printed\n'), isTrue);
  });

  test('the canonical print carries the clause closing with []', () {
    // The program lies under the root, programs/ (TGLP modules.tex, "Scope
    // construction": "A program lies at or below the root"), so its
    // self.glp is in main.glp's scope and defines Handle there.  Until
    // 2026-10-03 it was written to the system's temporary directory, where
    // main.glp's scope had no self.glp and Handle loaded only because an
    // undefined name in a declaration was read as a type parameter.
    final dir = Directory('../programs/tests').createTempSync('glp_printer_');
    try {
      // The program's self.glp exports go/3, forwarding it to main.glp: a
      // directory with no self.glp is not a program (TGLP modules.tex, "Entry
      // and the absence of a boot module").
      File('${dir.path}/self.glp').writeAsStringSync('''
$_handle
imported procedure main#go(Integer?, Handle, Stream(Integer)).
exported procedure go(Integer?, Handle, Stream(Integer)).
go(N, H?, Out?) :- main # go(N?, H, Out).
''');
      File('${dir.path}/main.glp').writeAsStringSync('''
exported procedure go(Integer?, Handle, Stream(Integer)).
go(N, H?, Out?) :- p(N?, H, Out).

$_decl$_clauses''');
      final print = flattenProject(dir.path, rootSelfGlpPath: _rootSelf).print;
      expect(print, contains('(0, withdraw, []).'));
      expect(print, contains('(_, withdraw, []).'));
    } finally {
      dir.deleteSync(recursive: true);
    }
  });

  // A printed term reads back as itself (GLP-Spec appendix-lp.tex, Definition
  // "Logic Programs Syntax": the text denotes the term; GLP #3 Cowork,
  // 2026-10-04 09:06 UTC, "23:49. Q1 and Q3"): a constant in single quotes
  // where unquoted it would read as a variable, an operator or a number, or
  // as no one name, escaped as the reader reads a quoted name; a functor bare
  // before "(" where the reader takes it there.  Until 2026-10-04 'G' and '+'
  // printed "G" and "+", string literals, and 'G'(a) printed G(a).
  group('a printed term reads back as itself', () {
    // Each source text, and the text it prints as.
    const cases = <String, String>{
      "'G'": "'G'",
      "'+'": "'+'",
      "'mod'": "'mod'",
      "'procedure'": "'procedure'",
      "'42'": "'42'",
      "'a b'": "'a b'",
      r"'it\'s'": r"'it\'s'",
      "'_x'": "'_x'",
      r"'back\\slash'": r"'back\\slash'",
      "'[]'": "'[]'",
      '[]': '[]',
      '[a | b]': '[a | b]',
      "[a, 'B' | 'C']": "[a, 'B' | 'C']",
      // A double-quoted string is the string of its characters, printed as
      // a quoted name is (GLP #3 Cowork, 2026-10-04 15:19 UTC, FAULT 2 (b));
      // until 2026-10-10 these printed in double quotes.
      '"str"': 'str',
      '"Str"': "'Str'",
      r'"say \"hi\""': '\'say "hi"\'',
      // The three-character string, quotes and all, kept as written.
      '\'"x"\'': '\'"x"\'',
      "'G'(a)": "'G'(a)",
      "f('=..', 'X')": "f('=..', 'X')",
      '=?=(a, b)': '=?=(a, b)',
      '+(1)': '+(1)',
      '1 + 2': '(1 + 2)',
      "'a b'(c)": "'a b'(c)",
      'foo': 'foo',
      'mod(a)': 'mod(a)',
      'foo()': 'foo()',
      "'G'()": "'G'()",
      '42': '42',
      '-3': '-3',
    };
    for (final c in cases.entries) {
      test('${c.key} prints as ${c.value} and reads back', () {
        final term = _readTerm(c.key);
        final printed = GlpPrinter().printTerm(term);
        expect(printed, c.value);
        expect(_sameTerm(_readTerm(printed), term), isTrue,
            reason: '$printed read back is not ${c.key}');
      });
    }

    // Until 2026-10-10 this held the two apart, "a quoted name and a string
    // of the same text stay apart", against the Definition's one kind of
    // constant besides numbers and [].
    test('a quoted name and a double-quoted string of the same text are one '
        'string, and print alike', () {
      expect(_sameTerm(_readTerm("'G'"), _readTerm('"G"')), isTrue);
      expect(GlpPrinter().printTerm(_readTerm("'G'")),
          GlpPrinter().printTerm(_readTerm('"G"')));
      expect(_sameTerm(_readTerm('\'"G"\''), _readTerm('"G"')), isFalse);
    });

    test('a clause with quoted predicate names reads back', () {
      const source = "'G'(X) :- '_send'(X?), 'a b'.\n";
      final printed = _printed(source).single;
      expect(printed, "'G'(X) :- '_send'(X?), 'a b'.");
      expect(_printed('$printed\n').single, printed);
    });
  });
}

/// The term [text] reads as, the argument of a unit clause.
Term _readTerm(String text) =>
    Parser(Lexer('t($text).').tokenize()).parse().procedures.single.clauses
        .single.head.args.single;

/// Whether [a] and [b] are the same term, node by node.
bool _sameTerm(Term? a, Term? b) {
  if (a == null || b == null) return a == b;
  if (a is ConstTerm && b is ConstTerm) return a.value == b.value;
  if (a is VarTerm && b is VarTerm) {
    return a.name == b.name && a.isReader == b.isReader;
  }
  if (a is UnderscoreTerm && b is UnderscoreTerm) {
    return a.isReader == b.isReader;
  }
  if (a is ListTerm && b is ListTerm) {
    return _sameTerm(a.head, b.head) && _sameTerm(a.tail, b.tail);
  }
  if (a is StructTerm && b is StructTerm) {
    if (a.functor != b.functor || a.args.length != b.args.length) return false;
    for (var i = 0; i < a.args.length; i++) {
      if (!_sameTerm(a.args[i], b.args[i])) return false;
    }
    return true;
  }
  return false;
}
