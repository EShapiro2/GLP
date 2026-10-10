/// The printer prints the anonymous variable as it is written: `_`, and `_?`,
/// TGLP's anonymous output.
///
/// TGLP typed-glp.tex, "Anonymous variables": "In a clause head, where a
/// produced position carries an output placeholder rather than a writer, an
/// anonymous variable is written `_?` there and denotes an output the clause
/// never produces".  Until 2026-10-02 GlpPrinter printed every anonymous
/// variable `_`, so a head's `_?` came out as a writer at a produced position,
/// a clause the type checker refuses, in the canonical print that h(M) hashes
/// (wire/flattening.dart) and in the GLP the old-syntax vGLP compilation emits
/// (GLP Cowork's task of 2026-10-02 08:40 UTC, F).
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

/// p's clauses: `_?` at the produced Handle position, `_` at the consumed
/// Integer position.
const _clauses = 'p(0, _?, []).\np(_, withdraw, []).\n';

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

  test('a clause prints with its anonymous output', () {
    expect(_printed('$_handle$_decl$_clauses'),
        ['p(0, _?, []).', 'p(_, withdraw, []).']);
  });

  test('the printed clauses load, as the source does', () {
    final printed = _printed('$_handle$_decl$_clauses').join('\n');
    expect(_engine().loadSource('$_handle$_decl$printed\n'), isTrue);
  });

  test('the canonical print carries the anonymous output', () {
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
      expect(print, contains('(0, _?, []).'));
      expect(print, isNot(contains('(0, _, []).')));
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
      '"str"': '"str"',
      r'"say \"hi\""': r'"say \"hi\""',
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

    test("a quoted name and a string of the same text stay apart", () {
      expect(GlpPrinter().printTerm(_readTerm("'G'")),
          isNot(GlpPrinter().printTerm(_readTerm('"G"'))));
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
