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
}
