/// The printed declaration denotes the declaration it printed.
///
/// An artefact carries its interface as text and the loader derives the
/// exported table from that text (TGLP Implementation Notes, "Deriving the
/// exported table when the artefact is read"), so the print must round-trip:
/// `Parser.parseInterface` reads back what `wire/flattening.dart` wrote.
///
/// A print dropping a declaration's type-parameter list does not round-trip.
/// The parameter returns as a bare undefined type name, and a declaration
/// naming no parameters is refused for exactly that (parameterized-types.tex,
/// "Declaration parameters"), so the text denotes a declaration the checker
/// rejects rather than the one printed.  No identity turns on the list: the
/// identity is the hash of the automaton, and a parameterised declaration
/// carries none at all (Implementation Notes, "The tables").
library;

import 'dart:io';

import 'package:glp_runtime/analysis/type_checker/param_expansion.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/program_linker.dart';
import 'package:glp_runtime/wire/flattening.dart';
import 'package:test/test.dart';

ProcDecl _parseOne(String text) => Parser(Lexer(text).tokenize())
    .parseInterface()
    .procDeclarations
    .single;

ProcDecl _roundTrip(ProcDecl d) => _parseOne(exportDeclarationText(d));

void main() {
  group('a declaration survives print and re-parse', () {
    test('a parameterised declaration keeps its list', () {
      final d = _parseOne('exported procedure(Y) tie(Y, Y?).');
      expect(d.typeParams, ['Y']);

      final back = _roundTrip(d);
      expect(back.name, d.name);
      expect(back.typeParams, d.typeParams);
      expect(back.argTypes.map((t) => t.toString()),
          d.argTypes.map((t) => t.toString()));
    });

    test('several parameters keep their order', () {
      final d = _parseOne('exported procedure(M, Ent) send(M?, Stream(Ent)).');
      final back = _roundTrip(d);
      expect(back.typeParams, ['M', 'Ent']);
      expect(back.argTypes.map((t) => t.toString()),
          d.argTypes.map((t) => t.toString()));
    });

    test('a declaration naming no parameter prints none', () {
      final d = _parseOne('exported procedure go(Request?, Integer).');
      expect(exportDeclarationText(d),
          'exported procedure go(Request?, Integer).');
      expect(_roundTrip(d).typeParams, isEmpty);
    });

    test('the re-parsed text is accepted, its parameter not read as a type', () {
      final d = _parseOne('exported procedure(Y) tie(Y, Y?).');
      final iface = Parser(Lexer(exportDeclarationText(d)).tokenize())
          .parseInterface();
      expect(() => expandParameterizedTypes(iface), returnsNormally);
    });
  });

  group('the interface text of a linked program', () {
    test('an exported parameterised alias carries its list and reads back', () {
      final dir =
          Directory('../programs/tests/param_import_linked').absolute.path;
      final modules = discoverProgram(dir, rootSelfGlpPath: '$dir/self.glp');
      final linked = linkProgram(modules, rootDir: dir);

      final alias = linked.scopeDeclarations
          .singleWhere((d) => '${d.name}/${d.argTypes.length}' == 'tie/2');
      expect(alias.typeParams, ['Y']);

      final text = exportDeclarationText(alias);
      expect(text, 'exported procedure(Y) tie(Y, Y?).');
      expect(_parseOne(text).typeParams, ['Y']);
    });
  });
}
