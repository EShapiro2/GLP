/// The artefact's interface table (§5): per-export declaration text and the
/// reachable type definitions, and reading that text back.
///
/// The code format carries the program interface as declaration source text so
/// the loader derives the exported type automata from the artefact itself
/// ("Carrying text rather than compiled automata keeps one source of truth").
/// The reader is `Parser.parseInterface`, which accepts declarations alone —
/// `parseModule`'s rule that a declaration is followed by its clauses is right
/// for a module and wrong for an interface section.
library;

import 'dart:io';

import 'package:glp_runtime/compiler/ast.dart' show Module;
import 'package:glp_runtime/compiler/error.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:glp_runtime/wire/artefact.dart';
import 'package:test/test.dart';

const _rootSelf = '../programs/self.glp';

/// A project exporting `go/2` over its own `Request` type, which reaches
/// `Payload`. `Unused` is defined and named by no export.
const _source = '''
Payload ::= item(Integer) ; none.
Request ::= go(Payload) ; stop.
Unused ::= spare(Integer).

exported procedure go(Request?, Integer).
go(stop, 0).
go(go(P), N?) :- count(P?, N).

procedure count(Payload?, Integer).
count(item(K), K?).
count(none, 0).
''';

Directory _project(String source) {
  final dir = Directory.systemTemp.createTempSync('glp_interface_');
  File('${dir.path}/self.glp').writeAsStringSync(source);
  return dir;
}

Artefact _artefactOf(Directory dir) {
  final engine = GlpEngine(rootSelfGlpPath: _rootSelf);
  engine.loadProgram(dir.path);
  final module = engine.appModule;
  expect(module, isA<rt.ModuleTerm>());
  return module!.artefact as Artefact;
}

void main() {
  group('interface table', () {
    late Directory dir;
    late Artefact artefact;

    setUp(() {
      dir = _project(_source);
      artefact = _artefactOf(dir);
    });

    tearDown(() => dir.deleteSync(recursive: true));

    test('each export carries its declaration text', () {
      final go = artefact.exports.singleWhere((e) => e.name == 'go');
      expect(go.arity, 2);
      expect(go.declarationText, 'exported procedure go(Request?, Integer).');
    });

    test('the reachable type definitions are carried, and only those', () {
      expect(artefact.typeDefsText, contains('Request ::= '));
      expect(artefact.typeDefsText, contains('Payload ::= '),
          reason: 'Payload is reached through Request');
      expect(artefact.typeDefsText, isNot(contains('Unused')),
          reason: 'no export names Unused');
      // Ambient root-scope and primitive types are not the program's source.
      expect(artefact.typeDefsText, isNot(contains('Integer ::=')));
    });

    test('type definitions are printed lexicographically by name', () {
      final names = artefact.typeDefsText
          .trim()
          .split('\n')
          .map((l) => l.split(' ::= ').first)
          .toList();
      expect(names, ['Payload', 'Request']);
    });

    test('the interface text survives serialization', () {
      final back = Artefact.fromBytes(artefact.toBytes());
      expect(back.typeDefsText, artefact.typeDefsText);
      expect(back.exports, artefact.exports);
    });

    test('parseInterface reads the carried text back', () {
      final text = artefact.typeDefsText +
          artefact.exports.map((e) => e.declarationText).join('\n');
      final iface = Parser(Lexer(text).tokenize()).parseInterface();

      expect(iface.procedures, isEmpty);
      expect(iface.typeDefs.map((t) => t.name), containsAll(['Payload', 'Request']));

      final go = iface.procDeclarations.singleWhere((d) => d.name == 'go');
      expect(go.exported, isTrue);
      expect(go.arity, 2);
      expect(go.argTypes[0].toString(), 'Request?');
      expect(go.argTypes[1].toString(), 'Integer');
    });
  });

  group('parseInterface', () {
    Module parse(String text) => Parser(Lexer(text).tokenize()).parseInterface();

    test('accepts a declaration with no clauses following it', () {
      final iface = parse('exported procedure p(Integer?, Integer).\n');
      expect(iface.procDeclarations.single.name, 'p');
      expect(iface.procedures, isEmpty);
    });

    test('accepts several declarations in a row', () {
      final iface = parse('procedure p(Integer?).\n'
          'exported procedure q(Integer?, Integer).\n'
          'imported procedure m#r(Integer?).\n');
      expect(iface.procDeclarations.map((d) => d.name), ['p', 'q', 'r']);
      expect(iface.procDeclarations[1].exported, isTrue);
      expect(iface.procDeclarations[2].imported, isTrue);
    });

    test('accepts type definitions alongside declarations', () {
      final iface = parse('Colour ::= red ; green.\n'
          'exported procedure paint(Colour?).\n');
      expect(iface.typeDefs.single.name, 'Colour');
      expect(iface.procDeclarations.single.name, 'paint');
    });

    test('accepts a parameterized declaration', () {
      final iface = parse('exported procedure head(Stream(X)?, X).\n');
      final d = iface.procDeclarations.single;
      expect(d.argTypes[0].toString(), 'Stream(X)?');
    });

    test('rejects a clause', () {
      expect(
          () => parse('exported procedure p(Integer?, Integer).\n'
              'p(X, Y?) :- Y := X? + 1.\n'),
          throwsA(isA<CompileError>()));
    });

    test('parseModule still requires clauses after a declaration', () {
      expect(
          () => Parser(Lexer('procedure p(Integer?, Integer).\n').tokenize())
              .parseModule(),
          throwsA(isA<CompileError>()));
    });
  });
}
