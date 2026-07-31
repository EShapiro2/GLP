/// Type identity for dynamic activation (TGLP modules.tex §Dynamic Activation;
/// step 1 of /Grassroots/docs/typed-dynamic-activation-plan.md).
///
/// The identity of a procedure declaration is the hash of its full type
/// automaton in canonical minimised form, every referenced type expanded — not
/// of the declaration's text. These tests pin the four properties that makes
/// it usable as the activation check: equal automata give equal identities
/// whatever the names, different automata give different identities,
/// minimisation collapses the difference between a type and a longer definition
/// of the same language, and a parameterised declaration has no identity.
library;

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_identity.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart'
    show setRootScopeEnvironmentSource;
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/program_linker.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/wire/artefact.dart' show Artefact;

TypeIdentityTables tablesOf(String source) =>
    typeIdentityTablesForModule(Parser(Lexer(source).tokenize()).parseModule());

String identityIn(String source, String key) {
  final id = tablesOf(source).identityOf(key);
  expect(id, isNotNull, reason: '$key has no identity in:\n$source');
  return id!;
}

void main() {
  // Root scope from programs/self.glp, as the engine sets it: the identities of
  // declarations over Number, Stream(X) and the rest are built against the same
  // scope the type checker uses.
  final rootSelfGlp = File('../programs/self.glp');
  final hasRootScope = rootSelfGlp.existsSync();
  if (hasRootScope) {
    setRootScopeEnvironmentSource(rootSelfGlp.readAsStringSync());
  }

  group('canonical print', () {
    test('carries the version tag and is stable across calls', () {
      const src = '''
Colour ::= red ; green.
procedure paint(Colour?).
paint(_).
''';
      final module = Parser(Lexer(src).tokenize()).parseModule();
      final first = typeIdentityTablesForModule(module);
      final second = typeIdentityTablesForModule(
          Parser(Lexer(src).tokenize()).parseModule());
      expect(first.identityOf('paint/1'), second.identityOf('paint/1'));
      expect(typeAutomatonPrintVersion, 'type-automaton/1');
    });
  });

  group('equal automata, equal identity', () {
    test('the procedure name is not part of the identity', () {
      final a = identityIn('''
Colour ::= red ; green.
procedure paint(Colour?).
paint(_).
''', 'paint/1');
      final b = identityIn('''
Colour ::= red ; green.
procedure tint(Colour?).
tint(_).
''', 'tint/1');
      expect(a, b);
    });

    test('the type name is not part of the identity', () {
      final a = identityIn('''
Colour ::= red ; green.
procedure paint(Colour?).
paint(_).
''', 'paint/1');
      final b = identityIn('''
Shade ::= red ; green.
procedure paint(Shade?).
paint(_).
''', 'paint/1');
      expect(a, b);
    });

    test('alternative order is not part of the identity', () {
      final a = identityIn('''
Colour ::= red ; green.
procedure paint(Colour?).
paint(_).
''', 'paint/1');
      final b = identityIn('''
Colour ::= green ; red.
procedure paint(Colour?).
paint(_).
''', 'paint/1');
      expect(a, b);
    });

    test('minimisation collapses a two-state definition of one language', () {
      // Even and Odd accept exactly the streams Ints accepts; their automata
      // minimise to the same one-state automaton.
      final split = identityIn('''
Even ::= [] ; [Integer | Odd].
Odd ::= [] ; [Integer | Even].
procedure walk(Even?).
walk(_).
''', 'walk/1');
      final single = identityIn('''
Ints ::= [] ; [Integer | Ints].
procedure walk(Ints?).
walk(_).
''', 'walk/1');
      expect(split, single);
    });

    test('a bare type-name alternative inherits the named type transitions',
        () {
      final union = identityIn('''
Num ::= Integer ; Real.
procedure measure(Num?).
measure(_).
''', 'measure/1');
      final reordered = identityIn('''
Num ::= Real ; Integer.
procedure measure(Num?).
measure(_).
''', 'measure/1');
      expect(union, reordered);
      final integerOnly = identityIn('''
procedure measure(Integer?).
measure(_).
''', 'measure/1');
      expect(union, isNot(integerOnly));
    });

    test('two declarations of one signature share an identity', () {
      final tables = tablesOf('''
Colour ::= red ; green.
exported procedure paint(Colour?).
paint(_).
procedure mix(Colour?).
mix(_).
''');
      expect(tables.declared['paint/1'], tables.declared['mix/1']);
    });
  });

  group('different automata, different identity', () {
    test('argument mode', () {
      final consumed = identityIn('''
Colour ::= red ; green.
procedure paint(Colour?).
paint(_).
''', 'paint/1');
      final produced = identityIn('''
Colour ::= red ; green.
procedure paint(Colour).
paint(_).
''', 'paint/1');
      expect(consumed, isNot(produced));
    });

    test('an embedded mode inversion', () {
      final plain = identityIn('''
Colour ::= red ; green.
Box ::= box(Colour).
procedure hold(Box?).
hold(_).
''', 'hold/1');
      final inverted = identityIn('''
Colour ::= red ; green.
Box ::= box(Colour?).
procedure hold(Box?).
hold(_).
''', 'hold/1');
      expect(plain, isNot(inverted));
    });

    test('an added alternative', () {
      final two = identityIn('''
Colour ::= red ; green.
procedure paint(Colour?).
paint(_).
''', 'paint/1');
      final three = identityIn('''
Colour ::= red ; green ; blue.
procedure paint(Colour?).
paint(_).
''', 'paint/1');
      expect(two, isNot(three));
    });

    test('the element type of a stream', () {
      final ints = identityIn('''
Ints ::= [] ; [Integer | Ints].
procedure walk(Ints?).
walk(_).
''', 'walk/1');
      final strings = identityIn('''
Strs ::= [] ; [String | Strs].
procedure walk(Strs?).
walk(_).
''', 'walk/1');
      expect(ints, isNot(strings));
    });

    test('arity', () {
      final one = identityIn('''
Colour ::= red ; green.
procedure paint(Colour?).
paint(_).
''', 'paint/1');
      final two = identityIn('''
Colour ::= red ; green.
procedure paint(Colour?, Colour?).
paint(_, _).
''', 'paint/2');
      expect(one, isNot(two));
    });

    test('a wildcard argument is not a concrete type', () {
      final wildcard = identityIn('''
procedure paint(_?).
paint(_).
''', 'paint/1');
      final colour = identityIn('''
Colour ::= red ; green.
procedure paint(Colour?).
paint(_).
''', 'paint/1');
      expect(wildcard, isNot(colour));
    });
  });

  group('the two tables', () {
    test('exported is the exported subset of declared', () {
      final tables = tablesOf('''
Colour ::= red ; green.
exported procedure paint(Colour?).
paint(_).
procedure mix(Colour?, Colour).
mix(_, _).
''');
      expect(tables.declared.keys, containsAll(['paint/1', 'mix/2']));
      expect(tables.exported.keys, ['paint/1']);
      expect(tables.exported['paint/1'], tables.declared['paint/1']);
    });

    test('a parameterised declaration has no identity', () {
      final tables = tablesOf('''
List2(X) ::= [] ; [X | List2(X)].
procedure head(List2(X)?, X).
head(_, _).
''');
      expect(tables.parametric, contains('head/2'));
      expect(tables.declared.containsKey('head/2'), isFalse);
      expect(tables.exported.containsKey('head/2'), isFalse);
      expect(tables.identityOf('head/2'), isNull);
    });

    test('an undeclared procedure has no identity', () {
      final tables = tablesOf('''
Colour ::= red ; green.
procedure paint(Colour?).
paint(_).
''');
      expect(tables.identityOf('scrub/1'), isNull);
    });

  });

  group('over the root scope', () {
    test('a parameterised type expands before the automaton is built', () {
      if (!hasRootScope) return;
      final viaStream = identityIn('''
procedure walk(Stream(Integer)?).
walk(_).
''', 'walk/1');
      final viaLocal = identityIn('''
Ints ::= [] ; [Integer | Ints].
procedure walk(Ints?).
walk(_).
''', 'walk/1');
      expect(viaStream, viaLocal);
    });

    test('a root-scope union expands to the primitives it accepts', () {
      if (!hasRootScope) return;
      final viaNumber = identityIn('''
procedure measure(Number?).
measure(_).
''', 'measure/1');
      final viaLocal = identityIn('''
Num ::= Integer ; Real.
procedure measure(Num?).
measure(_).
''', 'measure/1');
      expect(viaNumber, viaLocal);
    });

    test('a linked program has tables over its linked declarations', () {
      if (!hasRootScope) return;
      const root = 'test/programs/linker_nested';
      if (!Directory(root).existsSync()) return;
      final modules = discoverProgram(root,
          rootSelfGlpPath: rootSelfGlp.absolute.path);
      final linked = checkedLinkedProgram(modules, rootDir: root);
      final tables = linkedTypeIdentityTables(modules, linked);

      // `play` is the program's entry point: the root self.glp exports it, and
      // linking gives it a bare alias (modules.tex §Static Linking step 5).
      expect(tables.declared, contains('play/0'));
      for (final entry in tables.exported.entries) {
        expect(tables.declared[entry.key], entry.value);
      }
    });
  });

  // ===========================================================================
  // The exported table, derived from the artefact's interface text
  // ===========================================================================
  //
  // A shipped table can disagree with the source it describes; one recomputed
  // from the attested source cannot (modules.tex §Dynamic Activation; the
  // Implementation Notes appendix, "Deriving the exported table at load").  So
  // the table `run/3` compares against is derived at load from the artefact's
  // interface text, and these tests are what says the derivation and the
  // compiled program agree.
  group('exported table derived from the interface text', () {
    /// Loads [target] and returns the compiled program's exported table beside
    /// the one derived from its artefact's interface text.
    ({TypeIdentityTables compiled, TypeIdentityTables derived, Artefact art})
        bothPathsFor(String target, {required bool single}) {
      final rootSelf = rootSelfGlp.absolute.path;

      final engine = GlpEngine(rootSelfGlpPath: rootSelf);
      if (single) {
        engine.loadFile(target);
      } else {
        engine.loadProgram(target);
      }
      final art = engine.appModule!.artefact as Artefact;

      final modules = single
          ? discoverSingleModule(target, rootSelfGlpPath: rootSelf)
          : discoverProgram(target, rootSelfGlpPath: rootSelf);
      final linked = single
          ? linkProgram(modules,
              rootDir: File(target).parent.path, singleModulePath: target)
          : checkedLinkedProgram(modules, rootDir: target);

      return (
        compiled: linkedTypeIdentityTables(modules, linked),
        derived: interfaceTypeIdentityTables(
          typeDefsText: art.typeDefsText,
          exportDeclarationTexts: art.exports.map((e) => e.declarationText),
        ),
        art: art,
      );
    }

    test('a directory program: the two tables agree key for key', () {
      if (!hasRootScope) return;
      const root = 'test/programs/interface_table';
      if (!Directory(root).existsSync()) return;
      final r = bothPathsFor(root, single: false);

      expect(r.derived.exported, isNotEmpty);
      expect(r.derived.exported, equals(r.compiled.exported));
    });

    test('an export reaching a root-scope type resolves the same on both paths',
        () {
      if (!hasRootScope) return;
      const root = 'test/programs/interface_table';
      if (!Directory(root).existsSync()) return;
      final r = bothPathsFor(root, single: false);

      // The primitives and the root-scope types are absent from the interface
      // text by design — ambient at every runtime, as they are excluded from
      // h(M).  `run_jobs/2` reaches Stream(X) and `serve/1` reaches
      // Channel(In, Out); both must come out of buildRootScopeEnvironment() on
      // the derived path exactly as they come out of the scope on the compiled
      // one, or the same program has two identities.
      expect(r.art.typeDefsText, isNot(contains('Stream')));
      expect(r.art.typeDefsText, isNot(contains('Channel')));
      expect(r.derived.exported['run_jobs/2'],
          equals(r.compiled.exported['run_jobs/2']));
      expect(r.derived.exported['serve/1'],
          equals(r.compiled.exported['serve/1']));
      expect(r.derived.exported['serve/1'], isNotNull);
    });

    test('a parameterised export carries no identity on either path', () {
      if (!hasRootScope) return;
      const root = 'test/programs/interface_table';
      if (!Directory(root).existsSync()) return;
      final r = bothPathsFor(root, single: false);

      // A parameterised declaration is a template, not a type: it is checked
      // once per instantiation and has no automaton of its own.
      expect(r.art.exports.map((e) => '${e.name}/${e.arity}'),
          contains('drain/1'));
      expect(r.derived.parametric, contains('drain/1'));
      expect(r.compiled.parametric, contains('drain/1'));
      expect(r.derived.exported, isNot(contains('drain/1')));
      expect(r.compiled.exported, isNot(contains('drain/1')));
    });

    test('a single-module program: every procedure is an entry point', () {
      if (!hasRootScope) return;
      const target = 'test/programs/interface_table_single.glp';
      if (!File(target).existsSync()) return;
      final r = bothPathsFor(target, single: true);

      // "A single-module program, having no self.glp, exports all its
      // procedures, so every one is an entry point" (modules.tex §Static
      // Linking).  The linker keeps them bare and flags them exported; until it
      // did, the compiled table was empty here while the artefact listed all
      // three, and the two paths disagreed.
      expect(r.derived.exported, contains('tally/2'));
      expect(r.derived.exported, equals(r.compiled.exported));
    });

    test('an export with no declaration is absent from every field', () {
      if (!hasRootScope) return;
      const target = 'test/programs/interface_table_single.glp';
      if (!File(target).existsSync()) return;
      final r = bothPathsFor(target, single: true);

      // `nudge/1` has clauses and no declaration, so its interface text is
      // empty and it contributes no types.  The compiled path tables
      // declarations, so an undeclared procedure is in none of the four fields;
      // the derived path matches it.  `unresolved` is the narrower case of a
      // declaration naming a type the scope does not define.
      final texts = {
        for (final e in r.art.exports) '${e.name}/${e.arity}': e.declarationText
      };
      expect(texts['nudge/1'], isEmpty);
      for (final t in [r.derived, r.compiled]) {
        expect(t.exported, isNot(contains('nudge/1')));
        expect(t.parametric, isNot(contains('nudge/1')));
        expect(t.unresolved, isNot(contains('nudge/1')));
      }
    });

    test('an empty interface derives an empty table', () {
      expect(
          interfaceTypeIdentityTables(
                  typeDefsText: '', exportDeclarationTexts: const [])
              .exported,
          isEmpty);
    });
  });
}
