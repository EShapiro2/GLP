// glp_runtime/test/vglp/program_compilation_test.dart
//
// The deployed vGLP sources and IGLP's linker fixture: each parses as vGLP,
// and the fixture's committed modules are what :emit prints.  The tests of
// the old compilation, compileProgram against the generic mediator, went with
// it (vGLP #22's task, Code_inbox 2026-10-31 15:20 UTC).
// Spec: vGLP, Section 6, "The Implementation: Compiling vGLP onto GLP".

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/program_linker.dart' show emitVglpSources;
import 'package:glp_runtime/vglp/canonical.dart' show extractWidgetDeclarations;

const _programs = '../programs';

/// The text of a vGLP source as the compiler lexes it, its widget
/// declarations T =::= W set aside, GLP's lexer having no token =::=
/// (canonical.dart, extractWidgetDeclarations).  A module may carry its own:
/// "Widget declarations are scoped as type declarations are: a declaration at
/// the root holds for every program, one in a module holds in that module"
/// (vGLP, Definition "Widget Declaration").
String _lexed(File file) =>
    extractWidgetDeclarations(file.readAsStringSync()).stripped;

void main() {
  // The root self.glp, the root of the scope :emit compiles in.
  final rootSelfGlp = File('$_programs/self.glp');

  group('the deployed sources', () {
    // Five are asked here, and a list written from memory misses one:
    // coins_agent.vglp and bonds_agent.vglp arrived on 2026-09-03,
    // social/graph/core/home.vglp on 2026-09-09 and
    // sovereign/denominated/sovereign_agent.vglp on 2026-09-15.  The child-safe
    // social network's three agents and their self.vglp, asked here until
    // 2026-10-20, went with its hand-written program (CSSN-play 185a2c3f);
    // their copies in IGLP's linker_tree fixture are asked in the next group.
    // Four of the paths moved on 2026-09-08, each source going with its .glp
    // into the certified program of a mini-app --- social/graph/core,
    // coins/currency and cssn/childsafe --- since a lone .vglp is compiled by
    // the loader and must stand beside its .glp.  They all PARSE as vGLP.  coins/currency/coins_agent.vglp,
    // bonds/dated/bonds_agent.vglp and sovereign/denominated/sovereign_agent.vglp
    // also COMPILE and RUN --- Sections N2, N3 and N4 of test/run_all_tests.sh
    // load their emitted .glp and run the village market; social/graph/core/
    // home.vglp compiles, its home.glp being the compiler's output, which
    // Section SG runs on the dispatcher (superapp_ui/3); social/graph/core/
    // agent.vglp compiles, beside the hand-written agent.glp the program
    // loads.
    //
    // A currency source is asked what vGLP's Definition "Guarded Clause,
    // Volitional Procedure, Interactive Type, Interactive Term, Ordinary
    // Clause, Procedure, vGLP Program" says it holds: a volitional procedure,
    // "declared `procedure (T)*p(T1, ..., Tn).`", whose clauses are written
    // "(A)*p(S1, ..., Sn) :- G | B" (GLP #3 Cowork, 2026-10-09 19:15 UTC).
    // Each is asked it from its port: bonds on its own, coins and sovereign
    // in the three-owner merge (GLP #3 Cowork, 2026-10-09 18:38 UTC); the
    // social graph's agent, item 2 of GSG's port (GSG 2026-10-11 13:19 UTC),
    // and the super-app's person interface, home.vglp, its item 1, in the one
    // merge of the two.  The grassroots app's agent went with programs/grassapp
    // (GSG, 2026-10-13: one copy of the currency).
    for (final path in [
      'currencies/coins/currency/coins_agent.vglp',
      'currencies/bonds/dated/bonds_agent.vglp',
      'currencies/sovereign/denominated/sovereign_agent.vglp',
      'social/graph/core/agent.vglp',
      'social/graph/core/home.vglp',
    ]) {
      test('$path parses as vGLP and holds a volitional procedure', () {
        final file = File('$_programs/$path');
        expect(file.existsSync(), isTrue, reason: '$path is not on disc');
        final m = Parser(Lexer(_lexed(file)).tokenize(), vglp: true)
            .parseModule();
        expect(m.volitionalDeclarations, isNotEmpty);
        expect(m.procedures.where((p) => p.isVolitional), isNotEmpty);
      });
    }

    // home.vglp declares the widget of its introduction's card in the
    // module, which holds there (vGLP, Definition "Widget Declaration"):
    // GKK's words drawn by approval_sentence.
    test('social/graph/core/home.vglp declares the widget of its '
        'introduction\'s card, approval_sentence', () {
      final w = extractWidgetDeclarations(
          File('$_programs/social/graph/core/home.vglp').readAsStringSync());
      expect(w.byModedType.values, contains('approval_sentence'));
    });
  });

  group('the linker_tree fixture\'s sources', () {
    // IGLP's fixture, programs/tests/linker_tree: the child-safe social
    // network's hand-written program as it stood at GLP d2495778, its
    // comments removed and its names changed (program_linker_test.dart).  Its
    // nested directory agents/ holds three vGLP modules, adult.vglp,
    // parent.vglp and child.vglp, that hand a party to one another, each
    // beside its compiled .glp, and the questions of their self.vglp.
    const tree = 'tests/linker_tree';
    const agents = '$tree/agents';

    for (final name in ['adult', 'child', 'self']) {
      final path = '$agents/$name.vglp';
      test('$path parses as vGLP and holds a volitional procedure', () {
        final file = File('$_programs/$path');
        expect(file.existsSync(), isTrue, reason: '$path is not on disc');
        final m = Parser(Lexer(_lexed(file)).tokenize(), vglp: true)
            .parseModule();
        expect(m.volitionalDeclarations, isNotEmpty);
        expect(m.procedures.where((p) => p.isVolitional), isNotEmpty);
      });
    }

    // The parent agent declares no volitional procedure of its own: it asks
    // the questions its self.vglp shares with the adult and child agents, and
    // is a vGLP module because it reaches their questions (vGLP a89a751,
    // canonical-compilation.tex: "the questions of a program are those of
    // all its modules").
    test('$agents/parent.vglp parses as vGLP and asks the questions of its '
        'self.vglp', () {
      final file = File('$_programs/$agents/parent.vglp');
      expect(file.existsSync(), isTrue);
      final m = Parser(Lexer(_lexed(file)).tokenize(), vglp: true)
          .parseModule();
      expect(m.procedures, isNotEmpty);
      expect(
          m.procedures.expand((p) => p.clauses).any((c) => c.isVolitionGuarded),
          isFalse);
      final calls = {
        for (final c in m.procedures.expand((p) => p.clauses))
          if (c.body != null) for (final g in c.body!) g.functor
      };
      expect(calls, containsAll(['approve', 'approve_join', 'join_group']));
    });

    // A compiled module committed beside its source is the compiler's own
    // output, held to what :emit prints (GLP/CLAUDE.md, "Generated files").
    // :emit writes beside the sources it compiles, so it runs on a copy of
    // the fixture at the same depth below the root, with the same scope, and
    // the copy goes when the test ends.
    test('the committed adult.glp, parent.glp and child.glp are what :emit '
        'prints from their sources', () {
      final copy = Directory('$_programs/tests/linker_tree_emit_${pid}_'
          '${DateTime.now().microsecondsSinceEpoch}');
      addTearDown(() {
        if (copy.existsSync()) copy.deleteSync(recursive: true);
      });
      final from = Directory('$_programs/$tree');
      for (final f in from.listSync(recursive: true).whereType<File>()) {
        final rel = f.path.substring(from.path.length + 1);
        File('${copy.path}/$rel')
          ..createSync(recursive: true)
          ..writeAsStringSync(f.readAsStringSync());
      }
      final written = emitVglpSources(copy.absolute.path,
          rootSelfGlpPath: rootSelfGlp.absolute.path);
      final agentsCopy = '${copy.absolute.path}/agents';
      expect(written.toSet(), {
        '$agentsCopy/adult.glp',
        '$agentsCopy/parent.glp',
        '$agentsCopy/child.glp',
      });
      for (final name in ['adult', 'parent', 'child']) {
        expect(File('$agentsCopy/$name.glp').readAsStringSync(),
            File('$_programs/$agents/$name.glp').readAsStringSync(),
            reason: '$agents/$name.glp is not what :emit prints');
      }
    });
  });
}
