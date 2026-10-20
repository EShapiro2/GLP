// glp_runtime/test/vglp/program_compilation_test.dart
//
// The canonical compilation end to end: a vGLP module to one self-contained
// GLP module.
// Spec: vGLP, sections/elicitation.tex, Definition "Canonical Compilation".

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/ast.dart' show ConstTerm;
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/vglp/mediator.dart';
import 'package:glp_runtime/vglp/program_compilation.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart'
    show TypeEnvironment;
import 'package:glp_runtime/runtime/module_hierarchy.dart' show rootScope;
import 'package:glp_runtime/compiler/program_linker.dart' show emitVglpSources;

const _programs = '../programs';

void main() {
  // The scope of a module directly under the root, programs/self.glp its one
  // layer, passed in.
  final rootSelfGlp = File('$_programs/self.glp');
  final TypeEnvironment? scope =
      rootSelfGlp.existsSync() ? rootScope(rootSelfGlp.absolute.path) : null;

  final mediator = MediatorSource.fromDirectory('$_programs/vglp');

  CompiledProgram compile(String source) => compileProgram(
      Parser(Lexer(source).tokenize(), vglp: true).parseModule(), mediator,
      scope: scope);

  group('one self-contained module', () {
    const src = '''
IntroChannel   ::= Channel(Stream(Constant), Stream(Constant)).
Response       ::= accept(IntroChannel) ; no.
Decision       ::= yes ; no.
ColdCallAnswer ::= response(Response?).
UserContent    ::= response(Response).
UserInMsg      ::= msg(Constant, Constant, UserContent)
                 ; decision(Decision, Constant, ColdCallAnswer).
UserInStream   ::= [] ; [UserInMsg | UserInStream].
ColdCallOffer  ::= offer(Constant).

procedure respond(ColdCallOffer?, Response, UserInStream).
*(Answer=yes, From?)
respond(offer(From), Resp?, [decision(Answer?, From?, response(Resp))]) :-
    ground(From?) | true
*(no) true.
*(Answer=no, From?)
respond(offer(From), Resp?, [decision(Answer?, From?, response(Resp))]) :-
    ground(From?) | true.
''';

    test('it carries the source types, the added types and the mediator\'s',
        () {
      final out = compile(src).source;
      expect(out, contains('UserInStream ::='));       // the source's own
      expect(out, contains('Xs_respond_1 ::= xs_respond_1(Decision).'));  // added
      expect(out, contains('Answer ::= Xs_respond_1 ; Xs_respond_2.'));
      expect(out, contains('Reply_respond_1 ::= then(Xs_respond_1) ; else.'));
      expect(out, contains('Reply_respond_2 ::= then(Xs_respond_2).'));
      expect(out, contains('Escrow ::= esc_respond_1(Reply_respond_1?) ; '
          'esc_respond_2(Reply_respond_2?).'));
      expect(out, contains(                                             // instantiated
          'UserAnswer ::= answer(ReqId, Answer) ; decline(ReqId).'));
    });

    test('no type parameter survives the emission', () {
      final out = compile(src).source;
      expect(out, isNot(contains('(A)')));
      expect(out, isNot(contains('(A, E, X)')));
      expect(out, isNot(contains('(E, X)')));
      expect(out, isNot(contains('PendingList(E)')));
    });

    test('every compiled procedure is declared immediately above its clauses',
        () {
      final out = compile(src).source;
      final lines = out.split('\n');
      final declLine =
          lines.indexWhere((l) => l.startsWith('procedure respond('));
      expect(declLine, greaterThan(-1));
      expect(lines[declLine + 1], startsWith('respond('));
    });

    test('the mediator comes with it, clauses and declarations', () {
      final out = compile(src).source;
      for (final p in ['med', 'answer', 'close', 'drop', 'med_split']) {
        expect(out, contains('procedure $p('), reason: '$p is missing');
      }
      // abort/3 stays generic in the slot's reply type.
      expect(out, contains('procedure(R) abort('));
      // The timer and the deadline went on 2026-09-15 (Udi), so no compiled
      // program carries them.
      expect(out, isNot(contains('procedure timer(')));
      expect(out, isNot(contains('procedure deadline(')));
    });

    test('the pending table\'s program clauses come ahead of the search clauses',
        () {
      final out = compile(src).source;
      final own = out.indexOf('answer(ReqId, xs_respond_1(X1), ');
      final search = out.indexOf('otherwise | answer(');
      expect(own, greaterThan(-1));
      expect(search, greaterThan(own));
      expect(out, contains('answer(ReqId, xs_respond_2(X1), '));
      // close only for the clause with an else-branch
      expect(out, contains('close(ReqId, [pending(Id, esc_respond_1(R?)) | Ps], Ps?)'));
      expect(out, isNot(contains('esc_respond_2(R?)) | Ps], Ps?) :- (ReqId? =?= Id?) | R = else')));
    });

    test('the slots are typed by their clauses\' reply types', () {
      final out = compile(src).source;
      expect(out, contains('Slot(Reply_respond_1)?, Slot(Reply_respond_2)?)'));
      expect(out, contains('Slot(R) ::= none ; ask(R, ReqId).'));
    });

    test('the emitted module parses back as GLP', () {
      final out = compile(src).source;
      final back = Parser(Lexer(out).tokenize()).parseModule();
      expect(back.procedures, isNotEmpty);
      // Nothing volition-guarded survives: GLP is vGLP without volition guards.
      for (final p in back.procedures) {
        for (final c in p.clauses) {
          expect(c.isVolitionGuarded, isFalse);
        }
      }
    });

    test('display declarations are carried through unchanged', () {
      // Carried through in the one canonical print of GLP's printer
      // (glp_printer.dart): 'x' and "x" are two spellings of one string, the
      // double-quoted form read and never printed, a string single-quoted and
      // bare where the lexer reads it so (GLP-Spec appendix-lp.tex, Definition
      // "Logic Programs Syntax"; vGLP #5 Cowork, 2026-10-07 20:01 UTC, item
      // 2).  Until 2026-10-10 a string literal kept its double quotes,
      // label("Mint").
      final out = compile('''
$src
display respond *(Answer=yes, From?) : panel(inbox), label("Mint"), transient.
''');
      expect(out.source, contains('display respond'));
      expect(out.source, contains("label('Mint')"));
      expect(out.source, isNot(contains('"Mint"')));
      expect(out.source, contains('panel(inbox)'));
      expect(out.source, contains('*(Answer=yes, From?)'));
      expect(out.source, isNot(contains('"inbox"')));
      expect(out.source, isNot(contains('"yes"')));
      // The other spelling of the one string compiles to the same text.
      expect(
          compile('''
$src
display respond *(Answer=yes, From?) : panel(inbox), label('Mint'), transient.
''').source,
          out.source);
    });

    test('a string \'"x"\' round-trips through the compiler', () {
      // '"x"' is the three-character string, the reader keeping the
      // characters as written, and another value than "x" and 'x', the
      // one-character string (GLP-Spec appendix-lp.tex, Definition "Logic
      // Programs Syntax"): the compiled program prints it '"x"', and that
      // text reads back as the same three characters, in a clause and in a
      // display declaration alike.
      final out = compile('''
$src
procedure tag(String).
tag('"x"').
tag("x").
display respond *(Answer=yes, From?) : panel(inbox), label('"x"'), transient.
''').source;
      // tag/1 gains the mediator channel ahead of its own argument.
      expect(out, contains('tag(_, \'"x"\') :- true.'));
      expect(out, contains('tag(_, x) :- true.'));
      expect(out, contains('label(\'"x"\')'));
      final back = Parser(Lexer(out).tokenize()).parseModule();
      final tags = back.procedures.singleWhere((p) => p.name == 'tag');
      expect([
        for (final c in tags.clauses) (c.head.args.last as ConstTerm).value
      ], ['"x"', 'x']);
      final label = back.displayDecls.single.items
          .singleWhere((i) => i.name == 'label')
          .args
          .single as ConstTerm;
      expect(label.value, '"x"');
    });
  });

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
        final m = Parser(Lexer(file.readAsStringSync()).tokenize(), vglp: true)
            .parseModule();
        expect(m.volitionalDeclarations, isNotEmpty);
        expect(m.procedures.where((p) => p.isVolitional), isNotEmpty);
      });
    }
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
        final m = Parser(Lexer(file.readAsStringSync()).tokenize(), vglp: true)
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
      final m = Parser(Lexer(file.readAsStringSync()).tokenize(), vglp: true)
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
