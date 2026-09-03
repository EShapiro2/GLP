// glp_runtime/test/vglp/mediator_test.dart
//
// The mediator's instantiation and emission.
// Spec: vGLP, sections/elicitation.tex, Definition "Canonical Compilation":
// ⌈M⌉ consists of the compiled procedures together with the mediator and the
// timer, the mediator generic in A and X and instantiated at the program's.

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart' show TypeRef;
import 'package:glp_runtime/vglp/mediator.dart';
import 'package:glp_runtime/vglp/types.dart';

void main() {
  final dir = Directory('../programs/vglp');
  if (!dir.existsSync()) {
    // The generic source is the compilation's input; without it there is
    // nothing to instantiate and the tests would be checking nothing.
    return;
  }

  final source = MediatorSource.fromDirectory(dir.path);
  final med = instantiate(source);

  group('the generic source', () {
    test('carries the vocabulary and the clauses in separate files', () {
      expect(source.vocabulary.typeDefs, isNotEmpty);
      expect(source.clauses.procedures, isNotEmpty);
    });

    test('every procedure the Definition names is there', () {
      final names = source.clauses.procedures.map((p) => p.name).toSet();
      expect(names, containsAll(
          ['med', 'timer', 'deadline', 'answer', 'close', 'aborts', 'med_split']));
    });
  });

  group('instantiation', () {
    test('the answer parameter becomes the program\'s answer type', () {
      final reply = med.typeDefs.firstWhere((d) => d.name == 'Reply');
      expect(printTypeDef(reply), 'Reply ::= then($answerTypeName) ; else.');
    });

    test('the context parameter becomes the program\'s context type', () {
      final agentMsg = med.typeDefs.firstWhere((d) => d.name == 'AgentMsg');
      expect(printTypeDef(agentMsg), contains(contextTypeName));
      expect(printTypeDef(agentMsg), isNot(contains('(X)')));
    });

    test('an instantiated definition is monomorphic', () {
      for (final d in med.typeDefs) {
        expect(d.typeParams, isEmpty, reason: '${d.name} kept a parameter');
      }
    });

    test('an instantiated declaration is monomorphic too', () {
      for (final d in med.procDecls) {
        expect(d.typeParams, isEmpty, reason: '${d.name} kept a parameter');
      }
    });

    test('no type parameter survives anywhere in the emitted vocabulary', () {
      final text = med.typeDefs.map(printTypeDef).join('\n');
      expect(text, isNot(matches(RegExp(r'\b[AX]\b'))));
    });

    test('the mediator channel type of a compiled goal matches the emitted one',
        () {
      // The compiled procedures declare their channel argument; the mediator
      // declares the same channel on its own side.  They must name one type.
      expect(medChannelType(isInput: true).toString(),
          'Channel(Closed, Stream(AgentMsg))?');
    });

    test('a reference to an instantiated type drops its arguments', () {
      // Slot(A) is monomorphic once instantiated, so a reference to it inside
      // the vocabulary, Reply(A) in Slot, is bare too: a reference that kept
      // its arguments would name a type that no longer exists.
      final slot = med.typeDefs.firstWhere((d) => d.name == 'Slot');
      expect(printTypeDef(slot), 'Slot ::= none ; ask(Reply, ReqId).');
      final agentMsg = med.typeDefs.firstWhere((d) => d.name == 'AgentMsg');
      expect(printTypeDef(agentMsg),
          'AgentMsg ::= ask(Constant, $contextTypeName, Reply?, ReqId?, Deadline)'
          ' ; abort(ReqId).');
    });

    test("an expanded name off the checker's environment prints as source", () {
      // Stream<Coin> is the environment's internal name for an instantiation
      // and is not source syntax; it is printed as the instantiation it stands
      // for.
      expect(typeSource(TypeRef('Stream<Coin>', 0, 0)), 'Stream(Coin)');
      expect(typeSource(TypeRef('Channel<Closed, Stream<Coin>>', 0, 0)),
          'Channel(Closed, Stream(Coin))');
      expect(typeSource(TypeRef('Stream<Coin>', 0, 0, isInput: true)),
          'Stream(Coin)?');
    });
  });

  group('the clauses are carried over unchanged', () {
    test('they mention no type, so instantiation leaves them alone', () {
      expect(med.procedures, same(source.clauses.procedures));
    });

    test('the timer is the single-element stream produced on the deadline', () {
      final timer = med.procedures.firstWhere((p) => p.name == 'timer');
      expect(printProcedures([timer]), contains('wait_until(D?)'));
    });

    test('the deadline names the delay in one place', () {
      final deadline = med.procedures.firstWhere((p) => p.name == 'deadline');
      expect(printProcedures([deadline]), contains('now(T)'));
      // now/1 is a body goal, not a guard.
      expect(printProcedures([deadline]), isNot(contains('now(T) |')));
    });

    test('two escrow clauses, and only the one with a deadline starts a timer',
        () {
      final medProc = med.procedures.firstWhere((p) => p.name == 'med');
      // escrow with deadline, escrow without, answer, timeout, abort
      expect(medProc.clauses, hasLength(5));
      final text = printProcedures([medProc]);
      expect(text, contains('Id?, deadline)'));
      expect(text, contains('Id?, no_deadline)'));
      expect('timer('.allMatches(text), hasLength(1));
    });

    test('the mediator binds a non-arithmetic term with =, never :=', () {
      // Id := req(N?) is an arithmetic assignment of a non-arithmetic term and
      // fails at run time; the paper writes Id = req(N?).
      final text = printProcedures(med.procedures);
      expect(text, contains('Id = req(N?)'));
      expect(text, contains('R = then(Vs?)'));
      expect(text, contains('R = else'));
      expect(text, isNot(contains(':= req(')));
      expect(text, isNot(contains(':= then(')));
      expect(text, isNot(contains(':= else')));
    });
  });
}
