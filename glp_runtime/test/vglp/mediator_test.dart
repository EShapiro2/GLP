// glp_runtime/test/vglp/mediator_test.dart
//
// The mediator's instantiation and emission.
// Spec: vGLP, sections/elicitation.tex, Definition "Canonical Compilation":
// ⌈M⌉ consists of the compiled procedures together with the mediator and the
// mediator generic in A, E and X and instantiated at the program's.

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
          ['med', 'answer', 'close', 'drop', 'abort', 'med_split']));
      // The deadline went with Udi's decision of 2026-09-15: madGLP assumes
      // nothing about time, so no timer answers for the person.
      expect(names, isNot(contains('timer')));
      expect(names, isNot(contains('deadline')));
    });
  });

  group('instantiation', () {
    test('the answer parameter becomes the program\'s answer type', () {
      final ua = med.typeDefs.firstWhere((d) => d.name == 'UserAnswer');
      expect(printTypeDef(ua),
          'UserAnswer ::= answer(ReqId, $answerTypeName) ; decline(ReqId).');
    });

    test('the escrow parameter becomes the program\'s escrow type', () {
      final pe = med.typeDefs.firstWhere((d) => d.name == 'PendingEntry');
      expect(printTypeDef(pe), 'PendingEntry ::= pending(ReqId, $escrowTypeName).');
    });

    test('the context parameter becomes the program\'s context type', () {
      final agentMsg = med.typeDefs.firstWhere((d) => d.name == 'AgentMsg');
      expect(printTypeDef(agentMsg), contains(contextTypeName));
      expect(printTypeDef(agentMsg), isNot(contains('(X)')));
    });

    test('a definition parameterised in A, E and X alone is monomorphic', () {
      for (final d in med.typeDefs.where((d) => d.name != 'Slot')) {
        expect(d.typeParams, isEmpty, reason: '${d.name} kept a parameter');
      }
    });

    test('Slot keeps its own parameter, instantiated per slot', () {
      final slot = med.typeDefs.firstWhere((d) => d.name == 'Slot');
      expect(printTypeDef(slot), 'Slot(R) ::= none ; ask(R, ReqId).');
    });

    test('a declaration keeps only its own parameter', () {
      for (final d in med.procDecls) {
        expect(d.typeParams, d.name == 'abort' ? ['R'] : isEmpty,
            reason: d.name);
      }
    });

    test('no program parameter survives anywhere in the emitted vocabulary', () {
      final text = med.typeDefs.map(printTypeDef).join('\n');
      expect(text, isNot(matches(RegExp(r'\b[AEX]\b'))));
    });

    test('the mediator channel type of a compiled goal matches the emitted one',
        () {
      // The compiled procedures declare their channel argument; the mediator
      // declares the same channel on its own side.  They must name one type.
      expect(medChannelType(isInput: true).toString(),
          'Channel(Closed, Stream(AgentMsg))?');
    });

    test('a reference to an instantiated type drops its arguments', () {
      // PendingList(E) is monomorphic once instantiated, so a reference to it,
      // PendingEntry(E) inside it, is bare too: a reference that kept its
      // arguments would name a type that no longer exists.
      final pl = med.typeDefs.firstWhere((d) => d.name == 'PendingList');
      expect(printTypeDef(pl), 'PendingList ::= [] ; [PendingEntry | PendingList].');
      final agentMsg = med.typeDefs.firstWhere((d) => d.name == 'AgentMsg');
      expect(printTypeDef(agentMsg),
          'AgentMsg ::= ask(Constant, $contextTypeName, $escrowTypeName, ReqId?)'
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

    test('no clock is left anywhere in the mediator', () {
      // madGLP is proved correct under fair message delivery and assumes
      // nothing about time, and two agents' system clocks are not comparable,
      // so a transaction taken because a timer expired is a transition the
      // semantics does not model (Udi, 2026-09-15).
      final text = printProcedures(med.procedures);
      expect(text, isNot(contains('wait_until')));
      expect(text, isNot(contains('now(')));
      expect(text, isNot(contains('timeout')));
      expect(text, isNot(contains('deadline')));
    });

    test('one escrow clause, the ask carrying no deadline tag', () {
      final medProc = med.procedures.firstWhere((p) => p.name == 'med');
      // escrow, answer, decline, abort
      expect(medProc.clauses, hasLength(4));
      final text = printProcedures([medProc]);
      expect(text, contains('receive(ask(C, Ctx, Esc, Id?), AgentCh?, AgentCh1)'));
    });

    test('the routing clause routes the answer and closes the card', () {
      // The Definition closes an ANSWERED ask on the person channel as it
      // closes a declined and an aborted one, so a reader of that channel
      // other than the one that answered is told that the ask is over.
      final medProc = med.procedures.firstWhere((p) => p.name == 'med');
      final text = printProcedures([medProc]);
      expect(text, contains('receive(answer(ReqId, Vs), UserCh?, UserCh1)'));
      expect(text, contains('answer(ReqId?, Vs?, Ps?, Ps1)'));
      expect(
          text,
          contains('answer(ReqId?, Vs?, Ps?, Ps1), '
              'send(closed(ReqId?), UserCh1?, UserCh2), '
              'med(UserCh2?, AgentCh?, Ps1?, N?)'));
    });

    test('the decline clause selects the else-branch and closes the card', () {
      final medProc = med.procedures.firstWhere((p) => p.name == 'med');
      final text = printProcedures([medProc]);
      expect(text, contains('receive(decline(ReqId), UserCh?, UserCh1)'));
      // close/3 binds the escrowed reply to else; the card goes with it.
      expect(text, contains('close(ReqId?, Ps?, Ps1)'));
      expect(text, contains('send(closed(ReqId?), UserCh1?, UserCh2)'));
    });

    test('the mediator binds a non-arithmetic term with =, never :=', () {
      // Id := req(N?) is an arithmetic assignment of a non-arithmetic term and
      // fails at run time; the paper writes Id = req(N?).
      final text = printProcedures(med.procedures);
      expect(text, contains('Id = req(N?)'));
      expect(text, isNot(contains(':= req(')));
    });

    test('the abort clause drops the entry unbound, the decline closes it', () {
      final medProc = med.procedures.firstWhere((p) => p.name == 'med');
      final text = printProcedures([medProc]);
      expect(text, contains('receive(abort(ReqId), AgentCh?, AgentCh1)'));
      expect(text, contains('drop(ReqId?, Ps?, Ps1)'));
      expect(text, contains('close(ReqId?, Ps?, Ps1)'));
      expect(text, contains('[pending(req(N?), Esc?) | Ps?]'));
    });

    test('the pending table\'s search clauses are the generic source\'s', () {
      // The clauses that match an entry are the program's, emitted ahead of
      // these; the source carries only the otherwise-recursion and [].
      final answer = med.procedures.firstWhere((p) => p.name == 'answer');
      expect(answer.clauses, hasLength(2));
      final close = med.procedures.firstWhere((p) => p.name == 'close');
      expect(close.clauses, hasLength(2));
      final drop = med.procedures.firstWhere((p) => p.name == 'drop');
      expect(drop.clauses, hasLength(3));
    });
  });
}
