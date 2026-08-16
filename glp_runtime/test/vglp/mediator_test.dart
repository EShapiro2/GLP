// glp_runtime/test/vglp/mediator_test.dart
//
// The mediator's instantiation and emission.
// Spec: vGLP, sections/elicitation.tex, Definition "Canonical Compilation":
// ⌈M⌉ consists of the compiled procedures together with the mediator and the
// timer, the mediator generic in A and X and instantiated at the program's.

import 'dart:io';

import 'package:test/test.dart';
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
          ['med', 'timer', 'deadline', 'lookup', 'aborts', 'med_split']));
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
          'Channel(Closed, Stream(AgentMsg($answerTypeName, $contextTypeName)))?');
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
    });
  });
}
