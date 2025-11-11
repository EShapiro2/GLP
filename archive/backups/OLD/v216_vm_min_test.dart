import 'package:test/test.dart';

import 'package:glp_runtime/bytecode/v216/opcodes.dart';
import 'package:glp_runtime/bytecode/v216/runner.dart';
import 'package:glp_runtime/bytecode/v216/model.dart';

void main() {
  // Program for a single predicate with ONE unit clause "p(X)" where X='a'.
  // We model it as two clauses to accept either writer or reader at slot 0.
  // Clause 1: writer mode
  // Clause 2: reader mode
  final program = <Op>[
    // Clause 1: writer path
    const ClauseTry(),
    const HeadWriter(0),
    const HeadConstant(0, 'a'), // σ^w := {0 -> 'a'}
    const Commit(),             // apply σ^w
    const Proceed(),

    // Clause 2: reader path
    const ClauseTry(),
    const HeadReader(0),        // blocks if unbound; else continues
    const HeadConstant(0, 'a'), // requires value 'a'
    const Commit(),             // no pending writes here, still required by model
    const Proceed(),

    // End of predicate
    const NoMoreClauses(),
  ];

  group('v2.16 VM minimal', () {
    test('writer: success and bind to a', () {
      final vm = VM(program);
      final w = Writer();                             // unit goal p(W!)
      final frame = CallFrame('p', [w]);
      final out = vm.run(frame);
      expect(out, VmOutcome.success);
      expect(w.value, 'a');
    });

    test('reader: unbound -> suspend', () {
      final vm = VM(program);
      final r = Reader();                             // unit goal p(R?)
      final frame = CallFrame('p', [r]);
      final out = vm.run(frame);
      expect(out, VmOutcome.suspend);
      // blocker got added to U; runtime would suspend the goal on r.
    });

    test('reader: bound to a -> success', () {
      final vm = VM(program);
      final r = Reader('a');                          // unit goal p('a'?)
      final frame = CallFrame('p', [r]);
      final out = vm.run(frame);
      expect(out, VmOutcome.success);
    });

    test('reader: bound to b -> fail', () {
      final vm = VM(program);
      final r = Reader('b');                          // unit goal p('b'?) should not match
      final frame = CallFrame('p', [r]);
      final out = vm.run(frame);
      expect(out, VmOutcome.fail);
    });
  });
}
