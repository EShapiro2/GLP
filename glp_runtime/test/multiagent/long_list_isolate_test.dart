/// A list of 50,000 elements crosses from one agent to another: a cold call
/// carrying it, Globalized at the sender and Localized at the receiver (IGLP
/// Definitions "Globalize" and "Localize"), in the canonical encoding (IGLP
/// app:in-networking, "Payloads": "A payload is one assignment message in the
/// canonical encoding"), and the receiver's person shown it in order.
///
/// Until 2026-10-02 the sender's and the receiver's walks of the term --- its
/// variables, its globalization and localization, its mapping to the wire and
/// back, its encoding and decoding, and the traces that print it --- each
/// recursed a Dart frame or more a list element, and a list of some tens of
/// thousands of elements overflowed the Dart stack before it left the sender
/// (Integration #4 Code, 2026-10-02 19:05 UTC, E4; GLP #3 Cowork, 2026-10-02
/// 20:58 UTC, "19:05" Q3).
library;

import 'dart:io';

import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';
import 'package:glp_runtime/multiagent/mad_context.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:test/test.dart';

const int _n = 50000;

String get _rootSelf => File('../programs/self.glp').absolute.path;

/// alice sends bob a list of 50,000 once it is complete, and bob checks that
/// it is n, n - 1, ..., 1, element by element, and shows the person `ok`.
const String _boot = '''
procedure boot.
boot :- alice_init(alice, _)@alice, bob_init(bob, _)@bob.

procedure alice_init(_?, _?).
alice_init(_, _) :- gen($_n, Xs, D), ship(D?, Xs?).

procedure bob_init(_?, _?).
bob_init(_, [msg(_, Xs) | _]) :- check(Xs?, $_n, R), report(R?).

procedure gen(Integer?, Stream(Integer), Done).
gen(0, [], done).
gen(N, [N?|Xs?], D?) :- N? > 0 | N1 := N? - 1, gen(N1?, Xs, D).

procedure ship(Done?, Stream(Integer)?).
ship(done, Xs) :- send_to_net([msg(bob, Xs?)]).

procedure check(Stream(Integer)?, Integer?, Constant).
check([X|Xs], N, R?) :- X? =?= N? | N1 := N? - 1, check(Xs?, N1?, R).
check([], 0, ok).

procedure report(Constant?).
report(R) :- send_to_user([R?]).
''';

/// The list [1, 2, ..., n] as a Dart term.
Term _ints(int n) {
  Term t = ConstTerm('nil');
  for (var k = n; k >= 1; k--) {
    t = StructTerm('.', [ConstTerm(k), t]);
  }
  return t;
}

void main() {
  test('a cold call carrying a list of 50,000 reaches the receiver whole',
      () {
    final alice = MadContext(agentId: 'alice', runtime: GlpRuntime());
    final rtBob = GlpRuntime();
    final bob = MadContext(agentId: 'bob', runtime: rtBob);
    final (netIn, _) = rtBob.heap.allocateVariable();
    bob.wp.initializeSerializerEntry(netIn);
    // The traces are on, so the terms they print are printed.
    final traces = <String>[];
    alice.traceSink = traces.add;
    bob.traceSink = traces.add;
    alice.onMessageReady = (to, msg) {
      expect(to, 'bob');
      bob.handleIncomingPayload(payload: msg.payload, fromAgent: 'alice');
    };

    alice.send(StructTerm('msg', [ConstTerm('bob'), _ints(_n)]), true, 'bob',
        0, 'bob');
    alice.flushMessages();

    final cell = rtBob.heap.getValue(netIn) as StructTerm;
    expect(cell.functor, '.');
    final msg = rtBob.heap.dereference(cell.args[0]) as StructTerm;
    expect(msg.functor, 'msg');
    expect((msg.args[0] as ConstTerm).value, 'bob');
    final values = <Object?>[];
    var cur = msg.args[1];
    while (cur is StructTerm) {
      values.add((cur.args[0] as ConstTerm).value);
      cur = cur.args[1];
    }
    expect((cur as ConstTerm).value, 'nil');
    expect(values, [for (var k = 1; k <= _n; k++) k]);
    expect(traces.any((t) => t.length > _n), isTrue,
        reason: 'a trace printed the list');
  });

  group('two agents in isolates', () {
    late IsolateManager manager;
    setUp(() => manager = IsolateManager());
    tearDown(() async => manager.shutdown());

    test('alice sends bob a list of 50,000 and bob reads it whole, in order',
        () async {
      final config = BootLoader().load(_boot);
      config.rootSelfGlpPath = _rootSelf;
      await manager.boot(config);
      manager.start();
      await manager.settle(timeout: const Duration(seconds: 120));

      expect(manager.faults, isEmpty);
      expect(manager.outputOf('bob'), ['ok'],
          reason: "bob's check met 50,000, 49,999, ..., 1 and then []");
    }, timeout: const Timeout(Duration(seconds: 180)));
  });
}
