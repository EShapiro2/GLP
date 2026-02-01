/// Test actor boot file in single-isolate dGLP mode
/// This verifies the GLP logic works before testing in madGLP (multi-isolate)

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:glp_runtime/runtime/external_io.dart';

void main() {
  group('Actor Single Isolate Tests', () {
    /// Load the actor boot file source
    String loadActorBootSource() {
      final paths = [
        '/Users/udi/Grassroots/GLP/programs/typed_book/social_graph/play_alice_bob_charlie_actor_boot.glp',
        'programs/typed_book/social_graph/play_alice_bob_charlie_actor_boot.glp',
      ];

      for (final path in paths) {
        final file = File(path);
        if (file.existsSync()) {
          return file.readAsStringSync();
        }
      }
      throw StateError('Could not find actor boot file');
    }

    /// Strip boot clause for single-isolate testing
    String stripBootClause(String source) {
      // Remove procedure boot. and boot :- ... .
      var result = source.replaceFirst(
        RegExp(r'procedure\s+boot\s*\.\s*\n?', multiLine: true),
        '',
      );
      result = result.replaceFirst(
        RegExp(r'boot\s*:-\s*.*?\.\s*\n?', multiLine: true, dotAll: true),
        '',
      );
      return result.trim() + '\n';
    }

    test('compiles actor boot file without boot clause', () {
      final source = loadActorBootSource();
      final strippedSource = stripBootClause(source);

      print('\n=== Compiling actor boot file (stripped) ===');
      print('Source length: ${strippedSource.length} chars');

      final compiler = GlpCompiler();
      final program = compiler.compile(strippedSource);

      print('Compiled: ${program.ops.length} ops');
      print('Labels: ${program.labels.keys.take(20).join(', ')}...');
      expect(program.ops.length, greaterThan(0));
      expect(program.labels.containsKey('actor/2'), isTrue, reason: 'Should have actor/2');
    });

    test('traces actor(alice, Ch) execution', () async {
      final source = loadActorBootSource();
      final strippedSource = stripBootClause(source);

      print('\n=== Tracing actor(alice, Ch) ===');

      final compiler = GlpCompiler();
      final program = compiler.compile(strippedSource);
      final runtime = GlpRuntime();
      final runner = BytecodeRunner(program);
      final scheduler = Scheduler(rt: runtime, runners: {'main': runner});

      // Create a channel for the actor using the external_io helper
      // This creates a proper channel with input and output streams
      final actorChannel = createExternalChannel(runtime.heap, 'actor');

      print('Created channel for actor:');
      print('  inputReaderAddr: ${actorChannel.inputReaderAddr}');
      print('  inputWriterAddr: ${actorChannel.inputWriterAddr}');
      print('  outputReaderAddr: ${actorChannel.outputReaderAddr}');
      print('  outputWriterAddr: ${actorChannel.outputWriterAddr}');

      // Build the channel term: ch(InputReader, OutputWriter)
      final chTerm = buildChannelTerm(actorChannel);
      print('Channel term: $chTerm');

      // Create goal: actor(alice, Ch)
      // Per spec v2.16.3: CallEnv arguments must be VarRefs, so store terms on heap first
      final aliceAddr = runtime.heap.storeTermOnHeap(rt.ConstTerm('alice'));
      final chAddr = runtime.heap.storeTermOnHeap(chTerm);
      print('Stored alice at addr=$aliceAddr, ch at addr=$chAddr');

      final goalId = 1;
      final args = <int, rt.Term>{
        0: rt.VarRef(aliceAddr),
        1: rt.VarRef(chAddr),
      };
      runtime.setGoalEnv(goalId, CallEnv(args: args));
      runtime.setGoalProgram(goalId, 'main');

      final entryPC = program.labels['actor/2'];
      if (entryPC == null) {
        fail('No actor/2 label found');
      }
      runtime.gq.enqueue(GoalRef(goalId, entryPC));

      print('\nGoal spawned: actor(alice, ch(...)) at PC $entryPC');

      // Run with tracing
      for (int cycle = 1; cycle <= 20; cycle++) {
        print('\n=== Cycle $cycle ===');

        if (runtime.gq.isEmpty) {
          print('Goal queue empty - execution complete');
          break;
        }

        // Peek at goal queue
        final goals = <GoalRef>[];
        while (!runtime.gq.isEmpty) {
          goals.add(runtime.gq.dequeue()!);
        }

        for (final g in goals) {
          final label = program.labels.entries
              .where((e) => e.value == g.pc)
              .map((e) => e.key)
              .firstOrNull ?? 'PC${g.pc}';
          print('Goal ${g.id} at $label (PC ${g.pc})');

          // Dump environment
          final env = runtime.getGoalEnv(g.id);
          if (env != null) {
            for (final slot in env.argBySlot.keys.toList()..sort()) {
              final term = env.argBySlot[slot];
              print('  Arg slot $slot: $term');
              if (term is rt.VarRef) {
                final addr = term.addr;
                final isReaderVar = runtime.heap.isReader(addr);
                final isBound = runtime.heap.isBound(addr);
                final value = runtime.heap.getValue(addr);
                print('    -> addr=$addr, isReader=$isReaderVar, isBound=$isBound, value=$value');
              }
            }
          }

          runtime.gq.enqueue(g);
        }

        // Run one cycle
        final status = await scheduler.drainAsyncWithStatus(maxCycles: 1, debug: true);
        print('Cycle result: $status');

        // Check what actor wrote to output
        final outputValue = runtime.heap.getValue(actorChannel.outputWriterAddr);
        print('Actor output (writer ${actorChannel.outputWriterAddr}): $outputValue');

        if (status == ExecutionStatus.succeeded || status == ExecutionStatus.failed) {
          break;
        }
      }

      // Final check
      print('\n=== Final State ===');
      final outputValue = runtime.heap.getValue(actorChannel.outputWriterAddr);
      print('Actor output: $outputValue');

      // The actor should have written msg(user, alice, connect(bob))
      expect(outputValue, isNotNull);
    });

    test('traces simple send defined guard', () async {
      // Test just the send guard in isolation
      // The send guard pattern: send(X, ch(In, [X?|Out?]), ch(In?, Out))
      // This EXTENDS an existing output stream by prepending X
      //
      // Call: send(hello, ChIn?, ChOut)
      // ChIn = ch(In?, OutWriter) where OutWriter is unbound
      //
      // The pattern ch(In, [X?|Out?]) needs OutWriter to be [X?|Out?]
      // But OutWriter is just a variable, not a list!
      //
      // This means send CONSTRUCTS the list cell [X|Tail] and binds OutWriter to it
      final source = '''
send(X, ch(In, [X?|Out?]), ch(In?, Out)).

procedure test_send(Channel?, Channel).
test_send(ch(In, Out?), ch(In1?, Out1)) :-
    send(hello, ch(In?, Out), ch(In1, Out1?)) |
    true.
''';

      print('\n=== Testing send guard ===');

      final compiler = GlpCompiler();
      final program = compiler.compile(source);
      final runtime = GlpRuntime();
      final runner = BytecodeRunner(program);
      final scheduler = Scheduler(rt: runtime, runners: {'main': runner});

      print('Compiled: ${program.ops.length} ops');
      print('Labels: ${program.labels}');

      // Create a channel for test_send
      final ch = createExternalChannel(runtime.heap, 'test');
      final chTerm = buildChannelTerm(ch);

      // Per spec v2.16.3: CallEnv arguments must be VarRefs
      final chAddr = runtime.heap.storeTermOnHeap(chTerm);
      print('Stored ch at addr=$chAddr');

      final goalId = 1;
      final args = <int, rt.Term>{
        0: rt.VarRef(chAddr),
      };
      runtime.setGoalEnv(goalId, CallEnv(args: args));
      runtime.setGoalProgram(goalId, 'main');

      final entryPC = program.labels['test_send/1'];
      runtime.gq.enqueue(GoalRef(goalId, entryPC!));

      print('\nRunning test_send/1');

      final status = await scheduler.drainAsyncWithStatus(maxCycles: 10, debug: true);
      print('Result: $status');

      // Check output
      final outputValue = runtime.heap.getValue(ch.outputWriterAddr);
      print('Output: $outputValue');
    });
  });
}
