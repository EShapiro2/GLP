/// Tests for global_send goal mechanism
///
/// Derived from madGLP-spec.md Section 4: The global_send Predicate
///
/// The global_send mechanism watches a reader and sends its value to a
/// remote agent when it becomes known (bound to a non-variable term).

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/global_send.dart';
import 'package:glp_runtime/multiagent/global_writers_table.dart';
import 'package:glp_runtime/multiagent/mad_helpers.dart';

void main() {
  group('GlobalSendGoal', () {
    test('fires when reader becomes known', () {
      // Given: goal registered watching reader at address 100
      final registry = GlobalSendRegistry('p');
      final table = GlobalWritersTable('p');

      final goal = GlobalSendGoal(
        readerAddr: 100,
        globalName: GlobalName.writer('p', 0),
        destination: 'q',
      );
      registry.register(goal);

      // Verify goal is registered
      expect(registry.hasGoalFor(100), isTrue);

      // When: writer (same address) is bound to value 42
      final result = registry.onWriterBound(
        writerAddr: 100,
        value: 42,
        table: table,
        extractVariables: (_) => [], // No nested variables
      );

      // Then: goal fires with value
      expect(result, isNotNull);
      expect(result!.value, 42);
      expect(result.globalName, GlobalName.writer('p', 0));
      expect(result.destination, 'q');

      // Spec Section 4: "The guard known(T) succeeds when T is bound to
      // a non-variable term."
    });

    test('produces correct message', () {
      // Given: goal with global name _w(p,0), destination q
      final registry = GlobalSendRegistry('p');
      final table = GlobalWritersTable('p');

      registry.register(GlobalSendGoal(
        readerAddr: 200,
        globalName: GlobalName.writer('p', 0),
        destination: 'q',
      ));

      // When: fires with value 'hello'
      final result = registry.onWriterBound(
        writerAddr: 200,
        value: 'hello',
        table: table,
        extractVariables: (_) => [],
      );

      // Then: message contains (_w(p,0) := 'hello', q)
      expect(result, isNotNull);
      expect(result!.globalName.isWriter, isTrue);
      expect(result.globalName.agent, 'p');
      expect(result.globalName.index, 0);
      expect(result.destination, 'q');
      expect(result.value, 'hello');

      // Spec Section 4: "The builtin '_send'(T, G, Q) globalizes T and
      // adds message (G := T↑, Q) to the agent's outgoing message set."
    });

    test('nested variables spawn additional goals', () {
      // Given: goal watching reader at 300
      final registry = GlobalSendRegistry('p');
      final table = GlobalWritersTable('p');

      registry.register(GlobalSendGoal(
        readerAddr: 300,
        globalName: GlobalName.writer('p', 0),
        destination: 'q',
      ));

      // When: fires with value containing nested reader variable Y? at 401
      // Under corrected definitions, globalizing a reader spawns a goal
      final result = registry.onWriterBound(
        writerAddr: 300,
        value: 'foo(Y?)', // Symbolic - actual term contains reader Y?
        table: table,
        extractVariables: (_) => [TermVar.reader(401, writerAddr: 400)], // Y? is a reader at 401
      );

      // Then: value is globalized, spawning new goal for Y?
      // Corrected spec: reader → spawn global_send(Y?, _r(p,i), q)
      expect(result, isNotNull);
      expect(result!.newGoals.length, 1);
      expect(result.newGoals[0].readerAddr, 400); // writer addr for onBind
      expect(result.newGoals[0].globalName.isReader, isTrue);
      expect(result.newGoals[0].destination, 'q');

      // Spec Section 12: "Globalization may spawn additional global_send
      // goals for nested variables in T. These new goals must be registered
      // before the current goal completes."
    });

    test('goal removed after firing', () {
      // Given: goal registered on reader at 500
      final registry = GlobalSendRegistry('p');
      final table = GlobalWritersTable('p');

      registry.register(GlobalSendGoal(
        readerAddr: 500,
        globalName: GlobalName.writer('p', 0),
        destination: 'q',
      ));
      expect(registry.hasGoalFor(500), isTrue);
      expect(registry.pendingCount, 1);

      // When: fires
      registry.onWriterBound(
        writerAddr: 500,
        value: 'done',
        table: table,
        extractVariables: (_) => [],
      );

      // Then: goal no longer registered (one-shot)
      expect(registry.hasGoalFor(500), isFalse);
      expect(registry.pendingCount, 0);

      // Implicit in spec: goals represent single global_send goal that
      // fires once and is removed
    });
  });

  group('GlobalSendRegistry', () {
    test('registerSpawns converts GlobalSendSpawn to goals', () {
      final registry = GlobalSendRegistry('p');

      // Create spawns from globalize/localize
      final spawns = [
        GlobalSendSpawn(
          readerAddr: 100,
          globalName: GlobalName.writer('p', 0),
          destAgent: 'q',
        ),
        GlobalSendSpawn(
          readerAddr: 200,
          globalName: GlobalName.reader('r', 5),
          destAgent: 'r',
        ),
      ];

      // Register them
      registry.registerSpawns(spawns);

      // Verify both are registered
      expect(registry.hasGoalFor(100), isTrue);
      expect(registry.hasGoalFor(200), isTrue);
      expect(registry.pendingCount, 2);

      // Verify goal details
      final goal1 = registry.getGoalFor(100)!;
      expect(goal1.globalName.isWriter, isTrue);
      expect(goal1.destination, 'q');

      final goal2 = registry.getGoalFor(200)!;
      expect(goal2.globalName.isReader, isTrue);
      expect(goal2.destination, 'r');
    });

    test('onWriterBound returns null when no goal registered', () {
      final registry = GlobalSendRegistry('p');
      final table = GlobalWritersTable('p');

      // No goal registered for address 999
      final result = registry.onWriterBound(
        writerAddr: 999,
        value: 'ignored',
        table: table,
        extractVariables: (_) => [],
      );

      expect(result, isNull);
    });
  });
}
