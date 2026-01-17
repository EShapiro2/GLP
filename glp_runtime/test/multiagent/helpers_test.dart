/// Unit tests for irmaGLP Helper Routines
/// 
/// Tests abandon, request, export, and reactivate helpers
/// from irmaGLP-spec.md v1.1 Section 4
library;

import 'package:test/test.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/multiagent/variable_table.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';
import 'package:glp_runtime/multiagent/helpers.dart';

void main() {
  group('abandon(readerId) - CRITICAL: Only Readers', () {
    test('abandon imported reader notifies creator', () {
      final vp = VariableTable('alice');
      final mp = MessageQueue();
      final helpers = IrmaHelpers('alice');
      
      // Alice imported reader 100 from bob
      vp.add(100, VariableEntry(
        varId: 100,
        creator: 'bob',
        role: VariableRole.importedReader,
      ));
      
      // Alice abandons the reader
      helpers.abandon(100, vp, mp);
      
      // Should notify bob with WRITER (varId 100)
      expect(vp.contains(100), isFalse); // Removed from V_p
      expect(mp.countFor('bob'), 1);
      
      final msg = mp.poll('bob');
      expect(msg, isNotNull);
      expect(msg!.type, MessageType.abandon);
      expect(msg.destination, 'bob');
    });
    
    test('abandon created reader with requester notifies requester', () {
      final vp = VariableTable('alice');
      final mp = MessageQueue();
      final helpers = IrmaHelpers('alice');
      
      // Alice created reader 200, bob requested it
      vp.add(200, VariableEntry(
        varId: 200,
        creator: 'alice',
        role: VariableRole.createdReader,
        state: 'bob', // bob is requester
      ));
      
      // Alice abandons the reader
      helpers.abandon(200, vp, mp);
      
      // Should notify bob (requester) with WRITER
      expect(vp.contains(200), isFalse);
      expect(mp.countFor('bob'), 1);
      
      final msg = mp.poll('bob');
      expect(msg, isNotNull);
      expect(msg!.type, MessageType.abandon);
      expect(msg.destination, 'bob');
    });
    
    test('abandon created reader without requester just removes', () {
      final vp = VariableTable('alice');
      final mp = MessageQueue();
      final helpers = IrmaHelpers('alice');
      
      // Alice created reader 300, no requester yet
      vp.add(300, VariableEntry(
        varId: 300,
        creator: 'alice',
        role: VariableRole.createdReader,
        state: null, // No requester
      ));
      
      // Alice abandons the reader
      helpers.abandon(300, vp, mp);
      
      // Should just remove from V_p, no message
      expect(vp.contains(300), isFalse);
      expect(mp.isEmpty, isTrue);
    });
    
    test('abandon on missing variable does nothing', () {
      final vp = VariableTable('alice');
      final mp = MessageQueue();
      final helpers = IrmaHelpers('alice');
      
      // Variable 999 not in table
      helpers.abandon(999, vp, mp);
      
      // No error, no messages
      expect(mp.isEmpty, isTrue);
    });
  });
  
  group('request(readerId) - Idempotent', () {
    test('request sends message on first call', () {
      final vp = VariableTable('alice');
      final mp = MessageQueue();
      final helpers = IrmaHelpers('alice');
      
      // Alice imported reader 100 from bob, not requested yet
      vp.add(100, VariableEntry(
        varId: 100,
        creator: 'bob',
        role: VariableRole.importedReader,
        state: null,
      ));
      
      // First request
      helpers.request(100, 'alice', vp, mp);
      
      // Should update state and send message
      expect(vp.lookup(100)!.state, 'bob'); // Marked as requested
      expect(mp.countFor('bob'), 1);
      
      final msg = mp.poll('bob');
      expect(msg, isNotNull);
      expect(msg!.type, MessageType.readRequest);
    });
    
    test('request is idempotent - second call does nothing', () {
      final vp = VariableTable('alice');
      final mp = MessageQueue();
      final helpers = IrmaHelpers('alice');
      
      // Alice imported reader 100 from bob, already requested
      vp.add(100, VariableEntry(
        varId: 100,
        creator: 'bob',
        role: VariableRole.importedReader,
        state: 'bob', // Already requested
      ));
      
      // Second request (idempotent)
      helpers.request(100, 'alice', vp, mp);
      
      // No new message
      expect(mp.isEmpty, isTrue);
      expect(vp.lookup(100)!.state, 'bob'); // Unchanged
    });
    
    test('request on missing variable does nothing', () {
      final vp = VariableTable('alice');
      final mp = MessageQueue();
      final helpers = IrmaHelpers('alice');
      
      helpers.request(999, 'alice', vp, mp);
      
      expect(mp.isEmpty, isTrue);
    });
    
    test('request on created reader does nothing', () {
      final vp = VariableTable('alice');
      final mp = MessageQueue();
      final helpers = IrmaHelpers('alice');
      
      // Alice created this reader (not imported)
      vp.add(200, VariableEntry(
        varId: 200,
        creator: 'alice',
        role: VariableRole.createdReader,
      ));
      
      helpers.request(200, 'alice', vp, mp);
      
      // No request sent (it's our own reader)
      expect(mp.isEmpty, isTrue);
    });
    
    test('request on writer does nothing', () {
      final vp = VariableTable('alice');
      final mp = MessageQueue();
      final helpers = IrmaHelpers('alice');
      
      // Alice holds writer
      vp.add(300, VariableEntry(
        varId: 300,
        creator: 'alice',
        role: VariableRole.writer,
      ));
      
      helpers.request(300, 'alice', vp, mp);
      
      // No request sent (writers don't need requests)
      expect(mp.isEmpty, isTrue);
    });
  });
  
  group('reactivate(readerId, suspendedSet)', () {
    test('reactivates goals blocked on reader', () {
      final suspendedSet = <GoalRef, Set<int>>{};
      final helpers = IrmaHelpers('alice');
      
      // Goals suspended on different readers
      final goal1 = GoalRef(1, 100);
      final goal2 = GoalRef(2, 200);
      final goal3 = GoalRef(3, 300);
      
      suspendedSet[goal1] = {10, 20}; // Blocked on readers 10, 20
      suspendedSet[goal2] = {20, 30}; // Blocked on readers 20, 30
      suspendedSet[goal3] = {40};     // Blocked on reader 40
      
      // Reactivate on reader 20
      final reactivated = helpers.reactivate(20, suspendedSet);
      
      // Should reactivate goal1 and goal2
      expect(reactivated.length, 2);
      expect(reactivated.contains(goal1), isTrue);
      expect(reactivated.contains(goal2), isTrue);
      
      // Should remove from suspended set
      expect(suspendedSet.containsKey(goal1), isFalse);
      expect(suspendedSet.containsKey(goal2), isFalse);
      expect(suspendedSet.containsKey(goal3), isTrue); // Still suspended
    });
    
    test('reactivate on unblocking reader returns empty set', () {
      final suspendedSet = <GoalRef, Set<int>>{};
      final helpers = IrmaHelpers('alice');
      
      final goal1 = GoalRef(1, 100);
      suspendedSet[goal1] = {10, 20};
      
      // Reactivate on reader 99 (not blocking anyone)
      final reactivated = helpers.reactivate(99, suspendedSet);
      
      expect(reactivated, isEmpty);
      expect(suspendedSet.containsKey(goal1), isTrue); // Still suspended
    });
    
    test('reactivate on empty suspended set returns empty', () {
      final suspendedSet = <GoalRef, Set<int>>{};
      final helpers = IrmaHelpers('alice');
      
      final reactivated = helpers.reactivate(10, suspendedSet);
      
      expect(reactivated, isEmpty);
    });
    
    test('reactivate removes goal from suspended set', () {
      final suspendedSet = <GoalRef, Set<int>>{};
      final helpers = IrmaHelpers('alice');
      
      final goal = GoalRef(1, 100);
      suspendedSet[goal] = {10};
      
      expect(suspendedSet.length, 1);
      
      helpers.reactivate(10, suspendedSet);
      
      expect(suspendedSet, isEmpty);
    });
  });
  
  group('export(term, agentId, vp) - Local Variables', () {
    test('export local variable adds to V_p', () {
      final vp = VariableTable('alice');
      final helpers = IrmaHelpers('alice');
      final relayGoals = <GoalRef>[];
      
      // Alice exports local writer 100
      final term = VarRef(100, isReader: false);
      final result = helpers.export(
        term, 
        'alice', 
        vp, 
        relayGoals,
        (_, __) => [0, 0], // No allocation needed
      );
      
      // Should add to V_p as writer
      expect(vp.contains(100), isTrue);
      final entry = vp.lookup(100);
      expect(entry!.role, VariableRole.writer);
      expect(entry.creator, 'alice');
      
      // Term unchanged
      expect(result.term, isA<VarRef>());
      expect((result.term as VarRef).varId, 100);
      
      // No relay goals
      expect(relayGoals, isEmpty);
    });
    
    test('export local reader adds to V_p as createdReader', () {
      final vp = VariableTable('alice');
      final helpers = IrmaHelpers('alice');
      final relayGoals = <GoalRef>[];
      
      // Alice exports local reader 200
      final term = VarRef(200, isReader: true);
      final result = helpers.export(
        term, 
        'alice', 
        vp, 
        relayGoals,
        (_, __) => [0, 0],
      );
      
      // Should add to V_p as createdReader
      expect(vp.contains(200), isTrue);
      final entry = vp.lookup(200);
      expect(entry!.role, VariableRole.createdReader);
      expect(entry.creator, 'alice');
    });
  });
  
  group('export(term, agentId, vp) - Non-local Variables', () {
    test('export writer from other agent removes from V_p', () {
      final vp = VariableTable('alice');
      final helpers = IrmaHelpers('alice');
      final relayGoals = <GoalRef>[];
      
      // Alice has bob's writer (should not happen normally, but test removal)
      vp.add(100, VariableEntry(
        varId: 100,
        creator: 'bob',
        role: VariableRole.importedReader, // Pretend it's imported
      ));
      
      final term = VarRef(100, isReader: false);
      helpers.export(term, 'alice', vp, relayGoals, (_, __) => [0, 0]);
      
      // Should remove from V_p
      expect(vp.contains(100), isFalse);
    });
    
    test('export non-requested reader removes from V_p', () {
      final vp = VariableTable('alice');
      final helpers = IrmaHelpers('alice');
      final relayGoals = <GoalRef>[];
      
      // Alice has bob's reader, not requested
      vp.add(100, VariableEntry(
        varId: 100,
        creator: 'bob',
        role: VariableRole.importedReader,
        state: null,
      ));
      
      final term = VarRef(100, isReader: true);
      helpers.export(term, 'alice', vp, relayGoals, (_, __) => [0, 0]);
      
      // Should remove from V_p
      expect(vp.contains(100), isFalse);
    });
  });
  
  group('export(term, agentId, vp) - Relay Mechanism', () {
    test('export requested reader creates relay', () {
      final vp = VariableTable('alice');
      final helpers = IrmaHelpers('alice');
      final relayGoals = <GoalRef>[];
      
      // Alice has bob's reader, already requested
      vp.add(100, VariableEntry(
        varId: 100,
        creator: 'bob',
        role: VariableRole.importedReader,
        state: 'bob', // Request sent
      ));
      
      // Mock allocator returns fresh pair (500, 501)
      List<int> allocateFreshPair(int w, int r) {
        return [500, 501]; // writer=500, reader=501
      }
      
      final term = VarRef(100, isReader: true);
      final result = helpers.export(term, 'alice', vp, relayGoals, allocateFreshPair);
      
      // Term should be replaced with relay reader
      expect(result.term, isA<VarRef>());
      final replaced = result.term as VarRef;
      expect(replaced.varId, 501); // Relay reader
      expect(replaced.isReader, isTrue);
      
      // Should add relay reader to V_p
      expect(vp.contains(501), isTrue);
      final relayEntry = vp.lookup(501);
      expect(relayEntry!.role, VariableRole.createdReader);
      expect(relayEntry.creator, 'alice');
      
      // Should create forwarding goal
      expect(relayGoals.length, 1);
    });
  });
  
  group('export(term, agentId, vp) - Structures', () {
    test('export structure with local variables', () {
      final vp = VariableTable('alice');
      final helpers = IrmaHelpers('alice');
      final relayGoals = <GoalRef>[];
      
      // Structure with local variables
      final term = StructTerm('msg', [
        ConstTerm('alice'),
        VarRef(10, isReader: false),  // Local writer
        VarRef(20, isReader: true),   // Local reader
      ]);
      
      final result = helpers.export(term, 'alice', vp, relayGoals, (_, __) => [0, 0]);
      
      // Should add both variables to V_p
      expect(vp.contains(10), isTrue);
      expect(vp.contains(20), isTrue);
      expect(vp.lookup(10)!.role, VariableRole.writer);
      expect(vp.lookup(20)!.role, VariableRole.createdReader);
      
      // Structure preserved
      expect(result.term, isA<StructTerm>());
      final struct = result.term as StructTerm;
      expect(struct.functor, 'msg');
      expect(struct.args.length, 3);
    });
    
    test('export nested structure processes all variables', () {
      final vp = VariableTable('alice');
      final helpers = IrmaHelpers('alice');
      final relayGoals = <GoalRef>[];
      
      // Nested structure
      final term = StructTerm('outer', [
        StructTerm('inner', [
          VarRef(1, isReader: false),
          VarRef(2, isReader: true),
        ]),
        VarRef(3, isReader: false),
      ]);
      
      final result = helpers.export(term, 'alice', vp, relayGoals, (_, __) => [0, 0]);
      
      // All three variables should be in V_p
      expect(vp.contains(1), isTrue);
      expect(vp.contains(2), isTrue);
      expect(vp.contains(3), isTrue);
      
      // Structure preserved
      expect(result.term, isA<StructTerm>());
    });
    
    test('export constant term leaves V_p unchanged', () {
      final vp = VariableTable('alice');
      final helpers = IrmaHelpers('alice');
      final relayGoals = <GoalRef>[];
      
      final term = ConstTerm('hello');
      final result = helpers.export(term, 'alice', vp, relayGoals, (_, __) => [0, 0]);
      
      // V_p still empty
      expect(vp.isEmpty, isTrue);
      
      // Term unchanged
      expect(result.term, isA<ConstTerm>());
      expect((result.term as ConstTerm).value, 'hello');
    });
  });
  
  group('export(term, agentId, vp) - Already Exported', () {
    test('export same variable twice only adds once', () {
      final vp = VariableTable('alice');
      final helpers = IrmaHelpers('alice');
      
      // First export
      final term1 = VarRef(100, isReader: false);
      helpers.export(term1, 'alice', vp, [], (_, __) => [0, 0]);
      
      expect(vp.contains(100), isTrue);
      final initialLength = vp.length;
      
      // Second export of same variable
      final term2 = VarRef(100, isReader: false);
      helpers.export(term2, 'alice', vp, [], (_, __) => [0, 0]);
      
      // V_p length unchanged (already in table)
      expect(vp.length, initialLength);
    });
  });
  
  group('reactivate(readerId, suspendedSet) - Multiple Goals', () {
    test('reactivates all goals blocked on reader', () {
      final suspendedSet = <GoalRef, Set<int>>{};
      final helpers = IrmaHelpers('alice');
      
      // Three goals, all blocked on reader 50
      final goal1 = GoalRef(1, 100);
      final goal2 = GoalRef(2, 200);
      final goal3 = GoalRef(3, 300);
      
      suspendedSet[goal1] = {50};
      suspendedSet[goal2] = {50, 60};
      suspendedSet[goal3] = {50};
      
      final reactivated = helpers.reactivate(50, suspendedSet);
      
      // All three should reactivate
      expect(reactivated.length, 3);
      expect(reactivated.contains(goal1), isTrue);
      expect(reactivated.contains(goal2), isTrue);
      expect(reactivated.contains(goal3), isTrue);
      
      // All removed from suspended set
      expect(suspendedSet, isEmpty);
    });
    
    test('reactivate on partial blocker only affects relevant goals', () {
      final suspendedSet = <GoalRef, Set<int>>{};
      final helpers = IrmaHelpers('alice');
      
      final goal1 = GoalRef(1, 100);
      final goal2 = GoalRef(2, 200);
      final goal3 = GoalRef(3, 300);
      
      suspendedSet[goal1] = {10, 20}; // Blocked on 10 and 20
      suspendedSet[goal2] = {20};     // Blocked on 20 only
      suspendedSet[goal3] = {30};     // Blocked on 30 only
      
      // Reactivate on reader 20
      final reactivated = helpers.reactivate(20, suspendedSet);
      
      // goal1 and goal2 reactivate
      expect(reactivated.length, 2);
      expect(reactivated.contains(goal1), isTrue);
      expect(reactivated.contains(goal2), isTrue);
      
      // goal3 still suspended
      expect(suspendedSet.containsKey(goal3), isTrue);
      expect(suspendedSet[goal3], {30});
    });
  });
  
  group('Helper Integration', () {
    test('abandon + request workflow', () {
      final vp = VariableTable('alice');
      final mp = MessageQueue();
      final helpers = IrmaHelpers('alice');
      
      // Alice imports reader from bob
      vp.add(100, VariableEntry(
        varId: 100,
        creator: 'bob',
        role: VariableRole.importedReader,
      ));
      
      // Alice needs value - sends request
      helpers.request(100, 'alice', vp, mp);
      expect(mp.countFor('bob'), 1);
      expect(vp.lookup(100)!.state, 'bob');
      
      // Later, alice abandons the reader
      helpers.abandon(100, vp, mp);
      expect(mp.countFor('bob'), 2); // request + abandon
      expect(vp.contains(100), isFalse);
    });
    
    test('export + reactivate workflow', () {
      final vp = VariableTable('alice');
      final suspendedSet = <GoalRef, Set<int>>{};
      final helpers = IrmaHelpers('alice');
      
      // Goal suspended on reader 100
      final goal = GoalRef(1, 100);
      suspendedSet[goal] = {100};
      
      // Alice exports writer 100
      final term = VarRef(100, isReader: false);
      helpers.export(term, 'alice', vp, [], (_, __) => [0, 0]);
      
      expect(vp.contains(100), isTrue);
      
      // Reader 100 gets value - reactivate
      final reactivated = helpers.reactivate(100, suspendedSet);
      
      expect(reactivated.length, 1);
      expect(reactivated.contains(goal), isTrue);
    });
  });
}
