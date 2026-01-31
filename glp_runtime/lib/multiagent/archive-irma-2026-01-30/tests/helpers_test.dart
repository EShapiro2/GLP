/// Unit tests for irmaGLP Helper Routines
///
/// Tests abandon, request, export, and reactivate helpers
/// from irmaGLP-spec.md v3.0 Section 4
library;

import 'package:test/test.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart'; // For GoalRef in reactivate tests
import 'package:glp_runtime/multiagent/variable_table.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';
import 'package:glp_runtime/multiagent/helpers.dart';

/// Test helper: isReader callback using address parity (for tests without real heap)
/// Per spec Section 3.2.1, production code uses heap.isReader(addr).
bool testIsReader(int addr) => (addr & 1) == 1;

void main() {
  group('abandon(readerId) - CRITICAL: Only Readers', () {
    test('abandon imported reader notifies creator', () {
      final vp = VariableTable('alice');
      final mp = MessageQueue();
      final helpers = IrmaHelpers('alice');
      
      // Alice imported reader 100 from bob
      final key = VarKey(100, true); // reader
      vp.add(key, VariableEntry(
        varId: 100,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
      ));
      
      // Alice abandons the reader
      helpers.abandon(100, vp, mp);
      
      // Should notify bob with WRITER (varId 100)
      expect(vp.contains(key), isFalse); // Removed from V_p
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
      final key = VarKey(200, true); // reader
      vp.add(key, VariableEntry(
        varId: 200,
        isReader: true,
        creator: 'alice',
        role: VariableRole.createdReader,
        requester: 'bob', // bob is requester
      ));
      
      // Alice abandons the reader
      helpers.abandon(200, vp, mp);
      
      // Should notify bob (requester) with WRITER
      expect(vp.contains(key), isFalse);
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
      final key = VarKey(300, true); // reader
      vp.add(key, VariableEntry(
        varId: 300,
        isReader: true,
        creator: 'alice',
        role: VariableRole.createdReader,
        // requester defaults to null
      ));
      
      // Alice abandons the reader
      helpers.abandon(300, vp, mp);
      
      // Should just remove from V_p, no message
      expect(vp.contains(key), isFalse);
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
      final key = VarKey(100, true); // reader
      vp.add(key, VariableEntry(
        varId: 100,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
        // requestSent defaults to false
      ));

      // First request
      helpers.request(100, 'alice', vp, mp);

      // Should mark requestSent and send message
      expect(vp.lookup(key)!.requestSent, isTrue); // Marked as requested
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
      final key = VarKey(100, true); // reader
      vp.add(key, VariableEntry(
        varId: 100,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
        requestSent: true, // Already requested
      ));

      // Second request (idempotent)
      helpers.request(100, 'alice', vp, mp);

      // No new message
      expect(mp.isEmpty, isTrue);
      expect(vp.lookup(key)!.requestSent, isTrue); // Unchanged
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
      final key = VarKey(200, true); // reader
      vp.add(key, VariableEntry(
        varId: 200,
        isReader: true,
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
      final key = VarKey(300, false); // writer
      vp.add(key, VariableEntry(
        varId: 300,
        isReader: false,
        creator: 'alice',
        role: VariableRole.createdWriter,
      ));
      
      // request() looks up reader key, so it won't find the writer
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
      final relaySetups = <RelaySetup>[];
      
      // Alice exports local writer 100
      final term = VarRef(100);  // Writer at addr 100
      final result = helpers.export(
        term,
        'alice',
        vp,
        relaySetups,
        () => [0, 0], // No allocation needed
        testIsReader,
      );

      // Should add to V_p as createdWriter
      final key = VarKey(100, false);
      expect(vp.contains(key), isTrue);
      final entry = vp.lookup(key);
      expect(entry!.role, VariableRole.createdWriter);
      expect(entry.creator, 'alice');

      // Term unchanged
      expect(result.term, isA<VarRef>());
      expect((result.term as VarRef).addr, 100);

      // No relay setups
      expect(relaySetups, isEmpty);
    });

    test('export local reader adds to V_p as createdReader', () {
      final vp = VariableTable('alice');
      final helpers = IrmaHelpers('alice');
      final relaySetups = <RelaySetup>[];

      // Alice exports local reader at addr 201 (odd = reader)
      final term = VarRef(201);  // Reader at odd addr
      final result = helpers.export(
        term,
        'alice',
        vp,
        relaySetups,
        () => [0, 0],
        testIsReader,
      );

      // Should add to V_p as createdReader (key uses raw addr 201)
      final key = VarKey(201, true);
      expect(vp.contains(key), isTrue);
      final entry = vp.lookup(key);
      expect(entry!.role, VariableRole.createdReader);
      expect(entry.creator, 'alice');
    });
  });
  
  group('export(term, agentId, vp) - Non-local Variables', () {
    test('export writer from other agent removes from V_p', () {
      final vp = VariableTable('alice');
      final helpers = IrmaHelpers('alice');
      final relaySetups = <RelaySetup>[];

      // Alice has bob's writer (imported via introduction)
      final key = VarKey(100, false);
      vp.add(key, VariableEntry(
        varId: 100,
        isReader: false,
        creator: 'bob',
        role: VariableRole.importedWriter,
      ));

      final term = VarRef(100);  // Writer at addr 100
      helpers.export(term, 'alice', vp, relaySetups, () => [0, 0], testIsReader);

      // Should remove from V_p
      expect(vp.contains(key), isFalse);
    });

    test('export non-requested reader removes from V_p', () {
      final vp = VariableTable('alice');
      final helpers = IrmaHelpers('alice');
      final relaySetups = <RelaySetup>[];

      // Alice has bob's reader at addr 101, not requested
      final key = VarKey(101, true);
      vp.add(key, VariableEntry(
        varId: 101,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
        // requestSent defaults to false
      ));

      final term = VarRef(101);  // Reader at addr 101
      helpers.export(term, 'alice', vp, relaySetups, () => [0, 0], testIsReader);

      // Should remove from V_p
      expect(vp.contains(key), isFalse);
    });
  });

  group('export(term, agentId, vp) - Relay Mechanism', () {
    test('export requested reader creates relay', () {
      final vp = VariableTable('alice');
      final helpers = IrmaHelpers('alice');
      final relaySetups = <RelaySetup>[];

      // Alice has bob's reader at addr 101, already requested
      final key = VarKey(101, true);
      vp.add(key, VariableEntry(
        varId: 101,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
        requestSent: true, // Request sent
      ));

      // Mock allocator returns fresh pair (500, 501)
      List<int> allocateFreshPair() {
        return [500, 501]; // writer=500, reader=501
      }

      final term = VarRef(101);  // Reader at addr 101
      final result = helpers.export(term, 'alice', vp, relaySetups, allocateFreshPair, testIsReader);

      // Term should be replaced with relay reader
      expect(result.term, isA<VarRef>());
      final replaced = result.term as VarRef;
      expect(replaced.addr, 501); // Relay reader at addr 501
      expect(testIsReader(replaced.addr), isTrue); // Check via test helper

      // Should add relay reader Z? to V_p as created reader
      final relayReaderKey = VarKey(501, true);
      expect(vp.contains(relayReaderKey), isTrue);
      final relayReaderEntry = vp.lookup(relayReaderKey);
      expect(relayReaderEntry!.role, VariableRole.createdReader);
      expect(relayReaderEntry.creator, 'alice');

      // Should also add relay writer Z to V_p as created writer
      final relayWriterKey = VarKey(500, false);
      expect(vp.contains(relayWriterKey), isTrue);
      final relayWriterEntry = vp.lookup(relayWriterKey);
      expect(relayWriterEntry!.role, VariableRole.createdWriter);
      expect(relayWriterEntry.creator, 'alice');

      // Should create relay setup for forwarding
      // This implements: export_reader(Y?, Z) :- Z = Y?.
      expect(relaySetups.length, 1);
      expect(relaySetups[0].originalReaderId, 101); // Y? at addr 101
      expect(relaySetups[0].relayWriterId, 500);    // Z
      expect(relaySetups[0].relayReaderId, 501);    // Z?
    });
  });
  
  group('export(term, agentId, vp) - Structures', () {
    test('export structure with local variables', () {
      final vp = VariableTable('alice');
      final helpers = IrmaHelpers('alice');
      final relaySetups = <RelaySetup>[];

      // Structure with local variables
      final term = StructTerm('msg', [
        ConstTerm('alice'),
        VarRef(10),   // Local writer at addr 10 (even)
        VarRef(21),   // Local reader at addr 21 (odd)
      ]);

      final result = helpers.export(term, 'alice', vp, relaySetups, () => [0, 0], testIsReader);

      // Should add both variables to V_p (using raw addresses)
      final writerKey = VarKey(10, false);
      final readerKey = VarKey(21, true);
      expect(vp.contains(writerKey), isTrue);
      expect(vp.contains(readerKey), isTrue);
      expect(vp.lookup(writerKey)!.role, VariableRole.createdWriter);
      expect(vp.lookup(readerKey)!.role, VariableRole.createdReader);

      // Structure preserved
      expect(result.term, isA<StructTerm>());
      final struct = result.term as StructTerm;
      expect(struct.functor, 'msg');
      expect(struct.args.length, 3);
    });

    test('export nested structure processes all variables', () {
      final vp = VariableTable('alice');
      final helpers = IrmaHelpers('alice');
      final relaySetups = <RelaySetup>[];

      // Nested structure
      final term = StructTerm('outer', [
        StructTerm('inner', [
          VarRef(2),   // Writer at addr 2 (even)
          VarRef(3),   // Reader at addr 3 (odd)
        ]),
        VarRef(4),     // Writer at addr 4 (even)
      ]);

      final result = helpers.export(term, 'alice', vp, relaySetups, () => [0, 0], testIsReader);

      // All three variables should be in V_p (using raw addresses)
      expect(vp.contains(VarKey(2, false)), isTrue);  // Writer at addr 2
      expect(vp.contains(VarKey(3, true)), isTrue);   // Reader at addr 3
      expect(vp.contains(VarKey(4, false)), isTrue);  // Writer at addr 4

      // Structure preserved
      expect(result.term, isA<StructTerm>());
    });

    test('export constant term leaves V_p unchanged', () {
      final vp = VariableTable('alice');
      final helpers = IrmaHelpers('alice');
      final relaySetups = <RelaySetup>[];

      final term = ConstTerm('hello');
      final result = helpers.export(term, 'alice', vp, relaySetups, () => [0, 0], testIsReader);

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
      final relaySetups = <RelaySetup>[];

      // First export
      final term1 = VarRef(100);  // Writer at addr 100
      helpers.export(term1, 'alice', vp, relaySetups, () => [0, 0], testIsReader);

      final key = VarKey(100, false);
      expect(vp.contains(key), isTrue);
      final initialLength = vp.length;

      // Second export of same variable
      final term2 = VarRef(100);  // Writer at addr 100
      helpers.export(term2, 'alice', vp, relaySetups, () => [0, 0], testIsReader);

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
      final key = VarKey(100, true);
      vp.add(key, VariableEntry(
        varId: 100,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
      ));
      
      // Alice needs value - sends request
      helpers.request(100, 'alice', vp, mp);
      expect(mp.countFor('bob'), 1);
      expect(vp.lookup(key)!.requestSent, isTrue);
      
      // Later, alice abandons the reader
      helpers.abandon(100, vp, mp);
      expect(mp.countFor('bob'), 2); // request + abandon
      expect(vp.contains(key), isFalse);
    });
    
    test('export + reactivate workflow', () {
      final vp = VariableTable('alice');
      final suspendedSet = <GoalRef, Set<int>>{};
      final helpers = IrmaHelpers('alice');
      final relaySetups = <RelaySetup>[];

      // Goal suspended on reader 100
      final goal = GoalRef(1, 100);
      suspendedSet[goal] = {100};

      // Alice exports writer 100
      final term = VarRef(100);  // Writer at addr 100
      helpers.export(term, 'alice', vp, relaySetups, () => [0, 0], testIsReader);

      expect(vp.contains(VarKey(100, false)), isTrue);

      // Reader 100 gets value - reactivate
      final reactivated = helpers.reactivate(100, suspendedSet);

      expect(reactivated.length, 1);
      expect(reactivated.contains(goal), isTrue);
    });
  });
  
  group('Imported Writer Scenario', () {
    test('alice imports writer from bob and binds it', () {
      final vp = VariableTable('alice');
      final mp = MessageQueue();
      final helpers = IrmaHelpers('alice');
      
      // Alice imports writer CA from bob (via introduction)
      final key = VarKey(100, false);
      vp.add(key, VariableEntry(
        varId: 100,
        isReader: false,
        creator: 'bob',
        role: VariableRole.importedWriter,
        // boundValue defaults to null
      ));
      
      // When Alice binds the imported writer, she should notify bob
      // This is handled by processWriterBindings in IrmaContext
      // Here we just verify the V_p setup is correct
      expect(vp.lookup(key)!.role, VariableRole.importedWriter);
      expect(vp.lookup(key)!.creator, 'bob');
    });
  });
}
