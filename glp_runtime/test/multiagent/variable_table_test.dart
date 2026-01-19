/// Unit tests for VariableTable (V_p)
/// 
/// Tests the semantics from irmaGLP-spec.md v2.1
library;

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/variable_table.dart';

void main() {
  group('VariableEntry', () {
    test('creates entry with all fields', () {
      final entry = VariableEntry(
        varId: 42,
        creator: 'alice',
        role: VariableRole.createdWriter,
        state: 'hello',
      );
      
      expect(entry.varId, 42);
      expect(entry.creator, 'alice');
      expect(entry.role, VariableRole.createdWriter);
      expect(entry.state, 'hello');
    });
    
    test('creates entry with null state', () {
      final entry = VariableEntry(
        varId: 42,
        creator: 'alice',
        role: VariableRole.createdReader,
      );
      
      expect(entry.state, isNull);
    });
    
    test('toString includes all fields', () {
      final entry = VariableEntry(
        varId: 42,
        creator: 'alice',
        role: VariableRole.createdWriter,
        state: 'value',
      );
      
      final str = entry.toString();
      expect(str, contains('42'));
      expect(str, contains('alice'));
      expect(str, contains('createdWriter'));
      expect(str, contains('value'));
    });
  });
  
  group('VariableTable - Basic Operations', () {
    test('creates empty table for agent', () {
      final vp = VariableTable('alice');
      
      expect(vp.agentId, 'alice');
      expect(vp.isEmpty, isTrue);
      expect(vp.length, 0);
    });
    
    test('add and lookup entry', () {
      final vp = VariableTable('alice');
      final entry = VariableEntry(
        varId: 42,
        creator: 'alice',
        role: VariableRole.createdWriter,
      );
      
      vp.add(42, entry);
      
      expect(vp.contains(42), isTrue);
      expect(vp.length, 1);
      
      final retrieved = vp.lookup(42);
      expect(retrieved, isNotNull);
      expect(retrieved!.varId, 42);
      expect(retrieved.creator, 'alice');
      expect(retrieved.role, VariableRole.createdWriter);
    });
    
    test('remove entry', () {
      final vp = VariableTable('alice');
      final entry = VariableEntry(
        varId: 42,
        creator: 'alice',
        role: VariableRole.createdWriter,
      );
      
      vp.add(42, entry);
      expect(vp.contains(42), isTrue);
      
      vp.remove(42);
      expect(vp.contains(42), isFalse);
      expect(vp.lookup(42), isNull);
    });
    
    test('updateState modifies existing entry', () {
      final vp = VariableTable('alice');
      final entry = VariableEntry(
        varId: 42,
        creator: 'alice',
        role: VariableRole.createdWriter,
        state: null,
      );
      
      vp.add(42, entry);
      expect(vp.lookup(42)!.state, isNull);
      
      vp.updateState(42, 'hello');
      expect(vp.lookup(42)!.state, 'hello');
    });
    
    test('updateState throws on missing entry', () {
      final vp = VariableTable('alice');
      
      expect(
        () => vp.updateState(999, 'value'),
        throwsArgumentError,
      );
    });
    
    test('clear removes all entries', () {
      final vp = VariableTable('alice');
      
      vp.add(1, VariableEntry(varId: 1, creator: 'alice', role: VariableRole.createdWriter));
      vp.add(2, VariableEntry(varId: 2, creator: 'bob', role: VariableRole.importedReader));
      
      expect(vp.length, 2);
      
      vp.clear();
      expect(vp.isEmpty, isTrue);
      expect(vp.length, 0);
    });
  });
  
  group('VariableTable - Writer Types', () {
    test('created writer has creator = agentId', () {
      final vp = VariableTable('alice');
      
      final entry = VariableEntry(
        varId: 42,
        creator: 'alice',
        role: VariableRole.createdWriter,
      );
      vp.add(42, entry);
      
      expect(vp.lookup(42), isNotNull);
      expect(vp.lookup(42)!.creator, 'alice');
    });
    
    test('imported writer has creator != agentId', () {
      final vp = VariableTable('alice');
      
      // Alice imports writer from bob (e.g., via introduction)
      final entry = VariableEntry(
        varId: 43,
        creator: 'bob',
        role: VariableRole.importedWriter,
      );
      vp.add(43, entry);
      
      expect(vp.lookup(43), isNotNull);
      expect(vp.lookup(43)!.creator, 'bob');
      expect(vp.lookup(43)!.role, VariableRole.importedWriter);
    });
    
    test('readers can have different creators', () {
      final vp = VariableTable('alice');
      
      // Created reader: alice created it
      final createdReader = VariableEntry(
        varId: 50,
        creator: 'alice',
        role: VariableRole.createdReader,
      );
      vp.add(50, createdReader);
      expect(vp.lookup(50), isNotNull);
      
      // Imported reader: bob created it
      final importedReader = VariableEntry(
        varId: 51,
        creator: 'bob',
        role: VariableRole.importedReader,
      );
      vp.add(51, importedReader);
      expect(vp.lookup(51), isNotNull);
    });
  });
  
  group('VariableTable - getByCreator', () {
    test('returns entries for specific creator', () {
      final vp = VariableTable('alice');
      
      vp.add(1, VariableEntry(varId: 1, creator: 'alice', role: VariableRole.createdWriter));
      vp.add(2, VariableEntry(varId: 2, creator: 'bob', role: VariableRole.importedReader));
      vp.add(3, VariableEntry(varId: 3, creator: 'alice', role: VariableRole.createdReader));
      vp.add(4, VariableEntry(varId: 4, creator: 'charlie', role: VariableRole.importedReader));
      vp.add(5, VariableEntry(varId: 5, creator: 'bob', role: VariableRole.importedWriter));
      
      final aliceEntries = vp.getByCreator('alice');
      expect(aliceEntries.length, 2);
      expect(aliceEntries.map((e) => e.varId).toSet(), {1, 3});
      
      final bobEntries = vp.getByCreator('bob');
      expect(bobEntries.length, 2);
      expect(bobEntries.map((e) => e.varId).toSet(), {2, 5});
      
      final davidEntries = vp.getByCreator('david');
      expect(davidEntries, isEmpty);
    });
  });
  
  group('VariableTable - State Semantics', () {
    test('created writer state holds bound value or null', () {
      final vp = VariableTable('alice');
      
      // Unbound writer
      final unboundWriter = VariableEntry(
        varId: 10,
        creator: 'alice',
        role: VariableRole.createdWriter,
        state: null,
      );
      vp.add(10, unboundWriter);
      expect(vp.lookup(10)!.state, isNull);
      
      // Bind writer to value
      vp.updateState(10, 'hello');
      expect(vp.lookup(10)!.state, 'hello');
    });
    
    test('imported writer state holds bound value or null', () {
      final vp = VariableTable('alice');
      
      // Unbound imported writer
      final unboundWriter = VariableEntry(
        varId: 11,
        creator: 'bob',
        role: VariableRole.importedWriter,
        state: null,
      );
      vp.add(11, unboundWriter);
      expect(vp.lookup(11)!.state, isNull);
      
      // Bind writer to value
      vp.updateState(11, 'world');
      expect(vp.lookup(11)!.state, 'world');
    });
    
    test('created reader state holds requester or value or null', () {
      final vp = VariableTable('alice');
      
      // No request yet
      final noRequest = VariableEntry(
        varId: 20,
        creator: 'alice',
        role: VariableRole.createdReader,
        state: null,
      );
      vp.add(20, noRequest);
      expect(vp.lookup(20)!.state, isNull);
      
      // Bob requests this reader
      vp.updateState(20, 'bob');
      expect(vp.lookup(20)!.state, 'bob');
    });
    
    test('imported reader state shows if request sent', () {
      final vp = VariableTable('alice');
      
      // Imported but not requested
      final notRequested = VariableEntry(
        varId: 30,
        creator: 'bob',
        role: VariableRole.importedReader,
        state: null,
      );
      vp.add(30, notRequested);
      expect(vp.lookup(30)!.state, isNull);
      
      // Request sent to bob (creator)
      vp.updateState(30, 'bob');
      expect(vp.lookup(30)!.state, 'bob');
    });
  });
  
  group('VariableTable - Core Invariant', () {
    test('V_p contains exactly non-local variables', () {
      final vp = VariableTable('alice');
      
      // Scenario: Alice exports writer X to Bob
      // X is in Alice's resolvent, X? is in Bob's resolvent
      // Therefore X is in Alice's V_p (X? is remote)
      vp.add(100, VariableEntry(
        varId: 100,
        creator: 'alice',
        role: VariableRole.createdWriter,
      ));
      expect(vp.contains(100), isTrue); // X is non-local (X? remote)
      
      // Scenario: Bob sends X? back to Alice (imports it)
      // Now both X and X? are in Alice's resolvent
      // Therefore X should be removed from V_p (both parts local)
      vp.remove(100);
      expect(vp.contains(100), isFalse); // X is fully local now
    });
    
    test('variable with both parts local should not be in V_p', () {
      final vp = VariableTable('alice');
      
      // If Alice has both writer X and reader X? locally,
      // neither should be in V_p
      // This is checked by implementation - add would create entry
      // but when both parts become local, we remove it
      
      // Start: X exported (in V_p)
      vp.add(200, VariableEntry(
        varId: 200,
        creator: 'alice',
        role: VariableRole.createdWriter,
      ));
      
      // X? returns to alice (both parts local)
      vp.remove(200);
      
      expect(vp.contains(200), isFalse);
    });
  });
  
  group('VariableTable - Introduction Scenario', () {
    test('Bob creates channel and exports to Alice and Charlie', () {
      // Bob's table after creating and exporting channel
      final vpBob = VariableTable('bob');
      
      // Bob creates channel: ch(AC?, CA) for Alice, ch(CA?, AC) for Charlie
      // Bob exports CA and AC? to Alice
      vpBob.add(1, VariableEntry(varId: 1, creator: 'bob', role: VariableRole.createdWriter)); // CA
      vpBob.add(2, VariableEntry(varId: 2, creator: 'bob', role: VariableRole.createdReader)); // AC?
      
      // Bob exports AC and CA? to Charlie
      vpBob.add(3, VariableEntry(varId: 3, creator: 'bob', role: VariableRole.createdWriter)); // AC
      vpBob.add(4, VariableEntry(varId: 4, creator: 'bob', role: VariableRole.createdReader)); // CA?
      
      expect(vpBob.length, 4);
      expect(vpBob.getByCreator('bob').length, 4);
    });
    
    test('Alice receives imported writer from Bob', () {
      // Alice's table after receiving channel from Bob
      final vpAlice = VariableTable('alice');
      
      // Alice imports CA (writer) and AC? (reader) from Bob
      vpAlice.add(100, VariableEntry(
        varId: 100,
        creator: 'bob',
        role: VariableRole.importedWriter, // CA - created by Bob
      ));
      vpAlice.add(101, VariableEntry(
        varId: 101,
        creator: 'bob',
        role: VariableRole.importedReader, // AC? - created by Bob
      ));
      
      expect(vpAlice.lookup(100)!.role, VariableRole.importedWriter);
      expect(vpAlice.lookup(101)!.role, VariableRole.importedReader);
    });
    
    test('Charlie receives imported writer and reader from Bob', () {
      // Charlie's table after receiving channel from Bob
      final vpCharlie = VariableTable('charlie');
      
      // Charlie imports AC (writer) and CA? (reader) from Bob
      vpCharlie.add(200, VariableEntry(
        varId: 200,
        creator: 'bob',
        role: VariableRole.importedWriter, // AC - created by Bob
      ));
      vpCharlie.add(201, VariableEntry(
        varId: 201,
        creator: 'bob',
        role: VariableRole.importedReader, // CA? - created by Bob
      ));
      
      expect(vpCharlie.lookup(200)!.role, VariableRole.importedWriter);
      expect(vpCharlie.lookup(201)!.role, VariableRole.importedReader);
    });
  });
  
  group('VariableTable - Multiple Entries', () {
    test('handles multiple variables correctly', () {
      final vp = VariableTable('alice');
      
      // Alice exports writer W1
      vp.add(1, VariableEntry(
        varId: 1,
        creator: 'alice',
        role: VariableRole.createdWriter,
      ));
      
      // Alice creates reader R1 for remote writer
      vp.add(2, VariableEntry(
        varId: 2,
        creator: 'alice',
        role: VariableRole.createdReader,
      ));
      
      // Alice imports reader R2 from Bob
      vp.add(3, VariableEntry(
        varId: 3,
        creator: 'bob',
        role: VariableRole.importedReader,
      ));
      
      // Alice imports writer W2 from Bob (via introduction)
      vp.add(4, VariableEntry(
        varId: 4,
        creator: 'bob',
        role: VariableRole.importedWriter,
      ));
      
      expect(vp.length, 4);
      expect(vp.varIds.toSet(), {1, 2, 3, 4});
      
      // Verify each entry
      expect(vp.lookup(1)!.role, VariableRole.createdWriter);
      expect(vp.lookup(2)!.role, VariableRole.createdReader);
      expect(vp.lookup(3)!.role, VariableRole.importedReader);
      expect(vp.lookup(4)!.role, VariableRole.importedWriter);
    });
    
    test('toString provides readable output', () {
      final vp = VariableTable('alice');
      
      vp.add(1, VariableEntry(
        varId: 1,
        creator: 'alice',
        role: VariableRole.createdWriter,
      ));
      
      final str = vp.toString();
      expect(str, contains('alice'));
      expect(str, contains('1'));
      expect(str, contains('createdWriter'));
    });
  });
}
