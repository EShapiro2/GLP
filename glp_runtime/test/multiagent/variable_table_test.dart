/// Unit tests for VariableTable (V_p)
/// 
/// Tests the corrected semantics from irmaGLP-spec.md v1.1
library;

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/variable_table.dart';

void main() {
  group('VariableEntry', () {
    test('creates entry with all fields', () {
      final entry = VariableEntry(
        varId: 42,
        creator: 'alice',
        role: VariableRole.writer,
        state: 'hello',
      );
      
      expect(entry.varId, 42);
      expect(entry.creator, 'alice');
      expect(entry.role, VariableRole.writer);
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
        role: VariableRole.writer,
        state: 'value',
      );
      
      final str = entry.toString();
      expect(str, contains('42'));
      expect(str, contains('alice'));
      expect(str, contains('writer'));
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
        role: VariableRole.writer,
      );
      
      vp.add(42, entry);
      
      expect(vp.contains(42), isTrue);
      expect(vp.length, 1);
      
      final retrieved = vp.lookup(42);
      expect(retrieved, isNotNull);
      expect(retrieved!.varId, 42);
      expect(retrieved.creator, 'alice');
      expect(retrieved.role, VariableRole.writer);
    });
    
    test('remove entry', () {
      final vp = VariableTable('alice');
      final entry = VariableEntry(
        varId: 42,
        creator: 'alice',
        role: VariableRole.writer,
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
        role: VariableRole.writer,
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
      
      vp.add(1, VariableEntry(varId: 1, creator: 'alice', role: VariableRole.writer));
      vp.add(2, VariableEntry(varId: 2, creator: 'bob', role: VariableRole.importedReader));
      
      expect(vp.length, 2);
      
      vp.clear();
      expect(vp.isEmpty, isTrue);
      expect(vp.length, 0);
    });
  });
  
  group('VariableTable - Writer Invariant (CRITICAL)', () {
    test('writer entry must have creator = agentId', () {
      final vp = VariableTable('alice');
      
      // This should succeed
      final validEntry = VariableEntry(
        varId: 42,
        creator: 'alice',
        role: VariableRole.writer,
      );
      vp.add(42, validEntry);
      expect(vp.lookup(42), isNotNull);
    });
    
    test('writer entry with wrong creator throws AssertionError', () {
      final vp = VariableTable('alice');
      
      // This should throw - writer from bob cannot be in alice's table
      final invalidEntry = VariableEntry(
        varId: 43,
        creator: 'bob',
        role: VariableRole.writer,
      );
      
      expect(
        () => vp.add(43, invalidEntry),
        throwsA(isA<AssertionError>()),
      );
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
      
      vp.add(1, VariableEntry(varId: 1, creator: 'alice', role: VariableRole.writer));
      vp.add(2, VariableEntry(varId: 2, creator: 'bob', role: VariableRole.importedReader));
      vp.add(3, VariableEntry(varId: 3, creator: 'alice', role: VariableRole.createdReader));
      vp.add(4, VariableEntry(varId: 4, creator: 'charlie', role: VariableRole.importedReader));
      
      final aliceEntries = vp.getByCreator('alice');
      expect(aliceEntries.length, 2);
      expect(aliceEntries.map((e) => e.varId).toSet(), {1, 3});
      
      final bobEntries = vp.getByCreator('bob');
      expect(bobEntries.length, 1);
      expect(bobEntries.first.varId, 2);
      
      final davidEntries = vp.getByCreator('david');
      expect(davidEntries, isEmpty);
    });
  });
  
  group('VariableTable - State Semantics', () {
    test('writer state holds bound value or null', () {
      final vp = VariableTable('alice');
      
      // Unbound writer
      final unboundWriter = VariableEntry(
        varId: 10,
        creator: 'alice',
        role: VariableRole.writer,
        state: null,
      );
      vp.add(10, unboundWriter);
      expect(vp.lookup(10)!.state, isNull);
      
      // Bind writer to value
      vp.updateState(10, 'hello');
      expect(vp.lookup(10)!.state, 'hello');
    });
    
    test('created reader state holds requester or null', () {
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
        role: VariableRole.writer,
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
        role: VariableRole.writer,
      ));
      
      // X? returns to alice (both parts local)
      vp.remove(200);
      
      expect(vp.contains(200), isFalse);
    });
  });
  
  group('VariableTable - Multiple Entries', () {
    test('handles multiple variables correctly', () {
      final vp = VariableTable('alice');
      
      // Alice exports writer W1
      vp.add(1, VariableEntry(
        varId: 1,
        creator: 'alice',
        role: VariableRole.writer,
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
      
      expect(vp.length, 3);
      expect(vp.varIds.toSet(), {1, 2, 3});
      
      // Verify each entry
      expect(vp.lookup(1)!.role, VariableRole.writer);
      expect(vp.lookup(2)!.role, VariableRole.createdReader);
      expect(vp.lookup(3)!.role, VariableRole.importedReader);
    });
    
    test('toString provides readable output', () {
      final vp = VariableTable('alice');
      
      vp.add(1, VariableEntry(
        varId: 1,
        creator: 'alice',
        role: VariableRole.writer,
      ));
      
      final str = vp.toString();
      expect(str, contains('alice'));
      expect(str, contains('1'));
      expect(str, contains('writer'));
    });
  });
}
