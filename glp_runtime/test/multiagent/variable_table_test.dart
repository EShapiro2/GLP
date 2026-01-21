/// Unit tests for VariableTable (V_p)
/// 
/// Tests the semantics from irmaGLP-spec.md v2.1
/// Updated for VarKey composite key architecture
library;

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/variable_table.dart';
import 'package:glp_runtime/runtime/terms.dart';

void main() {
  group('VarKey', () {
    test('equality works correctly', () {
      final key1 = VarKey(42, true);
      final key2 = VarKey(42, true);
      final key3 = VarKey(42, false);
      final key4 = VarKey(43, true);
      
      expect(key1, equals(key2));
      expect(key1, isNot(equals(key3)));
      expect(key1, isNot(equals(key4)));
    });
    
    test('hashCode consistent with equality', () {
      final key1 = VarKey(42, true);
      final key2 = VarKey(42, true);
      
      expect(key1.hashCode, equals(key2.hashCode));
    });
    
    test('toString shows reader/writer', () {
      final reader = VarKey(42, true);
      final writer = VarKey(42, false);
      
      expect(reader.toString(), contains('42'));
      expect(reader.toString(), contains('?'));
      expect(writer.toString(), contains('42'));
      expect(writer.toString(), isNot(contains('?')));
    });
  });
  
  group('VariableEntry', () {
    test('creates entry with all fields', () {
      final entry = VariableEntry(
        varId: 42,
        isReader: false,
        creator: 'alice',
        role: VariableRole.createdWriter,
        requester: 'bob',
      );

      expect(entry.varId, 42);
      expect(entry.isReader, false);
      expect(entry.creator, 'alice');
      expect(entry.role, VariableRole.createdWriter);
      expect(entry.requester, 'bob');
    });

    test('creates entry with null typed fields', () {
      final entry = VariableEntry(
        varId: 42,
        isReader: true,
        creator: 'alice',
        role: VariableRole.createdReader,
      );

      expect(entry.requester, isNull);
      expect(entry.requestSent, isFalse);
      expect(entry.boundValue, isNull);
    });

    test('toString includes typed fields', () {
      final entry = VariableEntry(
        varId: 42,
        isReader: false,
        creator: 'alice',
        role: VariableRole.createdWriter,
        requester: 'bob',
      );

      final str = entry.toString();
      expect(str, contains('42'));
      expect(str, contains('alice'));
      expect(str, contains('createdWriter'));
      expect(str, contains('requester=bob'));
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
      final key = VarKey(42, false);
      final entry = VariableEntry(
        varId: 42,
        isReader: false,
        creator: 'alice',
        role: VariableRole.createdWriter,
      );
      
      vp.add(key, entry);
      
      expect(vp.contains(key), isTrue);
      expect(vp.length, 1);
      
      final retrieved = vp.lookup(key);
      expect(retrieved, isNotNull);
      expect(retrieved!.varId, 42);
      expect(retrieved.creator, 'alice');
      expect(retrieved.role, VariableRole.createdWriter);
    });
    
    test('same varId can have reader and writer entries', () {
      final vp = VariableTable('alice');
      final readerKey = VarKey(42, true);
      final writerKey = VarKey(42, false);
      
      vp.add(readerKey, VariableEntry(
        varId: 42,
        isReader: true,
        creator: 'alice',
        role: VariableRole.createdReader,
      ));
      
      vp.add(writerKey, VariableEntry(
        varId: 42,
        isReader: false,
        creator: 'alice',
        role: VariableRole.createdWriter,
      ));
      
      expect(vp.length, 2);
      expect(vp.lookup(readerKey)!.role, VariableRole.createdReader);
      expect(vp.lookup(writerKey)!.role, VariableRole.createdWriter);
    });
    
    test('remove entry', () {
      final vp = VariableTable('alice');
      final key = VarKey(42, false);
      final entry = VariableEntry(
        varId: 42,
        isReader: false,
        creator: 'alice',
        role: VariableRole.createdWriter,
      );
      
      vp.add(key, entry);
      expect(vp.contains(key), isTrue);
      
      vp.remove(key);
      expect(vp.contains(key), isFalse);
      expect(vp.lookup(key), isNull);
    });
    
    test('updateRequester modifies existing entry', () {
      final vp = VariableTable('alice');
      final key = VarKey(42, false);
      final entry = VariableEntry(
        varId: 42,
        isReader: false,
        creator: 'alice',
        role: VariableRole.createdWriter,
      );

      vp.add(key, entry);
      expect(vp.lookup(key)!.requester, isNull);

      vp.updateRequester(key, 'bob');
      expect(vp.lookup(key)!.requester, 'bob');
    });

    test('markRequestSent modifies existing entry', () {
      final vp = VariableTable('alice');
      final key = VarKey(42, true);
      final entry = VariableEntry(
        varId: 42,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
      );

      vp.add(key, entry);
      expect(vp.lookup(key)!.requestSent, isFalse);

      vp.markRequestSent(key);
      expect(vp.lookup(key)!.requestSent, isTrue);
    });

    test('updateRequester throws on missing entry', () {
      final vp = VariableTable('alice');
      final key = VarKey(999, false);

      expect(
        () => vp.updateRequester(key, 'value'),
        throwsArgumentError,
      );
    });
    
    test('clear removes all entries', () {
      final vp = VariableTable('alice');
      
      vp.add(VarKey(1, false), VariableEntry(varId: 1, isReader: false, creator: 'alice', role: VariableRole.createdWriter));
      vp.add(VarKey(2, true), VariableEntry(varId: 2, isReader: true, creator: 'bob', role: VariableRole.importedReader));
      
      expect(vp.length, 2);
      
      vp.clear();
      expect(vp.isEmpty, isTrue);
      expect(vp.length, 0);
    });
  });
  
  group('VariableTable - Writer Types', () {
    test('created writer has creator = agentId', () {
      final vp = VariableTable('alice');
      final key = VarKey(42, false);
      
      final entry = VariableEntry(
        varId: 42,
        isReader: false,
        creator: 'alice',
        role: VariableRole.createdWriter,
      );
      vp.add(key, entry);
      
      expect(vp.lookup(key), isNotNull);
      expect(vp.lookup(key)!.creator, 'alice');
    });
    
    test('imported writer has creator != agentId', () {
      final vp = VariableTable('alice');
      final key = VarKey(43, false);
      
      // Alice imports writer from bob (e.g., via introduction)
      final entry = VariableEntry(
        varId: 43,
        isReader: false,
        creator: 'bob',
        role: VariableRole.importedWriter,
      );
      vp.add(key, entry);
      
      expect(vp.lookup(key), isNotNull);
      expect(vp.lookup(key)!.creator, 'bob');
      expect(vp.lookup(key)!.role, VariableRole.importedWriter);
    });
    
    test('readers can have different creators', () {
      final vp = VariableTable('alice');
      
      // Created reader: alice created it
      final createdReaderKey = VarKey(50, true);
      final createdReader = VariableEntry(
        varId: 50,
        isReader: true,
        creator: 'alice',
        role: VariableRole.createdReader,
      );
      vp.add(createdReaderKey, createdReader);
      expect(vp.lookup(createdReaderKey), isNotNull);
      
      // Imported reader: bob created it
      final importedReaderKey = VarKey(51, true);
      final importedReader = VariableEntry(
        varId: 51,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
      );
      vp.add(importedReaderKey, importedReader);
      expect(vp.lookup(importedReaderKey), isNotNull);
    });
  });
  
  group('VariableTable - getByCreator', () {
    test('returns entries for specific creator', () {
      final vp = VariableTable('alice');
      
      vp.add(VarKey(1, false), VariableEntry(varId: 1, isReader: false, creator: 'alice', role: VariableRole.createdWriter));
      vp.add(VarKey(2, true), VariableEntry(varId: 2, isReader: true, creator: 'bob', role: VariableRole.importedReader));
      vp.add(VarKey(3, true), VariableEntry(varId: 3, isReader: true, creator: 'alice', role: VariableRole.createdReader));
      vp.add(VarKey(4, true), VariableEntry(varId: 4, isReader: true, creator: 'charlie', role: VariableRole.importedReader));
      vp.add(VarKey(5, false), VariableEntry(varId: 5, isReader: false, creator: 'bob', role: VariableRole.importedWriter));
      
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
  
  group('VariableTable - Typed Field Semantics', () {
    test('created writer requester field holds waiting agent', () {
      final vp = VariableTable('alice');
      final key = VarKey(10, false);

      // No requester yet
      final writer = VariableEntry(
        varId: 10,
        isReader: false,
        creator: 'alice',
        role: VariableRole.createdWriter,
      );
      vp.add(key, writer);
      expect(vp.lookup(key)!.requester, isNull);

      // Bob requests the paired reader
      vp.updateRequester(key, 'bob');
      expect(vp.lookup(key)!.requester, 'bob');
    });

    test('imported writer boundValue field holds value after binding', () {
      final vp = VariableTable('alice');
      final key = VarKey(11, false);

      // Unbound imported writer
      final importedWriter = VariableEntry(
        varId: 11,
        isReader: false,
        creator: 'bob',
        role: VariableRole.importedWriter,
      );
      vp.add(key, importedWriter);
      expect(vp.lookup(key)!.boundValue, isNull);

      // Bind writer to value (using Term in real code)
      vp.lookup(key)!.boundValue = ConstTerm('hello');
      expect(vp.lookup(key)!.boundValue, isNotNull);
    });

    test('created reader requester field holds waiting agent', () {
      final vp = VariableTable('alice');
      final key = VarKey(20, true);

      // No request yet
      final reader = VariableEntry(
        varId: 20,
        isReader: true,
        creator: 'alice',
        role: VariableRole.createdReader,
      );
      vp.add(key, reader);
      expect(vp.lookup(key)!.requester, isNull);

      // Bob requests this reader
      vp.updateRequester(key, 'bob');
      expect(vp.lookup(key)!.requester, 'bob');
    });

    test('imported reader requestSent field shows if request sent', () {
      final vp = VariableTable('alice');
      final key = VarKey(30, true);

      // Imported but not requested
      final importedReader = VariableEntry(
        varId: 30,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
      );
      vp.add(key, importedReader);
      expect(vp.lookup(key)!.requestSent, isFalse);

      // Request sent to creator
      vp.markRequestSent(key);
      expect(vp.lookup(key)!.requestSent, isTrue);
    });
  });
  
  group('VariableTable - Core Invariant', () {
    test('V_p contains exactly non-local variables', () {
      final vp = VariableTable('alice');
      final writerKey = VarKey(100, false);
      
      // Scenario: Alice exports writer X to Bob
      // X is in Alice's resolvent, X? is in Bob's resolvent
      // Therefore X is in Alice's V_p (X? is remote)
      vp.add(writerKey, VariableEntry(
        varId: 100,
        isReader: false,
        creator: 'alice',
        role: VariableRole.createdWriter,
      ));
      expect(vp.contains(writerKey), isTrue); // X is non-local (X? remote)
      
      // Scenario: Bob sends X? back to Alice (imports it)
      // Now both X and X? are in Alice's resolvent
      // Therefore X should be removed from V_p (both parts local)
      vp.remove(writerKey);
      expect(vp.contains(writerKey), isFalse); // X is fully local now
    });
    
    test('variable with both parts local should not be in V_p', () {
      final vp = VariableTable('alice');
      final key = VarKey(200, false);
      
      // If Alice has both writer X and reader X? locally,
      // neither should be in V_p
      // This is checked by implementation - add would create entry
      // but when both parts become local, we remove it
      
      // Start: X exported (in V_p)
      vp.add(key, VariableEntry(
        varId: 200,
        isReader: false,
        creator: 'alice',
        role: VariableRole.createdWriter,
      ));
      
      // X? returns to alice (both parts local)
      vp.remove(key);
      
      expect(vp.contains(key), isFalse);
    });
  });
  
  group('VariableTable - Introduction Scenario', () {
    test('Bob creates channel and exports to Alice and Charlie', () {
      // Bob's table after creating and exporting channel
      final vpBob = VariableTable('bob');
      
      // Bob creates channel: ch(AC?, CA) for Alice, ch(CA?, AC) for Charlie
      // Bob exports CA (writer) and AC? (reader) to Alice
      vpBob.add(VarKey(1, false), VariableEntry(varId: 1, isReader: false, creator: 'bob', role: VariableRole.createdWriter)); // CA
      vpBob.add(VarKey(2, true), VariableEntry(varId: 2, isReader: true, creator: 'bob', role: VariableRole.createdReader)); // AC?
      
      // Bob exports AC (writer) and CA? (reader) to Charlie
      vpBob.add(VarKey(3, false), VariableEntry(varId: 3, isReader: false, creator: 'bob', role: VariableRole.createdWriter)); // AC
      vpBob.add(VarKey(4, true), VariableEntry(varId: 4, isReader: true, creator: 'bob', role: VariableRole.createdReader)); // CA?
      
      expect(vpBob.length, 4);
      expect(vpBob.getByCreator('bob').length, 4);
    });
    
    test('Alice receives imported writer from Bob', () {
      // Alice's table after receiving channel from Bob
      final vpAlice = VariableTable('alice');
      
      // Alice imports CA (writer) and AC? (reader) from Bob
      vpAlice.add(VarKey(100, false), VariableEntry(
        varId: 100,
        isReader: false,
        creator: 'bob',
        role: VariableRole.importedWriter, // CA - created by Bob
      ));
      vpAlice.add(VarKey(101, true), VariableEntry(
        varId: 101,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader, // AC? - created by Bob
      ));
      
      expect(vpAlice.lookup(VarKey(100, false))!.role, VariableRole.importedWriter);
      expect(vpAlice.lookup(VarKey(101, true))!.role, VariableRole.importedReader);
    });
    
    test('Charlie receives imported writer and reader from Bob', () {
      // Charlie's table after receiving channel from Bob
      final vpCharlie = VariableTable('charlie');
      
      // Charlie imports AC (writer) and CA? (reader) from Bob
      vpCharlie.add(VarKey(200, false), VariableEntry(
        varId: 200,
        isReader: false,
        creator: 'bob',
        role: VariableRole.importedWriter, // AC - created by Bob
      ));
      vpCharlie.add(VarKey(201, true), VariableEntry(
        varId: 201,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader, // CA? - created by Bob
      ));
      
      expect(vpCharlie.lookup(VarKey(200, false))!.role, VariableRole.importedWriter);
      expect(vpCharlie.lookup(VarKey(201, true))!.role, VariableRole.importedReader);
    });
  });
  
  group('VariableTable - Multiple Entries', () {
    test('handles multiple variables correctly', () {
      final vp = VariableTable('alice');
      
      // Alice exports writer W1
      vp.add(VarKey(1, false), VariableEntry(
        varId: 1,
        isReader: false,
        creator: 'alice',
        role: VariableRole.createdWriter,
      ));
      
      // Alice creates reader R1 for remote writer
      vp.add(VarKey(2, true), VariableEntry(
        varId: 2,
        isReader: true,
        creator: 'alice',
        role: VariableRole.createdReader,
      ));
      
      // Alice imports reader R2 from Bob
      vp.add(VarKey(3, true), VariableEntry(
        varId: 3,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
      ));
      
      // Alice imports writer W2 from Bob (via introduction)
      vp.add(VarKey(4, false), VariableEntry(
        varId: 4,
        isReader: false,
        creator: 'bob',
        role: VariableRole.importedWriter,
      ));
      
      expect(vp.length, 4);
      
      // Verify each entry
      expect(vp.lookup(VarKey(1, false))!.role, VariableRole.createdWriter);
      expect(vp.lookup(VarKey(2, true))!.role, VariableRole.createdReader);
      expect(vp.lookup(VarKey(3, true))!.role, VariableRole.importedReader);
      expect(vp.lookup(VarKey(4, false))!.role, VariableRole.importedWriter);
    });
    
    test('toString provides readable output', () {
      final vp = VariableTable('alice');
      
      vp.add(VarKey(1, false), VariableEntry(
        varId: 1,
        isReader: false,
        creator: 'alice',
        role: VariableRole.createdWriter,
      ));
      
      final str = vp.toString();
      expect(str, contains('alice'));
      expect(str, contains('1'));
      expect(str, contains('createdWriter'));
    });
  });
  
  group('VariableTable - Composite Key Scenarios', () {
    test('Bob exports ch(AC?, CA) to Alice and ch(CA?, AC) to Charlie', () {
      // This is the key scenario that requires composite keys
      final vpBob = VariableTable('bob');
      
      // Variable IDs for the channel
      final acId = 1042; // AC writer / AC? reader
      final caId = 1043; // CA writer / CA? reader
      
      // To Alice: CA (writer) and AC? (reader)
      // To Charlie: AC (writer) and CA? (reader)
      
      // Register BOTH reader and writer entries for each varId
      // This is the key requirement that composite keys enable
      
      // For varId 1042 (AC/AC?):
      // - AC (writer) sent to Charlie → createdWriter entry
      // - AC? (reader) sent to Alice → createdReader entry
      vpBob.add(VarKey(acId, false), VariableEntry(
        varId: acId,
        isReader: false,
        creator: 'bob',
        role: VariableRole.createdWriter, // AC sent to Charlie
      ));
      vpBob.add(VarKey(acId, true), VariableEntry(
        varId: acId,
        isReader: true,
        creator: 'bob',
        role: VariableRole.createdReader, // AC? sent to Alice
      ));
      
      // For varId 1043 (CA/CA?):
      // - CA (writer) sent to Alice → createdWriter entry
      // - CA? (reader) sent to Charlie → createdReader entry
      vpBob.add(VarKey(caId, false), VariableEntry(
        varId: caId,
        isReader: false,
        creator: 'bob',
        role: VariableRole.createdWriter, // CA sent to Alice
      ));
      vpBob.add(VarKey(caId, true), VariableEntry(
        varId: caId,
        isReader: true,
        creator: 'bob',
        role: VariableRole.createdReader, // CA? sent to Charlie
      ));
      
      // Verify all 4 entries exist
      expect(vpBob.length, 4);
      
      // Verify we can look up both reader and writer for same varId
      expect(vpBob.lookup(VarKey(acId, false))!.role, VariableRole.createdWriter);
      expect(vpBob.lookup(VarKey(acId, true))!.role, VariableRole.createdReader);
      expect(vpBob.lookup(VarKey(caId, false))!.role, VariableRole.createdWriter);
      expect(vpBob.lookup(VarKey(caId, true))!.role, VariableRole.createdReader);
    });
  });
}
