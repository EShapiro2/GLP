/// Test for utility and I/O system predicates
///
/// Tests current_time/1, unique_id/1, file_read/2, and file_write/2

import 'dart:io';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/system_predicates.dart';
import 'package:glp_runtime/runtime/system_predicates_impl.dart';
import 'package:test/test.dart';

void main() {
  group('Utility System Predicates', () {
    late GlpRuntime rt;

    setUp(() {
      rt = GlpRuntime();
      registerStandardPredicates(rt.systemPredicates);
    });

    test('current_time/1: binds to current timestamp', () {
      print('\n=== CURRENT_TIME TEST: Bind Timestamp ===');

      // Create writer for time
      const wT = 1;
      const rT = 2;
      rt.heap.addWriter(WriterCell(wT, rT));
      rt.heap.addReader(ReaderCell(rT));

      // Call current_time
      final call = SystemCall('current_time', [WriterTerm(wT)]);
      final result = currentTimePredicate(rt, call);

      // Should succeed
      expect(result, SystemResult.success);

      // Check that time was bound
      expect(rt.heap.isWriterBound(wT), true);
      final time = rt.heap.writerValue[wT];
      expect(time, isA<ConstTerm>());
      expect((time as ConstTerm).value, isA<int>());
      expect((time.value as int) > 0, true);

      print('Result: $result');
      print('Bound time: ${time.value}');
      print('✓ current_time/1 successfully bound timestamp\n');
    });

    test('unique_id/1: generates unique IDs', () {
      print('\n=== UNIQUE_ID TEST: Generate Multiple IDs ===');

      // Generate 3 unique IDs
      final ids = <int>[];

      for (var i = 0; i < 3; i++) {
        final (wId, rId) = rt.heap.allocateFreshPair();
        rt.heap.addWriter(WriterCell(wId, rId));
        rt.heap.addReader(ReaderCell(rId));

        final call = SystemCall('unique_id', [WriterTerm(wId)]);
        final result = uniqueIdPredicate(rt, call);

        expect(result, SystemResult.success);
        expect(rt.heap.isWriterBound(wId), true);

        final idTerm = rt.heap.writerValue[wId];
        expect(idTerm, isA<ConstTerm>());
        final id = (idTerm as ConstTerm).value as int;
        ids.add(id);

        print('Generated ID $i: $id');
      }

      // Check that all IDs are unique
      expect(ids.toSet().length, 3);
      // Check that IDs are sequential
      expect(ids[1], ids[0] + 1);
      expect(ids[2], ids[1] + 1);

      print('✓ unique_id/1 generated unique sequential IDs\n');
    });
  });

  group('File I/O System Predicates', () {
    late GlpRuntime rt;
    late Directory tempDir;

    setUp(() {
      rt = GlpRuntime();
      registerStandardPredicates(rt.systemPredicates);
      // Create temporary directory for test files
      tempDir = Directory.systemTemp.createTempSync('glp_test_');
    });

    tearDown(() {
      // Clean up temporary directory
      if (tempDir.existsSync()) {
        tempDir.deleteSync(recursive: true);
      }
    });

    test('file_write/2 and file_read/2: write and read file', () {
      print('\n=== FILE I/O TEST: Write and Read ===');

      final testPath = '${tempDir.path}/test.txt';
      final testContents = 'Hello, GLP!';

      print('Test file path: $testPath');

      // Step 1: Write file
      final callWrite = SystemCall('file_write', [
        ConstTerm(testPath),
        ConstTerm(testContents),
      ]);
      final resultWrite = fileWritePredicate(rt, callWrite);

      expect(resultWrite, SystemResult.success);
      expect(File(testPath).existsSync(), true);

      print('✓ File written successfully');

      // Step 2: Read file
      const wContents = 1;
      const rContents = 2;
      rt.heap.addWriter(WriterCell(wContents, rContents));
      rt.heap.addReader(ReaderCell(rContents));

      final callRead = SystemCall('file_read', [
        ConstTerm(testPath),
        WriterTerm(wContents),
      ]);
      final resultRead = fileReadPredicate(rt, callRead);

      expect(resultRead, SystemResult.success);
      expect(rt.heap.isWriterBound(wContents), true);

      final contentsTerm = rt.heap.writerValue[wContents];
      expect(contentsTerm, isA<ConstTerm>());
      expect((contentsTerm as ConstTerm).value, testContents);

      print('✓ File read successfully: ${contentsTerm.value}');
      print('✓ Contents match: $testContents\n');
    });

    test('file_read/2: fails for non-existent file', () {
      print('\n=== FILE I/O TEST: Non-existent File ===');

      final nonExistentPath = '${tempDir.path}/does_not_exist.txt';

      const wContents = 1;
      const rContents = 2;
      rt.heap.addWriter(WriterCell(wContents, rContents));
      rt.heap.addReader(ReaderCell(rContents));

      final call = SystemCall('file_read', [
        ConstTerm(nonExistentPath),
        WriterTerm(wContents),
      ]);
      final result = fileReadPredicate(rt, call);

      expect(result, SystemResult.failure);
      print('✓ Correctly failed for non-existent file\n');
    });

    test('file_read/2: suspends on unbound reader path', () {
      print('\n=== FILE I/O TEST: Suspend on Unbound Reader ===');

      // Create unbound writer/reader for path
      const wPath = 1;
      const rPath = 2;
      rt.heap.addWriter(WriterCell(wPath, rPath));
      rt.heap.addReader(ReaderCell(rPath));
      // Do NOT bind wPath

      // Create writer for contents
      const wContents = 3;
      const rContents = 4;
      rt.heap.addWriter(WriterCell(wContents, rContents));
      rt.heap.addReader(ReaderCell(rContents));

      final call = SystemCall('file_read', [
        ReaderTerm(rPath),
        WriterTerm(wContents),
      ]);
      final result = fileReadPredicate(rt, call);

      expect(result, SystemResult.suspend);
      expect(call.suspendedReaders, contains(rPath));

      print('Result: $result');
      print('Suspended readers: ${call.suspendedReaders}');
      print('✓ Correctly suspended on unbound reader\n');
    });

    test('file_write/2: writes multiline content', () {
      print('\n=== FILE I/O TEST: Multiline Content ===');

      final testPath = '${tempDir.path}/multiline.txt';
      final testContents = 'Line 1\nLine 2\nLine 3';

      // Write multiline file
      final callWrite = SystemCall('file_write', [
        ConstTerm(testPath),
        ConstTerm(testContents),
      ]);
      final resultWrite = fileWritePredicate(rt, callWrite);

      expect(resultWrite, SystemResult.success);

      // Read it back
      const wContents = 1;
      const rContents = 2;
      rt.heap.addWriter(WriterCell(wContents, rContents));
      rt.heap.addReader(ReaderCell(rContents));

      final callRead = SystemCall('file_read', [
        ConstTerm(testPath),
        WriterTerm(wContents),
      ]);
      final resultRead = fileReadPredicate(rt, callRead);

      expect(resultRead, SystemResult.success);

      final contentsTerm = rt.heap.writerValue[wContents];
      expect((contentsTerm as ConstTerm).value, testContents);

      print('✓ Multiline content preserved correctly\n');
    });

    test('file_write/2: overwrites existing file', () {
      print('\n=== FILE I/O TEST: Overwrite Existing ===');

      final testPath = '${tempDir.path}/overwrite.txt';

      // Write first content
      final callWrite1 = SystemCall('file_write', [
        ConstTerm(testPath),
        ConstTerm('First content'),
      ]);
      fileWritePredicate(rt, callWrite1);

      // Write second content (overwrite)
      final callWrite2 = SystemCall('file_write', [
        ConstTerm(testPath),
        ConstTerm('Second content'),
      ]);
      final resultWrite2 = fileWritePredicate(rt, callWrite2);

      expect(resultWrite2, SystemResult.success);

      // Read to verify
      const wContents = 1;
      const rContents = 2;
      rt.heap.addWriter(WriterCell(wContents, rContents));
      rt.heap.addReader(ReaderCell(rContents));

      final callRead = SystemCall('file_read', [
        ConstTerm(testPath),
        WriterTerm(wContents),
      ]);
      fileReadPredicate(rt, callRead);

      final contentsTerm = rt.heap.writerValue[wContents];
      expect((contentsTerm as ConstTerm).value, 'Second content');

      print('✓ File correctly overwritten\n');
    });
  });
}
