/// Test for file handle operations
///
/// Tests file_open/3, file_close/1, file_read_handle/2, and file_write_handle/2

import 'dart:io';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/system_predicates.dart';
import 'package:glp_runtime/runtime/system_predicates_impl.dart';
import 'package:test/test.dart';

void main() {
  group('File Handle Operations', () {
    late GlpRuntime rt;
    late Directory tempDir;

    setUp(() {
      rt = GlpRuntime();
      registerStandardPredicates(rt.systemPredicates);
      tempDir = Directory.systemTemp.createTempSync('glp_test_');
    });

    tearDown(() {
      rt.closeAllFiles();
      if (tempDir.existsSync()) {
        tempDir.deleteSync(recursive: true);
      }
    });

    test('file_open/3: opens file for reading', () {
      print('\n=== FILE_OPEN TEST: Read Mode ===');

      // Create test file
      final testPath = '${tempDir.path}/read_test.txt';
      File(testPath).writeAsStringSync('test content');

      // Create writer for handle
      const wHandle = 1;
      const rHandle = 2;
      rt.heap.addWriter(WriterCell(wHandle, rHandle));
      rt.heap.addReader(ReaderCell(rHandle));

      // Call file_open
      final call = SystemCall('file_open', [
        ConstTerm(testPath),
        ConstTerm('read'),
        WriterTerm(wHandle),
      ]);
      final result = fileOpenPredicate(rt, call);

      expect(result, SystemResult.success);
      expect(rt.heap.isWriterBound(wHandle), true);

      final handleTerm = rt.heap.writerValue[wHandle];
      expect(handleTerm, isA<ConstTerm>());
      final handle = (handleTerm as ConstTerm).value as int;
      expect(handle, greaterThan(0));
      expect(rt.isValidHandle(handle), true);

      print('Allocated handle: $handle');
      print('✓ file_open/3 succeeded for read mode\n');
    });

    test('file_open/3: opens file for writing', () {
      print('\n=== FILE_OPEN TEST: Write Mode ===');

      final testPath = '${tempDir.path}/write_test.txt';

      const wHandle = 1;
      const rHandle = 2;
      rt.heap.addWriter(WriterCell(wHandle, rHandle));
      rt.heap.addReader(ReaderCell(rHandle));

      final call = SystemCall('file_open', [
        ConstTerm(testPath),
        ConstTerm('write'),
        WriterTerm(wHandle),
      ]);
      final result = fileOpenPredicate(rt, call);

      expect(result, SystemResult.success);
      expect(rt.heap.isWriterBound(wHandle), true);

      final handleTerm = rt.heap.writerValue[wHandle];
      final handle = (handleTerm as ConstTerm).value as int;
      expect(rt.isValidHandle(handle), true);

      print('Allocated handle: $handle');
      print('✓ file_open/3 succeeded for write mode\n');
    });

    test('file_open/3: fails for non-existent file in read mode', () {
      print('\n=== FILE_OPEN TEST: Non-existent File ===');

      final testPath = '${tempDir.path}/does_not_exist.txt';

      const wHandle = 1;
      const rHandle = 2;
      rt.heap.addWriter(WriterCell(wHandle, rHandle));
      rt.heap.addReader(ReaderCell(rHandle));

      final call = SystemCall('file_open', [
        ConstTerm(testPath),
        ConstTerm('read'),
        WriterTerm(wHandle),
      ]);
      final result = fileOpenPredicate(rt, call);

      expect(result, SystemResult.failure);
      print('✓ file_open/3 correctly failed for non-existent file\n');
    });

    test('file_close/1: closes valid handle', () {
      print('\n=== FILE_CLOSE TEST: Valid Handle ===');

      // Open file first
      final testPath = '${tempDir.path}/close_test.txt';
      File(testPath).writeAsStringSync('test');

      const wHandle = 1;
      const rHandle = 2;
      rt.heap.addWriter(WriterCell(wHandle, rHandle));
      rt.heap.addReader(ReaderCell(rHandle));

      fileOpenPredicate(rt, SystemCall('file_open', [
        ConstTerm(testPath),
        ConstTerm('read'),
        WriterTerm(wHandle),
      ]));

      final handleTerm = rt.heap.writerValue[wHandle];
      final handle = (handleTerm as ConstTerm).value as int;

      // Close the handle
      final call = SystemCall('file_close', [ConstTerm(handle)]);
      final result = fileClosePredicate(rt, call);

      expect(result, SystemResult.success);
      expect(rt.isValidHandle(handle), false);

      print('Closed handle: $handle');
      print('✓ file_close/1 succeeded\n');
    });

    test('file_close/1: fails for invalid handle', () {
      print('\n=== FILE_CLOSE TEST: Invalid Handle ===');

      final call = SystemCall('file_close', [ConstTerm(9999)]);
      final result = fileClosePredicate(rt, call);

      expect(result, SystemResult.failure);
      print('✓ file_close/1 correctly failed for invalid handle\n');
    });

    test('file_read_handle/2: reads from open file', () {
      print('\n=== FILE_READ_HANDLE TEST: Read Contents ===');

      // Create and open file
      final testPath = '${tempDir.path}/read_handle_test.txt';
      final testContents = 'Hello from file handle!';
      File(testPath).writeAsStringSync(testContents);

      const wHandle = 1;
      const rHandle = 2;
      rt.heap.addWriter(WriterCell(wHandle, rHandle));
      rt.heap.addReader(ReaderCell(rHandle));

      fileOpenPredicate(rt, SystemCall('file_open', [
        ConstTerm(testPath),
        ConstTerm('read'),
        WriterTerm(wHandle),
      ]));

      final handleTerm = rt.heap.writerValue[wHandle];
      final handle = (handleTerm as ConstTerm).value as int;

      // Read from handle
      const wContents = 3;
      const rContents = 4;
      rt.heap.addWriter(WriterCell(wContents, rContents));
      rt.heap.addReader(ReaderCell(rContents));

      final call = SystemCall('file_read_handle', [
        ConstTerm(handle),
        WriterTerm(wContents),
      ]);
      final result = fileReadHandlePredicate(rt, call);

      expect(result, SystemResult.success);
      expect(rt.heap.isWriterBound(wContents), true);

      final contentsTerm = rt.heap.writerValue[wContents];
      expect((contentsTerm as ConstTerm).value, testContents);

      print('Read contents: ${contentsTerm.value}');
      print('✓ file_read_handle/2 succeeded\n');
    });

    test('file_read_handle/2: fails for invalid handle', () {
      print('\n=== FILE_READ_HANDLE TEST: Invalid Handle ===');

      const wContents = 1;
      const rContents = 2;
      rt.heap.addWriter(WriterCell(wContents, rContents));
      rt.heap.addReader(ReaderCell(rContents));

      final call = SystemCall('file_read_handle', [
        ConstTerm(9999),
        WriterTerm(wContents),
      ]);
      final result = fileReadHandlePredicate(rt, call);

      expect(result, SystemResult.failure);
      print('✓ file_read_handle/2 correctly failed for invalid handle\n');
    });

    test('file_write_handle/2: writes to open file', () {
      print('\n=== FILE_WRITE_HANDLE TEST: Write Contents ===');

      // Open file for writing
      final testPath = '${tempDir.path}/write_handle_test.txt';

      const wHandle = 1;
      const rHandle = 2;
      rt.heap.addWriter(WriterCell(wHandle, rHandle));
      rt.heap.addReader(ReaderCell(rHandle));

      fileOpenPredicate(rt, SystemCall('file_open', [
        ConstTerm(testPath),
        ConstTerm('write'),
        WriterTerm(wHandle),
      ]));

      final handleTerm = rt.heap.writerValue[wHandle];
      final handle = (handleTerm as ConstTerm).value as int;

      // Write to handle
      final testContents = 'Written via handle';
      final call = SystemCall('file_write_handle', [
        ConstTerm(handle),
        ConstTerm(testContents),
      ]);
      final result = fileWriteHandlePredicate(rt, call);

      expect(result, SystemResult.success);

      // Close handle
      fileClosePredicate(rt, SystemCall('file_close', [ConstTerm(handle)]));

      // Verify contents
      final actualContents = File(testPath).readAsStringSync();
      expect(actualContents, testContents);

      print('Written contents: $testContents');
      print('Verified on disk: $actualContents');
      print('✓ file_write_handle/2 succeeded\n');
    });

    test('file_write_handle/2: fails for invalid handle', () {
      print('\n=== FILE_WRITE_HANDLE TEST: Invalid Handle ===');

      final call = SystemCall('file_write_handle', [
        ConstTerm(9999),
        ConstTerm('test'),
      ]);
      final result = fileWriteHandlePredicate(rt, call);

      expect(result, SystemResult.failure);
      print('✓ file_write_handle/2 correctly failed for invalid handle\n');
    });

    test('Full workflow: open → write → close → open → read → close', () {
      print('\n=== FULL WORKFLOW TEST ===');

      final testPath = '${tempDir.path}/workflow_test.txt';
      final testContents = 'Full workflow test';

      // Step 1: Open for writing
      const wHandle1 = 1;
      const rHandle1 = 2;
      rt.heap.addWriter(WriterCell(wHandle1, rHandle1));
      rt.heap.addReader(ReaderCell(rHandle1));

      fileOpenPredicate(rt, SystemCall('file_open', [
        ConstTerm(testPath),
        ConstTerm('write'),
        WriterTerm(wHandle1),
      ]));

      final handleTerm1 = rt.heap.writerValue[wHandle1];
      final handle1 = (handleTerm1 as ConstTerm).value as int;

      print('Step 1: Opened for writing, handle=$handle1');

      // Step 2: Write
      fileWriteHandlePredicate(rt, SystemCall('file_write_handle', [
        ConstTerm(handle1),
        ConstTerm(testContents),
      ]));

      print('Step 2: Written contents');

      // Step 3: Close
      fileClosePredicate(rt, SystemCall('file_close', [ConstTerm(handle1)]));

      print('Step 3: Closed write handle');

      // Step 4: Open for reading
      const wHandle2 = 3;
      const rHandle2 = 4;
      rt.heap.addWriter(WriterCell(wHandle2, rHandle2));
      rt.heap.addReader(ReaderCell(rHandle2));

      fileOpenPredicate(rt, SystemCall('file_open', [
        ConstTerm(testPath),
        ConstTerm('read'),
        WriterTerm(wHandle2),
      ]));

      final handleTerm2 = rt.heap.writerValue[wHandle2];
      final handle2 = (handleTerm2 as ConstTerm).value as int;

      print('Step 4: Opened for reading, handle=$handle2');

      // Step 5: Read
      const wContents = 5;
      const rContents = 6;
      rt.heap.addWriter(WriterCell(wContents, rContents));
      rt.heap.addReader(ReaderCell(rContents));

      fileReadHandlePredicate(rt, SystemCall('file_read_handle', [
        ConstTerm(handle2),
        WriterTerm(wContents),
      ]));

      final contentsTerm = rt.heap.writerValue[wContents];
      expect((contentsTerm as ConstTerm).value, testContents);

      print('Step 5: Read contents: ${contentsTerm.value}');

      // Step 6: Close
      fileClosePredicate(rt, SystemCall('file_close', [ConstTerm(handle2)]));

      print('Step 6: Closed read handle');
      print('✓ Full workflow succeeded\n');
    });

    test('Multiple handles: can have multiple files open simultaneously', () {
      print('\n=== MULTIPLE HANDLES TEST ===');

      final path1 = '${tempDir.path}/file1.txt';
      final path2 = '${tempDir.path}/file2.txt';
      File(path1).writeAsStringSync('File 1 contents');
      File(path2).writeAsStringSync('File 2 contents');

      // Open first file
      const wHandle1 = 1;
      const rHandle1 = 2;
      rt.heap.addWriter(WriterCell(wHandle1, rHandle1));
      rt.heap.addReader(ReaderCell(rHandle1));

      fileOpenPredicate(rt, SystemCall('file_open', [
        ConstTerm(path1),
        ConstTerm('read'),
        WriterTerm(wHandle1),
      ]));

      final handle1 = (rt.heap.writerValue[wHandle1] as ConstTerm).value as int;

      // Open second file
      const wHandle2 = 3;
      const rHandle2 = 4;
      rt.heap.addWriter(WriterCell(wHandle2, rHandle2));
      rt.heap.addReader(ReaderCell(rHandle2));

      fileOpenPredicate(rt, SystemCall('file_open', [
        ConstTerm(path2),
        ConstTerm('read'),
        WriterTerm(wHandle2),
      ]));

      final handle2 = (rt.heap.writerValue[wHandle2] as ConstTerm).value as int;

      // Both handles should be valid and different
      expect(rt.isValidHandle(handle1), true);
      expect(rt.isValidHandle(handle2), true);
      expect(handle1, isNot(equals(handle2)));

      print('Handle 1: $handle1');
      print('Handle 2: $handle2');
      print('✓ Multiple handles work correctly\n');
    });

    test('file_open/3: append mode creates and appends', () {
      print('\n=== FILE_OPEN TEST: Append Mode ===');

      final testPath = '${tempDir.path}/append_test.txt';
      File(testPath).writeAsStringSync('Initial content\n');

      // Open in append mode
      const wHandle = 1;
      const rHandle = 2;
      rt.heap.addWriter(WriterCell(wHandle, rHandle));
      rt.heap.addReader(ReaderCell(rHandle));

      fileOpenPredicate(rt, SystemCall('file_open', [
        ConstTerm(testPath),
        ConstTerm('append'),
        WriterTerm(wHandle),
      ]));

      final handle = (rt.heap.writerValue[wHandle] as ConstTerm).value as int;

      // Write additional content
      fileWriteHandlePredicate(rt, SystemCall('file_write_handle', [
        ConstTerm(handle),
        ConstTerm('Appended content'),
      ]));

      fileClosePredicate(rt, SystemCall('file_close', [ConstTerm(handle)]));

      // Verify both contents exist
      final finalContents = File(testPath).readAsStringSync();
      expect(finalContents, contains('Initial content'));
      expect(finalContents, contains('Appended content'));

      print('Final contents: $finalContents');
      print('✓ Append mode works correctly\n');
    });
  });
}
