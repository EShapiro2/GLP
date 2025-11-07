/// Test for extended system predicates
///
/// Tests terminal I/O, file operations, directory operations, and utilities

import 'dart:io';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/system_predicates.dart';
import 'package:glp_runtime/runtime/system_predicates_impl.dart';
import 'package:test/test.dart';

void main() {
  group('Terminal I/O Predicates', () {
    late GlpRuntime rt;

    setUp(() {
      rt = GlpRuntime();
      registerStandardPredicates(rt.systemPredicates);
    });

    test('write/1: writes constant to stdout', () {
      print('\n=== WRITE TEST: Constant ===');

      final call = SystemCall('write', [ConstTerm('Hello, GLP!')]);
      final result = writePredicate(rt, call);

      expect(result, SystemResult.success);
      print('\n✓ write/1 succeeded\n');
    });

    test('write/1: writes number to stdout', () {
      print('\n=== WRITE TEST: Number ===');

      final call = SystemCall('write', [ConstTerm(42)]);
      final result = writePredicate(rt, call);

      expect(result, SystemResult.success);
      print('\n✓ write/1 succeeded for number\n');
    });

    test('write/1: suspends on unbound reader', () {
      print('\n=== WRITE TEST: Suspend ===');

      const wX = 1;
      const rX = 2;
      rt.heap.addWriter(WriterCell(wX, rX));
      rt.heap.addReader(ReaderCell(rX));

      final call = SystemCall('write', [ReaderTerm(rX)]);
      final result = writePredicate(rt, call);

      expect(result, SystemResult.suspend);
      expect(call.suspendedReaders, contains(rX));
      print('✓ write/1 correctly suspended\n');
    });

    test('nl/0: writes newline', () {
      print('\n=== NL TEST ===');

      final call = SystemCall('nl', []);
      final result = nlPredicate(rt, call);

      expect(result, SystemResult.success);
      print('✓ nl/0 succeeded\n');
    });
  });

  group('File Operations', () {
    late GlpRuntime rt;
    late Directory tempDir;

    setUp(() {
      rt = GlpRuntime();
      registerStandardPredicates(rt.systemPredicates);
      tempDir = Directory.systemTemp.createTempSync('glp_test_');
    });

    tearDown(() {
      if (tempDir.existsSync()) {
        tempDir.deleteSync(recursive: true);
      }
    });

    test('file_exists/1: succeeds for existing file', () {
      print('\n=== FILE_EXISTS TEST: Existing ===');

      final testPath = '${tempDir.path}/exists.txt';
      File(testPath).writeAsStringSync('test');

      final call = SystemCall('file_exists', [ConstTerm(testPath)]);
      final result = fileExistsPredicate(rt, call);

      expect(result, SystemResult.success);
      print('✓ file_exists/1 succeeded for existing file\n');
    });

    test('file_exists/1: fails for non-existent file', () {
      print('\n=== FILE_EXISTS TEST: Non-existent ===');

      final testPath = '${tempDir.path}/does_not_exist.txt';

      final call = SystemCall('file_exists', [ConstTerm(testPath)]);
      final result = fileExistsPredicate(rt, call);

      expect(result, SystemResult.failure);
      print('✓ file_exists/1 correctly failed\n');
    });
  });

  group('Directory Operations', () {
    late GlpRuntime rt;
    late Directory tempDir;

    setUp(() {
      rt = GlpRuntime();
      registerStandardPredicates(rt.systemPredicates);
      tempDir = Directory.systemTemp.createTempSync('glp_test_');
    });

    tearDown() {
      if (tempDir.existsSync()) {
        tempDir.deleteSync(recursive: true);
      }
    };

    test('directory_list/2: lists directory contents', () {
      print('\n=== DIRECTORY_LIST TEST ===');

      // Create test files
      File('${tempDir.path}/file1.txt').writeAsStringSync('test1');
      File('${tempDir.path}/file2.txt').writeAsStringSync('test2');

      const wList = 1;
      const rList = 2;
      rt.heap.addWriter(WriterCell(wList, rList));
      rt.heap.addReader(ReaderCell(rList));

      final call = SystemCall('directory_list', [
        ConstTerm(tempDir.path),
        WriterTerm(wList),
      ]);
      final result = directoryListPredicate(rt, call);

      expect(result, SystemResult.success);
      expect(rt.heap.isWriterBound(wList), true);

      final listValue = rt.heap.writerValue[wList];
      expect(listValue, isA<ConstTerm>());
      final entries = (listValue as ConstTerm).value as List;
      expect(entries.length, 2);
      expect(entries, contains('file1.txt'));
      expect(entries, contains('file2.txt'));

      print('Listed files: $entries');
      print('✓ directory_list/2 succeeded\n');
    });

    test('directory_list/2: fails for non-existent directory', () {
      print('\n=== DIRECTORY_LIST TEST: Non-existent ===');

      const wList = 1;
      const rList = 2;
      rt.heap.addWriter(WriterCell(wList, rList));
      rt.heap.addReader(ReaderCell(rList));

      final call = SystemCall('directory_list', [
        ConstTerm('${tempDir.path}/nonexistent'),
        WriterTerm(wList),
      ]);
      final result = directoryListPredicate(rt, call);

      expect(result, SystemResult.failure);
      print('✓ directory_list/2 correctly failed\n');
    });
  });

  group('Utility Predicates', () {
    late GlpRuntime rt;

    setUp(() {
      rt = GlpRuntime();
      registerStandardPredicates(rt.systemPredicates);
    });

    test('variable_name/2: gets writer variable name', () {
      print('\n=== VARIABLE_NAME TEST: Writer ===');

      const wX = 123;
      const rX = 124;
      rt.heap.addWriter(WriterCell(wX, rX));
      rt.heap.addReader(ReaderCell(rX));

      const wName = 1;
      const rName = 2;
      rt.heap.addWriter(WriterCell(wName, rName));
      rt.heap.addReader(ReaderCell(rName));

      final call = SystemCall('variable_name', [
        WriterTerm(wX),
        WriterTerm(wName),
      ]);
      final result = variableNamePredicate(rt, call);

      expect(result, SystemResult.success);
      expect(rt.heap.isWriterBound(wName), true);

      final nameValue = rt.heap.writerValue[wName];
      expect(nameValue, isA<ConstTerm>());
      expect((nameValue as ConstTerm).value, 'W123');

      print('Variable name: ${nameValue.value}');
      print('✓ variable_name/2 succeeded for writer\n');
    });

    test('variable_name/2: gets reader variable name', () {
      print('\n=== VARIABLE_NAME TEST: Reader ===');

      const wX = 123;
      const rX = 456;
      rt.heap.addWriter(WriterCell(wX, rX));
      rt.heap.addReader(ReaderCell(rX));

      const wName = 1;
      const rName = 2;
      rt.heap.addWriter(WriterCell(wName, rName));
      rt.heap.addReader(ReaderCell(rName));

      final call = SystemCall('variable_name', [
        ReaderTerm(rX),
        WriterTerm(wName),
      ]);
      final result = variableNamePredicate(rt, call);

      expect(result, SystemResult.success);
      expect(rt.heap.isWriterBound(wName), true);

      final nameValue = rt.heap.writerValue[wName];
      expect((nameValue as ConstTerm).value, 'R456');

      print('Variable name: ${nameValue.value}');
      print('✓ variable_name/2 succeeded for reader\n');
    });

    test('copy_term/2: copies a constant', () {
      print('\n=== COPY_TERM TEST: Constant ===');

      const wCopy = 1;
      const rCopy = 2;
      rt.heap.addWriter(WriterCell(wCopy, rCopy));
      rt.heap.addReader(ReaderCell(rCopy));

      final call = SystemCall('copy_term', [
        ConstTerm('original'),
        WriterTerm(wCopy),
      ]);
      final result = copyTermPredicate(rt, call);

      expect(result, SystemResult.success);
      expect(rt.heap.isWriterBound(wCopy), true);

      final copyValue = rt.heap.writerValue[wCopy];
      expect((copyValue as ConstTerm).value, 'original');

      print('Copy: ${copyValue.value}');
      print('✓ copy_term/2 succeeded\n');
    });

    test('copy_term/2: suspends on unbound reader', () {
      print('\n=== COPY_TERM TEST: Suspend ===');

      const wX = 1;
      const rX = 2;
      rt.heap.addWriter(WriterCell(wX, rX));
      rt.heap.addReader(ReaderCell(rX));

      const wCopy = 3;
      const rCopy = 4;
      rt.heap.addWriter(WriterCell(wCopy, rCopy));
      rt.heap.addReader(ReaderCell(rCopy));

      final call = SystemCall('copy_term', [
        ReaderTerm(rX),
        WriterTerm(wCopy),
      ]);
      final result = copyTermPredicate(rt, call);

      expect(result, SystemResult.suspend);
      expect(call.suspendedReaders, contains(rX));

      print('✓ copy_term/2 correctly suspended\n');
    });
  });

  group('Module Loading Placeholders', () {
    late GlpRuntime rt;

    setUp(() {
      rt = GlpRuntime();
      registerStandardPredicates(rt.systemPredicates);
    });

    test('link/2: returns failure (placeholder)', () {
      print('\n=== LINK TEST: Placeholder ===');

      const wOffset = 1;
      const rOffset = 2;
      rt.heap.addWriter(WriterCell(wOffset, rOffset));
      rt.heap.addReader(ReaderCell(rOffset));

      final call = SystemCall('link', [
        ConstTerm(['file', 'math']),
        WriterTerm(wOffset),
      ]);
      final result = linkPredicate(rt, call);

      expect(result, SystemResult.failure);
      print('✓ link/2 placeholder correctly returns failure\n');
    });

    test('load_module/2: returns failure (placeholder)', () {
      print('\n=== LOAD_MODULE TEST: Placeholder ===');

      const wModule = 1;
      const rModule = 2;
      rt.heap.addWriter(WriterCell(wModule, rModule));
      rt.heap.addReader(ReaderCell(rModule));

      final call = SystemCall('load_module', [
        ConstTerm('test.glp'),
        WriterTerm(wModule),
      ]);
      final result = loadModulePredicate(rt, call);

      expect(result, SystemResult.failure);
      print('✓ load_module/2 placeholder correctly returns failure\n');
    });
  });
}
