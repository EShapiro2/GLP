/// Test runner for udi workspace
///
/// Runs specific GLP tests from the glp_runtime test suite
/// Usage: dart run run_test.dart <test_name>

import 'dart:io';

void main(List<String> args) {
  if (args.isEmpty) {
    print('Usage: dart run run_test.dart <test_name>');
    print('');
    print('Available tests:');
    print('  hello       - Hello world test');
    print('  echo        - Echo test');
    print('  merge       - Merge base case test');
    print('  merge_123   - Merge [1,2,3] and [a,b]');
    print('  p_q         - Simple p/q program test');
    print('  metainterp  - Metainterpreter test');
    print('  all         - Run all tests');
    exit(1);
  }

  final testName = args[0];
  final testPath = _getTestPath(testName);

  if (testPath == null) {
    print('Unknown test: $testName');
    print('Run without arguments to see available tests.');
    exit(1);
  }

  print('Running test: $testName');
  print('Path: $testPath');
  print('');

  final result = Process.runSync(
    'dart',
    ['test', testPath],
    workingDirectory: '../glp_runtime',
  );

  stdout.write(result.stdout);
  stderr.write(result.stderr);
  exit(result.exitCode);
}

String? _getTestPath(String name) {
  final tests = {
    'hello': 'test/custom/hello_world_test.dart',
    'echo': 'test/custom/echo_test.dart',
    'merge': 'test/custom/merge_test.dart',
    'merge_123': 'test/custom/merge_123_ab_test.dart',
    'p_q': 'test/custom/simple_p_q_test.dart',
    'metainterp': 'test/custom/metainterp_pq_test.dart',
    'all': '',  // Special case - run all
  };

  return tests[name];
}
