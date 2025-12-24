import 'dart:io';

void main() {
  final file = File('/tmp/test_failures.txt');
  final lines = file.readAsLinesSync();

  final failingTests = <String>[];

  for (final line in lines) {
    if (line.contains('[E]')) {
      // Extract test name - it's after the last colon before [E]
      final match = RegExp(r': ([^\[]+) \[1m\[31m\[E\]').firstMatch(line);
      if (match != null) {
        failingTests.add(match.group(1)!.trim());
      }
    }
  }

  // Remove duplicates and sort
  final uniqueTests = failingTests.toSet().toList()..sort();

  print('Total unique failing tests: ${uniqueTests.length}\n');

  // Categorize by test file/group
  final categories = <String, List<String>>{};

  for (final test in uniqueTests) {
    String category;
    if (test.contains('Buffer')) {
      category = 'Buffer Types';
    } else if (test.contains('Channel') || test.contains('send') || test.contains('receive') || test.contains('new_channel')) {
      category = 'Channel/Communication';
    } else if (test.contains('Stream') || test.contains('InvStream')) {
      category = 'Stream Types';
    } else if (test.contains('Every') && test.contains('List')) {
      category = 'EveryList';
    } else if (test.contains('Any') || test.contains('Every')) {
      category = 'Any/Every Mode Coverage';
    } else if (test.contains('DiffList') || test.contains('dl_')) {
      category = 'Difference Lists';
    } else if (test.contains('Defined guard') || test.contains('Guard')) {
      category = 'Guard Type Checking';
    } else if (test.contains('Primitive')) {
      category = 'Primitive Mode Tests';
    } else {
      category = 'Other';
    }

    categories.putIfAbsent(category, () => []).add(test);
  }

  // Print categories
  for (final entry in categories.entries.toList()..sort((a, b) => b.value.length.compareTo(a.value.length))) {
    print('${entry.key} (${entry.value.length} tests):');
    for (final test in entry.value) {
      print('  - $test');
    }
    print('');
  }
}
