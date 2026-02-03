import 'dart:io';
import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  final source = File('/Users/udi/Grassroots/GLP/programs/typed_book/social_graph/play_alice_bob_charlie_actor_boot.glp').readAsStringSync();

  // Strip boot clause
  var strippedSource = source.replaceFirst(
    RegExp(r'procedure\s+boot\s*\.\s*\n?', multiLine: true),
    '',
  );
  strippedSource = strippedSource.replaceFirst(
    RegExp(r'boot\s*:-\s*.*?\.\s*\n?', multiLine: true, dotAll: true),
    '',
  );

  final compiler = GlpCompiler();
  final program = compiler.compile(strippedSource);

  // Find actor/2 labels
  final actorLabels = program.labels.entries
      .where((e) => e.key.startsWith('actor/2'))
      .toList()
    ..sort((a, b) => a.value.compareTo(b.value));

  print('Actor labels:');
  for (final l in actorLabels) {
    print('  ${l.key} -> PC ${l.value}');
  }

  // Print ops around actor/2
  final startPC = program.labels['actor/2']!;
  final endPC = program.labels['actor/2_end'] ?? startPC + 100;
  print('\nBytecode for actor/2 (PC $startPC to $endPC):');
  for (var pc = startPC; pc < endPC && pc < program.ops.length; pc++) {
    final op = program.ops[pc];
    print('  $pc: $op');
  }
}
