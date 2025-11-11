library glp_bytecode_v216_runner;

import 'opcodes.dart';

/// Minimal skeleton VM for v2.16 bytecode.
/// For now it indexes labels and supports Goto/Proceed no-ops,
/// so we can wire tests incrementally.
class VM {
  VM(this.program) {
    _indexLabels();
  }

  final List<Op> program;
  final Map<String, int> _labelIndex = {};
  int ip = 0; // instruction pointer

  void _indexLabels() {
    for (var i = 0; i < program.length; i++) {
      final op = program[i];
      if (op is Label) _labelIndex[op.name] = i;
    }
  }

  void _jump(String label) {
    final idx = _labelIndex[label];
    if (idx == null) {
      throw StateError('Unknown label: $label');
    }
    ip = idx;
  }

  /// Execute until Proceed or end of program.
  void run() {
    ip = 0;
    while (ip >= 0 && ip < program.length) {
      final op = program[ip];

      if (op is Goto) {
        _jump(op.label);
        continue;
      }
      if (op is Proceed) {
        break; // predicate succeeded/returned
      }

      // TODO: Implement full v2.16 semantics here (ClauseTry, Commit, etc.)
      ip++; // default: step forward
    }
  }
}
