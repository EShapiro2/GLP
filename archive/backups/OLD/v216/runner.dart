import 'package:glp_runtime/bytecode/v216/opcodes.dart';
import 'package:glp_runtime/bytecode/v216/model.dart';

/// v2.16 VM: executes a single predicate (unit goals) over unit clauses.
/// - HEAD/GUARDS are pure; BODY mutates only after [Commit].
/// - Suspension: accumulate Si per clause, U across tried clauses; suspend only after full scan.
/// - σ̂w: computed in HEAD (e.g., constant vs writer), applied atomically at COMMIT.
class VM {
  VM(this.program) {
    _index();
  }

  final List<Op> program;

  // Indexed positions
  final Map<String, int> _labelIndex = <String, int>{};
  int _noMoreClausesIdx = -1;

  // Instruction pointer
  int ip = 0;

  void _index() {
    for (var i = 0; i < program.length; i++) {
      final op = program[i];
      if (op is Label) _labelIndex[op.name] = i;
      if (op is NoMoreClauses) _noMoreClausesIdx = i;
    }
    if (_noMoreClausesIdx < 0) {
      _noMoreClausesIdx = program.length - 1;
    }
  }

  void _jumpLabel(String label) {
    final idx = _labelIndex[label];
    if (idx == null) throw StateError('Unknown label: $label');
    ip = idx;
  }

  int _findNextClauseTry(int fromExclusive) {
    for (var i = fromExclusive + 1; i < program.length; i++) {
      if (program[i] is ClauseTry) return i;
    }
    return -1;
  }

  void _softFailToNextClause(CallFrame frame) {
    // Move to next clause; union Si into U.
    frame.blockersU.addAll(frame.blockersSi);
    frame.blockersSi.clear();
    final next = _findNextClauseTry(ip);
    ip = (next >= 0) ? next : _noMoreClausesIdx;
  }

  /// Execute until Proceed, NoMoreClauses, or SuspendNow.
  VmOutcome run(CallFrame frame) {
    frame.blockersU.clear();
    frame.blockersSi.clear();
    frame.pendingWrites.clear();
    frame.committed = false;

    ip = 0;
    while (ip >= 0 && ip < program.length) {
      final op = program[ip];

      // ----- Control -----
      if (op is Label) { ip++; continue; }
      if (op is Goto) { _jumpLabel(op.label); continue; }
      if (op is ClauseTry) {
        frame.blockersSi.clear();
        frame.pendingWrites.clear();
        frame.committed = false;
        ip++; continue;
      }
      if (op is ClauseNext) { _softFailToNextClause(frame); continue; }
      if (op is NoMoreClauses) {
        final hasBlockers = frame.blockersU.isNotEmpty;
        return hasBlockers ? VmOutcome.suspend : VmOutcome.fail;
      }
      if (op is Commit) {
        frame.committed = true;
        // Apply σ̂w atomically now.
        for (final e in frame.pendingWrites.entries) {
          final slot = e.key;
          final sym = e.value;
          final t = frame.args[slot];
          if (t is! Writer) { throw StateError('COMMIT: expected Writer at slot $slot'); }
          t.value = sym;
        }
        frame.pendingWrites.clear();
        ip++; continue;
      }
      if (op is Proceed) { return VmOutcome.success; }
      if (op is SuspendNow) { return VmOutcome.suspend; }

      // ----- HEAD (pure) -----
      if (op is HeadWriter) {
        final t = frame.args[op.slot];
        if (t is! Writer) { _softFailToNextClause(frame); continue; }
        ip++; continue;
      }
      if (op is HeadReader) {
        final t = frame.args[op.slot];
        if (t is Reader) {
          if (!t.isBound) { frame.blockersSi.add(t); _softFailToNextClause(frame); continue; }
          ip++; continue; // bound reader ok
        }
        _softFailToNextClause(frame); continue;
      }
      if (op is HeadConstant) {
        final t = frame.args[op.slot];
        // Writer: record σ̂w (tentative binding), no heap write yet.
        if (t is Writer) { frame.pendingWrites[op.slot] = op.symbol; ip++; continue; }
        // Reader: if unbound -> add blocker and try next clause; if bound -> match value.
        if (t is Reader) {
          if (!t.isBound) { frame.blockersSi.add(t); _softFailToNextClause(frame); continue; }
          if (t.value == op.symbol) { ip++; continue; }
          _softFailToNextClause(frame); continue;
        }
        // Ground constant: must match exactly.
        if (t is Const && t.symbol == op.symbol) { ip++; continue; }
        _softFailToNextClause(frame); continue;
      }
      if (op is HeadStructure) {
        // Not implemented in this core; treat as soft-fail.
        _softFailToNextClause(frame); continue;
      }

      // ----- GUARDS (pure) -----
      if (op is GuardKnown) {
        final t = frame.args[op.slot];
        final known = switch (t) {
          Reader r => r.isBound,
          Writer w => (w.value != null) || frame.pendingWrites.containsKey(op.slot),
          Const _ => true,
          _ => false,
        };
        if (!known) {
          if (t is Reader) frame.blockersSi.add(t);
          _softFailToNextClause(frame); continue;
        }
        ip++; continue;
      }
      if (op is GuardGround) {
        final t = frame.args[op.slot];
        final ground = switch (t) {
          Const _ => true,
          Reader r => r.isBound,
          Writer w => (w.value != null) || frame.pendingWrites.containsKey(op.slot),
          _ => false,
        };
        if (!ground) {
          if (t is Reader) frame.blockersSi.add(t);
          _softFailToNextClause(frame); continue;
        }
        ip++; continue;
      }
      if (op is GuardEqConst) {
        final t = frame.args[op.slot];
        if (t is Reader && !t.isBound) { frame.blockersSi.add(t); _softFailToNextClause(frame); continue; }
        final ok = switch (t) {
          Const c => c.symbol == op.symbol,
          Reader r => r.value == op.symbol,
          Writer w => (w.value == op.symbol) || (frame.pendingWrites[op.slot] == op.symbol),
          _ => false,
        };
        if (!ok) { _softFailToNextClause(frame); continue; }
        ip++; continue;
      }
      if (op is Otherwise) { ip++; continue; }

      // ----- BODY (mutating after COMMIT) -----
      if (op is PutConstant) {
        if (!frame.committed) { throw StateError('BODY op before Commit'); }
        final t = frame.args[op.slot];
        if (t is! Writer) { throw StateError('PutConstant expects Writer at slot ${op.slot}'); }
        t.value = op.symbol;
        ip++; continue;
      }
      if (op is Allocate || op is Deallocate || op is Spawn || op is Requeue) {
        // Not needed in this core; no-op placeholders.
        ip++; continue;
      }

      // Unknown op: advance to avoid infinite loop.
      ip++;
    }
    return VmOutcome.fail; // fell off program
  }
}
