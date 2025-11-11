import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/modules.dart';

BytecodeProgram programP1() => BytecodeProgram([
  // Clause 1: writer mode p(X) -> bind 'a'
  Label('CW'),
  ClauseTry(),
  RequireWriterArg(0, 'CR'),
  HeadBindWriterArg(0),
  Commit(),
  BodySetConstArg(0, 'a'),
  Proceed(),

  // Clause 2: reader mode p(X?) -> suspend if unbound
  Label('CR'),
  ClauseTry(),
  RequireReaderArg(0, 'END'),
  GuardNeedReaderArg(0),
  UnionSiAndGoto('END'),
  Label('END'),
  SuspendEnd(),
]);

void printUsage() {
  print('Usage:');
  print('  dart run bin/loader.dart activate writer <W> <R> <goalId>');
  print('  dart run bin/loader.dart activate reader <W> <R> <goalId>');
  print('  dart run bin/loader.dart bind <R>           (manual wake only; for demos)');
  print('  dart run bin/loader.dart dumpw <W>          (writer bound/value)');
  print('  dart run bin/loader.dart dumpr <R>          (reader queue status)');
  print('  dart run bin/loader.dart drain [N]');
  print('');
  print('Sequence mode (single process, preserves state):');
  print('  dart run bin/loader.dart seq "activate reader 1 1001 30" "drain 1" "activate writer 1 1001 40" "drain 3"');
}

bool runOne(
  GlpRuntime rt,
  ModuleRegistry reg,
  LoaderHost host,
  List<String> toks,
) {
  if (toks.isEmpty) return true;

  final cmd = toks[0];

  if (cmd == 'activate' && toks.length >= 5) {
    final mode = toks[1];
    final w = int.parse(toks[2]);
    final r = int.parse(toks[3]);
    final gid = int.parse(toks[4]);
    rt.heap.addWriter(WriterCell(w, r));
    rt.heap.addReader(ReaderCell(r));
    if (mode == 'writer') {
      host.activate(goalId: gid, module: 'main', predSig: 'p/1', env: CallEnv(writers: {0: w}), kappa: 1);
      print('ENQUEUED: writer goal $gid');
    } else if (mode == 'reader') {
      host.activate(goalId: gid, module: 'main', predSig: 'p/1', env: CallEnv(readers: {0: r}), kappa: 1);
      print('ENQUEUED: reader goal $gid');
    } else {
      print('Unknown mode: $mode');
      return false;
    }
    return true;
  }

  // For demo purposes only: manual wake without binding a value.
  if (cmd == 'bind' && toks.length >= 2) {
    final r = int.parse(toks[1]);
    final acts = rt.roq.processOnBind(r);
    for (final a in acts) {
      rt.gq.enqueue(a);
      print('ACTIVATED: goal ${a.id}@${a.pc}');
    }
    return true;
  }

  if (cmd == 'dumpw' && toks.length >= 2) {
    final w = int.parse(toks[1]);
    final bound = rt.heap.isWriterBound(w);
    final val = rt.heap.valueOfWriter(w);
    print('WRITER $w: bound=$bound${bound ? ' value=$val' : ''}');
    final wr = rt.heap.writer(w);
    if (wr != null) print('  paired reader=${wr.readerId}');
    return true;
  }

  if (cmd == 'dumpr' && toks.length >= 2) {
    final r = int.parse(toks[1]);
    final empty = rt.roq.isEmpty(r);
    final wid = rt.heap.writerIdForReader(r);
    print('READER $r: queueEmpty=$empty pairedWriter=${wid ?? 'unknown'}');
    return true;
  }

  if (cmd == 'drain') {
    final max = toks.length >= 2 ? int.parse(toks[1]) : 1000;
    final lines = host.drain(maxCycles: max);
    for (final l in lines) { print(l); }
    return true;
  }

  print('Bad command: ${toks.join(" ")}');
  return false;
}

void main(List<String> args) {
  final rt = GlpRuntime();
  final reg = ModuleRegistry();
  final host = LoaderHost(rt: rt, registry: reg);

  // Load module once into this process
  reg.register(module: 'main', predSig: 'p/1', program: programP1());

  if (args.isEmpty) {
    printUsage();
    return;
  }

  // Sequence mode: run multiple commands in one process to preserve runtime state.
  if (args[0] == 'seq') {
    if (args.length == 1) {
      printUsage();
      return;
    }
    for (var i = 1; i < args.length; i++) {
      final cmdString = args[i];
      final toks = cmdString.split(RegExp(r'\s+')).where((s) => s.isNotEmpty).toList();
      final ok = runOne(rt, reg, host, toks);
      if (!ok) break;
    }
    return;
  }

  // Single command mode (one-shot process)
  final ok = runOne(rt, reg, host, args);
  if (!ok) printUsage();
}
