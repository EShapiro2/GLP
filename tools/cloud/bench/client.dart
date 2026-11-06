// client.dart: starts target.dart under the VM service, resets the allocation counters and enables
// allocation tracing just before the measured phase, and reads the CPU samples, the allocation counts
// and the allocation sites after it.  Nothing of the client runs in the measured process.
//   dart run client.dart <file|dir> <path> [goal] [top]
import 'dart:convert';
import 'dart:io';
import 'package:vm_service/vm_service.dart';
import 'package:vm_service/vm_service_io.dart';

const dartBin = '/home/user/sdk/flutter/bin/cache/dart-sdk/bin/dart';
const rt = '/home/user/glp/glp_runtime';
const traced = ['_OneByteString', '_List', '_Map', '_Set', 'ProgramDFA', 'DFAState', 'Automaton', 'TransitionLabel', 'TypeRef', '_Record', 'TypeEnvironment', 'CollectedInstantiation', 'ModedPath', 'PathStep'];

Future<void> waitFor(String path) async {
  while (!File(path).existsSync()) { await Future.delayed(const Duration(milliseconds: 50)); }
}

String nameOf(ProfileFunction f) {
  final fn = f.function;
  if (fn is FuncRef) {
    final o = fn.owner;
    final pre = o is ClassRef ? '${o.name}.' : (o is LibraryRef ? '${(o.uri ?? '').split('/').last}:' : '');
    return '$pre${fn.name}';
  }
  if (fn is NativeFunction) return 'native:${fn.name}';
  return '$fn';
}

List<MapEntry<String, int>> sorted(Map<String, int> m) => m.entries.toList()..sort((a, b) => b.value.compareTo(a.value));

Future<void> main(List<String> args) async {
  final mode = args[0], path = args[1];
  final goal = args.length > 2 && args[2].isNotEmpty ? args[2] : null;
  final top = args.length > 3 ? int.parse(args[3]) : 25;
  final alloc = args.length > 4 && args[4] == 'alloc';  // trace allocation sites (slows the run) or sample CPU
  final work = Directory.systemTemp.createTempSync('glpprof').path;
  final proc = await Process.start(dartBin,
      ['--packages=$rt/.dart_tool/package_config.json', 'run', '--observe=0', '--disable-service-auth-codes',
       '--no-pause-isolates-on-exit', '${File(Platform.script.toFilePath()).parent.path}/target.dart', work, mode, path, goal ?? ''],
      workingDirectory: rt);
  final out = StringBuffer();
  String? uri;
  proc.stdout.transform(utf8.decoder).listen((s) { out.write(s); uri ??= RegExp(r'http://127\.0\.0\.1:\d+/').firstMatch(s)?.group(0); });
  proc.stderr.transform(utf8.decoder).listen(out.write);
  await waitFor('$work/ready');
  while (uri == null) { await Future.delayed(const Duration(milliseconds: 50)); }
  final vm = await vmServiceConnectUri('ws://${uri!.substring(7)}ws');
  final v = await vm.getVM();
  final iso = v.isolates!.firstWhere((i) => i.name == 'main', orElse: () => v.isolates!.first).id!;
  final classIds = <String, String>{};
  final classes = await vm.getClassList(iso);
  for (final c in classes.classes ?? <ClassRef>[]) { if (traced.contains(c.name) && c.id != null) classIds[c.name!] = c.id!; }

  // The VM's own clock, read as the last sample's time: the goal's samples are those after the mark.
  Future<int> vmNow() async {
    final c = await vm.getCpuSamples(iso, 0, 1 << 62);
    return (c.timeOriginMicros ?? 0) + (c.timeExtentMicros ?? 0);
  }
  Future<void> arm() async {
    await vm.getAllocationProfile(iso, reset: true);
    if (alloc) { for (final id in classIds.values) { await vm.setTraceClassAllocation(iso, id, true); } }
  }
  int mark;
  if (goal == null) {
    await arm(); mark = await vmNow();
    File('$work/go').writeAsStringSync('');
  } else {
    File('$work/go').writeAsStringSync('');
    await waitFor('$work/loaded');
    await arm(); mark = await vmNow();
    File('$work/go2').writeAsStringSync('');
  }
  await waitFor('$work/done');
  final phase = File('$work/done').readAsStringSync();
  final all = await vm.getCpuSamples(iso, 0, 1 << 62);
  final ap = await vm.getAllocationProfile(iso, gc: false);
  final traces = <String, CpuSamples>{};
  if (alloc) { for (final e in classIds.entries) { traces[e.key] = await vm.getAllocationTraces(iso, classId: e.value); } }
  File('$work/quit').writeAsStringSync('');
  // Keep the samples of the measured phase and recount the functions' ticks over them.
  final kept = (all.samples ?? <CpuSample>[]).where((x) => (x.timestamp ?? 0) > mark).toList();
  final s = CpuSamples(samplePeriod: all.samplePeriod, maxStackDepth: all.maxStackDepth, sampleCount: kept.length,
      timeOriginMicros: mark, timeExtentMicros: 0, pid: all.pid, functions: all.functions, samples: kept);
  for (final f in s.functions ?? <ProfileFunction>[]) { f.exclusiveTicks = 0; f.inclusiveTicks = 0; }
  for (final x in kept) {
    final st = x.stack ?? <int>[];
    final seen = <int>{};
    for (var i = 0; i < st.length; i++) {
      final f = s.functions![st[i]];
      if (i == 0) f.exclusiveTicks = (f.exclusiveTicks ?? 0) + 1;
      if (seen.add(st[i])) f.inclusiveTicks = (f.inclusiveTicks ?? 0) + 1;
    }
  }

  print('measured: ${goal == null ? 'the load' : 'the goal'} --- $phase');
  final funcs = s.functions ?? <ProfileFunction>[];
  final total = s.sampleCount ?? 0;
  final excl = <String, int>{}, incl = <String, int>{};
  for (final f in funcs) { final n = nameOf(f); excl[n] = (excl[n] ?? 0) + (f.exclusiveTicks ?? 0); incl[n] = (incl[n] ?? 0) + (f.inclusiveTicks ?? 0); }
  print('samples: $total (period ${s.samplePeriod} us, ${(total * (s.samplePeriod ?? 0) / 1e6).toStringAsFixed(1)} s sampled)');
  print('\n== exclusive (self time) ==');
  for (final e in sorted(excl).take(top)) print('${(100 * e.value / total).toStringAsFixed(1).padLeft(5)}%  ${e.value.toString().padLeft(7)}  ${e.key}');
  print('\n== inclusive ==');
  for (final e in sorted(incl).take(top)) print('${(100 * e.value / total).toStringAsFixed(1).padLeft(5)}%  ${e.value.toString().padLeft(7)}  ${e.key}');
  final names = {for (var i = 0; i < funcs.length; i++) i: nameOf(funcs[i])};
  final hottest = sorted(excl).first.key;
  final callers = <String, int>{};
  for (final smp in s.samples ?? <CpuSample>[]) {
    final st = smp.stack ?? <int>[];
    if (st.isEmpty || names[st[0]] != hottest) continue;
    final k = st.skip(1).map((i) => names[i] ?? '?').take(3).join(' <- ');
    callers[k] = (callers[k] ?? 0) + 1;
  }
  print('\n== beneath $hottest ==');
  for (final e in sorted(callers).take(10)) print('${(100 * e.value / total).toStringAsFixed(1).padLeft(5)}%  ${e.value.toString().padLeft(7)}  ${e.key}');
  // The Dart callers of the hash-container operations, together: the nearest two frames that are
  // neither the container's own nor the VM's.
  final mapCallers = <String, int>{};
  var mapTotal = 0;
  bool isContainer(String n) => n.contains('LinkedHashMap') || n.contains('LinkedHashSet') || n.contains('_HashBase') || n.startsWith('_Map') || n.startsWith('_Set') || n.contains('_CompactIterator') || n.contains('Map.Map') || n.contains('Set.Set') || n.contains('LinkedHashMap.') || n.contains('LinkedHashSet.');
  for (final smp in s.samples ?? <CpuSample>[]) {
    final st = (smp.stack ?? <int>[]).map((i) => names[i] ?? '?').toList();
    if (st.isEmpty || !isContainer(st[0])) continue;
    mapTotal++;
    final k = st.skip(1).where((n) => !isContainer(n) && !n.startsWith('native:') && !n.startsWith('_') && !n.startsWith('Object.') && !n.startsWith('Iterable.') && !n.startsWith('List.') && !n.startsWith('MapBase') && !n.startsWith('SetBase') && !n.startsWith('Map.') && !n.startsWith('Set.')).take(2).join(' <- ');
    mapCallers[k] = (mapCallers[k] ?? 0) + 1;
  }
  print('\n== the hash containers: ${(100 * mapTotal / total).toStringAsFixed(1)}% of samples, by their callers ==');
  for (final e in sorted(mapCallers).take(25)) print('${(100 * e.value / total).toStringAsFixed(1).padLeft(5)}%  ${e.value.toString().padLeft(7)}  ${e.key}');

  final members = (ap.members ?? <ClassHeapStats>[]).where((m) => (m.accumulatedSize ?? 0) > 0).toList()
    ..sort((a, b) => (b.accumulatedSize ?? 0).compareTo(a.accumulatedSize ?? 0));
  var allBytes = 0; for (final m in members) { allBytes += m.accumulatedSize ?? 0; }
  print('\n== allocation during the measured phase, by class: ${(allBytes / 1e6).toStringAsFixed(0)} MB ==');
  for (final m in members.take(20)) print('${(100 * (m.accumulatedSize ?? 0) / allBytes).toStringAsFixed(1).padLeft(5)}%  ${((m.accumulatedSize ?? 0) / 1e6).toStringAsFixed(1).padLeft(8)} MB  ${(m.instancesAccumulated ?? 0).toString().padLeft(10)} instances  ${m.classRef?.name}');
  for (final e in traces.entries) {
    final tr = e.value; final fs = tr.functions ?? <ProfileFunction>[];
    final sites = <String, int>{}; var n = 0;
    for (final smp in tr.samples ?? <CpuSample>[]) {
      n++;
      final st = (smp.stack ?? <int>[]).map((i) => nameOf(fs[i])).where((x) => !x.startsWith('native:') && !x.startsWith('_') && !x.startsWith('List.') && !x.startsWith('Map.') && !x.startsWith('Object.') && !x.startsWith('Iterable.') && !x.startsWith('String')).take(3).join(' <- ');
      sites[st] = (sites[st] ?? 0) + 1;
    }
    if (n == 0) continue;
    print('\n== allocation sites of ${e.key} ($n sampled) ==');
    for (final x in sorted(sites).take(10)) print('${(100 * x.value / n).toStringAsFixed(1).padLeft(5)}%  ${x.value.toString().padLeft(7)}  ${x.key}');
  }
  await vm.dispose();
  await proc.exitCode.timeout(const Duration(seconds: 30), onTimeout: () { proc.kill(); return -1; });
  Directory(work).deleteSync(recursive: true);
}
