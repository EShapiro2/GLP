// What the system makes of a program's directory named with and without a
// trailing separator, as GLP's linker names its modules (program_linker.dart's
// _normPath and _isRootSelfGlp): for the Windows refusal of
// "../programs/gsc/", which the REPL calls a directory with no self.glp.
// Run from glp_runtime: dart run ../servers/gas/tools/windiag.dart
import 'dart:io';

import 'package:path/path.dart' as ppath;

String norm(String p) {
  var n = ppath.normalize(Directory(p).absolute.path);
  if (n.length > 1 && n.endsWith(Platform.pathSeparator)) n = n.substring(0, n.length - 1);
  return n;
}

void main() {
  for (final root in ['../programs/gsc/', '../programs/gsc']) {
    final d = Directory(root);
    final self = d.listSync().whereType<File>().where((f) => f.path.endsWith('self.glp')).toList();
    print('root "$root": absolute ${d.absolute.path} | norm ${norm(root)}');
    for (final f in self) {
      print('  self.glp listed as "${f.path}" | split last "${f.path.split(Platform.pathSeparator).last}"'
          ' | parent "${f.parent.path}" | norm parent ${norm(f.parent.path)} | equal ${norm(f.parent.path) == norm(root)}');
    }
    final rec = d.listSync(recursive: true).whereType<File>().where((f) => f.path.endsWith('self.glp')).take(2);
    for (final f in rec) {
      print('  recursive: "${f.path}" | parent "${f.parent.path}"');
    }
  }
}
