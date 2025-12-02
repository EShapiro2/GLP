import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  final source = '''
helper(X?, X).
  ''';

  final result = GlpCompiler().compile(source);

  print('=== helper(X?, X) clause ===');
  var i = 0;
  for (final op in result.ops) {
    final details = op.toString();
    print('$i: ${op.runtimeType} $details');
    i++;
  }
}
