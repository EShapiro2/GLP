import 'package:glp_runtime/engine/glp_engine.dart';

void main() async {
  // Test with current engine (loads self.glp)
  final engine = GlpEngine(stdlibDir: '../programs/stdlib')..strictTypes = false;
  engine.runtime.outputCallback = (line) => print('OUTPUT: $line');

  engine.loadSource('''
-mode(system).
procedure send_to_user(Stream?).
send_to_user([T | In]) :- ground(T?) | '_output'(T?), send_to_user(In?).
send_to_user([]).
procedure test.
test :- send_to_user([hello | Tail?]), Tail = [world].
''');

  engine.debugTrace = true;
  final result = await engine.runGoal('test');
  print('Result: ${result.status}');
}
