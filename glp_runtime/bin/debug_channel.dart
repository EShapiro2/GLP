import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'package:glp_runtime/analysis/type_checker/type_compiler.dart';

void main() {
  final source = '''
    MyList ::= [] ; [_ | MyList].
    MyStream ::< MyList.
    MyChannel ::= ch(MyStream?, MyStream) ; ch(MyStream, MyStream?).
  ''';

  final env = parseTypes(source);
  final compiler = TypeCompiler(env);

  print('=== MyList DFA ===');
  final listDfa = compiler.compile('MyList');
  for (final entry in listDfa.transitions.entries) {
    final (from, pathElem) = entry.key;
    final to = entry.value;
    print('  ${from.name} --[$pathElem]--> ${to.name}');
  }

  print('\n=== MyChannel DFA ===');
  final channelDfa = compiler.compile('MyChannel');
  for (final entry in channelDfa.transitions.entries) {
    final (from, pathElem) = entry.key;
    final to = entry.value;
    print('  ${from.name} --[$pathElem]--> ${to.name}');
  }
}
