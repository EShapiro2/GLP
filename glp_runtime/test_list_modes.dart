import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'package:glp_runtime/analysis/type_checker/type_compiler.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';

void main() {
  // Test standard List
  final env = parseTypes('MyList ::= [] ; [_ | MyList].');
  final typeDef = env.getType('MyList');

  print('=== MyList Type Definition ===');
  print('Alternatives:');
  for (final alt in typeDef!.alternatives) {
    print('  $alt (${alt.runtimeType})');
    if (alt is ListConsAlt) {
      print('    head: ${alt.head} (${alt.head.runtimeType})');
      if (alt.head is PrimitiveModeAlt) {
        print('      isInput: ${(alt.head as PrimitiveModeAlt).isInput}');
      }
      print('    tail: ${alt.tail} (${alt.tail.runtimeType})');
      if (alt.tail is TypeRef) {
        print('      isInput: ${(alt.tail as TypeRef).isInput}');
      }
    }
  }

  // Test channel type
  final env2 = parseTypes('MyStream ::= [] ; [_ | MyStream]. MyChannel ::= ch(MyStream?, MyStream) ; ch(MyStream, MyStream?).');
  final channelDef = env2.getType('MyChannel');

  print('\n=== MyChannel Type Definition ===');
  print('Alternatives:');
  for (final alt in channelDef!.alternatives) {
    print('  $alt (${alt.runtimeType})');
    if (alt is StructAlt) {
      for (int i = 0; i < alt.args.length; i++) {
        final arg = alt.args[i];
        print('    arg${i+1}: $arg (${arg.runtimeType})');
        if (arg is TypeRef) {
          print('      isInput: ${arg.isInput}');
        }
      }
    }
  }

  // Compile and show DFAs
  final compiler = TypeCompiler(env);
  final listDFA = compiler.compile('MyList');
  print('\n=== MyList DFA Transitions ===');
  for (final entry in listDFA.transitions.entries) {
    final (from, pathElem) = entry.key;
    final to = entry.value;
    print('  ${from.name} --[$pathElem (mode=${pathElem.mode})]--> ${to.name}');
  }

  final compiler2 = TypeCompiler(env2);
  final channelDFA = compiler2.compile('MyChannel');
  print('\n=== MyChannel DFA Transitions ===');
  for (final entry in channelDFA.transitions.entries) {
    final (from, pathElem) = entry.key;
    final to = entry.value;
    print('  ${from.name} --[$pathElem (mode=${pathElem.mode})]--> ${to.name}');
  }
}
