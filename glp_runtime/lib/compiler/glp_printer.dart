// GLP Printer - Converts AST back to GLP source code
//
// This serializer produces valid GLP source from AST nodes,
// preserving SRSW annotations (X vs X?) and all term types.

import 'ast.dart';

/// Converts GLP AST back to source code
class GlpPrinter {
  /// Print a complete program
  String printProgram(Program program) {
    final buffer = StringBuffer();

    for (final procedure in program.procedures) {
      buffer.write(printProcedure(procedure));
      buffer.writeln();
    }

    return buffer.toString();
  }

  /// Print a procedure (all clauses)
  String printProcedure(Procedure procedure) {
    final buffer = StringBuffer();

    for (final clause in procedure.clauses) {
      buffer.writeln(printClause(clause));
    }

    return buffer.toString();
  }

  /// Print a single clause
  String printClause(Clause clause) {
    final buffer = StringBuffer();

    // Head
    buffer.write(printAtom(clause.head));

    // Guards and body
    final hasGuards = clause.guards != null && clause.guards!.isNotEmpty;
    final hasBody = clause.body != null && clause.body!.isNotEmpty;

    if (hasGuards || hasBody) {
      buffer.write(' :- ');

      // Guards
      if (hasGuards) {
        buffer.write(clause.guards!.map(printGuard).join(', '));
      }

      // Body separator - only use | if there are guards
      if (hasBody) {
        if (hasGuards) {
          buffer.write(' | ');
        }
        buffer.write(clause.body!.map(printGoal).join(', '));
      }
    }

    buffer.write('.');
    return buffer.toString();
  }

  /// Print an atom (clause head)
  String printAtom(Atom atom) {
    if (atom.args.isEmpty) {
      return atom.functor;
    }

    // Handle special infix operators
    if (_isInfixOperator(atom.functor) && atom.args.length == 2) {
      return '${printTerm(atom.args[0])} ${atom.functor} ${printTerm(atom.args[1])}';
    }

    return '${atom.functor}(${atom.args.map(printTerm).join(', ')})';
  }

  /// Print a goal (body call)
  String printGoal(Goal goal) {
    // Handle remote goals
    if (goal is RemoteGoal) {
      return '${printTerm(goal.module)} # ${printGoal(goal.goal)}';
    }

    if (goal.args.isEmpty) {
      return goal.functor;
    }

    // Handle special infix operators
    if (_isInfixOperator(goal.functor) && goal.args.length == 2) {
      return '${printTerm(goal.args[0])} ${goal.functor} ${printTerm(goal.args[1])}';
    }

    return '${goal.functor}(${goal.args.map(printTerm).join(', ')})';
  }

  /// Print a guard
  String printGuard(Guard guard) {
    final prefix = guard.negated ? '~' : '';

    if (guard.args.isEmpty) {
      return '$prefix${guard.predicate}';
    }

    // Handle special infix operators
    if (_isInfixGuardOperator(guard.predicate) && guard.args.length == 2) {
      return '$prefix(${printTerm(guard.args[0])} ${guard.predicate} ${printTerm(guard.args[1])})';
    }

    return '$prefix${guard.predicate}(${guard.args.map(printTerm).join(', ')})';
  }

  /// Print a term
  String printTerm(Term term) {
    if (term is VarTerm) {
      return term.isReader ? '${term.name}?' : term.name;
    }

    if (term is UnderscoreTerm) {
      return '_';
    }

    if (term is ConstTerm) {
      return _printConstValue(term.value);
    }

    if (term is ListTerm) {
      return _printList(term);
    }

    if (term is StructTerm) {
      return _printStruct(term);
    }

    // Fallback
    return term.toString();
  }

  /// Print a constant value
  String _printConstValue(Object? value) {
    if (value == null) {
      return 'null';
    }
    if (value is String) {
      // Check if it's an atom (no quotes needed) or a string (needs quotes)
      if (_isAtom(value)) {
        return value;
      }
      // Escape string properly
      return '"${_escapeString(value)}"';
    }
    if (value is int || value is double) {
      return value.toString();
    }
    return value.toString();
  }

  /// Print a list term
  String _printList(ListTerm list) {
    if (list.isNil) {
      return '[]';
    }

    // Collect elements if it's a proper list
    final elements = <Term>[];
    Term? current = list;
    Term? tail;

    while (current is ListTerm && !current.isNil) {
      if (current.head != null) {
        elements.add(current.head!);
      }
      if (current.tail == null) {
        break;
      }
      if (current.tail is ListTerm) {
        current = current.tail;
      } else {
        // Improper list with non-list tail
        tail = current.tail;
        break;
      }
    }

    if (tail != null) {
      // Improper list: [a, b | T]
      return '[${elements.map(printTerm).join(', ')} | ${printTerm(tail)}]';
    } else {
      // Proper list: [a, b, c]
      return '[${elements.map(printTerm).join(', ')}]';
    }
  }

  /// Print a structure term
  String _printStruct(StructTerm struct) {
    // Handle comma/conjunction specially - no functor prefix
    if (struct.functor == ',' && struct.args.length == 2) {
      return '(${printTerm(struct.args[0])}, ${printTerm(struct.args[1])})';
    }

    // Handle special infix operators
    if (_isInfixOperator(struct.functor) && struct.args.length == 2) {
      return '(${printTerm(struct.args[0])} ${struct.functor} ${printTerm(struct.args[1])})';
    }

    if (struct.args.isEmpty) {
      return struct.functor;
    }

    return '${struct.functor}(${struct.args.map(printTerm).join(', ')})';
  }

  /// Check if a functor is an infix operator
  bool _isInfixOperator(String functor) {
    const infixOps = {
      ':=', '=', '\\=', '=..',
      '+', '-', '*', '/', '//', 'mod',
      '<', '>', '=<', '>=', '=:=', '=\\=',
      '=?=',
    };
    return infixOps.contains(functor);
  }

  /// Check if a guard predicate is infix
  bool _isInfixGuardOperator(String predicate) {
    const infixGuards = {
      '<', '>', '=<', '>=', '=:=', '=\\=', '=?=',
    };
    return infixGuards.contains(predicate);
  }

  /// Check if a string is a valid atom (lowercase start, alphanumeric)
  bool _isAtom(String s) {
    if (s.isEmpty) return false;
    // Atoms start with lowercase letter
    if (!s[0].contains(RegExp(r'[a-z]'))) return false;
    // Rest is alphanumeric or underscore
    return s.substring(1).contains(RegExp(r'^[a-zA-Z0-9_]*$'));
  }

  /// Escape special characters in a string
  String _escapeString(String s) {
    return s
        .replaceAll('\\', '\\\\')
        .replaceAll('"', '\\"')
        .replaceAll('\n', '\\n')
        .replaceAll('\t', '\\t');
  }
}
