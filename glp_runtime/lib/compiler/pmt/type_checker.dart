/// MT Type Checker: Verifies terms match their declared types
///
/// Checks that constants, structs, and list elements conform to
/// the type definitions in the module.

import 'package:glp_runtime/compiler/ast.dart';
import 'type_table.dart';
import 'mode_table.dart';

/// Type checking error
class TypeError {
  final String message;
  final int line;
  final int column;
  final String? suggestion;

  TypeError(this.message, this.line, this.column, {this.suggestion});

  @override
  String toString() {
    var result = '[type] $message at Line $line, Column $column';
    if (suggestion != null) {
      result += '\n  Suggestion: $suggestion';
    }
    return result;
  }
}

/// Type checker for Moded Type definitions
class TypeChecker {
  final TypeTable typeTable;
  final ModeTable modeTable;

  TypeChecker(this.typeTable, this.modeTable);

  /// Check all clauses in a module
  List<TypeError> checkModule(Module module) {
    final errors = <TypeError>[];

    for (final proc in module.procedures) {
      final modeDecl = modeTable.getDeclaration(proc.name, proc.arity);
      if (modeDecl == null) continue; // No declaration, skip type checking

      for (final clause in proc.clauses) {
        errors.addAll(checkClause(clause, modeDecl));
      }
    }

    return errors;
  }

  /// Check a single clause against its mode declaration
  List<TypeError> checkClause(Clause clause, ModeDeclaration modeDecl) {
    final errors = <TypeError>[];

    // Check head arguments
    for (int i = 0; i < clause.head.args.length && i < modeDecl.args.length; i++) {
      final arg = clause.head.args[i];
      final declaredType = modeDecl.args[i].typeName;
      final typeParams = modeDecl.args[i].typeParams;
      errors.addAll(checkTerm(arg, declaredType, typeParams));
    }

    // Check body goals
    if (clause.body != null) {
      for (final goal in clause.body!) {
        final goalDecl = modeTable.getDeclaration(goal.functor, goal.arity);
        if (goalDecl == null) continue;

        for (int i = 0; i < goal.args.length && i < goalDecl.args.length; i++) {
          final arg = goal.args[i];
          final declaredType = goalDecl.args[i].typeName;
          final typeParams = goalDecl.args[i].typeParams;
          errors.addAll(checkTerm(arg, declaredType, typeParams));
        }
      }
    }

    return errors;
  }

  /// Check a term against an expected type
  List<TypeError> checkTerm(Term term, String expectedType, List<String> typeParams) {
    final errors = <TypeError>[];

    // Variables are not checked - their types are inferred
    if (term is VarTerm) {
      return errors;
    }

    // Underscore matches any type
    if (term is UnderscoreTerm) {
      return errors;
    }

    // Constants must be valid constructors of the type
    if (term is ConstTerm) {
      if (!isValidConstant(term, expectedType)) {
        final validConstructors = getValidConstructors(expectedType);
        errors.add(TypeError(
          "'${term.value}' is not a valid '$expectedType'",
          term.line,
          term.column,
          suggestion: validConstructors.isNotEmpty
              ? "Valid constructors: ${validConstructors.join(', ')}"
              : null,
        ));
      }
      return errors;
    }

    // List terms
    if (term is ListTerm) {
      // Empty list is valid for any list type
      if (term.isNil) {
        return errors;
      }

      // Check if expectedType has list constructors
      final typeDef = typeTable.getType(expectedType);
      if (typeDef != null) {
        final hasListCtor = typeDef.constructors.any((c) => c is ListConstructor);
        if (!hasListCtor && expectedType != 'List') {
          errors.add(TypeError(
            "List term not valid for type '$expectedType'",
            term.line,
            term.column,
          ));
          return errors;
        }

        // Build type parameter substitution map
        // e.g., List(A) with typeParams=["BinaryDigit"] -> {A: BinaryDigit}
        final typeParamSubst = <String, String>{};
        for (int i = 0; i < typeDef.typeParams.length && i < typeParams.length; i++) {
          typeParamSubst[typeDef.typeParams[i]] = typeParams[i];
        }

        // Find non-nil list constructor to get element type
        final listCtor = typeDef.constructors
            .whereType<ListConstructor>()
            .where((c) => !c.isNil)
            .firstOrNull;
        if (listCtor != null && listCtor.head != null && term.head != null) {
          final paramTypeName = listCtor.head!.typeName;
          // Substitute type parameter if applicable
          final elementType = typeParamSubst[paramTypeName] ?? paramTypeName;
          if (elementType != '_') {
            errors.addAll(checkTerm(term.head!, elementType, []));
          }
        }
      } else if (typeParams.isNotEmpty && term.head != null) {
        // Type not defined but we have type params - use first as element type
        errors.addAll(checkTerm(term.head!, typeParams[0], []));
      }

      // Check tail recursively
      if (term.tail != null) {
        errors.addAll(checkTerm(term.tail!, expectedType, typeParams));
      }

      return errors;
    }

    // Struct terms
    if (term is StructTerm) {
      if (!isValidStructConstructor(term, expectedType, typeParams)) {
        final validConstructors = getValidConstructors(expectedType);
        errors.add(TypeError(
          "'${term.functor}(...)' is not a valid '$expectedType'",
          term.line,
          term.column,
          suggestion: validConstructors.isNotEmpty
              ? "Valid constructors: ${validConstructors.join(', ')}"
              : null,
        ));
      }
      return errors;
    }

    return errors;
  }

  /// Check if a constant is a valid constructor for a type
  bool isValidConstant(ConstTerm term, String typeName) {
    // Built-in types
    if (typeName == 'Num' || typeName == 'Number' || typeName == 'Int' || typeName == 'Integer') {
      return term.value is num;
    }
    if (typeName == 'Atom' || typeName == 'String') {
      return term.value is String;
    }

    // User-defined types
    final typeDef = typeTable.getType(typeName);
    if (typeDef == null) {
      // Type not defined - allow (might be built-in or external)
      return true;
    }

    final termValue = term.value.toString();

    // Check if constant matches any atom constructor
    for (final ctor in typeDef.constructors) {
      if (ctor is AtomConstructor && ctor.name == termValue) {
        return true;
      }
    }

    // Check if type references another type (capitalized AtomConstructor)
    // e.g., Gates := And | Or | Not - And, Or, Not are type references
    for (final ctor in typeDef.constructors) {
      if (ctor is AtomConstructor && _isCapitalized(ctor.name)) {
        // This is a type reference - check if that type contains the constant
        final refTypeDef = typeTable.getType(ctor.name);
        if (refTypeDef != null && _typeContainsAtom(refTypeDef, termValue)) {
          return true;
        }
      }
    }

    return false;
  }

  /// Check if a struct term is valid for a type
  bool isValidStructConstructor(StructTerm term, String typeName, [List<String> typeParams = const []]) {
    final typeDef = typeTable.getType(typeName);
    if (typeDef == null) {
      return true; // Unknown type - allow
    }

    // Build type parameter substitution map
    final typeParamSubst = <String, String>{};
    for (int i = 0; i < typeDef.typeParams.length && i < typeParams.length; i++) {
      typeParamSubst[typeDef.typeParams[i]] = typeParams[i];
    }

    // Check direct struct constructors
    for (final ctor in typeDef.constructors) {
      if (ctor is StructConstructor && ctor.functor == term.functor) {
        return true;
      }
    }

    // Check type references (capitalized AtomConstructors)
    // e.g., Gates := And | Or | Not
    // If term.functor is 'and', check if And's mode declaration matches
    for (final ctor in typeDef.constructors) {
      if (ctor is AtomConstructor && _isCapitalized(ctor.name)) {
        final ctorName = ctor.name;

        // Check if this is a type parameter that should be substituted
        if (typeParamSubst.containsKey(ctorName)) {
          final substitutedType = typeParamSubst[ctorName]!;
          // Recursively check if term is valid for the substituted type
          if (isValidStructConstructor(term, substitutedType)) {
            return true;
          }
        }

        // This is a type reference - check if its mode declaration has this predicate
        final modeDecl = modeTable.getDeclarationByTypeName(ctorName);
        if (modeDecl != null && modeDecl.predicate == term.functor) {
          return true;
        }
      }
    }

    return false;
  }

  bool _isCapitalized(String name) {
    if (name.isEmpty) return false;
    final first = name[0];
    return first == first.toUpperCase() && first != first.toLowerCase();
  }

  bool _typeContainsAtom(TypeDefinition typeDef, String atomName) {
    for (final ctor in typeDef.constructors) {
      if (ctor is AtomConstructor) {
        if (ctor.name == atomName) {
          return true;
        }
        // If this is a type reference, recurse
        if (_isCapitalized(ctor.name)) {
          final refType = typeTable.getType(ctor.name);
          if (refType != null && _typeContainsAtom(refType, atomName)) {
            return true;
          }
        }
      }
    }
    return false;
  }

  /// Get list of valid constructors for error messages
  List<String> getValidConstructors(String typeName) {
    final result = <String>[];
    final typeDef = typeTable.getType(typeName);
    if (typeDef == null) return result;

    for (final ctor in typeDef.constructors) {
      if (ctor is AtomConstructor) {
        result.add(ctor.name);
      } else if (ctor is StructConstructor) {
        result.add('${ctor.functor}(...)');
      } else if (ctor is ListConstructor) {
        result.add(ctor.isNil ? '[]' : '[...|...]');
      } else if (ctor is TupleConstructor) {
        result.add('(...)');
      }
    }

    return result;
  }
}
