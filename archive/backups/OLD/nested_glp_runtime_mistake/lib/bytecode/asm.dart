// ignore_for_file: non_constant_identifier_names

import 'opcodes.dart';
import 'runner.dart';

/// Helper class for building bytecode programs
/// Provides convenient constructors for all opcodes
class BC {
  // Labels
  static Label L(String name) => Label(name);

  // Control flow
  static ClauseTry TRY() => ClauseTry();
  static Commit COMMIT() => Commit();
  static Proceed PROCEED() => Proceed();
  static SuspendEnd SUSP() => SuspendEnd();

  // HEAD instructions
  static HeadConstant headConst(Object? value, int argSlot) =>
      HeadConstant(value, argSlot);

  static HeadStructure headStruct(String functor, int arity, int argSlot) =>
      HeadStructure(functor, arity, argSlot);

  static HeadWriter headWriter(int varIndex) => HeadWriter(varIndex);

  static HeadReader headReader(int varIndex) => HeadReader(varIndex);

  static UnifyConstant unifyConst(Object? value) => UnifyConstant(value);

  static UnifyVoid unifyVoid({int count = 1}) => UnifyVoid(count: count);

  static GetVariable getVar(int varIndex, int argSlot) =>
      GetVariable(varIndex, argSlot);

  static GetValue getVal(int varIndex, int argSlot) =>
      GetValue(varIndex, argSlot);

  // GUARD instructions
  static Otherwise otherwise() => Otherwise();

  // BODY instructions
  static PutWriter putWriter(int varIndex, int argSlot) =>
      PutWriter(varIndex, argSlot);

  static PutReader putReader(int varIndex, int argSlot) =>
      PutReader(varIndex, argSlot);

  static PutConstant putConst(Object? value, int argSlot) =>
      PutConstant(value, argSlot);

  static PutStructure putStructure(String functor, int arity, int argSlot) =>
      PutStructure(functor, arity, argSlot);

  static SetWriter setWriter(int varIndex) => SetWriter(varIndex);

  static SetReader setReader(int varIndex) => SetReader(varIndex);

  static SetConstant setConst(Object? value) => SetConstant(value);

  // Process management
  static Spawn spawn(String label, int arity) => Spawn(label, arity);

  static Requeue requeue(String label, int arity) => Requeue(label, arity);

  // Program builder
  static BytecodeProgram prog(List<Op> ops) => BytecodeProgram(ops);
}
