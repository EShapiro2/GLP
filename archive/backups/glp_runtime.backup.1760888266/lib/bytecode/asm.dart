// ignore_for_file: non_constant_identifier_names
import 'opcodes.dart';
import 'runner.dart';

class BC {
  // lowerCamelCase helpers
  static Label l(String name) => Label(name);
  static ClauseTry try_() => ClauseTry();
  static GuardNeedReader r(int readerId) => GuardNeedReader(readerId);
  static HeadBindWriter w(int writerId) => HeadBindWriter(writerId);
  static Commit commit() => Commit();
  static UnionSiAndGoto u(String label) => UnionSiAndGoto(label);
  static ResetAndGoto next(String label) => ResetAndGoto(label);
  static SuspendEnd susp() => SuspendEnd();
  static Proceed proceed() => Proceed();
  static BodySetConst bconst(int writerId, Object? v) => BodySetConst(writerId, v);
  static BodySetStructConstArgs bstructC(int writerId, String f, List<Object?> constArgs)
    => BodySetStructConstArgs(writerId, f, constArgs);

  // UPPERCASE aliases
  static Label L(String name) => l(name);
  static ClauseTry TRY() => try_();
  static GuardNeedReader R(int readerId) => r(readerId);
  static HeadBindWriter W(int writerId) => w(writerId);
  static Commit COMMIT() => commit();
  static UnionSiAndGoto U(String label) => u(label);
  static ResetAndGoto NEXT(String label) => next(label);
  static SuspendEnd SUSP() => susp();
  static Proceed PROCEED() => proceed();
  static BodySetConst BCONST(int writerId, Object? v) => bconst(writerId, v);
  static BodySetStructConstArgs BSTRUCTC(int writerId, String f, List<Object?> constArgs)
    => bstructC(writerId, f, constArgs);

  static BytecodeProgram prog(List<Op> ops) => BytecodeProgram(ops);
}
