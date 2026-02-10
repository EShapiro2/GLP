/// Helper types and operations for madGLP
///
/// Provides Globalize and Localize operations as specified in
/// madGLP-spec.md Sections 5.1 and 5.2.
///
/// These operations transform terms between local and global representations
/// for inter-agent communication.
library;

import 'package:glp_runtime/runtime/terms.dart';
import 'global_writers_table.dart';

/// Type of global variable name
enum GlobalNameType {
  /// `_w(p, i)` - writer globalized at p
  writer,

  /// `_r(p, i)` - reader globalized at p
  reader,
}

/// Global variable name: `_w(p, i)` or `_r(p, i)`
///
/// See: madGLP-spec.md Section 2
class GlobalName {
  final GlobalNameType type;
  final String agent;
  final int index;

  GlobalName(this.type, this.agent, this.index);

  /// Create a writer global name `_w(agent, index)`
  GlobalName.writer(this.agent, this.index) : type = GlobalNameType.writer;

  /// Create a reader global name `_r(agent, index)`
  GlobalName.reader(this.agent, this.index) : type = GlobalNameType.reader;

  bool get isWriter => type == GlobalNameType.writer;
  bool get isReader => type == GlobalNameType.reader;

  @override
  String toString() =>
      type == GlobalNameType.writer ? '_w($agent, $index)' : '_r($agent, $index)';

  @override
  bool operator ==(Object other) =>
      other is GlobalName &&
      type == other.type &&
      agent == other.agent &&
      index == other.index;

  @override
  int get hashCode => Object.hash(type, agent, index);
}

/// Information needed to spawn a global_send goal
///
/// Represents the goal: `global_send(readerAddr, globalName, destAgent)`
///
/// See: madGLP-spec.md Section 4
class GlobalSendSpawn {
  /// Address of the reader to watch (the ? end of the variable pair)
  final int readerAddr;

  /// Global name identifying the link
  final GlobalName globalName;

  /// Destination agent
  final String destAgent;

  GlobalSendSpawn({
    required this.readerAddr,
    required this.globalName,
    required this.destAgent,
  });

  @override
  String toString() =>
      'GlobalSendSpawn(reader=$readerAddr, name=$globalName, dest=$destAgent)';
}

/// A variable reference in a term (for globalize/localize)
///
/// Represents either a writer (addr) or reader (addr with isReader=true).
/// Always carries both writer and reader addresses of the pair.
class TermVar {
  final int addr;
  final bool isReader;

  /// Writer address of the variable pair
  final int writerAddr;

  /// Reader address of the variable pair
  final int readerAddr;

  /// Create a writer variable reference
  TermVar.writer(this.addr, {required this.readerAddr})
      : isReader = false,
        writerAddr = addr;

  /// Create a reader variable reference
  TermVar.reader(this.addr, {required this.writerAddr})
      : isReader = true,
        readerAddr = addr;

  bool get isWriter => !isReader;

  /// Get the paired reader address
  int get pairedReaderAddr => readerAddr;

  @override
  String toString() => isReader
      ? 'TermVar.reader($addr, writer=$writerAddr)'
      : 'TermVar.writer($addr, reader=$readerAddr)';
}

/// Result of a Globalize operation
///
/// See: madGLP-spec.md Section 5.1
class GlobalizeResult {
  /// Global names substituted for variables, in order of occurrence
  final List<GlobalName> globalNames;

  /// Spawns needed for reader variables
  final List<GlobalSendSpawn> spawns;

  GlobalizeResult({
    required this.globalNames,
    required this.spawns,
  });
}

/// A fresh variable pair created during localization
class FreshPair {
  final int writerAddr;
  final int readerAddr;

  FreshPair(this.writerAddr, this.readerAddr);

  @override
  String toString() => 'FreshPair(writer=$writerAddr, reader=$readerAddr)';
}

/// Result of a Localize operation
///
/// See: madGLP-spec.md Section 5.2
class LocalizeResult {
  /// Fresh variable pairs created, one per global name
  final List<FreshPair> freshPairs;

  /// What to substitute in the term: writer addr or reader addr per position
  /// false = use writer (Y_q) for _w names, true = use reader (Z_q?) for _r names
  final List<bool> useReader;

  /// Spawns needed for _w(p,i) global names
  final List<GlobalSendSpawn> spawns;

  LocalizeResult({
    required this.freshPairs,
    required this.useReader,
    required this.spawns,
  });
}

/// Globalize operation
///
/// Given agent p, remote agent q, and a list of variables occurring in term T,
/// produces the globalized representation T_p↑.
///
/// Spec Section 5.1:
/// - If Y is a writer: allocate index i, create entry (Y, q) at index i,
///   replace with _w(p, i). No goal is spawned.
/// - If Y? is a reader: allocate index i, replace with _r(p, i),
///   spawn global_send(Y?, _r(p,i), q). No entry is created.
///
/// Returns GlobalizeResult with global names and spawn info.
/// Updates the GlobalWritersTable with entries for writers.
GlobalizeResult globalize({
  required List<TermVar> variables,
  required String localAgent,
  required String remoteAgent,
  required GlobalWritersTable table,
}) {
  final globalNames = <GlobalName>[];
  final spawns = <GlobalSendSpawn>[];

  for (final v in variables) {
    if (v.isWriter) {
      // Writer: create entry, no spawn
      // Spec: "allocate the next index i, create entry (Y, q) at index i
      //        in W'_p, and replace Y in T_p↑ with _w(p, i).
      //        No goal is spawned—p will receive the assignment on this link."
      // Entry stores the writer address Y for later binding when value arrives.
      final index = table.addGlobalizeEntry(v.writerAddr, remoteAgent);
      globalNames.add(GlobalName.writer(localAgent, index));
    } else {
      // Reader: spawn global_send, no entry
      // Spec: "allocate the next index i, replace Y? in T_p↑ with _r(p, i),
      //        and spawn global_send(Y?, _r(p,i), q). No entry is created—
      //        the global_send goal handles outgoing communication."
      final index = table.allocateIndex();
      final globalName = GlobalName.reader(localAgent, index);
      globalNames.add(globalName);

      // Spawn global_send(Y?, _r(p,i), q)
      // Note: GlobalSendSpawn.readerAddr is used as the key for heap.onBind(),
      // which is indexed by *writer* address. We pass writerAddr so the
      // callback fires when bindVariable is called on Y.
      spawns.add(GlobalSendSpawn(
        readerAddr: v.writerAddr,
        globalName: globalName,
        destAgent: remoteAgent,
      ));
    }
  }

  return GlobalizeResult(globalNames: globalNames, spawns: spawns);
}

/// Localize operation
///
/// Given agent q, remote agent p, and a list of global names from term T_p↑,
/// produces the localized representation T_q↓.
///
/// Spec Section 5.2:
/// - If _w(p, i): create fresh pair (Y_q, Y_q?), replace with Y_q (writer),
///   spawn global_send(Y_q?, _w(p,i), p). No entry is created.
/// - If _r(p, i): create fresh pair (Z_q, Z_q?), add entry (Z_q, p, i),
///   replace with Z_q? (reader). No goal is spawned.
///
/// Returns LocalizeResult with fresh pairs, usage info, and spawn info.
/// Updates the GlobalWritersTable with entries for _r names.
///
/// The [freshAddrAllocator] function is called to allocate each fresh variable pair,
/// returning (writerAddr, readerAddr).
LocalizeResult localize({
  required List<GlobalName> globalNames,
  required String localAgent,
  required GlobalWritersTable table,
  required (int, int) Function() freshAddrAllocator,
}) {
  final freshPairs = <FreshPair>[];
  final useReader = <bool>[];
  final spawns = <GlobalSendSpawn>[];

  for (final gn in globalNames) {
    // Allocate fresh pair
    final (writerAddr, readerAddr) = freshAddrAllocator();
    final pair = FreshPair(writerAddr, readerAddr);
    freshPairs.add(pair);

    if (gn.isWriter) {
      // _w(p, i): spawn global_send, use writer
      // Spec: "create fresh local pair (Y_q, Y_q?), replace _w(p, i) with
      //        Y_q (the writer) in T_q↓, and spawn global_send(Y_q?, _w(p,i), p).
      //        No entry is created—the global_send goal handles outgoing communication."
      useReader.add(false); // Use Y_q (writer)

      // Spawn global_send(Y_q?, _w(p,i), p)
      // When q assigns Y_q, Y_q? becomes known, gs fires and sends value to p.
      // Note: GlobalSendSpawn.readerAddr is used as the key for heap.onBind(),
      // which is indexed by *writer* address. We pass writerAddr so the callback
      // fires when bindVariable(writerAddr, ...) is called.
      spawns.add(GlobalSendSpawn(
        readerAddr: writerAddr,
        globalName: gn,
        destAgent: gn.agent, // Send back to agent p who created the name
      ));
    } else {
      // _r(p, i): create entry, use reader
      // Spec: "create fresh local pair (Z_q, Z_q?), allocate the next index k
      //        in W'_q, add entry (Z_q, p, i), and replace _r(p, i) with Z_q?
      //        (the reader) in T_q↓. No goal is spawned—q will receive the
      //        assignment on this link."
      table.addLocalizeEntry(writerAddr, gn.agent, gn.index);
      useReader.add(true); // Use Z_q? (reader)
    }
  }

  return LocalizeResult(
    freshPairs: freshPairs,
    useReader: useReader,
    spawns: spawns,
  );
}

// ============================================================================
// Term Transformation Functions
// ============================================================================

/// Transform a term by replacing variables with their global names
///
/// Takes the original term and the GlobalizeResult from globalize().
/// Returns a new term with VarRefs replaced by StructTerms representing
/// global names (_w(agent, index) or _r(agent, index)).
Term globalizeTermWithResult(
  Term term,
  List<TermVar> variables,
  GlobalizeResult result,
) {
  final varToGlobalName = <int, GlobalName>{};
  for (var i = 0; i < variables.length; i++) {
    varToGlobalName[variables[i].addr] = result.globalNames[i];
  }
  return _substituteGlobalNames(term, varToGlobalName);
}

Term _substituteGlobalNames(Term term, Map<int, GlobalName> mapping) {
  if (term is VarRef) {
    final gn = mapping[term.addr];
    if (gn != null) {
      final functor = gn.isWriter ? '_w' : '_r';
      return StructTerm(functor, [ConstTerm(gn.agent), ConstTerm(gn.index)]);
    }
    return term;
  } else if (term is StructTerm) {
    final newArgs = term.args.map((a) => _substituteGlobalNames(a, mapping)).toList();
    return StructTerm(term.functor, newArgs);
  }
  return term; // ConstTerm unchanged
}

/// Extract global name structures from a term
///
/// Finds all _w(agent, index) and _r(agent, index) structures in the term.
/// Returns the list of GlobalNames in order of occurrence.
List<GlobalName> extractGlobalNames(Term term) {
  final result = <GlobalName>[];
  _extractGlobalNamesRecursive(term, result);
  return result;
}

void _extractGlobalNamesRecursive(Term term, List<GlobalName> result) {
  if (term is StructTerm) {
    if ((term.functor == '_w' || term.functor == '_r') && term.args.length == 2) {
      final agentArg = term.args[0];
      final indexArg = term.args[1];
      if (agentArg is ConstTerm && indexArg is ConstTerm) {
        final agent = agentArg.value as String;
        final index = (indexArg.value as num).toInt();
        result.add(term.functor == '_w'
            ? GlobalName.writer(agent, index)
            : GlobalName.reader(agent, index));
      }
    } else {
      for (final arg in term.args) {
        _extractGlobalNamesRecursive(arg, result);
      }
    }
  }
}

/// Transform a term by replacing global names with local variables
///
/// Takes the globalized term and the LocalizeResult from localize().
/// Returns a new term with global name structures replaced by VarRefs.
Term localizeTermWithResult(
  Term term,
  List<GlobalName> globalNames,
  LocalizeResult result,
) {
  final globalNameToLocal = <String, int>{};
  for (var i = 0; i < globalNames.length; i++) {
    final gn = globalNames[i];
    final pair = result.freshPairs[i];
    final useReader = result.useReader[i];
    globalNameToLocal['${gn.type.name}:${gn.agent}:${gn.index}'] =
        useReader ? pair.readerAddr : pair.writerAddr;
  }
  return _substituteLocalVars(term, globalNameToLocal);
}

Term _substituteLocalVars(Term term, Map<String, int> mapping) {
  if (term is StructTerm) {
    if ((term.functor == '_w' || term.functor == '_r') && term.args.length == 2) {
      final agentArg = term.args[0];
      final indexArg = term.args[1];
      if (agentArg is ConstTerm && indexArg is ConstTerm) {
        final type = term.functor == '_w' ? 'writer' : 'reader';
        final key = '$type:${agentArg.value}:${indexArg.value}';
        final localAddr = mapping[key];
        if (localAddr != null) {
          return VarRef(localAddr);
        }
      }
    }
    final newArgs = term.args.map((a) => _substituteLocalVars(a, mapping)).toList();
    return StructTerm(term.functor, newArgs);
  }
  return term;
}
