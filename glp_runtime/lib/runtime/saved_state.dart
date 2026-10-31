/// The bytes of an agent's saved state: the codec the runtime writes its local
/// state with in each event cycle, and reads it back with at restart (IGLP,
/// Definition "Restart", madglp-spec.tex; Implementation Notes,
/// "Persistence": "In the meantime the runtime keeps it by a store the host
/// gives it, beside the layer's queue").
///
/// The heap is no array (IGLP app:in-heap, Variable pairs): a cell is reached
/// from a goal, a suspension, a table or another cell.  So the state is
/// written from its roots --- the goals, the tables, the records --- and every
/// cell they reach is written once, named by its serial number
/// ([HeapCell.id], unique for the run), as are the suspension records the
/// cells share, the module constants and the mutual references.  Read back,
/// each cell is made again under its serial number with its tag, its pointer,
/// its paired reader and its suspension list in order, so the restarted agent
/// holds the same graph, and its next reduction, activation and Send are the
/// ones it would have made.
///
/// The format: the magic `GLPS`, a version byte, the header (the program's
/// identity, the agent's name and key), then four tables --- module
/// constants, suspension records, mutual references, cells --- and the roots,
/// each section written by its owner in a fixed order and read back in it.
/// Integers are unsigned or zigzag LEB128; a string is UTF-8 behind its byte
/// count.  Terms are written iteratively, so a list of any length is written
/// and read in constant Dart stack depth.
library;

import 'dart:convert';
import 'dart:typed_data';

import 'package:glp_runtime/wire/artefact.dart' show Artefact;

import 'heap_fcp.dart';
import 'machine_state.dart';
import 'suspension.dart';
import 'terms.dart';

/// The saved state is not one this reader can read: corrupt, truncated, or
/// of another format version.
class SavedStateFormatException implements Exception {
  final String message;
  SavedStateFormatException(this.message);
  @override
  String toString() => 'SavedStateFormatException: $message';
}

/// The four magic bytes, `GLPS`, and the format version.
const List<int> savedStateMagic = [0x47, 0x4C, 0x50, 0x53];
const int savedStateVersion = 1;

// Term tags.
const int _tVar = 0;
const int _tConst = 1;
const int _tStruct = 2;
const int _tMutualRef = 3;
const int _tModule = 4;

// Constant tags.
const int _cNil = 0;
const int _cInt = 1;
const int _cFloat = 2;
const int _cString = 3;
const int _cBool = 4;
const int _cNull = 5;
const int _cBytes = 6;

// Cell content tags.
const int _kNull = 0;
const int _kPointer = 1;
const int _kWriter = 2;
const int _kTerm = 3;

/// Bytes written in sequence.
class _Out {
  final BytesBuilder _b = BytesBuilder(copy: false);

  void u8(int v) => _b.addByte(v & 0xFF);

  /// Unsigned LEB128.
  void uint(int v) {
    if (v < 0) throw ArgumentError('uint of a negative number: $v');
    while (v >= 0x80) {
      _b.addByte((v & 0x7F) | 0x80);
      v >>= 7;
    }
    _b.addByte(v);
  }

  /// Zigzag LEB128.
  void sint(int v) => uint(v >= 0 ? v << 1 : ((-v) << 1) - 1);

  void f64(double v) {
    final d = ByteData(8)..setFloat64(0, v, Endian.little);
    _b.add(d.buffer.asUint8List());
  }

  void bytes(List<int> raw) {
    uint(raw.length);
    _b.add(raw);
  }

  void str(String s) => bytes(utf8.encode(s));

  Uint8List take() => _b.takeBytes();
}

/// Bytes read in sequence.
class _In {
  final Uint8List _d;
  int _o = 0;
  _In(this._d);

  bool get atEnd => _o >= _d.length;

  void _need(int n) {
    if (_o + n > _d.length) {
      throw SavedStateFormatException('the saved state ends early');
    }
  }

  int u8() {
    _need(1);
    return _d[_o++];
  }

  int uint() {
    var v = 0;
    var shift = 0;
    while (true) {
      final b = u8();
      v |= (b & 0x7F) << shift;
      if (b < 0x80) return v;
      shift += 7;
      if (shift > 63) {
        throw SavedStateFormatException('an integer runs past 64 bits');
      }
    }
  }

  int sint() {
    final z = uint();
    return (z & 1) == 0 ? z >> 1 : -((z + 1) >> 1);
  }

  double f64() {
    _need(8);
    final v = ByteData.sublistView(_d, _o, _o + 8).getFloat64(0, Endian.little);
    _o += 8;
    return v;
  }

  Uint8List bytes() {
    final n = uint();
    _need(n);
    final out = Uint8List.fromList(Uint8List.sublistView(_d, _o, _o + n));
    _o += n;
    return out;
  }

  String str() => utf8.decode(bytes());
}

/// Writes an agent's state.  Its owners write their parts in a fixed order
/// through the methods below ([u8] to [goalRef]), the roots; [finish] then
/// writes every cell, record, module constant and mutual reference those parts
/// reached, and returns the bytes.
class StateWriter {
  final _Out _roots = _Out();
  final _Out _cells = _Out();

  final Uint8List programIdentity;
  final String agentName;
  final Uint8List agentKey;

  /// The module constant of the agent's own program, written as a reference:
  /// at restart the program is compiled again, and its constant is the one the
  /// restarted engine holds, its compiled identity checked against this one's.
  final ModuleTerm? appModule;

  /// Cells reached and not yet written, and those reached at all.
  final List<HeapCell> _pending = [];
  final Set<HeapCell> _reached = {};

  final Map<SuspensionRecord, int> _records = Map.identity();
  final Map<ModuleTerm, int> _modules = Map.identity();
  final Map<MutualRefTerm, int> _mutualRefs = Map.identity();

  StateWriter({
    required this.programIdentity,
    required this.agentName,
    required this.agentKey,
    this.appModule,
  });

  void u8(int v) => _roots.u8(v);
  void uint(int v) => _roots.uint(v);
  void boolean(bool v) => _roots.u8(v ? 1 : 0);
  void str(String s) => _roots.str(s);
  void optStr(String? s) {
    boolean(s != null);
    if (s != null) str(s);
  }

  void bytes(List<int> raw) => _roots.bytes(raw);

  /// A cell, by its serial number; the cell is written in [finish].
  void cell(HeapCell c) => _cellRef(_roots, c);

  void optCell(HeapCell? c) {
    boolean(c != null);
    if (c != null) cell(c);
  }

  void term(Term t) => _term(_roots, t);

  void goalRef(GoalRef g) {
    _roots.uint(g.id);
    _roots.uint(g.pc);
  }

  /// A module constant, or none.
  void module(ModuleTerm? m) {
    boolean(m != null);
    if (m != null) _roots.uint(_moduleIndex(m));
  }

  void _cellRef(_Out o, HeapCell c) {
    o.uint(c.id);
    if (_reached.add(c)) _pending.add(c);
  }

  int _moduleIndex(ModuleTerm m) =>
      _modules.putIfAbsent(m, () => _modules.length);

  int _recordIndex(SuspensionRecord r) =>
      _records.putIfAbsent(r, () => _records.length);

  int _mutualRefIndex(MutualRefTerm m) {
    final known = _mutualRefs[m];
    if (known != null) return known;
    final i = _mutualRefs.length;
    _mutualRefs[m] = i;
    // Its tail writer is a cell like any other.
    if (_reached.add(m.currentWriterAddr)) _pending.add(m.currentWriterAddr);
    return i;
  }

  void _term(_Out o, Term t) {
    final stack = <Term>[t];
    while (stack.isNotEmpty) {
      final x = stack.removeLast();
      if (x is StructTerm) {
        o.u8(_tStruct);
        o.str(x.functor);
        o.uint(x.args.length);
        for (var i = x.args.length - 1; i >= 0; i--) {
          stack.add(x.args[i]);
        }
      } else if (x is VarRef) {
        o.u8(_tVar);
        _cellRef(o, x.addr);
      } else if (x is ConstTerm) {
        o.u8(_tConst);
        _const(o, x.value);
      } else if (x is MutualRefTerm) {
        o.u8(_tMutualRef);
        o.uint(_mutualRefIndex(x));
      } else if (x is ModuleTerm) {
        o.u8(_tModule);
        o.uint(_moduleIndex(x));
      } else {
        throw StateError('a term of kind ${x.runtimeType} has no saved form');
      }
    }
  }

  static void _const(_Out o, Object? v) {
    if (v is Nil) {
      o.u8(_cNil);
    } else if (v is int) {
      o.u8(_cInt);
      o.sint(v);
    } else if (v is double) {
      o.u8(_cFloat);
      o.f64(v);
    } else if (v is String) {
      o.u8(_cString);
      o.str(v);
    } else if (v is bool) {
      o.u8(_cBool);
      o.u8(v ? 1 : 0);
    } else if (v == null) {
      o.u8(_cNull);
    } else if (v is Uint8List) {
      o.u8(_cBytes);
      o.bytes(v);
    } else {
      throw StateError('a constant of kind ${v.runtimeType} has no saved form');
    }
  }

  void _cell(HeapCell c) {
    final o = _cells;
    o.uint(c.id);
    o.u8(c.tag.index);
    final paired = c.pairedReader;
    if (paired == null) {
      o.u8(0);
    } else {
      o.u8(1);
      _cellRef(o, paired);
    }
    final content = c.content;
    if (content == null) {
      o.u8(_kNull);
    } else if (content is Pointer) {
      o.u8(_kPointer);
      _cellRef(o, content.targetAddr);
    } else if (content is WriterContent) {
      o.u8(_kWriter);
      _cellRef(o, content.readerAddr);
      // The suspension list, head first, each node naming its shared record.
      final nodes = <int>[];
      for (var n = content.suspensions; n != null; n = n.next) {
        nodes.add(_recordIndex(n.record));
      }
      o.uint(nodes.length);
      for (final i in nodes) {
        o.uint(i);
      }
    } else if (content is Term) {
      o.u8(_kTerm);
      _term(o, content);
    } else {
      throw StateError('cell ${c.id} holds ${content.runtimeType}, which has '
          'no saved form');
    }
  }

  /// Write every cell the roots reached, and the tables; return the bytes.
  Uint8List finish() {
    var cellCount = 0;
    while (_pending.isNotEmpty) {
      _cell(_pending.removeLast());
      cellCount++;
    }
    final out = _Out();
    for (final b in savedStateMagic) {
      out.u8(b);
    }
    out.u8(savedStateVersion);
    out.bytes(programIdentity);
    out.str(agentName);
    out.bytes(agentKey);

    // Module constants: the agent's own program by reference, any other by
    // its artefact's bytes and name.
    final modules = _modules.keys.toList();
    out.uint(modules.length);
    for (final m in modules) {
      if (identical(m, appModule)) {
        out.u8(1);
        out.bytes(_artefactBytes(m));
      } else {
        if (m.declaredTypes != null || m.directory != null) {
          throw StateError('module ${m.name} was compiled by the engine and is '
              'not the agent\'s program; its declared types have no saved form');
        }
        out.u8(0);
        out.bytes(_artefactBytes(m));
        out.str(m.name);
      }
    }

    // Suspension records: the goal, or none where disarmed, and the PC.
    final records = _records.keys.toList();
    out.uint(records.length);
    for (final r in records) {
      final g = r.goalId;
      out.uint(g == null ? 0 : g + 1);
      out.uint(r.resumePC);
    }

    // Mutual references: their own id and their tail writer.
    final refs = _mutualRefs.keys.toList();
    out.uint(refs.length);
    for (final m in refs) {
      out.uint(m.id);
      out.uint(m.currentWriterAddr.id);
    }

    out.uint(cellCount);
    final cells = _cells.take();
    out.uint(cells.length);
    final head = out.take();
    final roots = _roots.take();
    final all = BytesBuilder(copy: false)
      ..add(head)
      ..add(cells)
      ..add(roots);
    return all.takeBytes();
  }

  static Uint8List _artefactBytes(ModuleTerm m) =>
      (m.artefact as Artefact).toBytes();
}

/// Reads an agent's state back: the header first ([programIdentity],
/// [agentName], [agentKey]); then, once [readTables] has made the cells, the
/// roots, in the order they were written, through the methods below.
class StateReader {
  final _In _in;

  late final Uint8List programIdentity;
  late final String agentName;
  late final Uint8List agentKey;

  final Map<int, HeapCell> _cellsById = {};
  final Set<int> _defined = {};
  late final List<SuspensionRecord> _records;
  late final List<ModuleTerm> _modules;
  late final List<MutualRefTerm> _mutualRefs;

  StateReader(Uint8List bytes) : _in = _In(bytes) {
    for (final b in savedStateMagic) {
      if (_in.u8() != b) {
        throw SavedStateFormatException('no saved agent state (bad magic)');
      }
    }
    final version = _in.u8();
    if (version != savedStateVersion) {
      throw SavedStateFormatException(
          'saved state version $version; this runtime reads $savedStateVersion');
    }
    programIdentity = _in.bytes();
    agentName = _in.str();
    agentKey = _in.bytes();
  }

  /// Read the tables and make every cell.  [appModule] is the restarted
  /// engine's module constant of the agent's program, which stands for the
  /// saved one; any other module constant is made again from its artefact's
  /// bytes.  Throws where the saved program's compiled identity is not
  /// [appModule]'s.
  void readTables({required ModuleTerm? appModule}) {
    final moduleCount = _in.uint();
    _modules = [];
    for (var i = 0; i < moduleCount; i++) {
      final isApp = _in.u8() == 1;
      final bytes = _in.bytes();
      if (isApp) {
        if (appModule == null) {
          throw StateError('the saved state holds the agent\'s program as a '
              'value, and the restarted engine has none');
        }
        if (!_equal(Artefact.compiledIdentityOf(bytes),
            Artefact.compiledIdentityOf(
                (appModule.artefact as Artefact).toBytes()))) {
          throw StateError('the saved state holds another compilation of the '
              'agent\'s program than the restarted engine compiled');
        }
        _modules.add(appModule);
      } else {
        final artefact = Artefact.fromBytes(bytes);
        _modules.add(ModuleTerm(artefact, name: _in.str()));
      }
    }

    final recordCount = _in.uint();
    _records = [];
    for (var i = 0; i < recordCount; i++) {
      final g = _in.uint();
      final pc = _in.uint();
      _records.add(SuspensionRecord(g == 0 ? null : g - 1, pc));
    }

    final refCount = _in.uint();
    _mutualRefs = [];
    for (var i = 0; i < refCount; i++) {
      final id = _in.uint();
      final writer = _cellOf(_in.uint());
      _mutualRefs.add(MutualRefTerm.restored(writer, id));
    }

    final cellCount = _in.uint();
    final cellsLength = _in.uint();
    final cellsEnd = _in._o + cellsLength;
    for (var i = 0; i < cellCount; i++) {
      _readCell();
    }
    if (_in._o != cellsEnd) {
      throw SavedStateFormatException('the cell table is not its stated length');
    }
    for (final id in _cellsById.keys) {
      if (!_defined.contains(id)) {
        throw SavedStateFormatException('cell $id is referred to and not saved');
      }
    }
  }

  HeapCell _cellOf(int id) =>
      _cellsById.putIfAbsent(id, () => HeapCell(null, CellTag.WrtTag, id));

  void _readCell() {
    final c = _cellOf(_in.uint());
    if (!_defined.add(c.id)) {
      throw SavedStateFormatException('cell ${c.id} is saved twice');
    }
    final tag = _in.u8();
    if (tag >= CellTag.values.length) {
      throw SavedStateFormatException('cell ${c.id} has tag $tag');
    }
    c.tag = CellTag.values[tag];
    if (_in.u8() == 1) c.pairedReader = _cellOf(_in.uint());
    switch (_in.u8()) {
      case _kNull:
        c.content = null;
      case _kPointer:
        c.content = Pointer(_cellOf(_in.uint()));
      case _kWriter:
        final reader = _cellOf(_in.uint());
        final n = _in.uint();
        SuspensionListNode? head;
        SuspensionListNode? last;
        for (var i = 0; i < n; i++) {
          final node = SuspensionListNode(_record(_in.uint()));
          if (head == null) {
            head = node;
          } else {
            last!.next = node;
          }
          last = node;
        }
        c.content = WriterContent(reader, head);
      case _kTerm:
        c.content = _term();
      default:
        throw SavedStateFormatException('cell ${c.id} has a bad content tag');
    }
  }

  SuspensionRecord _record(int i) {
    if (i >= _records.length) {
      throw SavedStateFormatException('suspension record $i is not saved');
    }
    return _records[i];
  }

  Term _term() {
    final frames = <(String, int, List<Term>)>[];
    while (true) {
      final tag = _in.u8();
      Term done;
      switch (tag) {
        case _tStruct:
          final f = _in.str();
          final n = _in.uint();
          if (n > 0) {
            frames.add((f, n, <Term>[]));
            continue;
          }
          done = StructTerm(f, <Term>[]);
        case _tVar:
          done = VarRef(_cellOf(_in.uint()));
        case _tConst:
          done = ConstTerm(_const());
        case _tMutualRef:
          final i = _in.uint();
          if (i >= _mutualRefs.length) {
            throw SavedStateFormatException('mutual reference $i is not saved');
          }
          done = _mutualRefs[i];
        case _tModule:
          final i = _in.uint();
          if (i >= _modules.length) {
            throw SavedStateFormatException('module constant $i is not saved');
          }
          done = _modules[i];
        default:
          throw SavedStateFormatException('a term has the bad tag $tag');
      }
      while (true) {
        if (frames.isEmpty) return done;
        final (f, n, args) = frames.last;
        args.add(done);
        if (args.length < n) break;
        frames.removeLast();
        done = StructTerm(f, args);
      }
    }
  }

  Object? _const() {
    switch (_in.u8()) {
      case _cNil:
        return nil;
      case _cInt:
        return _in.sint();
      case _cFloat:
        return _in.f64();
      case _cString:
        return _in.str();
      case _cBool:
        return _in.u8() == 1;
      case _cNull:
        return null;
      case _cBytes:
        return _in.bytes();
      default:
        throw SavedStateFormatException('a constant has a bad tag');
    }
  }

  int u8() => _in.u8();
  int uint() => _in.uint();
  bool boolean() => _in.u8() == 1;
  String str() => _in.str();
  String? optStr() => boolean() ? str() : null;
  Uint8List bytes() => _in.bytes();
  HeapCell cell() => _cellOf(_in.uint());
  HeapCell? optCell() => boolean() ? cell() : null;
  Term term() => _term();
  GoalRef goalRef() => GoalRef(_in.uint(), _in.uint());

  ModuleTerm? module() {
    if (!boolean()) return null;
    final i = _in.uint();
    if (i >= _modules.length) {
      throw SavedStateFormatException('module constant $i is not saved');
    }
    return _modules[i];
  }

  /// Every root has been read.
  void expectEnd() {
    if (!_in.atEnd) {
      throw SavedStateFormatException(
          'the saved state has ${_in._d.length - _in._o} bytes past its roots');
    }
  }

  static bool _equal(List<int> a, List<int> b) {
    if (a.length != b.length) return false;
    for (var i = 0; i < a.length; i++) {
      if (a[i] != b[i]) return false;
    }
    return true;
  }
}
