/// External I/O for GLP - Phase 0 Implementation
/// Provides mechanism for Dart to inject input and observe output from GLP streams.
///
/// Based on docs/glp-io-spec.md
library;

import 'terms.dart';
import 'heap_fcp.dart';
import 'machine_state.dart'; // For GoalRef

/// An External Channel is a bidirectional connection between Dart and GLP.
///
/// Each channel has:
/// - Input stream: Dart injects terms, GLP reads them
/// - Output stream: GLP writes terms, Dart observes them
///
/// Per heap-pointer-architecture-spec.md Section 1.1:
/// "Heap navigation follows pointers explicitly rather than computing addresses
/// via arithmetic. There is no implicit relationship between adjacent heap addresses."
///
/// Therefore we store BOTH writer and reader addresses explicitly.
class ExternalChannel {
  final String name;  // 'user' or 'net'

  // Input: Dart → GLP
  // Dart holds the writer (to inject terms), GLP receives the reader
  final int inputWriterAddr;
  final int inputReaderAddr;

  // Output: GLP → Dart
  // GLP holds the writer (to produce terms), Dart holds the reader (to observe)
  final int outputWriterAddr;
  final int outputReaderAddr;

  ExternalChannel({
    required this.name,
    required this.inputWriterAddr,
    required this.inputReaderAddr,
    required this.outputWriterAddr,
    required this.outputReaderAddr,
  });

  @override
  String toString() => 'ExternalChannel($name, in=($inputWriterAddr,$inputReaderAddr), out=($outputWriterAddr,$outputReaderAddr))';
}

/// Factory function to create an ExternalChannel with fresh variables
ExternalChannel createExternalChannel(HeapFCP heap, String name) {
  // Create input stream variable (Dart=writer, GLP=reader)
  // allocateVariable returns (writerAddr, readerAddr) tuple
  final (inputWriterAddr, inputReaderAddr) = heap.allocateVariable();

  // Create output stream variable (GLP=writer, Dart=reader)
  final (outputWriterAddr, outputReaderAddr) = heap.allocateVariable();

  return ExternalChannel(
    name: name,
    inputWriterAddr: inputWriterAddr,
    inputReaderAddr: inputReaderAddr,
    outputWriterAddr: outputWriterAddr,
    outputReaderAddr: outputReaderAddr,
  );
}

/// Build ch(In, Out) term for GLP
///
/// GLP HEAD pattern: agent_init(Id, ch(UserIn, UserOut?), ...)
/// - UserIn (writer mode in HEAD) expects a READER to store
/// - UserOut? (reader mode in HEAD) expects a WRITER to bind
///
/// Per bytecode spec Section 8.1-8.2:
/// - Writer-mode HEAD position + unbound writer arg → WxW violation (FAIL)
/// - Writer-mode HEAD position + reader arg → stores reader in clause var (OK)
/// - Reader-mode HEAD position + writer arg → binds writer to HEAD's reader (OK)
///
/// Therefore: ch(Reader, Writer), NOT ch(Writer, Reader)
///
/// Per CGLP paper Definition 5.5: initial goal is agent(p, ch(_?, _), ch(_?, _))
/// where _? is reader and _ is writer.
Term buildChannelTerm(ExternalChannel channel) {
  // Use explicit reader/writer addresses - NO address arithmetic
  // Per heap-pointer-architecture-spec.md: "There is no implicit relationship
  // between adjacent heap addresses"
  return StructTerm('ch', [
    VarRef(channel.inputReaderAddr),   // In - READER (for writer-mode HEAD position)
    VarRef(channel.outputWriterAddr),  // Out - WRITER (for reader-mode HEAD position)
  ]);
}

/// Injects terms into a GLP input stream.
///
/// Dart holds the writer end of the input stream.
/// Each inject() call extends the stream: [term | newTail]
class InputInjector {
  final HeapFCP heap;
  final String channelName;
  int _currentWriterId;

  InputInjector(this.heap, this.channelName, int initialWriterId)
      : _currentWriterId = initialWriterId;

  /// Current writer variable ID (for debugging)
  int get currentWriterId => _currentWriterId;

  /// Inject a term into the input stream.
  ///
  /// Binds current writer to [term | newTail], advances writer to newTail.
  /// Returns list of goals that were woken up by the injection (should be enqueued).
  List<GoalRef> inject(Term term) {
    // Allocate fresh variable for tail (returns (writerAddr, readerAddr))
    final (tailWriterAddr, _) = heap.allocateVariable();

    // Build list cell: [term | tail] using '.' functor (GLP cons convention)
    // Tail is a writer (per FCP pattern, use readerForWriter() if reader needed)
    final listCell = StructTerm('.', [term, VarRef(tailWriterAddr)]);

    // Bind current writer to list cell - this may wake suspended goals
    final activations = heap.bindVariable(_currentWriterId, listCell);

    // Advance writer to tail for next injection
    _currentWriterId = tailWriterAddr;

    return activations;
  }

  /// Close the input stream (no more input).
  ///
  /// Binds current writer to empty list (nil).
  /// Returns list of goals that were woken up (should be enqueued).
  List<GoalRef> close() {
    return heap.bindVariable(_currentWriterId, ConstTerm('nil'));
  }
}

/// Observes terms written to a GLP output stream.
///
/// Dart holds the reader end of the output stream.
/// When GLP binds the writer to [term | newTail], the callback fires.
class OutputObserver {
  final HeapFCP heap;
  final String channelName;
  final void Function(Term) onTerm;
  final void Function() onClose;
  int _currentReaderId;
  bool _closed = false;

  OutputObserver(
    this.heap,
    this.channelName,
    int initialReaderId,
    this.onTerm,
    this.onClose,
  ) : _currentReaderId = initialReaderId {
    _observeNext();
  }

  /// Current reader variable ID (for debugging)
  int get currentReaderId => _currentReaderId;

  /// Whether the stream has been closed
  bool get isClosed => _closed;

  void _observeNext() {
    if (_closed) return;

    // Convert reader to writer per spec Section 7.1
    // onBind() expects writer address because bindings happen at the writer
    final writerAddr = heap.tryWriterForReader(_currentReaderId);
    if (writerAddr == null) {
      // Imported reader - no local writer to observe
      // This shouldn't happen for output streams, but handle gracefully
      return;
    }

    // Register callback for when writer is bound
    heap.onBind(writerAddr, (Term value) {
      if (_closed) return;

      if (value is StructTerm && value.functor == '.') {
        // Got [Head | Tail] - cons cell
        final head = value.args[0];
        final tail = value.args[1];

        // Notify observer of term
        onTerm(head);

        // Continue observing tail
        if (tail is VarRef) {
          _currentReaderId = tail.addr;
          _observeNext();
        } else if (tail is ConstTerm && tail.value == 'nil') {
          // Stream closed with []
          _closed = true;
          onClose();
        } else if (tail is StructTerm && tail.functor == '.') {
          // Nested cons - process recursively
          _processNestedCons(tail);
        }
      } else if (value is ConstTerm && value.value == 'nil') {
        // Empty list - stream closed
        _closed = true;
        onClose();
      }
    });
  }

  /// Process nested cons cells (when multiple terms bound at once)
  void _processNestedCons(StructTerm cons) {
    var current = cons;
    while (true) {
      final head = current.args[0];
      final tail = current.args[1];

      onTerm(head);

      if (tail is VarRef) {
        _currentReaderId = tail.addr;
        _observeNext();
        break;
      } else if (tail is ConstTerm && tail.value == 'nil') {
        _closed = true;
        onClose();
        break;
      } else if (tail is StructTerm && tail.functor == '.') {
        current = tail;
      } else {
        // Unexpected tail type
        break;
      }
    }
  }

  /// Stop observing (cleanup)
  void dispose() {
    _closed = true;
    // Convert reader to writer for callback removal (callbacks are keyed by writer)
    final writerAddr = heap.tryWriterForReader(_currentReaderId);
    if (writerAddr != null) {
      heap.removeBindCallback(writerAddr);
    }
  }
}

/// Context for an agent with both user and network channels.
///
/// Provides convenient access to all I/O components.
class AgentIOContext {
  final String agentId;
  final HeapFCP heap;

  // User channel (UI)
  final ExternalChannel userChannel;
  final InputInjector userInput;
  late final OutputObserver userOutput;

  // Network channel
  final ExternalChannel netChannel;
  final InputInjector netInput;
  late final OutputObserver netOutput;

  // Collected output for testing
  final List<Term> userOutputTerms = [];
  final List<Term> netOutputTerms = [];
  bool userOutputClosed = false;
  bool netOutputClosed = false;

  AgentIOContext._({
    required this.agentId,
    required this.heap,
    required this.userChannel,
    required this.userInput,
    required this.netChannel,
    required this.netInput,
  });

  /// Create an AgentIOContext with both channels set up.
  ///
  /// The output observers collect terms into lists for easy testing.
  factory AgentIOContext.create(HeapFCP heap, String agentId) {
    // Create channels
    final userChannel = createExternalChannel(heap, 'user');
    final netChannel = createExternalChannel(heap, 'net');

    // Create input injectors (Dart holds writer, injects via writer address)
    final userInput = InputInjector(heap, 'user', userChannel.inputWriterAddr);
    final netInput = InputInjector(heap, 'net', netChannel.inputWriterAddr);

    final context = AgentIOContext._(
      agentId: agentId,
      heap: heap,
      userChannel: userChannel,
      userInput: userInput,
      netChannel: netChannel,
      netInput: netInput,
    );

    // Create output observers that collect terms (Dart observes via reader address)
    context.userOutput = OutputObserver(
      heap,
      'user',
      userChannel.outputReaderAddr,
      (term) => context.userOutputTerms.add(term),
      () => context.userOutputClosed = true,
    );

    context.netOutput = OutputObserver(
      heap,
      'net',
      netChannel.outputReaderAddr,
      (term) => context.netOutputTerms.add(term),
      () => context.netOutputClosed = true,
    );

    return context;
  }

  /// Build the ch(UserIn, UserOut) term for GLP
  Term get userChannelTerm => buildChannelTerm(userChannel);

  /// Build the ch(NetIn, NetOut) term for GLP
  Term get netChannelTerm => buildChannelTerm(netChannel);

  /// Dispose all observers
  void dispose() {
    userOutput.dispose();
    netOutput.dispose();
  }

  @override
  String toString() => 'AgentIOContext($agentId)';
}
