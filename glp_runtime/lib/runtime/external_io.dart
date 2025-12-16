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
class ExternalChannel {
  final String name;  // 'user' or 'net'

  // Input: Dart → GLP
  // Dart holds the writer, GLP receives the reader
  final int inputVarId;

  // Output: GLP → Dart
  // GLP holds the writer, Dart holds the reader
  final int outputVarId;

  ExternalChannel({
    required this.name,
    required this.inputVarId,
    required this.outputVarId,
  });

  @override
  String toString() => 'ExternalChannel($name, in=$inputVarId, out=$outputVarId)';
}

/// Factory function to create an ExternalChannel with fresh variables
ExternalChannel createExternalChannel(HeapFCP heap, String name) {
  // Create input stream variable (Dart=writer, GLP=reader)
  final inputVarId = heap.allocateFreshVar();

  // Create output stream variable (GLP=writer, Dart=reader)
  final outputVarId = heap.allocateFreshVar();

  return ExternalChannel(
    name: name,
    inputVarId: inputVarId,
    outputVarId: outputVarId,
  );
}

/// Build ch(In, Out) term for GLP
///
/// GLP receives:
/// - In? (reader) for input stream
/// - Out (writer) for output stream
Term buildChannelTerm(ExternalChannel channel) {
  return StructTerm('ch', [
    VarRef(channel.inputVarId, isReader: true),   // In? - GLP reads from this
    VarRef(channel.outputVarId, isReader: false), // Out - GLP writes to this
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
    // Allocate fresh variable for tail
    final tailId = heap.allocateFreshVar();

    // Build list cell: [term | tail] using '.' functor (GLP cons convention)
    final listCell = StructTerm('.', [term, VarRef(tailId, isReader: false)]);

    // Bind current writer to list cell - this may wake suspended goals
    final activations = heap.bindVariable(_currentWriterId, listCell);

    // Advance writer to tail for next injection
    _currentWriterId = tailId;

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

    // Register callback for when reader is bound
    heap.onBind(_currentReaderId, (Term value) {
      if (_closed) return;

      if (value is StructTerm && value.functor == '.') {
        // Got [Head | Tail] - cons cell
        final head = value.args[0];
        final tail = value.args[1];

        // Notify observer of term
        onTerm(head);

        // Continue observing tail
        if (tail is VarRef) {
          _currentReaderId = tail.varId;
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
        _currentReaderId = tail.varId;
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
    heap.removeBindCallback(_currentReaderId);
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

    // Create input injectors
    final userInput = InputInjector(heap, 'user', userChannel.inputVarId);
    final netInput = InputInjector(heap, 'net', netChannel.inputVarId);

    final context = AgentIOContext._(
      agentId: agentId,
      heap: heap,
      userChannel: userChannel,
      userInput: userInput,
      netChannel: netChannel,
      netInput: netInput,
    );

    // Create output observers that collect terms
    context.userOutput = OutputObserver(
      heap,
      'user',
      userChannel.outputVarId,
      (term) => context.userOutputTerms.add(term),
      () => context.userOutputClosed = true,
    );

    context.netOutput = OutputObserver(
      heap,
      'net',
      netChannel.outputVarId,
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
