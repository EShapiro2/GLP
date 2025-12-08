# AofGLP Code Example Inventory

This inventory catalogs all GLP code examples for "The Art of Grassroots Logic Programming" book.

**Status Legend:**
- `in_book` - Confirmed in book
- `candidate` - Available but not yet confirmed in book
- `not_in_book` - Supplementary material, not intended for book

---

## Part I: Foundations

### Chapter: introduction
*No code examples*

### Chapter: logic_programs
*Basic examples introducing GLP syntax*

### Chapter: glp_core
*Core unification and execution examples*

### Chapter: constants
*Constant handling examples*

---

## Part II: Programming with Streams

### Chapter: streams

#### producer_consumer.glp
**Path:** `streams/producer_consumer/producer_consumer.glp`
**Status:** `in_book`
**Goal:** `producer(Xs), consumer(Xs?, 0).`
**Cases:** Stream generation, suspension/resumption, tail recursion

```glp
%% producer_consumer.glp - Basic producer-consumer pattern
%% Source: GLP Papers
%%
%% Producer generates infinite stream [0,1,2,3,...] continuously.
%% Consumer accumulates running sum as it receives each value.
%% In a fair run, each consumer goal must eventually be reduced.
%% As producer extends stream, fairness ensures consumer makes progress,
%% guaranteeing every number produced is eventually consumed.

producer(Xs) :- producer(Xs?,0).

producer([N?|Xs],N) :- N1 := N? + 1, producer(Xs,N1?).

consumer([X|Xs],Sum) :- Sum1 := Sum? + X?, consumer(Xs?,Sum1?).
```

---

#### observer.glp
**Path:** `streams/observation/observer.glp`
**Status:** `candidate`
**Goal:** `observe(In?, Out, Audit).`
**Cases:** Bidirectional observation, ground checking

```glp
%% observer.glp - Stream observation without consumption
%% Source: GLP Papers (Program: Concurrent Observer)
%%
%% For non-ground data requiring observation without consumption.
%% Forwards communication bidirectionally while producing a
%% replicable audit stream.

observe(X?, Y, Z) :- observe(Y?, X, Z).
observe(X, X?, X?) :- ground(X) | true.
observe(Xs, [Y1?|Ys1?], [Y2?|Ys2?]) :-
    Xs? = [X|Xs1] |
    observe(X?, Y1, Y2),
    observe(Xs1?, Ys1, Ys2).
```

---

### Chapter: buffered_communication

#### fair_merge.glp
**Path:** `merge/binary/fair_merge.glp`
**Status:** `in_book`
**Goal:** `merge([1,2,3], [a,b,c], Zs).`
**Cases:** Binary merge, clause alternation for fairness

```glp
%% fair_merge.glp - Fair binary stream merger
%% Source: GLP Papers (Program: GLP Fair Stream Merger)
%%
%% Merges two input streams into one output stream.
%% Alternates priority between streams for fairness.

merge([X|Xs],Ys,[X?|Zs?]) :- merge(Ys?,Xs?,Zs). % output from first stream
merge(Xs,[Y|Ys],[Y?|Zs?]) :- merge(Xs?,Ys?,Zs). % output from second stream
merge([],[],[]).                                % terminate on empty streams
```

---

#### mwm.glp
**Path:** `merge/mwm/mwm.glp`
**Status:** `candidate`
**Goal:** `mwm([stream([1,2,3]), stream([a,b])], Out).`
**Cases:** Multiway merge, mutual references, termination detection

```glp
% mwm/2 - Multiway merge with constant delay
% In: stream of stream(Xs) terms, may include merge(NewStream)
% Out: merged output stream
%
% Uses MutualRef for O(1) stream append.
% Uses short-circuit L/R pairs for termination detection.
% allocate_mutual_reference/2 is a kernel predicate called directly.

mwm(In, Out?) :-
    allocate_mutual_reference(Ref, Out),
    mwm_main(In?, Ref?).

% Intermediate predicate with guard to allow multiple Ref? occurrences
mwm_main(In, Ref) :-
    is_mutual_ref(Ref?) |
    mwm1(In?, Ref?, done, Done),
    close_when_done(Done?, Ref?).

mwm1([stream(Xs)|Streams], Ref, L, R?) :-
    is_mutual_ref(Ref?) |
    mwm_copy(Xs?, Ref?, L?, M),
    mwm1(Streams?, Ref?, M?, R).

mwm1([merge(NewIn)|Streams], Ref, L, R?) :-
    is_mutual_ref(Ref?) |
    mwm1(NewIn?, Ref?, L?, M),
    mwm1(Streams?, Ref?, M?, R).

mwm1([], _, L, L?).

mwm_copy([X|Xs], Ref, L, R?) :-
    is_mutual_ref(Ref?) |
    stream_append(X?, Ref?, Ref1),
    mwm_copy(Xs?, Ref1?, L?, R).

mwm_copy([], _, L, L?).

close_when_done(done, Ref) :-
    is_mutual_ref(Ref?) |
    close_mutual_reference(Ref?).

% stream_append/3 - Append value to stream via MutualRef
stream_append(Value, RefIn, RefOut?) :-
    is_mutual_ref(RefIn?) |
    kernel_stream_append(RefIn?, Value?, RefOut).

% close_mutual_reference/1 - Close stream by binding tail to []
close_mutual_reference(Ref) :-
    is_mutual_ref(Ref?) |
    kernel_close_mutual_reference(Ref?).
```

---

#### bounded_buffer.glp
**Path:** `protocols/bounded_buffer/bounded_buffer.glp`
**Status:** `in_book`
**Goal:** `sq_num_buffered(10, Squares, 3).`
**Cases:** Difference lists, bounded communication, flow control

```glp
%% bounded_buffer.glp - Bounded buffer stream communication
%% Source: CP Collected Papers, Ch 18 (Takeuchi & Furukawa)
%%
%% Bounded buffer communication limits sender when buffer is full.
%% The receiver controls buffer expansion by creating new slots.

%% Unbounded buffer send/receive (standard stream communication)
send_unbounded(Msg, [Msg|NStrm], NStrm).
receive_unbounded(Msg, [Msg|NStrm?], NStrm).

%% Bounded buffer send/receive
%% Buffer is represented as difference-list: Head--Tail
send(Msg, [Msg|NBuf?], NBuf).
receive(Msg, [Msg|NBuf]--[NSlot|NTail?], NBuf--NTail).

close([end_of_stream|_]).
closed(end_of_stream).

%% Open a buffer of given size
open(0, X--X).
open(N, [_|Y]--Z) :- N > 0 | N1 := N? - 1, open(N1?, Y--Z).

%% Example: Buffered stream of squares
sq_num_buffered(N, Ss, Size) :-
    open(Size?, Buf--Tail),
    integers(1, N?, Buf?),
    square(Buf?--Tail, Ss).

integers(I, N, Buf) :-
    I =< N |
    J := I? + 1,
    send(I?, Buf?, NBuf),
    integers(J?, N?, NBuf?).
integers(I, N, Buf) :-
    I > N |
    close(Buf?).

square(Buf, Ss) :-
    receive(I, Buf?, NBuf),
    square2(I?, NBuf?, Ss).

square2(I, _, []) :- closed(I?) | true.
square2(I, Buf, [I2|Ss?]) :-
    number(I?) |
    I2 := I? * I?,
    square(Buf?, Ss).
```

---

### Chapter: monitors

#### monitor.glp
**Path:** `monitors/basic/monitor.glp`
**Status:** `in_book`
**Goal:** `merge(Client1?, Client2?, Reqs), monitor(Reqs?).`
**Cases:** Stateful service, incomplete messages, bidirectional communication

```glp
%% monitor.glp - Concurrent monitor (accumulator)
%% Source: GLP Papers (Program: Concurrent Monitor)
%%
%% A stateful service handling add, subtract, and value requests.
%% Demonstrates incomplete messages for bidirectional communication.
%% value(V) request: monitor binds response variable V to current sum.

monitor(Reqs) :- monitor(Reqs?,0).

monitor([add(N)|Reqs],Sum) :-
    Sum1 := Sum? + N?, monitor(Reqs?,Sum1?).
monitor([subtract(N)|Reqs],Sum) :-
    Sum1 := Sum? - N?, monitor(Reqs?,Sum1?).
monitor([value(V)|Reqs],Sum) :-
    ground(Sum?) | V = Sum?, monitor(Reqs?,Sum?).
monitor([],_).

%% Example initial goal:
%% client1(Xs), client2(Ys), merge(Xs?,Ys?,Zs), monitor(Zs?).
```

---

#### observed_monitor.glp
**Path:** `monitors/observer/observed_monitor.glp`
**Status:** `candidate`
**Goal:** `play_accum(VA, VB, VC, VD, Log).`
**Cases:** Type-aware observation, duplicate for incomplete messages, wait guards

```glp
%% observed_monitor.glp - Type/mode-aware observer for monitor streams
%% Demonstrates observation of incomplete messages with duplicate

% Accumulator monitor
monitor(Reqs) :- monitor_loop(Reqs?, 0).

monitor_loop([add(N)|Reqs], Sum) :-
    Sum1 := Sum? + N?, monitor_loop(Reqs?, Sum1?).
monitor_loop([subtract(N)|Reqs], Sum) :-
    Sum1 := Sum? - N?, monitor_loop(Reqs?, Sum1?).
monitor_loop([value(V?)|Reqs], Sum) :-
    ground(Sum?) | V = Sum?, monitor_loop(Reqs?, Sum?).
monitor_loop([], _).

% Type/mode-aware observer for accumulator
observe_accum([add(N)|In], [add(N?)|Out?], [add(N?)|Log?]) :-
    ground(N?) | observe_accum(In?, Out, Log).
observe_accum([subtract(N)|In], [subtract(N?)|Out?], [subtract(N?)|Log?]) :-
    ground(N?) | observe_accum(In?, Out, Log).
observe_accum([value(V1?)|In], [value(V)|Out?], [value(V2?)|Log?]) :-
    duplicate(V?, V1, V2),
    observe_accum(In?, Out, Log).
observe_accum([], [], []).

% Duplicate: waits for X to be ground, then binds both outputs
duplicate(X, X?, X?) :- ground(X?) | true.

% Fair merge
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
merge([], [], []).

% Alice: immediate, adds and queries
alice(V, [add(10), add(5), value(V?)|Xs?]) :- alice_done(Xs).
alice_done([]).

% Bob: delayed 50ms, subtracts and queries
bob(V, Ys?) :-
    wait(50) |
    Ys = [subtract(3), value(V?)|Ys1?],
    bob_done(Ys1).
bob_done([]).

% Carol: delayed 100ms, adds and queries
carol(V, Zs?) :-
    wait(100) |
    Zs = [add(20), value(V?)|Zs1?],
    carol_done(Zs1).
carol_done([]).

% Diana: delayed 150ms, just queries
diana(V, Ws?) :-
    wait(150) |
    Ws = [value(V?)|Ws1?],
    diana_done(Ws1).
diana_done([]).

% Test harness: 4 actors, observed monitor
play_accum(VA?, VB?, VC?, VD?, Log?) :-
    alice(VA, As),
    bob(VB, Bs),
    carol(VC, Cs),
    diana(VD, Ds),
    merge(As?, Bs?, AB),
    merge(Cs?, Ds?, CD),
    merge(AB?, CD?, All),
    observe_accum(All?, Reqs, Log),
    monitor(Reqs?).
```

---

## Part III: Recursive Programming

### Chapter: lists

#### append.glp
**Path:** `lists/append/append.glp`
**Status:** `in_book`
**Goal:** `append([1,2,3], [4,5], Zs).`
**Cases:** List construction, difference list pattern

```glp
%% append.glp - List concatenation
%% Source: Standard GLP list operations

append([],Ys?,Ys?).
append([X?|Xs],Ys,[X?|Zs?]) :- append(Xs?,Ys?,Zs).

%% Metainterpreter support
reduce(append([],Ys?,Ys?), true).
reduce(append([X?|Xs],Ys,[X?|Zs?]), append(Xs?,Ys?,Zs)).
```

---

#### reverse.glp
**Path:** `lists/reverse/reverse.glp`
**Status:** `in_book`
**Goal:** `reverse([1,2,3], Rs).`
**Cases:** Accumulator pattern, tail recursion

```glp
%% reverse.glp - List reversal
%% Source: Standard GLP list operations

reverse(Xs,Ys) :- reverse(Xs?,[],Ys).
reverse([],Acc?,Acc?).
reverse([X?|Xs],Acc,Ys?) :- reverse(Xs?,[X?|Acc?],Ys).

%% Metainterpreter support
reduce(reverse(Xs,Ys), reverse(Xs?,[],Ys)).
reduce(reverse([],Acc?,Acc?), true).
reduce(reverse([X?|Xs],Acc,Ys?), reverse(Xs?,[X?|Acc?],Ys)).
```

---

#### length.glp
**Path:** `lists/length/length.glp`
**Status:** `in_book`
**Goal:** `length([a,b,c,d], N).`
**Cases:** Accumulator, arithmetic assignment

```glp
%% length.glp - List Length
%% Source: Standard GLP list operations

length(Xs, N?) :- length(Xs?, 0, N).
length([], Acc?, Acc?).
length([_|Xs], Acc, N?) :- Acc1 := Acc? + 1, length(Xs?, Acc1?, N).

%% Metainterpreter support
reduce(length(Xs, N?), length(Xs?, 0, N)).
reduce(length([], Acc?, Acc?), true).
reduce(length([_|Xs], Acc, N?), (Acc1 := Acc? + 1, length(Xs?, Acc1?, N))).
reduce((X? := T), true) :- X := T? | true.
```

---

#### member.glp
**Path:** `lists/member/member.glp`
**Status:** `in_book`
**Goal:** `member(b, [a,b,c]).`
**Cases:** Simple recursion, pattern matching

```glp
%% member.glp - List Membership
%% Source: Standard GLP list operations

member(X, [X|_]).
member(X, [_|Xs]) :- member(X?, Xs?).

%% Metainterpreter support
reduce(member(X, [X|_]), true).
reduce(member(X, [_|Xs]), member(X?, Xs?)).
```

---

### Chapter: sorting

#### quicksort.glp
**Path:** `sorting/quicksort/quicksort.glp`
**Status:** `in_book`
**Goal:** `quicksort([3,1,4,1,5,9,2,6], Sorted).`
**Cases:** Divide-and-conquer, partition with guards, difference lists

```glp
%% quicksort.glp - Quicksort in GLP
%% Source: Standard GLP sorting

quicksort(Unsorted, Sorted?) :- qsort(Unsorted?, Sorted, []).

qsort([X|Unsorted], Sorted?, Rest) :-
    number(X?) |
    partition(Unsorted?, X?, Smaller, Larger),
    qsort(Smaller?, Sorted, [X?|Sorted1?]),
    qsort(Larger?, Sorted1, Rest?).
qsort([], Rest?, Rest).

partition([X|Xs], A, Smaller?, [X?|Larger?]) :-
    A? < X? | partition(Xs?, A?, Smaller, Larger).
partition([X|Xs], A, [X?|Smaller?], Larger?) :-
    A? >= X? | partition(Xs?, A?, Smaller, Larger).
partition([], A, [], []) :- number(A?) | true.

%% Metainterpreter support
reduce(quicksort(Unsorted, Sorted?), qsort(Unsorted?, Sorted, [])).
reduce(qsort([X|Unsorted], Sorted?, Rest),
       (partition(Unsorted?, X?, Smaller, Larger),
        qsort(Smaller?, Sorted, [X?|Sorted1?]),
        qsort(Larger?, Sorted1, Rest?))) :- number(X?) | true.
reduce(qsort([], Rest?, Rest), true).
reduce(partition([X|Xs], A, Smaller?, [X?|Larger?]),
       partition(Xs?, A?, Smaller, Larger)) :- A? < X? | true.
reduce(partition([X|Xs], A, [X?|Smaller?], Larger?),
       partition(Xs?, A?, Smaller, Larger)) :- A? >= X? | true.
reduce(partition([], A, [], []), true) :- number(A?) | true.
```

---

#### merge_sort.glp
**Path:** `sorting/merge_sort/merge_sort.glp`
**Status:** `in_book`
**Goal:** `merge_sort([3,1,4,1,5,9,2,6], Sorted).`
**Cases:** Divide-and-conquer, split, merge with guards

```glp
%% merge_sort.glp - Merge Sort in GLP
%% Source: Standard GLP sorting

merge_sort([], []).
merge_sort([X], [X?]).
merge_sort(Xs, Sorted?) :-
    Xs? = [_,_|_] |
    split(Xs?, Left, Right),
    merge_sort(Left?, SortedLeft),
    merge_sort(Right?, SortedRight),
    merge_sorted(SortedLeft?, SortedRight?, Sorted).

split([], [], []).
split([X], [X?], []).
split([X,Y|Xs], [X?|Left?], [Y?|Right?]) :- split(Xs?, Left, Right).

merge_sorted([], Ys?, Ys?).
merge_sorted(Xs?, [], Xs?).
merge_sorted([X|Xs], [Y|Ys], [X?|Zs?]) :-
    X? =< Y? | merge_sorted(Xs?, [Y?|Ys?], Zs).
merge_sorted([X|Xs], [Y|Ys], [Y?|Zs?]) :-
    X? > Y? | merge_sorted([X?|Xs?], Ys?, Zs).
```

---

### Chapter: arithmetic

#### factorial.glp
**Path:** `arithmetic/factorial/factorial.glp`
**Status:** `in_book`
**Goal:** `factorial(5, F).`
**Cases:** Arithmetic guards, sequential dependencies

```glp
%% factorial.glp - Factorial in GLP
%% Source: Standard GLP arithmetic

factorial(0, 1).
factorial(1, 1).
factorial(N, F?) :-
    N? > 1 |
    N1 := N? - 1,
    factorial(N1?, F1),
    F := N? * F1?.

%% Metainterpreter support
reduce(factorial(0, 1), true).
reduce(factorial(1, 1), true).
reduce(factorial(N, F?), (N1 := N? - 1, factorial(N1?, F1), F := N? * F1?)) :- N? > 1 | true.
reduce((X? := T), true) :- X := T? | true.
```

---

#### fibonacci.glp
**Path:** `arithmetic/fibonacci/fibonacci.glp`
**Status:** `in_book`
**Goal:** `fib(10, F).`
**Cases:** True recursion (multiple calls), concurrent computation

```glp
%% fibonacci.glp - Fibonacci in GLP
%% Source: Standard GLP arithmetic

fib(0, 0).
fib(1, 1).
fib(N, F?) :-
    N? > 1 |
    N1 := N? - 1,
    N2 := N? - 2,
    fib(N1?, F1),
    fib(N2?, F2),
    F := F1? + F2?.

%% Metainterpreter support
reduce(fib(0, 0), true).
reduce(fib(1, 1), true).
reduce(fib(N, F?), (N1 := N? - 1, N2 := N? - 2, fib(N1?, F1), fib(N2?, F2), F := F1? + F2?)) :- N? > 1 | true.
reduce((X? := T), true) :- X := T? | true.
```

---

#### recursive.glp (Combined Examples)
**Path:** `book_examples/recursive/recursive.glp`
**Status:** `in_book`
**Goal:** Various (mergesort, quicksort, fib, flatten)
**Cases:** All recursive patterns in one file

```glp
%% recursive.glp - Recursive programming examples
%% True recursion: multiple recursive calls spawn concurrent processes

%% ===== MERGESORT =====

mergesort([], []).
mergesort([X], [X?]).
mergesort(Xs, Sorted?) :-
    split(Xs?, Left, Right),
    mergesort(Left?, SortedL),
    mergesort(Right?, SortedR),
    merge_sorted(SortedL?, SortedR?, Sorted).

split([], [], []).
split([X], [X?], []).
split([X,Y|Xs], [X?|Left?], [Y?|Right?]) :- split(Xs?, Left, Right).

merge_sorted([], Ys, Ys?).
merge_sorted(Xs, [], Xs?).
merge_sorted([X|Xs], [Y|Ys], [X?|Zs?]) :-
    X? =< Y? |
    merge_sorted(Xs?, [Y?|Ys?], Zs).
merge_sorted([X|Xs], [Y|Ys], [Y?|Zs?]) :-
    Y? < X? |
    merge_sorted([X?|Xs?], Ys?, Zs).

%% ===== QUICKSORT =====

quicksort(Unsorted, Sorted?) :- qsort(Unsorted?, Sorted, []).

qsort([X|Unsorted], Sorted?, Rest) :-
    number(X?) |
    partition(Unsorted?, X?, Smaller, Larger),
    qsort(Smaller?, Sorted, [X?|Sorted1?]),
    qsort(Larger?, Sorted1, Rest?).
qsort([], Rest?, Rest).

partition([X|Xs], A, Smaller?, [X?|Larger?]) :-
    A? < X? | partition(Xs?, A?, Smaller, Larger).
partition([X|Xs], A, [X?|Smaller?], Larger?) :-
    A? >= X? | partition(Xs?, A?, Smaller, Larger).
partition([], A, [], []) :- number(A?) | true.

%% ===== FIBONACCI =====

fib(0, 0).
fib(1, 1).
fib(N, F?) :-
    N? > 1 |
    N1 := N? - 1,
    N2 := N? - 2,
    fib(N1?, F1),
    fib(N2?, F2),
    F := F1? + F2?.

%% ===== FLATTEN =====

flatten(Xs, Ys?) :- flatten_dl(Xs?, Ys, []).

flatten_dl([], Front?, Front).
flatten_dl([X|Xs], Front?, Back) :-
    ground(X?), is_list(X?) |
    flatten_dl(X?, Front, Mid?),
    flatten_dl(Xs?, Mid, Back?).
flatten_dl([X|Xs], [X?|Front?], Back) :-
    otherwise |
    flatten_dl(Xs?, Front, Back?).
```

---

## Part IV: Objects and Processes

### Chapter: objects

#### counter.glp
**Path:** `objects/basics/counter.glp`
**Status:** `in_book`
**Goal:** `counter([up,up,show(V),down,show(V2)], 0).`
**Cases:** Stateful object, message stream, state threading

```glp
%% counter.glp - Counter object example
%% Source: CP Collected Papers, Ch 29 (Shapiro & Takeuchi)
%%
%% Simple stateful object demonstrating OOP in logic programming.
%% Responds to clear, up, down, show messages via input stream.

counter([clear|S?], _) :-
    counter(S?, 0).
counter([up|S?], State) :-
    NewState := State? + 1,
    counter(S?, NewState?).
counter([down|S?], State) :-
    NewState := State? - 1,
    counter(S?, NewState?).
counter([show(State?)|S?], State) :-
    counter(S?, State?).
counter([], _).

%% Usage example:
%% terminal(X), use_counter(X?, C1), counter(C1?, 0)

%% use_counter receives commands from terminal and passes to counter
use_counter([show(Val)|Input?], [show(Val)|Command?]) :-
    use_counter(Input?, Command),
    wait_write(Val?).
use_counter([X|Input?], [X|Command?]) :-
    X =\= show(_) |
    use_counter(Input?, Command).
use_counter([], []).

wait_write(X) :- known(X?) | write(X?).
```

---

### Chapter: inheritance

#### frame.glp
**Path:** `objects/inheritance/frame.glp`
**Status:** `in_book`
**Goal:** `create_frame(Msgs, params(10,20,100,80)).`
**Cases:** Filter pattern, delegation, message interception

```glp
%% frame.glp - Window frame (subclass of rectangular_area)
%% Source: CP Collected Papers, Ch 29 (Shapiro & Takeuchi)
%%
%% A rectangular area with four border lines.
%% Demonstrates filter pattern for inheritance.

create_frame(M, Parameters) :-
    rectangular_area(M1?, Parameters?),
    frame(M?, M1).

frame([draw|M?], [ask(Parameters)|M1?]) :-
    draw_lines(Parameters?),
    frame(M?, M1).

frame([refresh|M?], [clear|M1?]) :-
    frame([draw|M?], M1).

frame([X|M?], [X|M1?]) :-
    X =\= draw, X =\= refresh |
    frame(M?, M1).

%% draw_lines/1 draws four border lines
```

---

## Part V: Metaprogramming

### Chapter: plain_meta

#### plain_meta.glp
**Path:** `meta/plain/plain_meta.glp`
**Status:** `in_book`
**Goal:** `run(merge([1,2],[3,4],Xs)).`
**Cases:** Basic metainterpreter, reduce/2 encoding

```glp
%% plain_meta.glp - Plain GLP metainterpreter
%% Source: GLP Papers (Program: GLP plain metainterpreter)
%%
%% Basic metainterpreter using reduce/2 to encode program clauses.
%% If a goal unifies with reduce's first argument, the body is
%% returned in the second argument.

run(true).  % halt
run((A,B)) :- run(A?), run(B?). % fork
run(A) :- known(A) | reduce(A?,B), run(B?). %  reduce

%% Example: reduce encoding of merge
reduce(merge([X|Xs],Ys,[X?|Zs?]),merge(Xs?,Ys?,Zs)).
reduce(merge(Xs,[Y|Ys],[Y?|Zs?]),merge(Xs?,Ys?,Zs)).
reduce(merge([],[],[]),true).

%% Example initial goal:
%% run((merge([1,2,3],[4,5],Xs), merge([a,b],[c,d,e],Ys), merge(Xs?,Ys?,Zs))).
```

---

#### tracing_meta.glp
**Path:** `meta/tracing/tracing_meta.glp`
**Status:** `in_book`
**Goal:** `run(Goal, Trace).`
**Cases:** Execution tracing, timestamped reduction

```glp
%% tracing_meta.glp - Tracing GLP metainterpreter
%% Source: GLP Papers (Program: GLP a tracing metainterpreter)
%%
%% Produces a trace of the run for single-stepping through the same
%% run, making the same nondeterministic scheduling choices.
%%
%% Assumes each program clause A :- D | B is represented by a unit clause:
%%   reduce(A,B,I) :- G | true.
%% where I is the serial number of the clause in the program.

run(true,true).  % halt
run((A,B),(TA?,TB?)) :- run(A?,TA), run(B?,TB). % fork
run(A,((I?:Time?):-TB?)) :- known(A) |
    time(Time), reduce(A?,B,I), run(B?, TB).
```

---

### Chapter: enhanced_meta
*Enhanced metainterpreters*

### Chapter: debugging

#### debugger_meta.glp
**Path:** `meta/debugging/debugger_meta.glp`
**Status:** `in_book`
**Goal:** `reduce(Program, Goal, 5, Tree).`
**Cases:** Budget-limited reduction, debug/raw modes, execution tree

```glp
%% debugger_meta.glp - Parallel debugger metainterpreter
%% Source: CP Collected Papers, Ch 25 (Safra & Shapiro)
%%
%% Builds execution tree while simulating program.
%% Processes run in "control" mode with limited reductions,
%% or "raw" mode without interpretation.

reduce(P, true, _, halted).                      %% halt
reduce(P, (A, B), Time, (Ta?, Tb?)) :-           %% fork
    reduce(P?, A?, Time?, Ta),
    reduce(P?, B?, Time?, Tb).
reduce(P, G, Time, (G? :- Tbody?)) :-            %% reduce (with budget)
    Time > 0,
    New_t := Time? - 1,
    G =\= true, G =\= (_, _),
    clause(G?, P?, Body) |
    reduce(P?, Body?, New_t?, Tbody).
reduce(P, G, 0, what_now(Mode)) :-               %% wait for command
    G =\= true, G =\= (_, _) |
    next(P?, G?, Mode?).

next(P, G, debug(Time, Tree)) :-                 %% debug mode
    reduce(P?, G?, Time?, Tree).
next(_, G, raw) :- G?.                           %% raw mode - no interpretation

%% Usage:
%% reduce(Program, Goal, N, Tree)
%% N = number of reductions before waiting
%% Tree = execution tree being built
%% When N reaches 0, waits for Mode:
%%   - debug(M, T) continues with M reductions, T gets subtree
%%   - raw executes Goal directly without interpretation
```

---

## Part VI: Grassroots Protocols

### Chapter: social_graph

#### agent.glp
**Path:** `social_graph/agent/agent.glp`
**Status:** `in_book`
**Goal:** `agent(alice, ChUser, ChNet).`
**Cases:** Channel initialization, stream merging, agent setup

```glp
%% agent.glp - Social Graph Agent Initialization
%% Source: GLP Papers (Program: Social Graph Initialization)
%%
%% Each agent begins with goal agent(Id, ChUser, ChNet) where Id is the
%% agent's unique identifier, ChUser provides bidirectional communication
%% with the user interface, and ChNet connects to the network.

agent(Id, ChUser, ChNet) :-
    ChUser = ch(UserIn, UserOut), ChNet = ch(NetIn, NetOut) |
    merge(UserIn?, NetIn?, In),
    social_graph(Id?, In?, [(user, UserOut), (net, NetOut)]).

%% The initialization extracts input and output streams from user and network
%% channels, merges inputs into unified stream In, and stores output streams
%% in initial friends list with special identifiers "user" and "net".
```

---

### Chapter: befriending

#### cold_call.glp
**Path:** `social_graph/cold_call/cold_call.glp`
**Status:** `in_book`
**Goal:** `social_graph(alice, Messages, Friends).`
**Cases:** Protocol state machine, attestation, inject pattern

```glp
%% cold_call.glp - Cold Call Befriending Protocol
%% Source: GLP Papers (Program: Social Graph Cold-Call Befriending Protocol)
%%
%% Enables agents to establish friendship without prior shared variables.
%% Four phases: user initiation, offer transmission, user consultation,
%% channel establishment.

% Process user request to connect (self-introduction)
social_graph(Id, [msg(user, Id, connect(Target))|In], Fs) :-
    ground(Id), ground(Target) |
    lookup_send(net, msg(Id, Target, intro(Id?, Id?, Resp)), Fs?, Fs1),
    inject(Resp?, msg(Target, Id, response(Resp)), In?, In1),
    social_graph(Id, In1?, Fs1?).

% Process received self-introduction
social_graph(Id, [msg(From, Id, intro(From, From, Resp))|In], Fs) :-
    ground(Id), attestation(intro(From, From, Resp), att(From, _)) |
    lookup_send(user, msg(agent, user, befriend(From?, Resp)), Fs?, Fs1),
    social_graph(Id, In?, Fs1?).

% Process user decision on received introduction
social_graph(Id, [msg(user, Id, decision(Dec, From, Resp?))|In], Fs) :-
    ground(Id) |
    bind_response(Dec?, From?, Resp, Fs?, Fs1, In?, In1),
    social_graph(Id, In1?, Fs1?).

% Process response to sent introduction
social_graph(Id, [msg(From, Id, response(Resp))|In], Fs) :-
    ground(Id) |
    handle_response(Resp?, From?, Fs?, Fs1, In?, In1),
    social_graph(Id, In1?, Fs1?).

% Application message handling
social_graph(Id, [msg(From, To, Content)|In], Fs) :-
    ground(Id), otherwise |
    % Forward to application layer
    social_graph(Id, In?, Fs?).

%% inject defers insertion until response variable becomes bound
inject(X,Y,Ys,[Y?|Ys?]) :- known(X) | true.
inject(X,Y,[Y1|Ys],[Y1?|Ys1?]) :- unknown(X) | inject(X?,Y?,Ys?,Ys1).
```

---

### Chapter: networking

#### direct_messaging.glp
**Path:** `networking/direct_messaging/direct_messaging.glp`
**Status:** `in_book`
**Goal:** *Part of agent protocol*
**Cases:** Channel establishment, attestation verification

```glp
%% direct_messaging.glp - Direct Messaging Channel Establishment
%% Source: GLP Papers (Program: Direct Messaging Channel Establishment)
%%
%% Establishes dedicated conversation channels between friends,
%% separate from protocol control channels.

% Modified establishment for direct messaging
% Secure version - verifies DM channel attestation
establish(yes, From, Resp, Fs, Fs1, In, In1) :-
    new_channel(ch(FIn, FOut), FCh),
    new_channel(ch(DMIn, DMOut), DMCh),
    Resp = accept(FCh, DMCh),
    attestation(DMCh, att(From, _)) |  % Verify DM channel from authenticated friend
    handle_friend(From?, FIn?, FOut?, DMIn?, DMOut?, Fs?, Fs1, In?, In1).

handle_friend(From, FIn, FOut, DMIn, DMOut, Fs,
             [(From, FOut), (dm(From), DMOut)|Fs], In, In1) :-
    tag_stream(From?, FIn?, Tagged),
    merge(In?, Tagged?, In1),
    forward_to_app(dm_channel(From?, DMIn?)).

%% The protocol maintains separation between control and messaging channels.
%% Each DM message carries attestation for non-repudiation.
```

---

### Chapter: security

#### attestation_guards.glp
**Path:** `security/attestation/attestation_guards.glp`
**Status:** `in_book`
**Goal:** `process_message(Msg, Result).`
**Cases:** Security guards, attestation checking, module identity

```glp
%% attestation_guards.glp - Security Guard Predicates
%% Source: GLP Papers (Section: Securing Multiagent GLP)
%%
%% Programs require ability to inspect attestations on received messages.
%% GLP provides guard predicates for security operations.
%%
%% attestation(X, Info) - succeeds if X carries an attestation
%%   Info binds to att(Agent, Module) containing attesting agent's
%%   public key and module identifier. For locally-produced terms,
%%   Agent binds to distinguished constant 'self'.
%%
%% module(M) - binds M to identifier of currently executing module.
%%   Agents use this to determine own module identity when evaluating
%%   compatibility with other agents' attested modules.

%% Example: Verify message attestation before processing
process_message(Msg, Result?) :-
    attestation(Msg, att(Agent, Module)),
    trusted_module(Module?) |
    handle_verified(Msg?, Agent?, Result).

process_message(Msg, rejected) :-
    attestation(Msg, att(_, Module)),
    \+ trusted_module(Module?) |
    true.

%% Example: Check own module for protocol decisions
negotiate_protocol(Other, Result?) :-
    module(MyModule),
    attestation(Other, att(_, OtherModule)),
    compatible(MyModule?, OtherModule?) |
    Result = accept.
```

---

## Supplementary Examples (Not in Main Book)

### Logic Gates

#### gates.glp
**Path:** `logic_gates/gates/gates.glp`
**Status:** `not_in_book`
**Goal:** `and(1, 1, X), or(0, 1, Y).`
**Cases:** Boolean logic, simple pattern matching

---

### Puzzles

#### hanoi.glp
**Path:** `puzzles/hanoi/hanoi.glp`
**Status:** `candidate`
**Goal:** `hanoi(3, a, b, c, Moves).`
**Cases:** Classic recursion, move generation

---

### Distribution Patterns

#### distribute.glp (broadcast)
**Path:** `distribution/broadcast/distribute.glp`
**Status:** `candidate`

#### distribute_indexed.glp
**Path:** `distribution/indexed/distribute_indexed.glp`
**Status:** `candidate`

#### distribute_binary.glp
**Path:** `distribution/tree/distribute_binary.glp`
**Status:** `candidate`

---

### Library Utilities

#### lookup.glp
**Path:** `lib/lookup/lookup.glp`
**Status:** `not_in_book` (utility)

#### tag_stream.glp
**Path:** `lib/streams/tag_stream.glp`
**Status:** `not_in_book` (utility)

#### inject.glp
**Path:** `lib/streams/inject.glp`
**Status:** `not_in_book` (utility)

---

## Case Catalog

### Unification Cases
| Case | First Use | Description |
|------|-----------|-------------|
| Atomic term unification | glp_core | Constant-to-constant matching |
| Compound term two-phase | glp_core | Writer binding + reader verification |
| List construction | append.glp | Building output lists with [H\|T] |
| Difference list threading | quicksort.glp | Efficient append via holes |

### Execution Cases
| Case | First Use | Description |
|------|-----------|-------------|
| Body activation (spawn) | producer_consumer.glp | Creating concurrent goals |
| Guard evaluation | factorial.glp | Arithmetic comparisons |
| Suspension/resumption | producer_consumer.glp | Reader waiting for writer |
| Tail recursion | producer.glp | State threading without stack growth |
| True recursion | fibonacci.glp | Multiple spawned recursive calls |

### Communication Cases
| Case | First Use | Description |
|------|-----------|-------------|
| Stream generation | producer_consumer.glp | Incremental list production |
| Stream merge | fair_merge.glp | Combining concurrent streams |
| Incomplete messages | monitor.glp | Bidirectional communication |
| Observation/duplication | observer.glp | Non-destructive stream inspection |

---

*Last updated: December 2025*
