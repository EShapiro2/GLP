# AofGLP Code Index

All 87 GLP programs organized by book chapter with goals and cases demonstrated.

---

## Part I: Foundations

### Chapter: glp_core
*Core unification examples - see glp_core.tex tables*

---

## Part II: Programming with Streams

### Chapter: streams

| File | Goal | Cases |
|------|------|-------|
| `streams/producer_consumer/producer_consumer.glp` | `producer(Xs), consumer(Xs?, 0).` | stream generation, suspension/resumption, tail recursion |
| `streams/observation/observer.glp` | `observe(In?, Out, Audit).` | bidirectional observation, ground guard |

### Chapter: buffered_communication

| File | Goal | Cases |
|------|------|-------|
| `merge/binary/fair_merge.glp` | `merge([1,2,3], [a,b,c], Zs).` | binary merge, clause alternation |
| `merge/mwm/mwm.glp` | `mwm([stream([1,2,3])], Out).` | multiway merge, mutual references |
| `merge/dynamic/dynamic_merger.glp` | `merger(Ws, Xs, Out).` | dynamic stream addition |
| `merge/tree/merge_tree.glp` | `merge_tree([[a,b],[1,2]], Out).` | logarithmic merge tree |
| `merge/channels/channels.glp` | `read(M, Ch, Ch1).` | channels (partially ordered streams) |
| `merge/channels/parallel_table.glp` | `table(Self, Low, Val, High).` | parallel lookup via channels |
| `protocols/bounded_buffer/bounded_buffer.glp` | `sq_num_buffered(10, Ss, 3).` | difference lists, flow control |
| `protocols/bounded_buffer/switch2x2.glp` | `switch2x2(In1, In2, Out1, Out2).` | 2x2 routing switch |

### Chapter: monitors

| File | Goal | Cases |
|------|------|-------|
| `monitors/basic/monitor.glp` | `monitor([add(5),value(V)]).` | stateful service, incomplete messages |
| `monitors/basic/monitor_test.glp` | `test_monitor(V1, V2).` | wait guards, temporal coordination |
| `monitors/observer/observed_monitor.glp` | `play_accum(VA,VB,VC,VD,Log).` | type-aware observation, duplicate |
| `monitors/observer/play_absolute.glp` | `play_absolute(VA,VB,VC,VD,Log).` | absolute time targets |

---

## Part III: Recursive Programming

### Chapter: lists

| File | Goal | Cases |
|------|------|-------|
| `lists/append/append.glp` | `append([1,2],[3,4],Zs).` | list construction |
| `lists/reverse/reverse.glp` | `reverse([1,2,3],Rs).` | accumulator pattern |
| `lists/length/length.glp` | `length([a,b,c],N).` | accumulator, arithmetic |
| `lists/member/member.glp` | `member(b,[a,b,c]).` | simple recursion |
| `lists/copy/copy.glp` | `copy([1,2,3],Ys).` | element-by-element copy |
| `lists/nth/nth.glp` | `nth(2,[a,b,c],X).` | indexed access |
| `book_examples/lists/reverse.glp` | `reverse([1,2,3],Rs).` | naive vs accumulator comparison |

### Chapter: sorting

| File | Goal | Cases |
|------|------|-------|
| `sorting/quicksort/quicksort.glp` | `quicksort([3,1,4,1,5],S).` | partition, difference lists |
| `sorting/merge_sort/merge_sort.glp` | `merge_sort([3,1,4],S).` | divide-conquer, merge with guards |
| `sorting/bubble_sort/bubble_sort.glp` | `bubble_sort([3,1,4],S).` | iterative sorting, swap flag |
| `sorting/insertion_sort/insertion_sort.glp` | `insertion_sort([3,1,4],S).` | insert into sorted |
| `book_examples/recursive/quicksort.glp` | `quicksort([3,1,4],S).` | SRSW-compliant partition |
| `book_examples/recursive/mergesort.glp` | `mergesort([3,1,4],S).` | concurrent split/merge |

### Chapter: arithmetic

| File | Goal | Cases |
|------|------|-------|
| `arithmetic/factorial/factorial.glp` | `factorial(5,F).` | arithmetic guards, sequential deps |
| `arithmetic/fibonacci/fibonacci.glp` | `fib(10,F).` | true recursion, concurrent calls |
| `arithmetic/primes/primes.glp` | `primes(30,Ps).` | sieve, filter pattern |
| `arithmetic/sum_list/sum_list.glp` | `sum([1,2,3],S).` | accumulator |
| `book_examples/recursive/fib.glp` | `fib(10,F).` | tree of concurrent processes |
| `book_examples/recursive/flatten.glp` | `flatten([[1,2],[3]],Ys).` | difference lists, otherwise guard |
| `book_examples/recursive/recursive.glp` | various | combined: mergesort,quicksort,fib,flatten |

---

## Part IV: Objects and Processes

### Chapter: objects

| File | Goal | Cases |
|------|------|-------|
| `objects/basics/counter.glp` | `counter([up,up,show(V)],0).` | stateful object, message stream |
| `objects/basics/many_counters.glp` | `use_many_counters(Input,List).` | dynamic object creation |
| `objects/basics/queue_manager.glp` | `qm([dequeue(X),enqueue(1)],Q,Q).` | incomplete messages, difference list queue |
| `objects/constraints/plus_constraint.glp` | `plus(X,1,5).` | constraint propagation, known guard |

### Chapter: inheritance

| File | Goal | Cases |
|------|------|-------|
| `objects/inheritance/rectangular_area.glp` | `rectangular_area(Msgs,Params).` | base class |
| `objects/inheritance/frame.glp` | `create_frame(Msgs,Params).` | filter pattern, delegation |
| `objects/inheritance/window_with_label.glp` | `create_window_with_label(M,L,P).` | otherwise guard, inheritance chain |

---

## Part V: Metaprogramming

### Chapter: plain_meta

| File | Goal | Cases |
|------|------|-------|
| `meta/plain/plain_meta.glp` | `run(merge([1,2],[3],Xs)).` | basic metainterpreter, reduce/2 |
| `meta/plain/cp_meta.glp` | `reduce(Program,Goal).` | program as argument |
| `meta/plain/certainty_meta.glp` | `reduce(P,Goal,Certainty).` | certainty factors |

### Chapter: enhanced_meta

| File | Goal | Cases |
|------|------|-------|
| `meta/control/control_meta.glp` | `run(Goal,ControlStream).` | suspend/resume/abort |
| `meta/control/abortable_meta.glp` | `reduce(P,Goal,Abort).` | abort via variable binding |
| `meta/failsafe/failsafe_meta.glp` | `run(Goal,Failures).` | failure reporting |
| `meta/termination/termination_meta.glp` | `run(Goal,done,R).` | short-circuit termination |
| `meta/termination/termination_detection_meta.glp` | `reduce(P,G,done--Done).` | distributed termination |
| `meta/snapshot/snapshot_meta.glp` | `run(A,Cs,[],R).` | snapshot on abort |
| `meta/snapshot/snapshot_meta_cp.glp` | `reduce(P,G,L--R).` | shot command for inspection |

### Chapter: debugging

| File | Goal | Cases |
|------|------|-------|
| `meta/debugging/debugger_meta.glp` | `reduce(P,Goal,5,Tree).` | budget-limited, execution tree |
| `meta/debugging/runtime_control_meta.glp` | `run(Goal,Cs,L,R).` | runtime control, dump |
| `meta/tracing/tracing_meta.glp` | `run(Goal,Trace).` | timestamped trace |
| `meta/tracing/timestamped_tree_meta.glp` | `run(Goal,Tree).` | execution tree with timestamps |

---

## Part VI: Grassroots Protocols

### Chapter: social_graph

| File | Goal | Cases |
|------|------|-------|
| `social_graph/agent/agent.glp` | `agent(alice,ChUser,ChNet).` | channel init, stream merge |

### Chapter: befriending

| File | Goal | Cases |
|------|------|-------|
| `social_graph/cold_call/cold_call.glp` | `social_graph(Id,Msgs,Fs).` | attestation, inject pattern |
| `social_graph/introduction/friend_introduction.glp` | `social_graph(Id,Msgs,Fs).` | friend-mediated introduction |
| `social_graph/response/response_handling.glp` | `bind_response(yes,From,Resp,Fs,Fs1,In,In1).` | channel establishment |

### Chapter: networking

| File | Goal | Cases |
|------|------|-------|
| `networking/direct_messaging/direct_messaging.glp` | `establish(yes,From,Resp,Fs,Fs1,In,In1).` | DM channel, attestation |
| `networking/feed/feed.glp` | `post(Content,Followers,Fs1).` | broadcast, attestation preservation |
| `networking/groups/group_formation.glp` | `social_graph(Id,Msgs,Fs).` | group creation, invitations |
| `networking/groups/group_messaging.glp` | `group_member(Id,Group,Streams).` | interlaced streams |
| `networking/blocklace/interlaced_streams.glp` | `streams(MyStream,Others).` | blocklace DAG, reader guard |
| `networking/replication/replicate.glp` | `replicate2(X,Y1,Y2).` | non-ground replication |
| `protocols/network_switch/network_switch.glp` | `network((p,ChP),(q,ChQ),(r,ChR)).` | 3-way message routing |
| `protocols/network_switch/network_switch_3way.glp` | `network((p,ChP),(q,ChQ),(r,ChR)).` | same as above |

### Chapter: security

| File | Goal | Cases |
|------|------|-------|
| `security/attestation/attestation_guards.glp` | `process_message(Msg,Result).` | attestation guard, module guard |
| `security/blockchain/stream_security.glp` | `extend_stream([H|T],H,T).` | SRSW security properties |

---

## Distribution Patterns (candidate)

| File | Goal | Cases |
|------|------|-------|
| `distribution/broadcast/distribute.glp` | `distribute([a,b,c],Y,Z).` | ground guard, replication |
| `distribution/ground/distribute.glp` | `distribute([a,b],Y,Z).` | same pattern |
| `distribution/indexed/distribute_indexed.glp` | `distribute_indexed([send(1,a)],Y,Z).` | tag-based routing |
| `distribution/tree/distribute_binary.glp` | `distribute_binary([req([0],a)],Y,Z).` | binary address routing |
| `distribution/cooperative/cooperative_producers.glp` | `producer_a(control(Xs,Next)).` | cooperative handover |

---

## Logic Gates (candidate)

| File | Goal | Cases |
|------|------|-------|
| `logic_gates/gates/gates.glp` | `and([one,zero],[one,one],Zs).` | stream-based logic |

---

## Puzzles (candidate)

| File | Goal | Cases |
|------|------|-------|
| `puzzles/hanoi/hanoi.glp` | `hanoi(3,a,b,c,Moves).` | classic recursion, append |

---

## Library Utilities (not_in_book)

| File | Goal | Cases |
|------|------|-------|
| `lib/broadcast/broadcast.glp` | `broadcast(Msg,Fs,Fs1).` | list traversal, stream append |
| `lib/channels/channel_ops.glp` | `send(X,Ch,Ch1).` | channel primitives |
| `lib/channels/relay.glp` | `relay(In,Out,Ch).` | bidirectional relay |
| `lib/guards/guard_utils.glp` | n/a | guard documentation |
| `lib/lookup/lookup.glp` | `lookup(Key,List,Val).` | association list |
| `lib/streams/inject.glp` | `inject(Trigger,Msg,In,Out).` | deferred insertion |
| `lib/streams/tag_stream.glp` | `tag_stream(Name,In,Out).` | source tagging |
| `lib/time/time_utils.glp` | `current_time(T).` | time wrapper |

---

## Exercise Solutions (not_in_book)

| File | Goal | Cases |
|------|------|-------|
| `exercise_solutions/streams/filter_even.glp` | `filter_even([1,2,3,4],Ys).` | filter pattern |
| `exercise_solutions/streams/length.glp` | `length([a,b,c],N).` | accumulator |
| `exercise_solutions/streams/map_inc.glp` | `map_inc([1,2,3],Ys).` | map pattern |

---

## Case Catalog

### First Use of Key Patterns

| Pattern | First Example | Chapter |
|---------|---------------|---------|
| Stream generation | producer_consumer.glp | streams |
| Suspension/resumption | producer_consumer.glp | streams |
| Binary merge | fair_merge.glp | buffered_communication |
| Difference lists | bounded_buffer.glp | buffered_communication |
| Incomplete messages | monitor.glp | monitors |
| Accumulator pattern | reverse.glp | lists |
| Partition with guards | quicksort.glp | sorting |
| True recursion (concurrent) | fibonacci.glp | arithmetic |
| Stateful object | counter.glp | objects |
| Filter/delegation | frame.glp | inheritance |
| Basic metainterpreter | plain_meta.glp | plain_meta |
| Execution control | control_meta.glp | enhanced_meta |
| Short-circuit termination | termination_meta.glp | enhanced_meta |
| Attestation guards | cold_call.glp | befriending |
| Inject pattern | cold_call.glp | befriending |
| Interlaced streams | interlaced_streams.glp | networking |
| Ground replication | distribute.glp | (candidate) |
| Constraint propagation | plus_constraint.glp | objects |
| Otherwise guard | window_with_label.glp | inheritance |
| Wait guards | monitor_test.glp | monitors |

---

*Total: 87 programs*
*Last updated: December 2025*
