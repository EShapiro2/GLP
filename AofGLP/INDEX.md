# AofGLP Code Index

All GLP programs organized by book chapter with goals and cases demonstrated.

---

## Part I: Foundations

### Chapter: Constants

| File | Goal | Cases |
|------|------|-------|
| `constants/gates/gates.glp` | `and([one,zero],[one,one],Zs).` | stream-based logic gates |

---

## Part II: Concurrent Programming

### Section: Streams - Producers, Consumers, Merge

| File | Goal | Cases |
|------|------|-------|
| `streams/producers_consumers/producer_consumer.glp` | `producer(Xs), consumer(Xs?, 0).` | stream generation, suspension/resumption |
| `streams/producers_consumers/observer.glp` | `observe(In?, Out, Audit).` | bidirectional observation |
| `streams/producers_consumers/fair_merge.glp` | `merge([1,2,3], [a,b,c], Zs).` | binary merge, clause alternation |
| `streams/producers_consumers/mwm.glp` | `mwm([stream([1,2,3])], Out).` | multiway merge |
| `streams/producers_consumers/dynamic_merger.glp` | `merger(Ws, Xs, Out).` | dynamic stream addition |
| `streams/producers_consumers/merge_tree.glp` | `merge_tree([[a,b],[1,2]], Out).` | logarithmic merge tree |
| `streams/producers_consumers/channels.glp` | `read(M, Ch, Ch1).` | channels (partially ordered streams) |
| `streams/producers_consumers/parallel_table.glp` | `table(Self, Low, Val, High).` | parallel lookup via channels |
| `streams/producers_consumers/distribute.glp` | `distribute([a,b,c],Y,Z).` | ground guard, replication |
| `streams/producers_consumers/distribute_ground.glp` | `distribute([a,b],Y,Z).` | ground distribution |
| `streams/producers_consumers/distribute_indexed.glp` | `distribute_indexed([send(1,a)],Y,Z).` | tag-based routing |
| `streams/producers_consumers/distribute_binary.glp` | `distribute_binary([req([0],a)],Y,Z).` | binary address routing |
| `streams/producers_consumers/cooperative_producers.glp` | `producer_a(control(Xs,Next)).` | cooperative handover |

### Section: Buffered Communication

| File | Goal | Cases |
|------|------|-------|
| `streams/buffered_communication/bounded_buffer.glp` | `sq_num_buffered(10, Ss, 3).` | difference lists, flow control |
| `streams/buffered_communication/switch2x2.glp` | `switch2x2(In1, In2, Out1, Out2).` | 2x2 routing switch |

### Section: Objects and Monitors

| File | Goal | Cases |
|------|------|-------|
| `streams/objects_monitors/monitor.glp` | `monitor([add(5),value(V)]).` | stateful service, incomplete messages |
| `streams/objects_monitors/monitor_test.glp` | `test_monitor(V1, V2).` | wait guards, temporal coordination |
| `streams/objects_monitors/observed_monitor.glp` | `play_accum(VA,VB,VC,VD,Log).` | type-aware observation |
| `streams/objects_monitors/play_absolute.glp` | `play_absolute(VA,VB,VC,VD,Log).` | absolute time targets |
| `streams/objects_monitors/counter.glp` | `counter([up,up,show(V)],0).` | stateful object, message stream |
| `streams/objects_monitors/many_counters.glp` | `use_many_counters(Input,List).` | dynamic object creation |
| `streams/objects_monitors/queue_manager.glp` | `qm([dequeue(X),enqueue(1)],Q,Q).` | incomplete messages, difference list queue |
| `streams/objects_monitors/plus_constraint.glp` | `plus(X,1,5).` | constraint propagation, known guard |
| `streams/objects_monitors/network_switch.glp` | `network((p,ChP),(q,ChQ),(r,ChR)).` | 3-way message routing |
| `streams/objects_monitors/network_switch_3way.glp` | `network((p,ChP),(q,ChQ),(r,ChR)).` | 3-way routing variant |

---

## Part II: Recursive Programming

### Section: Arithmetic and Trees

| File | Goal | Cases |
|------|------|-------|
| `recursive/arithmetic_trees/factorial.glp` | `factorial(5,F).` | arithmetic guards, sequential deps |
| `recursive/arithmetic_trees/fibonacci.glp` | `fib(10,F).` | true recursion, concurrent calls |
| `recursive/arithmetic_trees/primes.glp` | `primes(30,Ps).` | sieve, filter pattern |
| `recursive/arithmetic_trees/sum_list.glp` | `sum([1,2,3],S).` | accumulator |
| `recursive/arithmetic_trees/hanoi.glp` | `hanoi(3,a,b,c,Moves).` | classic recursion, append |

### Section: List Processing

| File | Goal | Cases |
|------|------|-------|
| `recursive/list_processing/append.glp` | `append([1,2],[3,4],Zs).` | list construction |
| `recursive/list_processing/reverse.glp` | `reverse([1,2,3],Rs).` | accumulator pattern |
| `recursive/list_processing/length.glp` | `length([a,b,c],N).` | accumulator, arithmetic |
| `recursive/list_processing/member.glp` | `member(b,[a,b,c]).` | simple recursion |
| `recursive/list_processing/copy.glp` | `copy([1,2,3],Ys).` | element-by-element copy |
| `recursive/list_processing/nth.glp` | `nth(2,[a,b,c],X).` | indexed access |
| `recursive/list_processing/quicksort.glp` | `quicksort([3,1,4,1,5],S).` | partition, difference lists |
| `recursive/list_processing/merge_sort.glp` | `merge_sort([3,1,4],S).` | divide-conquer, merge with guards |
| `recursive/list_processing/bubble_sort.glp` | `bubble_sort([3,1,4],S).` | iterative sorting, swap flag |
| `recursive/list_processing/insertion_sort.glp` | `insertion_sort([3,1,4],S).` | insert into sorted |

### Section: Structure Processing

*(Future: symbolic differentiation, polynomial recognition)*

---

## Part II: Metaprogramming

### Section: Plain Meta-Interpreter

| File | Goal | Cases |
|------|------|-------|
| `meta/plain/plain_meta.glp` | `run(merge([1,2],[3],Xs)).` | basic metainterpreter, reduce/2 |
| `meta/plain/cp_meta.glp` | `reduce(Program,Goal).` | program as argument |
| `meta/plain/certainty_meta.glp` | `reduce(P,Goal,Certainty).` | certainty factors |
| `meta/plain/failsafe_meta.glp` | `run(Goal,Failures).` | failure reporting |

### Section: Enhanced Meta-Interpreters

| File | Goal | Cases |
|------|------|-------|
| `meta/enhanced/control_meta.glp` | `run(Goal,ControlStream).` | suspend/resume/abort |
| `meta/enhanced/abortable_meta.glp` | `reduce(P,Goal,Abort).` | abort via variable binding |
| `meta/enhanced/termination_meta.glp` | `run(Goal,done,R).` | short-circuit termination |
| `meta/enhanced/termination_detection_meta.glp` | `reduce(P,G,done--Done).` | distributed termination |
| `meta/enhanced/snapshot_meta.glp` | `run(A,Cs,[],R).` | snapshot on abort |
| `meta/enhanced/snapshot_meta_cp.glp` | `reduce(P,G,L--R).` | shot command for inspection |
| `meta/enhanced/tracing_meta.glp` | `run(Goal,Trace).` | timestamped trace |
| `meta/enhanced/timestamped_tree_meta.glp` | `run(Goal,Tree).` | execution tree with timestamps |

### Section: Debugging

| File | Goal | Cases |
|------|------|-------|
| `meta/debugging/debugger_meta.glp` | `reduce(P,Goal,5,Tree).` | budget-limited, execution tree |
| `meta/debugging/runtime_control_meta.glp` | `run(Goal,Cs,L,R).` | runtime control, dump |

---

## Part III: Simulating Multiagent Systems

### Section: Social Graph

| File | Goal | Cases |
|------|------|-------|
| `multiagent/social_graph/agent.glp` | `agent(alice,ChUser,ChNet).` | channel init, stream merge |
| `multiagent/social_graph/cold_call.glp` | `social_graph(Id,Msgs,Fs).` | attestation, inject pattern |
| `multiagent/social_graph/friend_introduction.glp` | `social_graph(Id,Msgs,Fs).` | friend-mediated introduction |
| `multiagent/social_graph/response_handling.glp` | `bind_response(yes,From,Resp,Fs,Fs1,In,In1).` | channel establishment |
| `multiagent/social_graph/attestation_guards.glp` | `process_message(Msg,Result).` | attestation guard, module guard |
| `multiagent/social_graph/stream_security.glp` | `extend_stream([H|T],H,T).` | SRSW security properties |

### Section: Social Networks

| File | Goal | Cases |
|------|------|-------|
| `multiagent/social_networks/direct_messaging.glp` | `establish(yes,From,Resp,Fs,Fs1,In,In1).` | DM channel, attestation |
| `multiagent/social_networks/feed.glp` | `post(Content,Followers,Fs1).` | broadcast, attestation preservation |
| `multiagent/social_networks/group_formation.glp` | `social_graph(Id,Msgs,Fs).` | group creation, invitations |
| `multiagent/social_networks/group_messaging.glp` | `group_member(Id,Group,Streams).` | interlaced streams |
| `multiagent/social_networks/interlaced_streams.glp` | `streams(MyStream,Others).` | blocklace DAG, reader guard |
| `multiagent/social_networks/replicate.glp` | `replicate2(X,Y1,Y2).` | non-ground replication |

---

## Library Utilities

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

## Case Catalog

### First Use of Key Patterns

| Pattern | First Example | Section |
|---------|---------------|---------|
| Stream generation | producer_consumer.glp | streams |
| Suspension/resumption | producer_consumer.glp | streams |
| Binary merge | fair_merge.glp | streams |
| Difference lists | bounded_buffer.glp | buffered_communication |
| Incomplete messages | monitor.glp | objects_monitors |
| Accumulator pattern | reverse.glp | list_processing |
| Partition with guards | quicksort.glp | list_processing |
| True recursion (concurrent) | fibonacci.glp | arithmetic_trees |
| Stateful object | counter.glp | objects_monitors |
| Basic metainterpreter | plain_meta.glp | plain |
| Execution control | control_meta.glp | enhanced |
| Short-circuit termination | termination_meta.glp | enhanced |
| Attestation guards | cold_call.glp | social_graph |
| Inject pattern | cold_call.glp | social_graph |
| Interlaced streams | interlaced_streams.glp | social_networks |
| Constraint propagation | plus_constraint.glp | objects_monitors |
| Wait guards | monitor_test.glp | objects_monitors |

---

*Last updated: December 2025*
