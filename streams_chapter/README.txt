Streams Chapter - GLP Test Programs and Traces
================================================

This directory contains 6 GLP source files demonstrating stream programming
patterns, along with their execution traces.

Source Files:
-------------
1. streams_basic.glp     - Bounded producer/consumer (countdown, sum)
2. difference_lists.glp  - Difference list operations (create, convert, append, close)
3. bb_dlist.glp          - Bounded buffer using difference lists
4. cooperative.glp       - Cooperative stream production (bob/alice handoff)
5. transducers.glp       - Stream transducers (copier, duplicator, separator, differentiator)
6. bb_list.glp           - List-based bounded buffer (for comparison)

Trace Files:
------------
Each trace_*.txt file contains the REPL session with :trace enabled, showing
the reduction of goals step by step.

Key Results:
------------
- streams_basic:    producer(H, 5) -> H = [5,4,3,2,1]
- difference_lists: create_dl(3, H, T) -> H = [_,_,_|T]
- bb_dlist:         test_simple(R) -> R = 15 (sum of 5+4+3+2+1)
- cooperative:      test_coop(S, D) -> S = [a,a,b,b,b,a,a], D = done
                    test_read(C) -> C = 7 (count of elements)
- transducers:      duplicator([1,2], O) -> O = [1,1,2,2]
                    test_chain(O) -> O = [1,0,1,0,2,0,2,0]
- bb_list:          test_bb_list(R) -> R = 15

All programs are SRSW-compliant (Single-Reader/Single-Writer).

The traces show concurrent interleaving of producer and consumer goals,
demonstrating GLP's stream-based concurrent programming model.
