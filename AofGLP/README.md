# The Art of Grassroots Logic Programming - Example Programs

This repository contains example programs for the textbook "The Art of Grassroots Logic Programming" (AoGLP), demonstrating GLP programming techniques from basic stream operations to secure multiagent protocols.

## Source Priority

Programs are drawn from these sources in order of priority:
1. **GLP Papers** - Authoritative source, used exactly as written
2. **FCP/CP Book** - Converted to GLP only when not available in GLP papers
3. **Test Files** - Standard examples cleaned and documented

## Structure

Each program file includes:
- The GLP program with full comments
- Inline `reduce` clauses for metainterpreter support (until module system exists)

## Directory Organization

### Core Programming (01-06)

#### 02_streams/ - Stream Processing
- `producer_consumer/` - Basic producer-consumer pattern
- `observation/` - Stream observation without consumption

#### 03_merge/ - Stream Merging
- `binary/` - Fair binary stream merger (the quintessential GLP program)
- `dynamic/` - Dynamic stream merger with runtime stream addition
- `channels/` - Channel-based merging

#### 04_distribution/ - Stream Distribution
- `ground/` - Ground-stream distribution to multiple consumers
- `cooperative/` - Cooperative producers with handover

#### 05_monitors/ - Concurrent Monitors
- `basic/` - Stateful service with add/subtract/value requests

#### 06_protocols/ - Communication Protocols
- `network_switch/` - 3-way network message routing
- `bounded_buffer/` - Bounded buffer implementations

### Metaprogramming (07)

#### 07_meta/ - Metainterpreters
- `plain/` - Plain GLP metainterpreter
- `failsafe/` - Reports failures instead of aborting
- `control/` - Suspend/resume/abort via control stream
- `termination/` - Termination detection via short-circuit
- `snapshot/` - Collects resolvent snapshot on abort
- `tracing/` - Produces execution trace with timestamps
- `debugging/` - Full runtime control with dump collection

### Data Structures (08-12)

#### 08_lists/ - List Operations
- `append/`, `reverse/`, `copy/` - Basic operations
- `member/`, `length/`, `nth/` - Query operations

#### 09_sorting/ - Sorting Algorithms
- `quicksort/` - Quicksort with partition
- `insertion_sort/` - Insertion sort
- `bubble_sort/` - Bubble sort
- `merge_sort/` - Merge sort

#### 10_arithmetic/ - Arithmetic Programs
- `factorial/`, `fibonacci/` - Recursive functions
- `primes/` - Sieve of Eratosthenes
- `sum_list/` - List summation

#### 11_logic_gates/ - Hardware Simulation
- `gates/` - AND/OR gate simulation on bit streams

#### 12_puzzles/ - Classic Puzzles
- `hanoi/` - Tower of Hanoi

### Object-Oriented (14)

#### 14_objects/ - Object-Oriented Patterns
- `basics/` - Counter, queue manager, many counters
- `constraints/` - Constraint objects
- `inheritance/` - Frame-based inheritance

### Grassroots Platforms (20-22)

#### 20_social_graph/ - Grassroots Social Graph
- `agent/` - Agent initialization
- `cold_call/` - Cold-call befriending protocol
- `response/` - Channel establishment and response handling
- `introduction/` - Friend-mediated introductions

#### 21_social_networking/ - Social Networking Applications
- `direct_messaging/` - DM channel establishment
- `feed/` - Authenticated feed distribution
- `groups/` - Group formation and messaging
- `blocklace/` - Interlaced streams (distributed blocklace)
- `replication/` - Non-ground term replication

#### 22_security/ - Security Mechanisms
- `attestation/` - Attestation guard predicates
- `blockchain/` - Blockchain security properties of streams

### Library (lib/)

#### lib/ - Utility Programs
- `channels/` - Channel operations, relay
- `streams/` - Tag stream, inject
- `lookup/` - List lookup utilities
- `broadcast/` - Broadcast to multiple recipients

## Usage

Each `.glp` file is self-contained with its `reduce` clauses for metainterpreter execution. To run a program under the plain metainterpreter:
```glp
run(merge([1,2,3], [a,b,c], Result)).
```

## File Count by Category

| Category | Files |
|----------|-------|
| 02_streams | 2 |
| 03_merge | 4 |
| 04_distribution | 2 |
| 05_monitors | 1 |
| 06_protocols | 4 |
| 07_meta | 12 |
| 08_lists | 6 |
| 09_sorting | 4 |
| 10_arithmetic | 4 |
| 11_logic_gates | 1 |
| 12_puzzles | 1 |
| 14_objects | 7 |
| 20_social_graph | 4 |
| 21_social_networking | 6 |
| 22_security | 2 |
| lib | 8 |
| **Total** | **70** |

## References

- GLP Paper: "GLP: A Secure, Multiagent, Grassroots, Concurrent, Logic Programming Language"
- Concurrent Prolog / FCP Book: "Concurrent Prolog: Collected Papers" (MIT Press)
- Art of Prolog: Sterling & Shapiro, "The Art of Prolog" (MIT Press)

## Future Work

- Module system for automatic metainterpreter integration
- Additional examples from FCP book (converted to GLP)
- Test harness integration
