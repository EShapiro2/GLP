# Play: The Coffee Shop Network

**Actors:** Alice, Bob, Carol, Dave

**Scenario:** Alice and Bob meet at a coffee shop and become friends via cold-call. Later, Bob introduces Alice to Carol (his existing friend). Dave attempts to befriend Alice but is rejected. Finally, Alice sends a message to Bob through their established channel.

---

## Scene 1: Initialization

All four agents initialize with user and network channels.

```glp
% Initial goals (spawned concurrently)
agent(alice, ch(AliceUserIn, AliceUserOut), ch(AliceNetIn, AliceNetOut)),
agent(bob, ch(BobUserIn, BobUserOut), ch(BobNetIn, BobNetOut)),
agent(carol, ch(CarolUserIn, CarolUserOut), ch(CarolNetIn, CarolNetOut)),
agent(dave, ch(DaveUserIn, DaveUserOut), ch(DaveNetIn, DaveNetOut)).
```

**Expected state after initialization:**
Each agent runs `social_graph(Id, In, [(user, UserOut), (net, NetOut)])` with merged input streams.

---

## Scene 2: Alice Cold-Calls Bob (Accepted)

Alice's user requests connection to Bob.

**Step 2.1:** Alice's user sends connect request.
```glp
AliceUserIn = [msg(user, alice, connect(bob))|AliceUserIn1]
```

**Step 2.2:** Alice's agent processes the request:
- Sends `msg(alice, bob, intro(alice, alice, Resp1))` to network
- Spawns `response_stream(Resp1?, bob?, alice?, Rs1)`
- Spawns `merge(AliceIn?, Rs1?, AliceIn1)`
- Continues with `social_graph(alice, AliceIn1?, ...)`

**Step 2.3:** Network delivers intro to Bob.
```glp
BobNetIn = [msg(alice, bob, intro(alice, alice, Resp1))|BobNetIn1]
```

**Step 2.4:** Bob's agent receives intro, asks user:
- Sends `msg(agent, user, befriend(alice, Resp1?))` to Bob's user

**Step 2.5:** Bob's user accepts.
```glp
BobUserIn = [msg(user, bob, decision(yes, alice, Resp1))|BobUserIn1]
```

**Step 2.6:** Bob's agent processes decision:
- Calls `bind_response(yes, alice, Resp1?, ...)`
- Creates channel: `new_channel(ch(FIn, FOut), FCh)` produces paired channels
- Binds `Resp1 = accept(FCh)`
- Calls `handle_response(accept(FCh?), alice, ...)` to add Alice to friends list

**Step 2.7:** Back on Alice's side:
- `response_stream` succeeds because `known(Resp1?)` now true
- Produces `Rs1 = [msg(bob, alice, response(accept(ch(...))))]`
- `merge` delivers this to Alice's input stream

**Step 2.8:** Alice's agent processes response:
- Calls `handle_response(accept(ch(FIn, FOut)), bob, ...)`
- Tags and merges Bob's input stream
- Adds Bob to friends list

**Expected state:** Alice and Bob are now friends with bidirectional channels.

---

## Scene 3: Dave Cold-Calls Alice (Rejected)

**Step 3.1:** Dave's user requests connection to Alice.
```glp
DaveUserIn = [msg(user, dave, connect(alice))|DaveUserIn1]
```

**Step 3.2:** Dave's agent sends intro through network.

**Step 3.3:** Network delivers to Alice.
```glp
% Arrives on Alice's merged input stream
msg(dave, alice, intro(dave, dave, Resp2))
```

**Step 3.4:** Alice's agent asks user about Dave.

**Step 3.5:** Alice's user rejects.
```glp
AliceUserIn = [msg(user, alice, decision(no, dave, Resp2))|...]
```

**Step 3.6:** Alice's agent processes rejection:
- Calls `bind_response(no, _, no, Fs, Fs?, In, In?)`
- Binds `Resp2 = no`
- Friends list unchanged

**Step 3.7:** Dave's agent receives response:
- `handle_response(no, _, Fs, Fs?, In, In?)` — no action

**Expected state:** Dave and Alice are not friends.

---

## Scene 4: Bob Introduces Alice to Carol

**Precondition:** Bob and Carol are already friends (established before play begins or via another cold-call).

**Step 4.1:** Bob's user requests introduction.
```glp
BobUserIn = [msg(user, bob, introduce(alice, carol))|...]
```

**Step 4.2:** Bob's agent processes introduction:
- Creates channel pair: `new_channel(ch(ACIn, ACOut), ch(CAIn, CAOut))`
- Sends to Alice: `msg(bob, alice, intro(carol, ch(CAIn?, ACOut?)))`
- Sends to Carol: `msg(bob, carol, intro(alice, ch(ACIn?, CAOut?)))`

**Step 4.3:** Alice receives introduction from Bob (through friend channel).
```glp
msg(bob, alice, intro(carol, ch(CAIn, ACOut)))
```

**Step 4.4:** Alice's agent asks user about Carol (introduced by Bob).
- Sends `msg(agent, user, befriend_intro(bob, carol, ch(...)))`

**Step 4.5:** Alice's user accepts.
```glp
AliceUserIn = [msg(user, alice, accept_intro(carol, ch(CAIn, ACOut)))|...]
```

**Step 4.6:** Alice's agent establishes connection:
- Tags Carol's input stream
- Merges into main input
- Adds Carol to friends list

**Step 4.7:** Carol follows symmetric process.

**Expected state:** Alice and Carol are now friends via Bob's introduction.

---

## Scene 5: Alice Messages Bob

**Step 5.1:** Alice's user sends message to Bob.
```glp
AliceUserIn = [msg(user, alice, send(bob, hello_friend))|...]
```

**Step 5.2:** Alice's agent looks up Bob in friends list, sends through friend channel.
```glp
% Through established friend channel (not network)
msg(alice, bob, hello_friend)
```

**Step 5.3:** Bob receives message on merged input (tagged with alice).

**Expected state:** Bob receives `msg(alice, bob, hello_friend)` through the friend channel, not the network.

---

## Expected Final State

| Agent | Friends |
|-------|---------|
| Alice | bob, carol |
| Bob | alice, carol (pre-existing) |
| Carol | bob (pre-existing), alice |
| Dave | (none) |
