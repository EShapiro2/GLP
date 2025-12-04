Title: Alice and Bob become friends via cold call
Actors: Alice, Bob
Precondition: Public keys are names (alice, bob)

---

Scene 1: Alice initiates connection

Alice says to her agent: connect(bob)

  [Alice's agent sends intro(alice, alice, Resp) to Bob via network]

---

Scene 2: Bob receives and considers the offer

Bob's agent, after receiving intro from network,
  says to Bob: befriend(alice, Resp)

Bob, after seeing befriend(alice, _),
  says to his agent: decision(yes, alice, Resp)

  [Bob's agent creates channel, binds Resp to accept(ch(...))]

---

Scene 3: Alice receives acceptance

Alice's agent, after receiving response(accept(Ch)),
  establishes friend channel to Bob

---

Scene 4: Alice sends a message to Bob

Alice says to her agent: send(bob, hello)

  [Alice's agent sends msg(alice, bob, hello) via friend channel]

Bob's agent, after receiving msg(alice, bob, hello),
  says to Bob: msg(alice, hello)

Bob, after seeing msg(alice, hello): done

---

Postcondition:
- Alice's friends list contains (bob, ...)
- Bob's friends list contains (alice, ...)
- Bob received hello from Alice
