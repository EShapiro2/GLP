# Step 2.4d-fix: Correct Parameterized Proc Decl Signatures

**Problem**: The proc decls committed in 2.4d have wrong arity for Channel. `Channel(In, Out)` takes 2 params but `send`/`receive` provide 1. The expansion engine silently fell back to monomorphic `Channel`, masking the bug. This is why the monomorphic types can't be removed.

**Fix**: Correct the Channel arity and convert bare Stream refs. Then remove monomorphic types.

---

## Step 1: Fix send/receive/=../..= in `programs/self.glp`

### send — Channel needs 2 args (read stream Y, write stream Stream(X)):
```
procedure send(X?, Channel(Stream(X))?, Channel(Stream(X))).
```
→
```
procedure send(X?, Channel(Y, Stream(X))?, Channel(Y, Stream(X))).
```

### receive — Channel needs 2 args (read stream Stream(X), write stream Y):
```
procedure receive(X, Channel(Stream(X))?, Channel(Stream(X))).
```
→
```
procedure receive(X, Channel(Stream(X), Y)?, Channel(Stream(X), Y)).
```

### =.. — bare Stream → Stream(_):
```
procedure =..(_, Stream?).      % Compose: Stream? → Compound
```
→
```
procedure =..(_, Stream(_)?).   % Compose: Stream? → Compound
```

### ..= — bare Stream → Stream(_):
```
procedure ..=(Stream, _?).      % Decompose: Compound? → Stream
```
→
```
procedure ..=(Stream(_), _?).   % Decompose: Compound? → Stream
```

Run tests. All 390 must pass. Commit: `fix(types): correct Channel arity in send/receive, Stream(_) in univ`

---

## Step 2: Remove monomorphic types from `programs/self.glp`

Remove:
```
% Collections (monomorphic — required by prelude proc decls and DFA builder)
Stream ::= [] ; [_|Stream].
OpenStream ::= [_|Stream].
DiffList ::= Stream \ Stream?.
```
and:
```
% Communication (monomorphic — required by prelude proc decls and DFA builder)
Channel ::= ch(Stream, Stream?).
```

Update comments on parameterized versions:
```
% Collections
Stream(X) ::= [] ; [X | Stream(X)].
OpenStream(X) ::= [X | Stream(X)].
DiffList(X) ::= Stream(X) \ Stream(X)?.

% Communication
Channel(In, Out) ::= ch(In, Out?).
```

Run tests. All 390 must pass. Commit: `feat(types): remove monomorphic Stream/Channel/DiffList/OpenStream from self.glp`

---

## Step 3: Update current_plan.md

Mark Step 7 as done.

---

## Troubleshooting

If Step 1 fails: the issue is likely that some downstream file's parameterized proc decl also has wrong Channel arity. Search for `Channel(Stream(` in .glp files — any 1-arg Channel reference is wrong.

If Step 2 fails: remaining bare Stream/Channel references in files not yet converted. Fix them using the same patterns as earlier batches.
