# Paper Harmonization: Moded-Types ↔ Implementation

**Date**: 2026-03-11
**Context**: Parameterized types Stage 2 complete. Paper needs to reflect the current implementation.

---

## Gap 1: Modules section — obsolete "untyped boot" claim

**File**: `sections/modules.tex`, end of Implementation subsection.

**Current text** (last paragraph of §9.4):
```latex
The \verb|boot| module in the CSSG project remains untyped because its generic stream utilities (\verb|tee|, \verb|merge|, \verb|sink|) cannot express ``this operation preserves the specific type of its input'' without parameterized types.  This motivates the extension presented in Section~\ref{sec:parameterized-types}.
```

**Replace with**:
```latex
Before parameterized types, the \verb|boot| module in the CSSG project could not be fully typed: its generic stream utilities (\verb|tee|, \verb|merge|, \verb|sink|) could not express ``this operation preserves the specific type of its input.''  With the parameterized type declarations of Section~\ref{sec:parameterized-types}, these utilities are now declared as \verb|procedure tee(Stream(X)?, Stream(X), Stream(X)).| and \verb|procedure merge(Stream(X)?, Stream(X)?, Stream(X)).|, and all boot modules are fully type-checked.
```

---

## Gap 2: Modules section — example uses monomorphic merge

**File**: `sections/modules.tex`, the module example in §9.1.

**Current text**:
```latex
The agent module exports a single procedure:

\begin{verbatim}
%% agent.glp
exported procedure agent(Constant?, UserInStream?,
                         NetInStream?, OutputsList?).
procedure merge(Stream?, Stream?, Stream).
\end{verbatim}
```

**Replace with**:
```latex
The agent module exports a single procedure:

\begin{verbatim}
%% agent.glp
exported procedure agent(Constant?, UserInStream?,
                         NetInStream?, OutputsList?).
procedure merge(Stream(X)?, Stream(X)?, Stream(X)).
\end{verbatim}
```

---

## Gap 3: Parameterized types section — add send/receive/new_channel declarations

**File**: `sections/parameterized-types.tex`, in §8.3 (Parameterized Procedure Declarations).

After the `merge` example and before the "Multiple parameters" paragraph, **add**:

```latex
\mypara{Prelude channel operations}
The parameterized prelude declares channel operations using the two-parameter \verb|Channel(In, Out)| template:

\begin{verbatim}
Channel(In, Out) ::= ch(In, Out?).

procedure send(X?, Channel(Y, Stream(X))?,
               Channel(Y, Stream(X))).
procedure receive(X, Channel(Stream(X), Y)?,
                  Channel(Stream(X), Y)).
procedure new_channel(Channel(X, Y), Channel(Y, X)).
\end{verbatim}

Here \verb|send| has two type parameters: \verb|X| (the message element type) and \verb|Y| (the read stream type, unconstrained).  The write stream is \verb|Stream(X)|---a stream of elements of the same type as the message being sent.  The read stream \verb|Y| is preserved unchanged.  The type checker infers both \verb|X| and \verb|Y| from the call context.

For \verb|new_channel|, the parameters \verb|X| and \verb|Y| represent the two stream types.  The clause \verb|new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).| produces two cross-linked channel endpoints: what one end writes (\verb|Y|), the other reads, and vice versa.
```

---

## Gap 4: Prelude appendix — add parameterized type definitions and key proc decls

**File**: `sections/appendix-prelude.tex`.

After the existing Type Aliases subsection, **add**:

```latex
\subsection{Predefined Parameterized Types}
\label{app:predefined-param-types}

The prelude defines four parameterized type templates, available in every program:

\begin{verbatim}
Stream(X) ::= [] ; [X | Stream(X)].
OpenStream(X) ::= [X | Stream(X)].
DiffList(X) ::= Stream(X) \ Stream(X)?.
Channel(In, Out) ::= ch(In, Out?).
\end{verbatim}

\verb|Stream(X)| is a list of elements of type~\verb|X|, possibly empty.  \verb|OpenStream(X)| is a non-empty stream (no \verb|[]| base case).  \verb|DiffList(X)| is a difference list: a pair of a stream and a hole (reader) for constant-time concatenation.  \verb|Channel(In, Out)| is a bidirectional communication channel with read stream \verb|In| and write stream \verb|Out?| (the mode annotation on \verb|Out| indicates it is consumed by the channel holder).

These templates are expanded at each use site before type automaton construction (Section~\ref{sec:param-expansion}).

\subsection{Predefined Defined Guards}
\label{app:predefined-defined-guards}

The prelude defines single-unit-clause procedures that serve as defined guards.  When called in guard position, the partial evaluator unfolds them at compile time.

\begin{verbatim}
procedure =(_?, _).
X? = X.

procedure dl_append(DiffList(X)?, DiffList(X)?,
                    DiffList(X)).
dl_append(A\B?, B\C?, A?\C).

procedure dl_to_list(DiffList(X)?, Stream(X)).
dl_to_list(L\[], L?).

procedure new_channel(Channel(X, Y), Channel(Y, X)).
new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).

procedure send(X?, Channel(Y, Stream(X))?,
               Channel(Y, Stream(X))).
send(X, ch(In, [X?|Out?]), ch(In?, Out)).

procedure receive(X, Channel(Stream(X), Y)?,
                  Channel(Stream(X), Y)).
receive(X?, ch([X|In], Out?), ch(In?, Out)).
\end{verbatim}
```

---

## Gap 5: Appendix channel example — note on parameterized form

**File**: `sections/appendix-examples.tex`, at the start of §A.6 (Bidirectional Channel).

After "A bidirectional channel has two streams..." and before the Typed GLP Program, **add**:

```latex
\noindent\textit{Note:} The monomorphic types below illustrate the theory developed in Sections~\ref{sec:typed-glp}--\ref{section:well-typing}.  The implementation uses the parameterized forms \verb|Stream(X)| and \verb|Channel(In, Out)| from Section~\ref{sec:parameterized-types}; see Appendix~\ref{app:predefined-defined-guards} for the parameterized declarations.
```

---

## Execution

These are all edits to the Moded-Types paper. Apply them to the LaTeX source, then push to GitHub and sync with Overleaf (Menu → GitHub → Pull).

No GLP code changes needed.
