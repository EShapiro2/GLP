# GrassApp Redesign — Handoff

Restructure the running GrassApp from the current **Chats | Wallet | Requests**
(one shared inbox) to **one panel per platform, each with its own per-item
alerts**. The GLP mediator does not change; this is a Dart-side rebuild.

## Session start-up
1. Read `/Users/udi/Grassroots/GLP/CLAUDE.md` (mandatory startup), then this file.
2. Read `lib/ui_runtime/{manifest.dart, runtime.dart, agent_surface.dart}`,
   `lib/manifests/grassroots.dart`, `lib/main.dart`.
3. Discussion mode is default — agree the Panel abstraction with Udi before coding.

## Agreed design (decided with Udi)
- ONE app, **GrassApp**. Bottom nav = **one icon per platform**: **Friends**
  (social graph), **Coins**, **Chats** (social network). A new platform = a new
  bottom icon + its own panel + its own alerts. Extensible.
- The app-bar title shows the **active platform's name** (Friends / Coins /
  Chats); the app overall is GrassApp.
- **Per-platform inbox, per-item, WhatsApp-style.** An alert badges the **row**
  (per person / per chat / per coin-holder); tapping the row opens the request
  to **accept / decline** (confirmed gesture — not inline buttons). The tab icon
  shows that platform's aggregate badge.
- **One mediator, Dart-side routing.** The existing single mediator handles all
  platforms; the Dart side routes each inbox card to its platform's panel and
  item by notify constructor. NOT per-platform mediators.
- **Volition → panel mapping:**
  - **Friends** (social graph): friend offers (`befriend`) + introductions.
    First-contact friend offer goes HERE, not Chats (confirmed).
  - **Coins**: a swap a friend proposes (`swap_offer`) → badges that friend's
    row in the wallet.
  - **Chats** (social network): group invitations only. One-to-one
    conversations appear only AFTER the friendship is made in Friends.
    (Udi: "may be cumbersome, do it and rethink later.")

## Current state
- Live scenario: `programs/book/coins/` — one mediator (`coins_mediator.glp`) +
  agent (`coins_agent.glp`) over a single-isolate crossbar; `bob` is the live
  UI, `alice`/`charlie` scripted actors. Verified on macOS, iOS simulator, and a
  physical iPhone (release build, custom icon, "GrassApp" name).
- Dart UI: `lib/ui_runtime/{manifest.dart, runtime.dart, agent_surface.dart}`;
  `lib/manifests/grassroots.dart` (current unified manifest: `chat` + `wallet` +
  ONE shared `inbox` of [`befriend`, `swap_offer`]); `lib/main.dart` wires the
  scenario and mounts `AgentSurface`.
- `agent_surface.dart`: `_stateTabs` builds [chat?, wallet?, friends?] + a single
  **Requests** tab (`_StateTab`). This is exactly what changes — drop the shared
  Requests tab; each platform tab carries its own per-item inbox.
- Manifest schema (`manifest.dart`): `Manifest{commands, inbox:[InboxDesc],
  activity:[ActivityDesc], state:[StateView], chat:ChatView?, wallet:WalletView?}`.
  `InboxDesc` = notify→card with `answers`. Activity rules apply effects
  (`AppendTo/RemoveFrom/SetValue/OpenChat/PushChat/SetBalance/Toast`).
  `WalletView` = friends-list drilldown + per-friend actions; `ChatView` = chat
  list + conversation.

## The rebuild (Dart only; GLP mediator unchanged)
Primary file: `lib/ui_runtime/agent_surface.dart`, plus the manifest structure.

1. **Panels, not surfaces.** A `Panel` per platform: a name (app-bar title + tab
   label), an icon, a state/outbox view (friends list / wallet / chat list), and
   its **own** inbox of cards. Bottom nav = the panels (no separate Requests tab).
2. **Per-item alert routing.** Each inbox card belongs to a panel AND an item key
   (a person / holder / chat). Render the alert as a badge on that item's row;
   tapping the row opens the card to accept/decline. Tab badge = count of that
   panel's pending cards.
   - Friends: a friend offer is a row (the offering person) with an alert →
     tap → accept/decline. Established friends are plain rows.
   - Coins: a `swap_offer` badges the proposing friend's wallet row → tap →
     "gives X for Y" → accept/decline.
   - Chats: a group invitation is an alerting row → tap → accept/decline.
3. **Manifest declares panels.** Extend the manifest so each Panel maps: which
   notify constructors are its inbox cards, which field keys a card to a row, its
   state view, and its outbox commands. `runtime.dart` dispatches the single
   mediator's notify stream to panels by constructor (keep the activity rules).
4. **One mediator, Dart routing.** `runtime.dart` keeps consuming the one
   mediator's notify stream and routes each card to the owning panel + item.

## After the build
- **Re-shoot `fig:grassapp`** from the REAL unified app: one GrassApp build,
  screenshot the three panels (Friends / Coins / Chats) with consistent green
  theme + the 9:41 phone status bar + the Friends|Coins|Chats bottom nav. The
  current `figures/gsg-app-friends.png` is the OLD orange build with a
  Friends|Requests nav — wrong; replace it. Pattern:
  `test/paper_screenshots_grassroots_test.dart`. Copy PNGs to
  `/Users/udi/Grassroots/UIVE/figures/`.
- **Paper edits already proposed** (hand to Udi / the paper restructure — do NOT
  apply unless told; Udi reviews in Overleaf):
  - §7 (sections/ui-primitives.tex, ~line 51): "as a per-platform *mediator* …
    under each platform directory in programs/" → "by a single *mediator* … one
    generic machine every platform shares, parameterised only by its message
    vocabulary".
  - §7.2 (~line 95): "All three share the two surfaces: an outbox … and an inbox
    of volitions to answer." → each panel its own outbox and inbox, the one
    mediator routing each volition to its platform's panel (friend offer →
    social-graph panel, swap → coins, group invite → social-network).
  - Minor (separate): §7 line 51 "coins and bonds" — the coins panel is
    coins-among-friends, no bonds; elsewhere it's "Coins".

## Rules
- GLP changes: read `GLP/CLAUDE.md` first; don't modify core GLP without approval.
- The GLP coins mediator/agent stay as-is for this rebuild.
- Commit Dart files with specific `git add`, single-line message,
  `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>`.
