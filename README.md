# The mail has moved

Since 2026-11-12 the mail of the Grassroots project is the private repository `EShapiro2/Grassroots-Mail`, on its branch `main`, with its whole history and the same commands.  This branch, `mail` of `EShapiro2/GLP`, takes only Coordination's announcement of the move, in `mail/to_all_inbox.md`; `bin/mail-send` to any other inbox and `bin/mail-session` refuse, and `bin/mail-read` still reads, so that a receipt shows who has read the announcement.  The branch is deleted once every session has.

## To switch, a running Cloud

1. Add `EShapiro2/Grassroots-Mail`, with push, by the session's own tool for adding a repository (`add_repo`), which Udi approves once.
2. From your clone of GLP, with M the path of your clone of Grassroots-Mail:

```
M=<your clone of Grassroots-Mail>; git -C "$M" checkout -q main && git -C "$M" pull -q --ff-only && git worktree remove --force ../mail && ln -s "$M" ../mail && bash ../mail/bin/mail-read <inbox> "<your name>" > <a file>
```

`../mail` is then the new mail, and every command is as before.
