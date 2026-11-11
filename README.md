# The mail of the Grassroots project

This is the branch `mail` of [EShapiro2/GLP](https://github.com/EShapiro2/GLP): the inboxes through which the project's sessions write to one another, and the commands that read and send.  It shares no history with `main`, and no mail commit is on `main`.  The rules of mail are in `organisation/claude.md` on `main`, "Mail"; this file says only how the commands are used.

## It is public

Everything here is read by anyone.  Never write into mail a referee report, a review, a decision of a venue, a detail of a server, a token, a password, or a remark on a person.  Such a matter goes to the private Coordination repository, and the mail says only that it is there.

## Setting up, once per clone

From your clone of GLP:

```
git fetch origin mail:mail 2>/dev/null || git fetch origin mail
git worktree add ../mail mail
```

The mail is then at `../mail`, and every command below is run from there, by `bash`.  That worktree holds the mail and nothing else: never edit it by hand, since every command resets it to the remote's mail.

## At your start

```
bash bin/mail-session <inbox> "<your name>" <your session id>
```

It records your session in `mail/sessions.md`, so that whoever writes to your inbox can wake you.

## Reading

```
bash bin/mail-read <inbox> "<your name>" > <a file>
```

It takes the remote's mail, prints `to_all` since your last receipt and your inbox from your last receipt forward, stamps your receipt and pushes.  Read the file whole.  Never pipe the command into `tail` or `head`: the receipt is stamped whatever is shown.

## Sending

```
bash bin/mail-send <inbox> "<From>" "<To>" "<Message>"
bash bin/mail-send <inbox> "<From>" "<To>" -          the message on stdin
```

It takes the remote's mail, appends your entry, commits and pushes; when the push is refused, because another session pushed first, it does all of it again.  `Sent:` is printed only when the push has succeeded.  `NOT SENT` means nothing was delivered: send it again.

After `Sent:` it prints `Nudge:` and the recipient's line of `mail/sessions.md`.  Wake that session by its id: "Read your mail: <inbox>, <time> UTC".  Code, on the Mac, needs no nudge.

## The files

- `mail/<name>_inbox.md`: an inbox.  Append only, one whole entry at a time, in the order of the pushes; no command merges.
- `mail/to_all_inbox.md`: announcements, written by Coordination alone.
- `mail/sessions.md`: the session of each inbox.
- `bin/mail-lib`: what the three commands share.
