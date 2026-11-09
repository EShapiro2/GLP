# The debugging log service

The log service of `GLP/phones/docs/debugging.md`, "The log service": every debug build of the Grassroots App posts its events here, and every session reads them from here.  It is debugging infrastructure, not a grassroots platform and not GLP, and the app does not depend on it; a release build has no reporter.

## What it does

Every request carries the password as the header `Authorization: Bearer <password>`; one without it, or with another, is answered 401.

- `POST /log/<phone>/<run>`: a batch of events, one JSON object a line, as the reporter in the app writes them (`debugging.md`, "The phone side", 3).  Each line is appended to `<phone>/<run>.jsonl` wrapped with the batch's arrival time in Unix milliseconds, `{"arrived":<ms>,"event":<the line as sent>}`, and the answer is 204.  The service never reads the events: a line is copied byte for byte, and blank lines are dropped.  `<phone>` is the agent's key in hex and `<run>` the launch time in Unix milliseconds; any other name is answered 400.  A batch is at most 64 MiB, or `MAX_BATCH`; a longer one is answered 413 and not written.
- `GET /log/`: the phones and their runs in launch order, as JSON, each run with its size in bytes and its first and last event as stored.
- `GET /log/<phone>/<run>`: the run's file; `?from=<byte offset>` starts there.
- `GET /log/<phone>/<run>?follow`: the file, then what is appended to it, for as long as the reader stays; a run not yet begun is waited for.  With `from`, a follower that reconnects asks only for what it has not got; `phones/tools/follow.sh` does so.

The password is in the repository, `phones/tools/debuglog-password`, and is not secret (`phones/docs/debugging.md`, "For Udi's approval", 1).  With curl, from the repository's root:

    curl -sS -H "Authorization: Bearer $(cat phones/tools/debuglog-password)" https://peoplesweb.org/log/

## Build and run

With Dart, from this directory:

    dart pub get
    dart test
    DEBUGLOG_PASSWORD=<password> dart run bin/debuglog_server.dart

## What is where

- `lib/store.dart`: the runs, one file a run, the stamp and the listing.
- `lib/server.dart`: the requests, the password and the follower's stream.
- `bin/debuglog_server.dart`: the web server.
- `deploy/debuglog.service`: the systemd unit; Caddy's route is in `servers/peoplesweb/deploy/Caddyfile`, and the thirty days' retention in `servers/peoplesweb/deploy/sweep.sh`.

## Environment

`HOST` (default 127.0.0.1), `PORT` (default 8095), `DATA` (the directory of the runs, default `debuglog-data`), `DEBUGLOG_PASSWORD` (required: the service does not start without it) and `MAX_BATCH` (bytes, default 67108864).

## The server

On the peoplesweb VPS, as root, the clone at /srv/glp as for peoplesweb.  The service takes the repository's password from `/etc/glp/debuglog.env`, which systemd reads before it starts the service as `glp`:

    mkdir -p /etc/glp /var/lib/glp && chown glp:glp /var/lib/glp
    printf 'DEBUGLOG_PASSWORD=%s\n' "$(tr -d '\r\n' < /srv/glp/phones/tools/debuglog-password)" > /etc/glp/debuglog.env
    cp /srv/glp/servers/debuglog/deploy/debuglog.service /etc/systemd/system/
    systemctl daemon-reload && systemctl enable --now debuglog
    cp /srv/glp/servers/peoplesweb/deploy/Caddyfile /etc/caddy/Caddyfile
    systemctl reload caddy

`phones/tools/build.sh` and `build_sim.sh` build the same password into the debug app, and `phones/tools/follow.sh` reads with it.

A new password is a commit of `phones/tools/debuglog-password`, the `printf` line above and `systemctl restart debuglog` on the VPS after the sweep has pulled it, and a new debug build on every phone.  The sweep deletes runs whose last event is more than thirty days old; the runs are never in git.
