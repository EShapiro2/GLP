# GDS, the Grassroots Directory Service

The directory of GNA's Section 7, the contract GDS (`programs/examples/rendezvous/GDS.txt`), by hand: a page at `<address>/directory` listing people who chose to be found, by an email address the directory verified and a name of their choosing; an unlisted person found by email address and name; a click on a person a lookup, which gives an invitation link of theirs, as text to open in a Grassroots App (GRV:3).  The links are the person's own, made and registered with their rendezvous server by their app (GRV:2) and given to the directory, which keeps a buffer of unexpired ones, at least one, two more wanted for each taken, each good for a day.

What is here: the page, the verification mail and the store of listings and their links.  What is not yet: the directory's agent, which takes a person's List, List unlisted and Unlist from their app and the links it supplies, and asks it for more; it waits on GNA giving the messages between the app and the directory, as Section 6 gives the rendezvous server's.

## Build and run

With Dart, from this directory:

    dart pub get
    dart test
    dart run bin/directory_server.dart

Then open http://127.0.0.1:8090/directory.

## What is where

- `lib/store.dart`: the listings and their links, a JSON file written whole after each change: a request to list, its verification, unlisting, the links supplied, a lookup taking the link that expires first, and how many links are wanted of each person.
- `lib/server.dart`: the page, the lookups and the verification link.
- `lib/mail.dart`: the verification mail, through an SMTP relay given by configuration, or written to the log where none is.
- `bin/directory_server.dart`: the web server.
- `deploy/directory.service`: the systemd unit; Caddy's route is in `servers/peoplesweb/deploy/Caddyfile`.

## Environment

`HOST` (default 127.0.0.1), `PORT` (default 8090), `BASE_URL` (the address people reach, for the mail's link; default `http://HOST:PORT`), `DATA` (the store, default `directory.json`), and the relay: `SMTP_HOST`, `SMTP_PORT` (default 587, STARTTLS), `SMTP_SSL` (`true` for TLS from the start, port 465), `SMTP_USER`, `SMTP_PASSWORD_FILE` (a file holding the password, outside the repository) and `MAIL_FROM` (default `directory@peoplesweb.org`).

## The server

On the peoplesweb VPS, as root, the clone at /srv/glp as for peoplesweb:

    mkdir -p /etc/glp /var/lib/glp && chown glp:glp /var/lib/glp
    cp /srv/glp/servers/directory/deploy/directory.service /etc/systemd/system/
    systemctl daemon-reload && systemctl enable --now directory
    cp /srv/glp/servers/peoplesweb/deploy/Caddyfile /etc/caddy/Caddyfile
    systemctl reload caddy

The mail needs a relay that accepts mail from the VPS for any address: an account with a mail provider (host, port, user, password) whose sender domain is set up for peoplesweb.org, its SPF record allowing the provider, so that the mails are not taken for spam.  Its settings go in `/etc/glp/directory.env`, readable by root alone:

    SMTP_HOST=smtp.example.net
    SMTP_PORT=587
    SMTP_USER=directory@peoplesweb.org
    SMTP_PASSWORD_FILE=/etc/glp/smtp-password
    MAIL_FROM=directory@peoplesweb.org

and the password in `/etc/glp/smtp-password`, owned by `glp`, mode 600.  Without `SMTP_HOST` the mails are written to the journal (`journalctl -u directory`) and not sent.
