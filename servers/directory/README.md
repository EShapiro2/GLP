# GDS, the Grassroots Directory Service

The directory of GNA's Section 7, the contract GDS (`programs/examples/rendezvous/GDS.txt`), by hand: a page at `<address>/directory` listing people who chose to be found, by an email address the directory verified and a name of their choosing; an unlisted person found by email address and name; a click on a person a lookup, which gives an invitation link of theirs, as text to open in a Grassroots App (GRV:3).  The links are the person's own, made and registered with their rendezvous server by their app (GRV:2) and given to the directory, which keeps a buffer of unexpired ones, at least one, two more asked for each taken, each good for a day.

The directory is hosted as a rendezvous server is (GNA Section 7, "Hosting"): a headless layer under its own key, marked so that it answers unsolicited WAN contact from any key, and the directory's agent over it, `programs/examples/rendezvous/gds_agent.glp`, with the web server for the page and the mail for the verification, in one process over one store.

## What is where

- `programs/examples/rendezvous/gds_agent.glp`: the directory's agent, in GLP.  It takes a person's `list(A, Signed, Supply, Reply)`, `supply(A, Links)` and `unlist(A, Signed, Reply)` from their app (GNA Section 7, "The messages"), admits each by `signature/2`, answers a list with `verify` once the mail is sent or `rejected`, and asks the person's app for links with `more(N)` on the Supply stream of their latest list.  Its comment says what passes between it and its host.
- `lib/agent.dart`: the agent's host: the layer, the agent over it, and what passes between the agent and the store and the mail.
- `lib/store.dart`: the listings and their links, a JSON file written whole after each change: a request to list and its verification, unlisting, the links supplied, a lookup taking the link that expires first, and how many links to ask each person's app for.
- `lib/server.dart`: the page, the lookups and the verification link.
- `lib/mail.dart`: the verification mail, through an SMTP relay given by configuration, or written to the log where none is.
- `bin/directory_server.dart`: the server: the web server and the agent.
- `deploy/directory.service`: the systemd unit; Caddy's route is in `servers/peoplesweb/deploy/Caddyfile`.

## Build and test

The package depends on the glue package (`networking/glue`) for the adapter the agent runs over, and the glue on Flutter, so its dependencies are resolved by the Flutter SDK, and libsodium is needed, as for the rendezvous server.  From this directory:

    flutter test
    dart build cli -t bin/directory_server.dart -o build/directory

`dart test` runs them as well where `dart` is the Flutter SDK's.  The tests run the agent against a scripted app over the loopback, the server built and run as a process among them; the suite runs them in Section NET.

## Run

    build/directory/bundle/bin/directory_server --identity gds-identity.json --port 9518 \
      --public-address 203.0.113.7:9518 --outbound-queue-file gds-queue.json --state-file gds-state \
      --program <GLP>/programs/examples/rendezvous/gds_agent.glp \
      --root-self <GLP>/programs/self.glp [--rendezvous <key-hex>@<ip:port> ...]

`--identity` is the directory's key pair, a JSON file `{"seed": <hex>}` created on first run; `--port` and `--public-address` its server address, the UDP port and the static public `ip:port`; `--outbound-queue-file` the layer's fair-delivery queue; `--state-file` where the agent's local state is kept across a restart, as IGLP's Definition Restart requires, in the files `gds-state`, `gds-state.key` and `gds-state.program`; `--rendezvous` a rendezvous server its layer declares, as many as given.  It prints `READY <udp-port> <key-hex> <base>/directory` once listening.  A person's app is configured with the directory's key and address (GSA, Appendix A, Boot).

The web side takes the environment: `HOST` (default 127.0.0.1), `PORT` (default 8090), `BASE_URL` (the address people reach, for the mail's link; default `http://HOST:PORT`), `DATA` (the store, default `directory.json`), and the relay: `SMTP_HOST`, `SMTP_PORT` (default 587, STARTTLS), `SMTP_SSL` (`true` for TLS from the start, port 465), `SMTP_USER`, `SMTP_PASSWORD_FILE` (a file holding the password, outside the repository) and `MAIL_FROM` (default `directory@peoplesweb.org`).

## The server

On the peoplesweb VPS, as root, the clone at /srv/glp as for peoplesweb, with the Flutter SDK and libsodium installed as for the rendezvous server:

    cd /srv/glp/servers/directory && dart pub get && dart build cli -t bin/directory_server.dart -o /srv/directory
    mkdir -p /etc/glp /var/lib/glp && chown glp:glp /var/lib/glp
    cp /srv/glp/servers/directory/deploy/directory.service /etc/systemd/system/
    systemctl daemon-reload && systemctl enable --now directory
    cp /srv/glp/servers/peoplesweb/deploy/Caddyfile /etc/caddy/Caddyfile
    systemctl reload caddy

The firewall lets the directory's UDP port in.  `/etc/glp/directory.env`, readable by root alone, holds the directory's public address and the relay's settings:

    GDS_PUBLIC_ADDRESS=162.35.126.126:9518
    SMTP_HOST=smtp.example.net
    SMTP_PORT=587
    SMTP_USER=directory@peoplesweb.org
    SMTP_PASSWORD_FILE=/etc/glp/smtp-password
    MAIL_FROM=directory@peoplesweb.org

and the password is in `/etc/glp/smtp-password`, owned by `glp`, mode 600.  The mail needs a relay that accepts mail from the VPS for any address, whose sender domain is set up for peoplesweb.org, its SPF record allowing the provider, so that the mails are not taken for spam.  Without `SMTP_HOST` the mails are written to the journal (`journalctl -u directory`) and not sent.
