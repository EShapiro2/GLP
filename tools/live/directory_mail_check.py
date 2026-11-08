#!/usr/bin/env python3
"""Parts 2 and 3 of the live suite (GSA #2's task for GNA, Code_inbox.md
2026-11-08 12:51 UTC): the directory, GDS (GNA Section 7, servers/directory),
and the mail service on the VPS that its verification mail needs.

    directory_mail_check.py <base-url> <vps-ssh-target> [--mail-to ADDRESS]

Prints one line per check, RESULT <id> PASS|FAIL|PENDING|INFO <what>.  Reads
the VPS by SSH and changes nothing on it; reads DNS over HTTPS (dns.google),
since the Mac's network answers no DNS query sent to a name server directly
(Server log, step 4).

Part 2 runs its checks of the page once the directory answers at
<base-url>/directory; the flow of a listing --- List, the verification mail and
its link, the supply of links, the lookups, the redemption at the rendezvous
server, Unlist, a restart, the mail rate --- runs through the directory's agent,
which takes the person's acts and the links from the app (GNA Section 7, "The
messages"), and is PENDING until the agent is built and deployed.

Part 3 checks the route a verification mail takes off the VPS: the relay the
directory is configured with (/etc/glp/directory.env, whose presence and
SMTP_HOST alone are read, never the password), the reachability of the
relay's submission port and of port 25 from the VPS, and peoplesweb.org's
MX, SPF, DMARC, DKIM and reverse DNS.  The mail sent and received waits on a
relay and on an address Udi names (--mail-to); whether it arrived only its
recipient can say.
"""
import json
import subprocess
import sys
import urllib.error
import urllib.parse
import urllib.request

BASE = sys.argv[1].rstrip('/')
VPS = sys.argv[2]
MAIL_TO = sys.argv[sys.argv.index('--mail-to') + 1] if '--mail-to' in sys.argv else None
DOMAIN = urllib.parse.urlparse(BASE).hostname
VPS_IP = '162.35.126.126'
UA = {'User-Agent': 'glp-live-suite/1 (GSA #2 for GNA)'}


def result(i, v, what):
    print(f'RESULT {i} {v} {what}', flush=True)


def http(url, data=None):
    req = urllib.request.Request(url, data=data, headers=UA)
    try:
        with urllib.request.urlopen(req, timeout=30) as r:
            return r.status, r.read().decode('utf-8', 'replace')
    except urllib.error.HTTPError as e:
        return e.code, e.read().decode('utf-8', 'replace')
    except Exception as e:  # noqa: BLE001 - reported
        return f'{type(e).__name__}: {e}', ''


def ssh(cmd):
    p = subprocess.run(['ssh', '-o', 'BatchMode=yes', '-o', 'ConnectTimeout=10', VPS, cmd],
                       capture_output=True, text=True, timeout=60)
    return p.stdout.strip()


def dns(name, rtype):
    s, body = http(f'https://dns.google/resolve?name={urllib.parse.quote(name)}&type={rtype}')
    if s != 200:
        return None
    return [a['data'] for a in json.loads(body).get('Answer', [])
            if a.get('type') == {'MX': 15, 'TXT': 16, 'PTR': 12, 'A': 1}[rtype]]


# ---------------------------------------------------------------- part 2

FLOW = [
    ('2.2-list-verify', 'a listing by List: the verification mail sent and its link opened'),
    ('2.3-listing-kept', 'the listing kept once verified'),
    ('2.4-supply', 'the supply of links asked, more(N), and answered, supply(A, Links)'),
    ('2.5-lookup-name-email', 'a lookup on the page of a listed person by name, and of one '
                              'listed without a name by email address'),
    ('2.6-unlisted', 'an unlisted person found by email address and name'),
    ('2.7-link-redeemed', 'the link handed out and redeemed at the rendezvous server (GRV:3)'),
    ('2.8-unlist', 'Unlist: the listing and its links gone, Supply closed'),
    ('2.9-restart', 'the listings and links kept across a restart, the buffer asked again '
                    'when the app next connects'),
    ('2.10-mail-rate', 'at most one verification mail per address per hour'),
]


def part2():
    s, body = http(f'{BASE}/directory')
    deployed = s == 200 and 'Grassroots Directory' in body
    unit = ssh('systemctl is-active directory 2>&1; systemctl is-enabled directory 2>&1 | head -1')
    route = ssh("grep -c 'reverse_proxy /directory' /etc/caddy/Caddyfile")
    if not deployed:
        result('2.0-deployed', 'PENDING',
               f'{BASE}/directory answers {s}; the unit directory is '
               f'"{" / ".join(unit.splitlines())}", Caddy routes /directory: '
               f'{"yes" if route not in ("", "0") else "no"}: not deployed '
               f'(servers/directory/README.md, "The server")')
    else:
        result('2.0-deployed', 'PASS', f'{BASE}/directory answers 200 with the page')
        s, b = http(f'{BASE}/directory/find',
                    urllib.parse.urlencode({'email': 'nobody@invalid.example',
                                            'name': 'Nobody'}).encode())
        result('2.1-find-unknown', 'PASS' if s == 200 and 'Nobody is listed' in b else 'FAIL',
               f'a lookup by an email address and name nobody listed: {s}')
        s, b = http(f'{BASE}/directory/lookup', b'id=0000000000000000')
        result('2.1-lookup-unknown', 'PASS' if 'No such listing' in b else 'FAIL',
               f'a lookup of a listing that does not exist: {s}')
        s, b = http(f'{BASE}/directory/verify?t=00')
        result('2.1-verify-forged', 'PASS' if 'not one the directory sent' in b else 'FAIL',
               f'a verification link the directory did not send: {s}')
    for i, what in FLOW:
        result(i, 'PENDING',
               f'{what}: runs through the directory\'s agent, which takes List, '
               f'List unlisted, Unlist and the links from the app (GNA Section 7, '
               f'"The messages", 2ce8e10) and is not yet built'
               + ('' if deployed else ', and the directory is not deployed'))


# ---------------------------------------------------------------- part 3

def part3():
    env = ssh('test -f /etc/glp/directory.env && sed -n "s/^SMTP_HOST=//p;s/^SMTP_PORT=//p" '
              '/etc/glp/directory.env || echo NONE')
    pw = ssh('test -f /etc/glp/smtp-password && stat -c "%U %a" /etc/glp/smtp-password || echo NONE')
    relay = None if env in ('', 'NONE') else env.splitlines()
    result('3.1-relay-configured', 'PASS' if relay else 'FAIL',
           f'/etc/glp/directory.env: {"SMTP_HOST " + " port ".join(relay) if relay else "absent"}; '
           f'/etc/glp/smtp-password: {pw if pw != "NONE" else "absent"} '
           f'(Mail Baby ordered 2026-11-08, provisioning ticket UXJ-815-98113)')
    pf = ssh('postconf relayhost default_transport inet_interfaces myhostname 2>&1')
    result('3.2-postfix', 'INFO',
           'the provider image\'s Postfix, not the route: ' + '; '.join(pf.splitlines()))
    # The SMTP greeting read, and QUIT: no mail is sent.  A connection that
    # opens and gives no greeting is the provider's block, which accepts the
    # connection and passes nothing.
    banner = ssh('timeout 10 bash -c "exec 3<>/dev/tcp/gmail-smtp-in.l.google.com/25; '
                 'head -n 1 <&3; echo QUIT >&3" 2>&1')
    result('3.3-port-25', 'INFO',
           'outbound port 25 from the VPS to gmail-smtp-in.l.google.com: '
           + (f'greeting "{banner.splitlines()[0]}"' if banner.startswith('220')
              else 'no SMTP greeting within 10s (blocked)')
           + '; direct delivery is not the route, a relay is')
    if relay:
        host, port = relay[0], (relay[1] if len(relay) > 1 else '587')
        sub = ssh(f'timeout 10 bash -c "exec 3<>/dev/tcp/{host}/{port}; head -n 1 <&3; '
                  f'echo QUIT >&3" 2>&1')
        ok = sub.startswith('220')
        result('3.4-relay-reachable', 'PASS' if ok else 'FAIL',
               f'{host}:{port} from the VPS: '
               + (f'greeting "{sub.splitlines()[0]}"' if ok else 'no SMTP greeting within 10s'))
    else:
        result('3.4-relay-reachable', 'PENDING', 'no relay configured to reach')

    mx = dns(DOMAIN, 'MX')
    txt = dns(DOMAIN, 'TXT') or []
    spf = [t for t in txt if 'v=spf1' in t]
    dmarc = [t for t in (dns(f'_dmarc.{DOMAIN}', 'TXT') or []) if 'v=DMARC1' in t]
    ptr = dns('.'.join(reversed(VPS_IP.split('.'))) + '.in-addr.arpa', 'PTR')
    result('3.5-spf', 'PASS' if spf else 'FAIL',
           f'SPF of {DOMAIN}: {spf or "none"}; a relay sending as {DOMAIN} needs its include here')
    result('3.6-dkim', 'PENDING',
           f'DKIM of {DOMAIN}: the selector is the relay\'s, given on its page once '
           f'provisioned; none is published to check')
    result('3.7-dmarc', 'PASS' if dmarc else 'INFO',
           f'DMARC of {DOMAIN}: {dmarc or "none"} (recommended, not required)')
    result('3.8-mx', 'INFO',
           f'MX of {DOMAIN}: {mx or "none"}; the directory receives no mail, so none is '
           f'needed, but a bounce to directory@{DOMAIN} has nowhere to go')
    result('3.9-ptr', 'INFO',
           f'reverse DNS of {VPS_IP}: {ptr}; a relay sends from its own addresses, so this '
           f'matters only for direct delivery')
    if MAIL_TO and relay:
        result('3.10-mail-sent', 'PENDING',
               f'a verification mail to {MAIL_TO} is sent by a List of that address '
               f'through the directory\'s agent, not yet built; then its recipient '
               f'confirms it arrived')
    else:
        result('3.10-mail-sent', 'PENDING',
               'a verification mail sent and received: waits on '
               + ' and '.join(x for x, ok in [('the relay (Mail Baby, provisioning)', relay),
                                               ('an address Udi names', MAIL_TO)] if not ok)
               + '; its arrival is the recipient\'s to confirm')


part = sys.argv[3] if len(sys.argv) > 3 and sys.argv[3] in ('2', '3') else None
if part in (None, '2'):
    part2()
if part in (None, '3'):
    part3()
