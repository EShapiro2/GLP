#!/usr/bin/env python3
"""The iOS Simulator's screen taken as a phone's screenshot, for the screens of
the apps' figures (programs/apps/<app>/screens/) and of CSSN's Figure 3
(programs/cssn/figures/).

    python3 tool/sim_shot.py serve <udid> <programs> <raw-dir> <port-file>
    python3 tool/sim_shot.py frame <raw.png> <out.png>

serve: an HTTP service on 127.0.0.1, its port written to <port-file>, started
by tool/sim_screens.sh and asked by integration_test/apps_screens_test.dart,
GET /shot?app=<app>&dir=<dir>&file=<file>, at each screen of the scenario,
<dir> the directory under <programs> the screen is written to, by default
apps/<app>/screens.  It takes the simulator's screenshot, `xcrun simctl io
<udid> screenshot --mask=alpha`: the display as the simulator draws it, the
status bar included, its rounded corners and its sensor housing transparent.
It keeps it as <raw-dir>/<app>/<file>, frames it (below) into
<programs>/<dir>/<file>, and answers once that is written, so the scenario
goes on only after its screen is taken.

frame: the screenshot set in the phone's black bezel.  The simulator's
screenshot is the display alone --- the Simulator app's device chrome is not in
it --- so the bezel is drawn here: a black rounded rectangle around the display,
as wide on every side, its width the one that makes the phone 1080 by 2220 as
vGLP's figure of the app is (vGLP/figures/gsg-app-friends.png), its corner
radius the display's plus that width; the display's corner radius read off the
mask, the radius of the circle that leaves the gap the mask leaves along the
corner's diagonal.  The display is laid over it, so its masked corners and its
sensor housing show the bezel's black, and the phone is scaled to 1080 by 2220
and set on white, as vGLP's figure is.
"""
import math
import os
import re
import subprocess
import sys
from http.server import BaseHTTPRequestHandler, HTTPServer
from urllib.parse import parse_qs, urlparse

from PIL import Image, ImageDraw

SIZE = (1080, 2220)  # the phone, as vGLP's figure of the app
SUPER = 4  # the bezel's outline drawn at 4 times the size, then scaled down


def frame(raw, out):
    display = Image.open(raw).convert("RGBA")
    w, h = display.size
    pw, ph = SIZE
    # The bezel's width b: (w + 2b) / (h + 2b) = pw / ph.
    b = round((pw * h - ph * w) / (2 * (ph - pw)))
    if b <= 0:
        raise ValueError(f"{raw}: a display of {w} by {h} is wider than {pw}:{ph}")
    alpha = display.getchannel("A")
    gap = next((i for i in range(min(w, h) // 2) if alpha.getpixel((i, i)) >= 128), None)
    if gap is None or gap == 0:
        raise ValueError(f"{raw}: no rounded corner in the mask; take it with --mask=alpha")
    radius = gap / (1 - 1 / math.sqrt(2)) + b
    W, H = w + 2 * b, h + 2 * b
    outline = Image.new("L", (W * SUPER, H * SUPER), 0)
    ImageDraw.Draw(outline).rounded_rectangle(
        [0, 0, W * SUPER - 1, H * SUPER - 1], radius=radius * SUPER, fill=255)
    outline = outline.resize((W, H), Image.LANCZOS)
    phone = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    phone.paste((0, 0, 0, 255), (0, 0), outline)
    phone.alpha_composite(display, (b, b))
    phone = phone.resize(SIZE, Image.LANCZOS)
    page = Image.new("RGB", SIZE, (255, 255, 255))
    page.paste(phone, (0, 0), phone.getchannel("A"))
    os.makedirs(os.path.dirname(os.path.abspath(out)), exist_ok=True)
    page.save(out, optimize=True)
    return b, gap


def serve(udid, programs, raw_dir, port_file):
    name = re.compile(r"^[a-z0-9][a-z0-9_-]*$")
    png = re.compile(r"^[a-z0-9][a-z0-9_-]*\.png$")
    path = re.compile(r"^[a-z0-9][a-z0-9_-]*(/[a-z0-9][a-z0-9_-]*)*$")

    class Shot(BaseHTTPRequestHandler):
        def answer(self, status, text):
            body = (text + "\n").encode()
            self.send_response(status)
            self.send_header("Content-Type", "text/plain; charset=utf-8")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)

        def do_GET(self):
            url = urlparse(self.path)
            q = parse_qs(url.query)
            app = (q.get("app") or [""])[0]
            file = (q.get("file") or [""])[0]
            where = (q.get("dir") or [f"apps/{app}/screens"])[0]
            if (url.path != "/shot" or not name.match(app) or not png.match(file)
                    or not path.match(where)):
                return self.answer(400, f"not a screen: {self.path}")
            raw = os.path.join(raw_dir, app, file)
            os.makedirs(os.path.dirname(raw), exist_ok=True)
            took = subprocess.run(
                ["xcrun", "simctl", "io", udid, "screenshot", "--type=png",
                 "--mask=alpha", raw],
                capture_output=True, text=True)
            if took.returncode != 0:
                return self.answer(500, f"simctl: {took.stderr.strip()}")
            out = os.path.join(programs, where, file)
            try:
                b, gap = frame(raw, out)
            except Exception as e:  # reported to the scenario, which fails on it
                return self.answer(500, f"frame: {e}")
            size = Image.open(raw).size
            self.answer(200, f"wrote programs/{where}/{file} "
                             f"(display {size[0]}x{size[1]}, bezel {b}, corner gap {gap})")

        def log_message(self, fmt, *args):
            sys.stderr.write("sim_shot: " + (fmt % args) + "\n")

    server = HTTPServer(("127.0.0.1", 0), Shot)
    with open(port_file + ".part", "w") as f:
        f.write(str(server.server_address[1]))
    os.replace(port_file + ".part", port_file)
    server.serve_forever()


def main(argv):
    if len(argv) == 6 and argv[1] == "serve":
        serve(*argv[2:])
    elif len(argv) == 4 and argv[1] == "frame":
        b, gap = frame(argv[2], argv[3])
        print(f"wrote {argv[3]} (bezel {b}, corner gap {gap})")
    else:
        sys.exit(__doc__)


if __name__ == "__main__":
    main(sys.argv)
