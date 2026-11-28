"""The site's tab icon, web/favicon.ico: a globe in the site's colours, drawn for each size on its own, so that the smallest keeps only the lines that still show (Udi, 2026-11-12, to GSA #4: a little globe icon for the tab, easy to find).  Run it to remake the icon: python3 -I tools/favicon.py; it needs Pillow."""
import math
import os

from PIL import Image, ImageDraw

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
# The colours of the site's mark, the conic gradient beside its name.
STOPS = [(0.00, (245, 166, 35)), (0.25, (22, 148, 130)), (0.50, (10, 122, 255)), (0.75, (138, 99, 210)), (1.00, (245, 166, 35))]


def colour(t):
    for (a, ca), (b, cb) in zip(STOPS, STOPS[1:]):
        if a <= t <= b:
            f = (t - a) / (b - a)
            return tuple(round(x + (y - x) * f) for x, y in zip(ca, cb))
    return STOPS[0][1]


def globe(size, parallels, meridians):
    """A globe of that size: a disc in the mark's colours with no rim, so that it shows on a light tab and on a dark one, its parallels and meridians in white, drawn eight times larger and reduced."""
    k = 8
    n = size * k
    im = Image.new('RGBA', (n, n), (0, 0, 0, 0))
    c, r = n / 2, n / 2 - k * 0.5
    px = im.load()
    for y in range(n):
        for x in range(n):
            dx, dy = x + 0.5 - c, y + 0.5 - c
            if dx * dx + dy * dy <= r * r:
                px[x, y] = colour((math.atan2(dy, dx) / (2 * math.pi)) % 1.0) + (255,)
    d = ImageDraw.Draw(im)
    w = max(k, round(n / 20))
    white = (255, 255, 255, 235)
    for lat in parallels:          # a parallel, a horizontal chord of the disc
        y = c - r * math.sin(math.radians(lat))
        h = r * math.cos(math.radians(lat))
        d.line([(c - h, y), (c + h, y)], fill=white, width=w)
    for lon in meridians:          # a meridian, an ellipse through the two poles
        a = r * math.sin(math.radians(lon))
        if a < 1:
            d.line([(c, c - r), (c, c + r)], fill=white, width=w)
        else:
            d.ellipse([c - a, c - r, c + a, c + r], outline=white, width=w)
    return im.resize((size, size), Image.LANCZOS)


if __name__ == '__main__':
    big = globe(48, [-40, 0, 40], [0, 50])
    mid = globe(32, [-40, 0, 40], [0, 50])
    small = globe(16, [0], [0, 55])
    big.save(os.path.join(HERE, 'web', 'favicon.ico'), sizes=[(48, 48), (32, 32), (16, 16)], append_images=[mid, small])
