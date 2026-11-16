"""The QR code of the site's address on the home page, web/qr.svg, for sharing it by a phone's camera (Udi, 2026-11-16, to Website #2).  Made once, with segno (pip install segno), and committed, as the favicon and og.jpg are; run again only if the address changes: python3 tools/qr.py."""
import os
import segno

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
segno.make('https://peoplesweb.org', error='m').save(os.path.join(HERE, 'web', 'qr.svg'), scale=4, border=2, dark='#1d1d1f', light='#ffffff', xmldecl=False, svgns=True, nl=False)
