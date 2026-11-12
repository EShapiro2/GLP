// The site's one menu bar, the same on every page (Udi, 2026-11-12, to GSA
// #5): the site's name, the five tabs --- Home, Contracts, Papers, Projects
// and How it works --- and the search.  A page holds <header id="bar"
// data-tab="..."> and loads this file after it; data-tab names the tab the
// page is under, and setTab(name) changes it, as index.html does when it
// shows another of its tabs.  The first four tabs are parts of index.html,
// which shows each by the address after the #.  On a page other than
// index.html the search opens the contracts that match, on index.html
// (data-search), which reads what is typed itself.  The addresses are
// relative, so the bar works at the site's root and on the test site, under
// /test, which the bar marks as such.
(function(){
  const TABS = [['home', 'Home', './'], ['contracts', 'Contracts', './#contracts'], ['papers', 'Papers', './#papers'],
                ['projects', 'Projects', './#projects'], ['how', 'How it works', 'how.html']];
  const TEST = /\/test\//.test(location.pathname);
  document.head.insertAdjacentHTML('beforeend', `<style>
#bar{position:sticky;top:0;z-index:10;background:rgba(245,245,247,.82);-webkit-backdrop-filter:saturate(180%) blur(18px);backdrop-filter:saturate(180%) blur(18px);border-bottom:1px solid rgba(0,0,0,.06);font:15px/1.5 Inter,-apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif}
.bar-in{max-width:1120px;margin:0 auto;padding:8px 24px;display:flex;align-items:center;gap:6px 18px}
.bar-logo{font-weight:700;font-size:16px;letter-spacing:-.01em;display:flex;align-items:center;gap:9px;color:#1d1d1f;white-space:nowrap;text-decoration:none}
.bar-logo .mark{width:26px;height:26px;border-radius:8px;background:conic-gradient(from 200deg,#4f7bd9,#2bb3a0,#9b7bd4,#f0b54a,#4f7bd9);flex:none}
.bar-logo .test{font-size:11px;font-weight:700;letter-spacing:.06em;text-transform:uppercase;color:#a35b00;background:#fff7e8;border:1px solid #f3d39a;border-radius:6px;padding:1px 6px}
.bar-tabs{display:flex;gap:2px;margin-left:8px}
.bar-tabs a{padding:6px 12px;border-radius:999px;font-size:14.5px;font-weight:500;color:#6e6e73;white-space:nowrap;text-decoration:none}
.bar-tabs a:hover{color:#1d1d1f;background:rgba(118,118,128,.1)}
.bar-tabs a.on{color:#1d1d1f;font-weight:600;background:rgba(118,118,128,.16)}
.bar-search{margin-left:auto;position:relative}
.bar-search input{width:min(220px,40vw);padding:8px 12px 8px 32px;border:none;border-radius:10px;background:rgba(118,118,128,.12);font:inherit;font-size:14px;outline:none}
.bar-search input:focus{background:#fff;box-shadow:0 0 0 3px rgba(10,122,255,.25)}
.bar-search svg{position:absolute;left:10px;top:50%;transform:translateY(-50%);opacity:.5}
@media (max-width:900px){.bar-in{flex-wrap:wrap;padding:8px 16px}.bar-logo{font-size:14.5px}
  .bar-tabs{order:3;width:100%;margin:0 -6px;overflow-x:auto;scrollbar-width:none}.bar-tabs::-webkit-scrollbar{display:none}
  .bar-tabs a{padding:5px 10px;font-size:14px}.bar-search{flex:1 1 70px;max-width:150px;min-width:70px}.bar-search input{width:100%}}
@media (max-width:480px){.bar-tabs{justify-content:space-between;gap:0;margin:0 -4px;width:calc(100% + 8px)}.bar-tabs a{padding:5px 7px;font-size:13px}}
</style>`);
  const bar = document.getElementById('bar');
  bar.insertAdjacentHTML('afterbegin', `<div class="bar-in">
  <a class="bar-logo" href="./" data-t="home"><span class="mark"></span><span>The Grassroots App Store</span>${TEST ? '<span class="test" title="The test site: what it shows is not yet on the live site">Test</span>' : ''}</a>
  <nav class="bar-tabs">${TABS.map(([t, name, href]) => `<a href="${href}" data-t="${t}">${name}</a>`).join('')}</nav>
  <div class="bar-search"><svg width="14" height="14" viewBox="0 0 24 24"><circle cx="10" cy="10" r="7" stroke="#000" stroke-width="2.4" fill="none"/><path d="M15.5 15.5L21 21" stroke="#000" stroke-width="2.4" stroke-linecap="round"/></svg><input id="q" placeholder="Search contracts" autocomplete="off" aria-label="Search contracts"></div>
</div>`);
  window.setTab = t => bar.querySelectorAll('.bar-tabs a').forEach(a => {
    a.classList.toggle('on', a.dataset.t === t);
    if (a.dataset.t === t) a.setAttribute('aria-current', 'page'); else a.removeAttribute('aria-current'); });
  setTab(bar.dataset.tab || '');
  if (!('search' in bar.dataset))
    bar.querySelector('#q').addEventListener('keydown', e => {
      if (e.key === 'Enter' && e.target.value.trim()) location.href = './?q=' + encodeURIComponent(e.target.value.trim()) + '#contracts'; });
})();
