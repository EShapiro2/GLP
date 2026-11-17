#!/bin/bash
# A replay page of GDO simulation records, for the eye (programs/sglp/lbm/sim/README.md):
#
#   bash programs/sglp/lbm/sim/replay.sh <out.html> <record> [<record> ...]
#
# each <record> a directory holding 1.log and 1.census (census.sh's map, its
# first line the configuration), and a map.svg where the town has a background
# to draw, the island and its streets for Manhattan; the page shows, for the record chosen, the
# persons at their locations on the day the slider stands at (travelling as
# GDO_sglp.glp's gd_pos_at moves them), the friendships (grey), the pointings to
# a friend (blue) and the transitive pointings (red), replayed from the views
# in the log.  Self-contained: the data is inlined, nothing is fetched.
set -u
OUT=$1; shift
{
cat <<'HTML'
<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>GDO Overlay Replay</title>
<style>
body{margin:0;font:14px/1.45 system-ui,sans-serif;background:#fafaf7;color:#1f2328;padding:16px}
h1{font-size:18px;margin:0 0 4px}.sub{color:#6b7280;margin:0 0 12px;max-width:700px}
.row{display:flex;gap:12px;align-items:center;flex-wrap:wrap;margin-bottom:10px}
select,button{font:inherit;padding:4px 8px}input[type=range]{flex:1;min-width:200px}
svg{width:min(92vw,680px);height:auto;background:#fff;border:1px solid #e5e7eb;border-radius:8px;display:block}
svg.tall{width:auto;height:min(88vh,1000px);max-width:92vw}
.legend span{display:inline-block;margin-right:14px}.sw{display:inline-block;width:22px;height:3px;vertical-align:middle;margin-right:5px}
.dot{display:inline-block;width:10px;height:10px;border-radius:5px;vertical-align:middle;margin-right:5px}
.stats{color:#6b7280;margin-top:8px}
.sum{max-width:900px;font-size:14px;line-height:1.5}.sum p{margin:6px 0}.sum b{color:#374151}
h2{font-size:17px;margin:28px 0 4px}h3{font-size:14px;margin:18px 0 4px;color:#374151}
table{border-collapse:collapse;font-size:13px;margin-top:8px}th,td{border-bottom:1px solid #e5e7eb;padding:4px 10px;text-align:right;white-space:nowrap}th{color:#6b7280;font-weight:600}td:first-child,th:first-child{text-align:left}tr.sel td{background:#eef2ff;font-weight:600}
</style></head><body>
<h1>GDO overlay, replayed from the simulation logs</h1>
<p class="sub">Travelling persons are orange, with the trail of their last days; the others stay put. Grey lines are friendships, blue arrows pointings to a friend, red arrows transitive pointings. A pointing just made flashes green for half a day; one just ended fades out dashed. The travellers go from place to place along the streets. Under the map: the overlay against the optimum, the graph in which everyone points to their d nearest. Drag the slider or press Play (a day takes about four seconds).</p>
<div class="row"><label>Run <select id="run"></select></label><button id="play">Play</button><input type="range" id="t" min="0" max="2000" value="0"><span id="time"></span></div>
<svg id="map" viewBox="-300 -300 10600 10600"></svg>
<div class="legend"><span><i class="dot" style="background:#f97316"></i>travels</span><span><i class="dot" style="background:#111"></i>stays</span><span><i class="sw" style="background:#c9ccd1"></i>friendship</span><span><i class="sw" style="background:#1d4ed8"></i>points to a friend</span><span><i class="sw" style="background:#dc2626"></i>transitive pointing</span><span><i class="sw" style="background:#16a34a"></i>just made</span></div>
<div class="stats" id="stats"></div>
<h2>Summary of the run</h2>
<div class="sum" id="sum"></div>
<h2>Against the optimum</h2>
<p class="sub">The optimum is the graph in which every person points to its d nearest persons of all, at the positions of the moment; the ideal of friends, the one in which every person points to its d nearest friends. Stretch is the mean pointing edge over the optimum's mean; optimal edges, the pointings that are to one of the person's d nearest of all, as a share of the pointings. From each record's <code>measures.txt</code> (<code>overlay.awk</code>), at the end of the run; the chosen run's row is marked, and its rows by day follow.</p>
<div style="overflow:auto"><table id="all"></table></div>
<h3 id="daysTitle"></h3>
<table id="days"></table>
<script>
const RUNS=[
HTML
first=1
for R in "$@"; do
    [ $first = 1 ] || echo ","
    first=0
    R="$R" awk -F'\t' -v name="$(basename "$R")" '
    BEGIN { printf "{\"name\":\"%s\"", name; n = 0; cfg = ",\"cfg\":{\"metric\":\"l1\",\"d\":3,\"e\":0,\"mapseed\":0,\"step\":0}"; persons = ""
        while ((getline l < (ENVIRON["R"] "/1.census")) > 0) { nf = split(l, f, " ")
            if (f[1] == "#") { cfg = sprintf(",\"cfg\":{\"metric\":\"%s\",\"d\":%d,\"e\":%d,\"mapseed\":%d,\"step\":%d}", f[4], f[7], f[8], f[9], f[10]); continue }
            persons = persons (n++ ? "," : "") sprintf("{\"a\":%d,\"k\":\"%s\",\"x\":%d,\"y\":%d,\"w\":%d}", f[1], f[2], f[3], f[4], (nf >= 6 ? f[6] : 1)) }
        printf "%s,\"persons\":[%s],\"events\":[", cfg, persons; m = 0; horizon = 0 }
    { if ($1 + 0 > horizon) horizon = $1 + 0 }
    $3 == "output" && $4 ~ /^view\(/ { s = $4; sub(/^view\(\[/, "", s); sub(/\].*$/, "", s); p = ""; fr = ""
        while (match(s, /(points_to|friend)\([a-z0-9]+\)/)) { atom = substr(s, RSTART, RLENGTH); q = substr(atom, index(atom, "(") + 1); sub(/\)/, "", q)
            if (atom ~ /^points_to/) p = p (p ? "," : "") "\"" q "\""; else fr = fr (fr ? "," : "") "\"" q "\""; s = substr(s, RSTART + RLENGTH) }
        key = $2 "|" p "|" fr; if (last[$2] == key) next; last[$2] = key
        printf "%s[%d,%d,[%s],[%s]]", (m++ ? "," : ""), $1, $2, p, fr }
    END { printf "],\"horizon\":%d", horizon }' "$R/1.log"
    # The measures (overlay.awk's measures.txt): the rows by day and the summary line, for the tables under the map.
    if [ -f "$R/summary.md" ]; then printf ',"summary_md":"'; sed -e 's/\\/\\\\/g; s/"/\\"/g' "$R/summary.md" | awk '{ printf "%s\\n", $0 }'; printf '"'; fi
    if [ -f "$R/measures.txt" ]; then awk '
        BEGIN { printf ",\"days\":[" }
        $1 == "day" { printf "%s[%d,%d,%d,%d,%d,%s,%d,%d,%d]", (n++ ? "," : ""), $2, $3, $4, $5, $6, $7, $8, $9, $10 }
        $1 == "summary" { sm = ""; for (i = 2; i <= NF; i++) { split($i, kv, "="); sm = sm (i > 2 ? "," : "") "\"" kv[1] "\":\"" kv[2] "\"" } }
        END { printf "],\"summary\":{%s}", sm }' FS='[ \t%]+' "$R/measures.txt"; fi
    if [ -f "$R/map.svg" ]; then printf ',"side":21000,"bg":"'; sed -e 's/\\/\\\\/g; s/"/\\"/g' "$R/map.svg" | tr -d '\n'; printf '"}'; else printf ',"side":10000}'; fi
done
cat <<'HTML'
];
const sel=document.getElementById('run'),slider=document.getElementById('t'),svg=document.getElementById('map'),timeEl=document.getElementById('time'),stats=document.getElementById('stats');
RUNS.forEach((r,i)=>{const o=document.createElement('option');o.value=i;o.textContent=r.name;sel.appendChild(o)});
const M=2147483647;
function dist(r,a,b){const dx=Math.abs(a.x-b.x),dy=Math.abs(a.y-b.y);return r.cfg.metric==='l2'?Math.hypot(dx,dy):dx+dy}
// The trips, as gd_pos_at: a destination drawn by the minimal standard generator (in Manhattan one of the drivers' places,
// the k-th person's start, k = s mod 200 + 1; on a square a point drawn uniformly), Step a day towards it along the grid,
// x first, then the next destination.  The positions of every day, computed once per run.
function tripTable(r){ if(r.table) return r.table; const days=Math.ceil(r.horizon/86400)+1; r.table={}; const st=r.cfg.step, side=r.side, manh=!!r.bg;
  const lcg=s=>Number((BigInt(s)*48271n)%2147483647n);
  for(const p of r.persons){ const pts=[{x:p.x,y:p.y}]; let s=p.x*10007+p.y+r.cfg.mapseed+1,x=p.x,y=p.y,tx,ty;
    const target=()=>{ if(manh){s=lcg(s); const q=r.persons[s%200]; tx=q.x; ty=q.y} else {s=lcg(s); const s2=lcg(s); tx=s%(side+1); ty=s2%(side+1); s=s2} };
    const towards=(v,tv,bd)=>tv-v>bd?v+bd:(v-tv>bd?v-bd:tv);
    if(p.w&&st) target();
    for(let k=1;k<=days;k++){ if(!p.w||!st){pts.push({x,y});continue}
      const used=Math.min(Math.abs(tx-x),st), rem=st-used; x=towards(x,tx,st); y=towards(y,ty,rem); if(x===tx&&y===ty) target(); pts.push({x,y}) }
    r.table[p.k]=pts } return r.table }
function posAt(r,p,t){ const T=tripTable(r)[p.k]; const d=t/86400, k=Math.floor(d), f=d-k; const a=T[Math.min(k,T.length-1)], b=T[Math.min(k+1,T.length-1)];
  return {x:a.x+(b.x-a.x)*f, y:a.y+(b.y-a.y)*f} }
function render(){
  const r=RUNS[sel.value]; const t=slider.value/2000*r.horizon; const H=r.side;
  if(r.bg){svg.setAttribute('viewBox','-700 -300 6400 21600');svg.classList.add('tall')}else{svg.setAttribute('viewBox','-300 -300 10600 10600');svg.classList.remove('tall')}
  const Y=y=>H-y;  // north up
  const P={}; r.persons.forEach(p=>{P[p.k]=Object.assign({k:p.k,w:p.w},posAt(r,p,t))});
  // The state at t, and when each pointing was made or ended.
  const pts={},fr={},made={},ended={};
  for(const e of r.events){ if(e[0]>t)break; const a=e[1]; const old=new Set(pts[a]||[]); const now=new Set(e[2]);
    for(const q of now) if(!old.has(q)) {made[a+'>'+q]=e[0]; delete ended[a+'>'+q]}
    for(const q of old) if(!now.has(q)) {ended[a+'>'+q]=e[0]}
    pts[a]=e[2]; fr[a]=e[3] }
  const half=43200;
  let out=(r.bg?`<g transform="translate(0,${H}) scale(1,-1)">${r.bg}</g>`:'')+'<defs><marker id="mb" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="4" markerHeight="4" orient="auto"><path d="M0,0L10,5L0,10z" fill="#1d4ed8"/></marker><marker id="mr" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="4" markerHeight="4" orient="auto"><path d="M0,0L10,5L0,10z" fill="#dc2626"/></marker><marker id="mg" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="4" markerHeight="4" orient="auto"><path d="M0,0L10,5L0,10z" fill="#16a34a"/></marker></defs>';
  for(const p of r.persons){const a=P[p.k];for(const q of (fr[p.a]||[])){const b=P[q];if(b&&p.k<q)out+=`<line x1="${a.x}" y1="${Y(a.y)}" x2="${b.x}" y2="${Y(b.y)}" stroke="#b9bec6" stroke-width="35"/>`}}
  // trails of the travellers: the last five days
  for(const p of r.persons) if(p.w&&r.cfg.step){ const T=tripTable(r)[p.k]; const k=Math.floor(t/86400); const a=P[p.k]; let d='';
    for(let j=Math.max(0,k-5);j<=k;j++) d+=(d?' L':'M')+T[j].x+' '+Y(T[j].y); d+=' L'+a.x+' '+Y(a.y);
    out+=`<path d="${d}" fill="none" stroke="#fdba74" stroke-width="60" stroke-linecap="round" stroke-linejoin="round" opacity="0.8"/>` }
  // pointings that just ended, fading
  for(const key in ended){ const age=t-ended[key]; if(age>half) continue; const [a,q]=key.split('>'); const pa=P[r.persons[a-1].k], pb=P[q]; if(!pa||!pb) continue;
    out+=`<line x1="${pa.x}" y1="${Y(pa.y)}" x2="${pb.x}" y2="${Y(pb.y)}" stroke="#9ca3af" stroke-width="45" stroke-dasharray="160 120" opacity="${(1-age/half).toFixed(2)}"/>` }
  let edges=0,trans=0,len=0,fresh=0,opt=0,optLen=0,optE=0;
  for(const p of r.persons){const a=P[p.k],f=new Set(fr[p.a]||[]);
    // the d nearest of all
    const near=r.persons.filter(q=>q!==p).map(q=>[dist(r,a,P[q.k]),q.k]).sort((u,v)=>u[0]-v[0]).slice(0,r.cfg.d); const nearSet=new Set(near.map(u=>u[1])); for(const u of near){optLen+=u[0];optE++}
    for(const q of (pts[p.a]||[])){const b=P[q];if(!b)continue;edges++;len+=dist(r,a,b);if(nearSet.has(q))opt++;const isT=!f.has(q);if(isT)trans++;
    const age=t-(made[p.a+'>'+q]||0), isNew=age<half; if(isNew) fresh++;
    const dx=b.x-a.x,dy=b.y-a.y,L=Math.hypot(dx,dy)||1; const col=isNew?'#16a34a':(isT?'#dc2626':'#1d4ed8'), mk=isNew?'mg':(isT?'mr':'mb');
    out+=`<line x1="${a.x}" y1="${Y(a.y)}" x2="${b.x-dx/L*160}" y2="${Y(b.y-dy/L*160)}" stroke="${col}" stroke-width="${isNew?90:(isT?55:45)}" marker-end="url(#${mk})"/>`}}
  for(const p of r.persons){const a=P[p.k]; const walks=p.w&&r.cfg.step;
    out+=walks?`<circle cx="${a.x}" cy="${Y(a.y)}" r="150" fill="#f97316" stroke="#fff" stroke-width="40"/>`:`<circle cx="${a.x}" cy="${Y(a.y)}" r="110" fill="#111"/>`;
    if(r.persons.length<=60) out+=`<text x="${a.x+150}" y="${Y(a.y)-130}" font-size="220" fill="#6b7280">${p.k}</text>`}
  svg.innerHTML=out; timeEl.textContent='day '+(t/86400).toFixed(2);
  stats.textContent=`${edges} pointing edges (${trans} transitive, ${fresh} made in the last half day), mean length ${edges?Math.round(len/edges):0} m (${r.cfg.metric}) against the optimum's ${optE?Math.round(optLen/optE):0} m — stretch ${edges&&optE?(len/edges/(optLen/optE)).toFixed(2):'–'}, ${edges?Math.round(100*opt/edges):0}% of the pointings optimal; d=${r.cfg.d}, e=${r.cfg.e} m, step ${r.cfg.step} m/day, ${r.persons.filter(p=>p.w).length} travelling`}
function summary(){ const r=RUNS[sel.value]; const el=document.getElementById('sum'); if(!r.summary_md){el.innerHTML='';return}
  el.innerHTML=r.summary_md.split('\n').filter(x=>x.trim()).map(x=>{const m=x.match(/^(What we see|Against the optimum|To improve):\s*(.*)$/); return m?`<p><b>${m[1]}.</b> ${m[2]}</p>`:`<p>${x}</p>`}).join('') }
function tables(){ const cur=+sel.value; const all=document.getElementById('all');
  let h='<tr><th>Record</th><th>Days</th><th>Edges</th><th>Mean edge</th><th>Optimum</th><th>Stretch</th><th>Gap</th><th>Optimal edges</th><th>Ideal of friends</th><th>Transitive</th><th>Pointings made / ended after day 1</th><th>Friendships made / ended after day 1</th><th>Above d</th></tr>';
  RUNS.forEach((r,i)=>{const m=r.summary; if(!m) return; h+=`<tr class="${i===cur?'sel':''}"><td>${r.name}</td><td>${Math.round(r.horizon/86400)}</td><td>${m.edges}</td><td>${m.mean} m</td><td>${m.optimum} m</td><td>${m.stretch}</td><td>${m.gap} m</td><td>${m.optedge}%</td><td>${m.ideal} m</td><td>${m.transitive}</td><td>${m.made1} / ${m.ended1}</td><td>${m.fmade1} / ${m.fended1}</td><td>${m.over}</td></tr>`});
  all.innerHTML=h; const r=RUNS[cur]; const dt=document.getElementById('days'), title=document.getElementById('daysTitle');
  if(!r.days||!r.days.length){title.textContent='';dt.innerHTML='';return}
  title.textContent=r.name+', by day (the overlay at the end of each day, at the positions of that day)';
  let g='<tr><th>Day</th><th>Edges</th><th>Transitive</th><th>Mean edge</th><th>Optimum</th><th>Stretch</th><th>Ideal of friends</th><th>Optimal edges</th><th>Of the optimum\'s edges</th></tr>';
  for(const d of r.days) g+=`<tr><td>${d[0]}</td><td>${d[1]}</td><td>${d[2]}</td><td>${d[3]} m</td><td>${d[4]} m</td><td>${d[5].toFixed(2)}</td><td>${d[6]} m</td><td>${d[7]}%</td><td>${d[8]}%</td></tr>`;
  dt.innerHTML=g }
sel.onchange=()=>{slider.value=0;render();tables();summary()}; slider.oninput=render; tables(); summary();
let timer=null; document.getElementById('play').onclick=function(){ if(timer){clearInterval(timer);timer=null;this.textContent='Play';return}
  if(slider.value>=2000)slider.value=0; this.textContent='Pause'; const r=RUNS[sel.value]; const perTick=2000/(r.horizon/86400)/80; // 80 ticks a day, 50 ms each: four seconds a day
  timer=setInterval(()=>{slider.value=Math.min(2000,+slider.value+perTick);render();if(slider.value>=2000){clearInterval(timer);timer=null;document.getElementById('play').textContent='Play'}},50)};
render();
</script></body></html>
HTML
} > "$OUT"
echo "replay: $(basename "$OUT"), $# records"
