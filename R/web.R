#' Refresh web leaderboard (minimal site)
#'
#' Writes `docs/leaderboard.json` from `results/master_leaderboard.csv` and
#' (re)writes the minimal `docs/index.html` shell that renders it. The HTML
#' is static and small; all tables/charts are rendered client-side from the
#' JSON, so each `rank_model()` run only needs to update the JSON.
#'
#' @param leaderboard_path path to master_leaderboard.csv
#' @param output_dir path to results/ (used to resolve leaderboard_path)
#' @param root project root (used to resolve docs/)
#' @param docs_dir path to docs/
#' @param verbose logical
#' @return invisible list with json + html paths
#' @export
update_web <- function(leaderboard_path = file.path(output_dir, "master_leaderboard.csv"),
                       output_dir = file.path(find_root(), "results"),
                       root = find_root(),
                       docs_dir = file.path(root, "docs"),
                       verbose = TRUE) {
  if (!file.exists(leaderboard_path)) stop("Leaderboard not found: ", leaderboard_path)
  if (!dir.exists(docs_dir)) dir.create(docs_dir, recursive = TRUE)

  df <- utils::read.csv(leaderboard_path, stringsAsFactors = FALSE)

  # ---- dataset meta (short, Indonesian) ----
  meta <- list(
    ds01_continuous_linear = list(code = "ds01", short = "Linear Dasar",
      desc = "Log pengeluaran per kapita, 3 kovariat linear. Uji baseline EBLUP vs Direct."),
    ds02_bounded_rate = list(code = "ds02", short = "Proporsi (0,1)",
      desc = "Kemiskinan rate (0,1). Estimasi wajib dalam [0,1]."),
    ds03_highdim_sparse = list(code = "ds03", short = "High-Dim Sparse",
      desc = "25 kovariat (3 sinyal + 22 noise). Uji regularisasi vs overfitting."),
    ds04_nonlinear_interaction = list(code = "ds04", short = "Nonlinear",
      desc = "Sin + kuadratik + interaksi. Wilayah MERF / tree-based."),
    ds05_spatial_correlated = list(code = "ds05", short = "Spasial SAR",
      desc = "Random effect SAR rho=0.65. Uji spatial borrowing (SEBLUP/INLA-Besag)."),
    ds06_spatiotemporal_panel = list(code = "ds06", short = "Panel ST",
      desc = "Panel D=50 x T=5, AR(1) phi=0.7. Uji Rao-Yu / panel SAE."),
    ds07_extreme_outliers = list(code = "ds07", short = "Pencilan",
      desc = "4 kabupaten shock +/-8 sigma. Uji robust (Huber / heavy-tail)."),
    ds08_nested_subarea = list(code = "ds08", short = "Nested",
      desc = "Hierarki provinsi > kabupaten. Uji two-fold subarea.")
  )

  keep_cols <- c("dataset_id", "dataset_name", "model", "N", "N_valid",
                 "ARB_pct", "RRMSE_pct", "RMSE", "MAE", "Corr", "RelEff_pct",
                 "Regular_RRMSE", "Outlier_RRMSE", "Boundary_Violations",
                 "Peak_RAM_MB", "Runtime_sec")
  keep_cols <- intersect(keep_cols, names(df))
  rows <- df[, keep_cols, drop = FALSE]

  updated <- format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC")

  # ---- leaderboard.json ----
  json_path <- file.path(docs_dir, "leaderboard.json")
  payload <- list(updated = updated, datasets = meta, rows = rows)
  json_str <- jsonlite::toJSON(payload, auto_unbox = TRUE, digits = 4, na = "null")
  writeLines(json_str, json_path, useBytes = TRUE)

  # ---- index.html (minimal shell) ----
  html <- minimal_html_template()
  html <- gsub("{{UPDATED}}", updated, html, fixed = TRUE)
  html_path <- file.path(docs_dir, "index.html")
  writeLines(html, html_path, useBytes = TRUE)

  if (isTRUE(verbose)) {
    cat(sprintf("Web updated:\n -> %s (%d rows)\n -> %s\n",
                json_path, nrow(rows), html_path))
  }
  invisible(list(json = json_path, html = html_path, n = nrow(rows)))
}

minimal_html_template <- function() {
'<!DOCTYPE html>
<html lang="id">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>SAE Benchmark Lab — Leaderboard</title>
<link rel="icon" href="data:,">
<script src="https://cdn.jsdelivr.net/npm/chart.js@4.4.2/dist/chart.umd.min.js"></script>
<style>
:root{--bg:#f8fafc;--card:#fff;--line:#e2e8f0;--tx:#0f172a;--mut:#64748b;--acc:#2563eb;--best:#ecfdf5;--best-tx:#047857}
[data-theme=dark]{--bg:#0b1220;--card:#111c33;--line:#1e2d4d;--tx:#e8eefc;--mut:#93a4c4;--acc:#60a5fa;--best:rgba(16,185,129,.15);--best-tx:#6ee7b7}
*{box-sizing:border-box}body{margin:0;font-family:system-ui,-apple-system,"Segoe UI",Roboto,sans-serif;background:var(--bg);color:var(--tx)}
.wrap{max-width:1080px;margin:0 auto;padding:24px 16px 64px}
.top{display:flex;justify-content:space-between;align-items:flex-start;gap:12px;flex-wrap:wrap}
h1{margin:0;font-size:28px;letter-spacing:-.02em}.sub{color:var(--mut);margin:6px 0 0;font-size:14px}
.btn{border:1px solid var(--line);background:var(--card);color:var(--tx);border-radius:999px;padding:7px 14px;font-size:13px;cursor:pointer}
.btn.on{background:var(--acc);border-color:var(--acc);color:#fff}
.kpis{display:grid;grid-template-columns:repeat(4,1fr);gap:10px;margin:18px 0}
.kpi{background:var(--card);border:1px solid var(--line);border-radius:12px;padding:12px 14px}
.kpi .l{font-size:11px;text-transform:uppercase;letter-spacing:.06em;color:var(--mut)}
.kpi .v{font-size:22px;font-weight:750;margin-top:2px}.kpi .s{font-size:12px;color:var(--mut)}
.card{background:var(--card);border:1px solid var(--line);border-radius:14px;overflow:hidden}
.tabs{display:flex;gap:8px;flex-wrap:wrap;margin:16px 0 12px}
.tab{border:1px solid var(--line);background:var(--card);color:var(--tx);border-radius:10px;padding:8px 12px;font-size:13px;cursor:pointer}
.tab.on{background:var(--acc);border-color:var(--acc);color:#fff}
.tab small{opacity:.75}
.toolbar{display:flex;gap:8px;flex-wrap:wrap;align-items:center;padding:12px 14px;border-bottom:1px solid var(--line)}
.toolbar input,.toolbar select{background:var(--bg);border:1px solid var(--line);color:var(--tx);border-radius:8px;padding:7px 10px;font-size:13px}
.desc{padding:12px 14px;border-bottom:1px solid var(--line);font-size:13px;color:var(--mut)}
table{width:100%;border-collapse:collapse;font-size:13.5px}
th{font-size:11px;text-transform:uppercase;letter-spacing:.05em;color:var(--mut);text-align:right;padding:10px 12px;border-bottom:1px solid var(--line);background:rgba(127,140,160,.06)}
th:first-child,td:first-child{text-align:center}th:nth-child(2),td:nth-child(2){text-align:left}
td{padding:10px 12px;border-bottom:1px solid var(--line);text-align:right;font-variant-numeric:tabular-nums}
td.model{font-family:ui-monospace,Menlo,monospace;font-size:12.5px}
tr:hover td{background:rgba(37,99,235,.05)}
.best{background:var(--best)!important;color:var(--best-tx)!important;font-weight:700;border-radius:6px}
.rank{display:inline-block;min-width:38px;padding:2px 8px;border-radius:7px;font-weight:700;font-size:12px;border:1px solid var(--line)}
.r1{background:#fef3c7;border-color:#f59e0b;color:#92400e}.r2{background:#f1f5f9;border-color:#94a3b8;color:#475569}.r3{background:#ffedd5;border-color:#fb923c;color:#9a3412}
.grid2{display:grid;grid-template-columns:7fr 5fr;gap:12px;margin-top:12px}
@media(max-width:900px){.kpis{grid-template-columns:repeat(2,1fr)}.grid2{grid-template-columns:1fr}}
.box{background:var(--card);border:1px solid var(--line);border-radius:14px;padding:14px}
.box h3{margin:0 0 4px;font-size:14px}.box p{margin:0 0 10px;font-size:12.5px;color:var(--mut)}
details{margin-top:12px;font-size:13px;color:var(--mut)}summary{cursor:pointer;color:var(--tx);font-weight:600}
footer{margin-top:18px;font-size:12px;color:var(--mut);text-align:center}
.mono{font-family:ui-monospace,Menlo,monospace}
</style>
</head>
<body>
<div class="wrap">
  <div class="top">
    <div>
      <h1>SAE Benchmark Lab</h1>
      <p class="sub">Tolok ukur model Small Area Estimation vs <i>ground truth</i> populasi (BPS two-stage). Pilih dataset untuk melihat peringkat model.</p>
      <p class="sub mono" id="updated">Terakhir diperbarui: {{UPDATED}}</p>
    </div>
    <div style="display:flex;gap:8px">
      <button class="btn" id="themeBtn" title="Gelap / Terang">&#127769; Tema</button>
      <a class="btn" style="text-decoration:none" href="leaderboard.json" target="_blank">JSON</a>
    </div>
  </div>

  <div class="kpis">
    <div class="kpi"><div class="l">Dataset</div><div class="v" id="kDs">–</div><div class="s">8 arketipe SAE</div></div>
    <div class="kpi"><div class="l">Model</div><div class="v" id="kModels">–</div><div class="s">unik di leaderboard</div></div>
    <div class="kpi"><div class="l">RelEff tertinggi</div><div class="v" id="kEff" style="color:var(--best-tx)">–</div><div class="s" id="kEffSub">–</div></div>
    <div class="kpi"><div class="l">RRMSE terkecil</div><div class="v" id="kRrmse">–</div><div class="s" id="kRrmseSub">–</div></div>
  </div>

  <div class="tabs" id="tabs"></div>

  <div class="card">
    <div class="toolbar">
      <input id="q" placeholder="Cari model… (mis. fastsae, MERF)" style="flex:1;min-width:200px">
      <select id="sort"><option value="RelEff_pct">Urut: RelEff ↓</option><option value="RRMSE_pct">Urut: RRMSE ↑</option><option value="ARB_pct">Urut: ARB ↑</option><option value="Runtime_sec">Urut: Waktu ↑</option></select>
    </div>
    <div class="desc" id="dsDesc"></div>
    <div style="overflow-x:auto"><table>
      <thead><tr><th>#</th><th>Model</th><th>RelEff (%)</th><th>RRMSE (%)</th><th>ARB (%)</th><th>Corr</th><th>RAM</th><th>Waktu</th></tr></thead>
      <tbody id="rows"></tbody>
    </table></div>
  </div>

  <div class="grid2">
    <div class="box">
      <div style="display:flex;justify-content:space-between;align-items:center;gap:8px;flex-wrap:wrap;margin-bottom:4px">
        <h3 id="barTitle">RRMSE per model (%)</h3>
        <select id="barMetric" style="background:var(--bg);border:1px solid var(--line);color:var(--tx);border-radius:8px;padding:6px 10px;font-size:12.5px">
          <option value="RRMSE_pct">RRMSE (%)</option>
          <option value="ARB_pct">ARB (%)</option>
          <option value="RelEff_pct">RelEff (%)</option>
          <option value="Corr">Corr</option>
          <option value="Runtime_sec">Waktu (detik)</option>
        </select>
      </div>
      <p id="chartSub"></p><canvas id="bar" height="220"></canvas>
    </div>
    <div class="box">
      <div style="display:flex;justify-content:space-between;align-items:center;gap:8px;flex-wrap:wrap;margin-bottom:4px">
        <h3>Waktu vs RelEff</h3>
        <select id="scatScale" style="background:var(--bg);border:1px solid var(--line);color:var(--tx);border-radius:8px;padding:6px 10px;font-size:12.5px">
          <option value="logarithmic">Skala log</option>
          <option value="linear">Skala linear</option>
        </select>
      </div>
      <p>Atas-kiri = unggul. Hover untuk nama model.</p><canvas id="scat" height="220"></canvas>
    </div>
  </div>

  <div class="box" style="margin-top:12px">
    <div style="display:flex;justify-content:space-between;align-items:center;gap:8px;flex-wrap:wrap;margin-bottom:4px">
      <h3>Jejak model lintas 8 dataset</h3>
      <div style="display:flex;gap:8px;flex-wrap:wrap">
        <select id="trackModel" style="background:var(--bg);border:1px solid var(--line);color:var(--tx);border-radius:8px;padding:6px 10px;font-size:12.5px;max-width:280px"></select>
        <select id="trackMetric" style="background:var(--bg);border:1px solid var(--line);color:var(--tx);border-radius:8px;padding:6px 10px;font-size:12.5px">
          <option value="RelEff_pct">RelEff (%)</option>
          <option value="RRMSE_pct">RRMSE (%)</option>
        </select>
      </div>
    </div>
    <p id="trackSub">Bandingkan satu model (garis biru) vs Direct (garis abu, baseline 100% untuk RelEff) di semua arketipe.</p>
    <div style="position:relative;height:260px"><canvas id="track"></canvas></div>
  </div>

  <details><summary>Metodologi singkat</summary>
    <p>Populasi sintetis ±120rb rumah tangga. Sampling <b>two-stage stratified cluster</b> ala Susenas BPS: (1) Blok Sensus via PPS per strata kota/desa, (2) 10 rumah tangga via sistematik. Direct + varians via linearisasi Taylor (<span class="mono">survey::svydesign</span>, Deff&gt;1). Ground truth = agregat populasi penuh.</p>
    <p>Metrik: <b>RRMSE</b> = akar rata-rata squared relative error; <b>ARB</b> = rata-rata absolute relative bias; <b>RelEff</b> = MSE(Direct)/MSE(Model)×100 (&gt;100% = lebih efisien dari Direct); <b>Corr</b> = korelasi Pearson vs truth.</p>
    <p>Cara menambah model (R): <span class="mono">source(&quot;R/metrics.R&quot;); source(&quot;R/datasets.R&quot;); source(&quot;R/benchmark.R&quot;); source(&quot;R/web.R&quot;); rank_model(fn_saya, &quot;modelku (pkg, v1)&quot;)</span>. Fungsi menerima <span class="mono">(ds, formula_str)</span> dan mengembalikan vektor numerik sepanjang <span class="mono">nrow(ds)</span>.</p>
  </details>

  <footer>SAE Benchmark Lab · minimal leaderboard · data dari <span class="mono">results/master_leaderboard.csv</span></footer>
</div>
<script>
let DB=null, cur=null, bar=null, scat=null, track=null;
const $=id=>document.getElementById(id);
const METRICS={RRMSE_pct:{label:"RRMSE (%)",fmt:v=>v.toFixed(2)+"%",low:true,color:"#2563eb"},ARB_pct:{label:"ARB (%)",fmt:v=>v.toFixed(2)+"%",low:true,color:"#7c3aed"},RelEff_pct:{label:"RelEff (%)",fmt:v=>v.toFixed(1)+"%",low:false,color:"#059669"},Corr:{label:"Corr",fmt:v=>v.toFixed(4),low:false,color:"#0891b2"},Runtime_sec:{label:"Waktu (detik)",fmt:v=>v.toFixed(3)+"s",low:true,color:"#ea580c"}};
fetch("leaderboard.json").then(r=>r.json()).then(db=>{
  DB=db; $("updated").textContent="Terakhir diperbarui: "+db.updated;
  const ids=Object.keys(db.datasets);
  $("kDs").textContent=ids.length;
  const models=[...new Set(db.rows.map(r=>r.model))].sort();
  $("kModels").textContent=models.length;
  const ok=db.rows.filter(r=>r.RelEff_pct!=null);
  if(ok.length){const b=ok.reduce((a,b)=>a.RelEff_pct>b.RelEff_pct?a:b);$("kEff").textContent=b.RelEff_pct.toFixed(1)+"%";$("kEffSub").textContent=b.model.slice(0,34);}
  const rr=db.rows.filter(r=>r.RRMSE_pct!=null);
  if(rr.length){const b=rr.reduce((a,b)=>a.RRMSE_pct<b.RRMSE_pct?a:b);$("kRrmse").textContent=b.RRMSE_pct.toFixed(2)+"%";$("kRrmseSub").textContent=(db.datasets[b.dataset_id]?.short||b.dataset_id)+" · "+b.model.slice(0,24);}
  const tabs=$("tabs"); tabs.innerHTML="";
  ids.forEach((id,i)=>{const m=db.datasets[id];const b=document.createElement("button");b.className="tab"+(i===0?" on":"");b.innerHTML=`<b class="mono">${m.code}</b> <small>${m.short}</small>`;b.onclick=()=>{cur=id;[...tabs.children].forEach(x=>x.classList.remove("on"));b.classList.add("on");render();};tabs.appendChild(b);});
  cur=ids[0];
  const tm=$("trackModel"); tm.innerHTML=models.map(m=>`<option>${m}</option>`).join("");
  const guess=models.find(m=>/fastsae.*eblup_fh.*REML[^,]*$/.test(m))||models.find(m=>/fastsae/i.test(m))||models[0];
  if(guess)tm.value=guess;
  render(); drawTrack();
});
$("q").addEventListener("input",render); $("sort").addEventListener("change",render);
$("barMetric").addEventListener("change",()=>render());
$("scatScale").addEventListener("change",()=>render());
$("trackModel").addEventListener("change",drawTrack); $("trackMetric").addEventListener("change",drawTrack);
$("themeBtn").onclick=()=>{const h=document.documentElement;h.dataset.theme=h.dataset.theme==="dark"?"":"dark";};
function render(){
  if(!DB||!cur)return;
  const q=$("q").value.toLowerCase(), sort=$("sort").value;
  let rows=DB.rows.filter(r=>r.dataset_id===cur);
  if(q)rows=rows.filter(r=>(r.model||"").toLowerCase().includes(q));
  rows=rows.slice().sort((a,b)=>{
    if(sort==="RelEff_pct")return (b.RelEff_pct??-1)-(a.RelEff_pct??-1);
    return (a[sort]??1e18)-(b[sort]??1e18);
  });
  $("dsDesc").textContent=(DB.datasets[cur].short)+" — "+(DB.datasets[cur].desc)+" · "+rows.length+" model.";
  const tb=$("rows"); tb.innerHTML="";
  const rrs=rows.map(r=>r.RRMSE_pct).filter(v=>v!=null), ars=rows.map(r=>r.ARB_pct).filter(v=>v!=null),
        efs=rows.map(r=>r.RelEff_pct).filter(v=>v!=null), crs=rows.map(r=>r.Corr).filter(v=>v!=null),
        tms=rows.map(r=>r.Runtime_sec).filter(v=>v!=null);
  const bR=Math.min(...rrs), bA=Math.min(...ars), bE=Math.max(...efs), bC=Math.max(...crs), bT=Math.min(...tms);
  rows.forEach((r,i)=>{
    const tr=document.createElement("tr");
    const rc=i===0?"r1":i===1?"r2":i===2?"r3":"";
    const c=(v,b,fmt)=>{const best=(v!=null&&v===b)?"best":"";return `<td class="${best}">${v==null?"–":fmt(v)}</td>`;};
    tr.innerHTML=`<td><span class="rank ${rc}">#${i+1}</span></td><td class="model">${r.model}</td>`
      +c(r.RelEff_pct,bE,v=>v.toFixed(1)+"%")+c(r.RRMSE_pct,bR,v=>v.toFixed(2)+"%")+c(r.ARB_pct,bA,v=>v.toFixed(2)+"%")+c(r.Corr,bC,v=>v.toFixed(4))
      +`<td>${r.Peak_RAM_MB==null?"–":r.Peak_RAM_MB.toFixed(1)+" MB"}</td><td class="mono">${r.Runtime_sec==null?"–":r.Runtime_sec.toFixed(2)+"s"}</td>`;
    tb.appendChild(tr);
  });
  drawCharts(rows);
}
function drawCharts(rows){
  const mkey=$("barMetric").value, M=METRICS[mkey];
  const labels=rows.map(r=>r.model.length>22?r.model.slice(0,22)+"…":r.model);
  if(bar)bar.destroy(); if(scat)scat.destroy();
  $("barTitle").textContent=M.label+" per model"+(M.low?" — makin kecil makin baik":" — makin besar makin baik");
  $("chartSub").textContent=(DB.datasets[cur].short)+" · "+rows.length+" model · "+M.label;
  bar=new Chart($("bar"),{type:"bar",data:{labels,datasets:[{data:rows.map(r=>r[mkey]),backgroundColor:M.color,borderRadius:5}]},options:{indexAxis:"y",plugins:{legend:{display:false},tooltip:{callbacks:{label:c=>" "+(c.raw==null?"–":M.fmt(c.raw))}}}},scales:{x:{beginAtZero:mkey!=="Corr"}}});
  const xs=$("scatScale").value;
  scat=new Chart($("scat"),{type:"scatter",data:{datasets:[{data:rows.filter(r=>r.Runtime_sec!=null&&r.RelEff_pct!=null).map(r=>({x:Math.max(r.Runtime_sec,1e-3),y:r.RelEff_pct,label:r.model})),backgroundColor:"#059669"}]},options:{plugins:{legend:{display:false},tooltip:{callbacks:{label:c=>" "+c.raw.label+" — "+c.raw.y.toFixed(1)+"% / "+c.raw.x.toFixed(3)+"s"}}},scales:{x:{type:xs,title:{display:true,text:xs==="logarithmic"?"detik (log)":"detik"}},y:{title:{display:true,text:"RelEff (%)"}}}}});
}
function drawTrack(){
  if(!DB)return;
  const model=$("trackModel").value, mkey=$("trackMetric").value, M=METRICS[mkey];
  const ids=Object.keys(DB.datasets), cl=DB.datasets;
  const val=(ds,mo)=>{const r=DB.rows.find(r=>r.dataset_id===ds&&r.model===mo);return r?r[mkey]:null;};
  const series=ids.map(id=>val(id,model));
  const direct=ids.map(id=>val(id,"survey (direct, Taylor)"));
  if(track)track.destroy();
  $("trackSub").textContent=model+" · "+M.label+" di 8 arketipe (vs Direct).";
  track=new Chart($("track"),{type:"line",data:{labels:ids.map(id=>cl[id].code),datasets:[
    {label:model.length>30?model.slice(0,30)+"…":model,data:series,borderColor:"#2563eb",backgroundColor:"rgba(37,99,235,.12)",fill:true,tension:.3,spanGaps:true,pointRadius:4},
    {label:"Direct (baseline)",data:direct,borderColor:"#94a3b8",borderDash:[6,4],tension:.3,spanGaps:true,pointRadius:3}
  ]},options:{responsive:true,maintainAspectRatio:false,plugins:{legend:{position:"bottom",labels:{boxWidth:14,font:{size:11}}}},scales:{y:{title:{display:true,text:M.label}}}}});
}
</script>
</body>
</html>
'
}
