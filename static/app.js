const form = document.getElementById("uploadForm");
const statusDiv = document.getElementById("status");
const resultsDiv = document.getElementById("results");
const downloadsDiv = document.getElementById("downloads");
const dlTech = document.getElementById("dlTech");
const dlFunc = document.getElementById("dlFunc");

function badge(ok) {
  return ok ? `<span class="badge text-bg-success">Sí</span>` : `<span class="badge text-bg-secondary">No</span>`;
}

function card(title, bodyHtml, opts = {}) {
  const icon = opts.icon ?? "📌";
  const accentClass = opts.accentClass ?? "bg-primary";
  const subtitle = opts.subtitle ?? "";

  return `
    <div class="col-md-6 col-lg-4">
      <div class="card shadow-sm dash-card dash-hover h-100">
        <div class="dash-accent ${accentClass}"></div>
        <div class="card-body">
          <div class="d-flex align-items-start justify-content-between mb-2">
            <div>
              <div class="dash-title">
                <span class="me-2">${icon}</span>${title}
              </div>
              ${subtitle ? `<div class="dash-subtle mt-1">${subtitle}</div>` : ``}
            </div>
          </div>
          <div>${bodyHtml}</div>
        </div>
      </div>
    </div>
  `;
}
 

function heroCard(title, bodyHtml) {
  return `
    <div class="col-12">
      <div class="card shadow-sm border-0">
        <div class="card-body p-4 bg-primary-subtle rounded-3 border-start border-5 border-primary">
          <div class="d-flex align-items-center gap-2 mb-2">
            <span style="font-size: 1.2rem;">🧠</span>
            <div class="h5 mb-0 fw-bold">${title}</div>
          </div>

          <div class="fs-6">${bodyHtml}</div>
        </div>
      </div>
    </div>
  `;
}


function list(items) {
  if (!items || items.length === 0) return `<div class="text-muted">—</div>`;
  return `<ul class="mb-0">${items.map(x => `<li>${x}</li>`).join("")}</ul>`;
}

form.addEventListener("submit", async (e) => {
  e.preventDefault();

  const file = document.getElementById("fileInput").files[0];
  if (!file) return;

  statusDiv.innerHTML = `<div class="text-muted">Analizando...</div>`;
  resultsDiv.style.display = "none";
  downloadsDiv.style.display = "none";
  resultsDiv.innerHTML = "";

  const fd = new FormData();
  fd.append("file", file);

  try {
    const res = await fetch("/analyze", { method: "POST", body: fd });
    const data = await res.json();

    if (!data.ok) {
      statusDiv.innerHTML = `<div class="alert alert-danger mb-0">${data.error}</div>`;
      return;
    }

    const s = data.summary;
    const uploadedName = file.name;

    statusDiv.innerHTML = `
    <div class="alert alert-success mb-0">
        ✅ Resultado satisfactorio — Analizado el documento <b>"${uploadedName}"</b>
    </div>
    `;

    // Functional summary
    resultsDiv.innerHTML += heroCard("Funcionalidad (estimada)", `
      ${s.functional_summary ? `<div>${s.functional_summary}</div>` : `<div class="text-muted">—</div>`}
    `);


    // Cards
    const progType = s.program_type?.type ?? "—";

    resultsDiv.innerHTML += card(`Programa: ${s.program_id ?? "N/D"}`, `
    <div class="mt-1"><span class="text-muted">Tipo:</span> <b>${progType}</b></div>
    <div class="mt-1"><span class="text-muted">Bucles:</span> <b>${s.loops_count}</b></div>
    <div><span class="text-muted">Condicionales:</span> <b>${s.ifs_count}</b></div>
    <div><span class="text-muted">Asignaciones:</span> <b>${s.moves_count}</b></div>
    `, { icon: "🧾", accentClass: "bg-dark" });

    const inputs = s.inputs || [];
    const outputs = s.outputs || [];
    const assign = s.assign || {};
    const fdSizes = s.fd_sizes || {};   // ✅ NUEVO

    resultsDiv.innerHTML += card("Archivos de entrada", renderFiles(inputs, assign, fdSizes),
    { icon: "📥", accentClass: "bg-success", subtitle: "Lecturas detectadas por OPEN INPUT" }
    );
    resultsDiv.innerHTML += card("Archivos de salida", renderFiles(outputs, assign, fdSizes),
     { icon: "📤", accentClass: "bg-info", subtitle: "Escrituras detectadas por OPEN OUTPUT/EXTEND/I-O" }
    );

    function renderFiles(files, assign, fdSizes) {
    if (!files || files.length === 0) return `<div class="text-muted">—</div>`;

    return `
        <ul class="list-unstyled mb-0">
        ${files.map(f => {
            const phys = assign?.[f] ?? "N/D";
            const size = fdSizes?.[f]?.size;

            return `
            <li class="mb-3">
                <div><b>${f}</b> → <span class="text-muted">ASSIGN:</span> ${phys}</div>
                ${size ? `<div class="small text-muted">Longitud: ${size}</div>` : ""}
            </li>
            `;
        }).join("")}
        </ul>
    `;
    }

    // Tablas SQL
    const tables = s.sql_tables || {};
    const tableLines = Object.entries(tables).map(
    ([t, ops]) => `${t} → ${ops.join(", ")}`
    );
    resultsDiv.innerHTML += card("Base de datos", list(tableLines),
    { icon: "🗄️", accentClass: "bg-warning" }
    );

    // Tx
    const tx = s.tx || {};
    resultsDiv.innerHTML += card("Transaccional", `
    <div>
        ${tx.has_commit ? "✅ Se detecta COMMIT" : "⚠️ No se detecta COMMIT"}
    </div>
    <div>
        ${tx.has_rollback ? "✅ Se detecta ROLLBACK" : "⚠️ No se detecta ROLLBACK"}
    </div>
    `, { icon: "🧷", accentClass: "bg-secondary" });

    // Copybooks
    resultsDiv.innerHTML += card("Copybooks", list(s.copybooks),
      { icon: "📚", accentClass: "bg-primary" }
    );

    // CALLs (subprogramas)
resultsDiv.innerHTML += card("CALLs", list(s.calls_static ?? []),
  { icon: "📞", accentClass: "bg-primary" , subtitle: "Llamadas estáticas"}
);

resultsDiv.innerHTML += card("CALLs", list(s.calls_dynamic ?? []),
  { icon: "🧩", accentClass: "bg-primary" , subtitle: "Llamadas dinámicas"}
);

    // Downloads
    dlTech.href = s.downloads.tech;
    dlFunc.href = s.downloads.func;
    downloadsDiv.style.display = "block";

    resultsDiv.style.display = "flex";
  } catch (err) {
    statusDiv.innerHTML = `<div class="alert alert-danger mb-0">Error: ${err}</div>`;
  }
});