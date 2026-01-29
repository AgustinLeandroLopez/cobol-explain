const form = document.getElementById("uploadForm");
const statusDiv = document.getElementById("status");
const resultsDiv = document.getElementById("results");
const downloadsDiv = document.getElementById("downloads");
const dlTech = document.getElementById("dlTech");
const dlFunc = document.getElementById("dlFunc");

function badge(ok) {
  return ok ? `<span class="badge text-bg-success">Sí</span>` : `<span class="badge text-bg-secondary">No</span>`;
}

function card(title, bodyHtml) {
  return `
    <div class="col-md-6 col-lg-4">
      <div class="card shadow-sm h-100">
        <div class="card-body">
          <div class="fw-semibold mb-2">${title}</div>
          <div>${bodyHtml}</div>
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


    // Cards
    resultsDiv.innerHTML += card(`Programa: ${s.program_id ?? "N/D"}`, `
    <div class="mt-1"><span class="text-muted">Bucles:</span> <b>${s.loops_count}</b></div>
    <div><span class="text-muted">Condicionales:</span> <b>${s.ifs_count}</b></div>
    <div><span class="text-muted">Asignaciones:</span> <b>${s.moves_count}</b></div>
    `);

    const inputs = s.inputs || [];
    const outputs = s.outputs || [];
    const assign = s.assign || {};
    const fdSizes = s.fd_sizes || {};   // ✅ NUEVO

    resultsDiv.innerHTML += card("Archivos de entrada", renderFiles(inputs, assign, fdSizes));
    resultsDiv.innerHTML += card("Archivos de salida", renderFiles(outputs, assign, fdSizes));

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
    resultsDiv.innerHTML += card("Base de datos", list(tableLines));

    // Tx
    const tx = s.tx || {};
    resultsDiv.innerHTML += card("Transaccional", `
    <div>
        ${tx.has_commit ? "✅ Se detecta COMMIT" : "⚠️ No se detecta COMMIT"}
    </div>
    <div>
        ${tx.has_rollback ? "✅ Se detecta ROLLBACK" : "⚠️ No se detecta ROLLBACK"}
    </div>
    `);



    // Downloads
    dlTech.href = s.downloads.tech;
    dlFunc.href = s.downloads.func;
    downloadsDiv.style.display = "block";

    resultsDiv.style.display = "flex";
  } catch (err) {
    statusDiv.innerHTML = `<div class="alert alert-danger mb-0">Error: ${err}</div>`;
  }
});