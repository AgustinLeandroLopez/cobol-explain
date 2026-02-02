import os
import uuid
from flask import Flask, request, render_template, jsonify, send_file
from werkzeug.utils import secure_filename

# Importás tu lógica existente
# Asegurate de tener estas funciones/métodos como abajo indico:
from cobol_explain import explain_cobol_file  # ya la tenés

ALLOWED_EXTENSIONS = {".cbl", ".cob", ".cobol", ".txt"}  # por si subís fuentes con .txt
MAX_FILE_MB = 5

app = Flask(__name__)
app.config["UPLOAD_FOLDER"] = "uploads"
app.config["OUTPUT_FOLDER"] = "outputs"
app.config["MAX_CONTENT_LENGTH"] = MAX_FILE_MB * 1024 * 1024

os.makedirs(app.config["UPLOAD_FOLDER"], exist_ok=True)
os.makedirs(app.config["OUTPUT_FOLDER"], exist_ok=True)


def allowed_file(filename: str) -> bool:
    _, ext = os.path.splitext(filename.lower())
    return ext in ALLOWED_EXTENSIONS


@app.get("/")
def index():
    return render_template("index.html")


@app.post("/analyze")
def analyze():
    if "file" not in request.files:
        return jsonify({"ok": False, "error": "No se envió ningún archivo."}), 400

    f = request.files["file"]
    if not f.filename:
        return jsonify({"ok": False, "error": "Nombre de archivo vacío."}), 400

    if not allowed_file(f.filename):
        return jsonify({"ok": False, "error": "Extensión no permitida. Subí un .cbl/.cob/.cobol."}), 400

    safe_name = secure_filename(f.filename)
    run_id = str(uuid.uuid4())[:8]

    upload_path = os.path.join(app.config["UPLOAD_FOLDER"], f"{run_id}_{safe_name}")
    f.save(upload_path)

    # Ejecutar análisis
    explanation = explain_cobol_file(upload_path)

    # Generar los 2 txt (técnico + funcional)
    tech_txt = explanation.to_text_technical()
    func_txt = explanation.to_text_functional()

    tech_name = f"{run_id}_Analisis_Cobol_Tecnico.txt"
    func_name = f"{run_id}_Analisis_Cobol_Funcional.txt"

    tech_path = os.path.join(app.config["OUTPUT_FOLDER"], tech_name)
    func_path = os.path.join(app.config["OUTPUT_FOLDER"], func_name)

    with open(tech_path, "w", encoding="utf-8") as out:
        out.write(tech_txt)

    with open(func_path, "w", encoding="utf-8") as out:
        out.write(func_txt)

    # Resumen para el front (no es archivo, solo respuesta)
    files = explanation.files or {}
    summary = {
        "program_id": explanation.program_id,
        "inputs": sorted(files.get("inputs", [])),
        "outputs": sorted(files.get("outputs", [])),
        "assign": files.get("assign", {}),
        "sql_tables": files.get("sql_tables", {}),
        "tx": files.get("tx", {}),
        "fd_sizes": files.get("fd_sizes", {}),
        "copybooks": files.get("copybooks", []),   # 👈 NUEVO
        "calls": files.get("calls", []),   # 👈 NUEVO
        "calls_static": files.get("calls_static", []),
        "calls_dynamic": files.get("calls_dynamic", []),
        "cics_commands": files.get("cics_commands", {"LINK": [], "XCTL": [], "START": [], "RETURN": []}),
        "functional_summary": files.get("functional_summary"),
        "program_type": files.get("program_type", {"type": "BATCH", "evidence": []}),
        "loops_count": len(explanation.loops or []),
        "ifs_count": files.get("ifs_count", len(explanation.conditions or [])),
        "moves_count": len(explanation.moves or []),
        "downloads": {
            "tech": f"/download/{tech_name}",
            "func": f"/download/{func_name}",
        }
    }

    return jsonify({"ok": True, "summary": summary})


@app.get("/download/<filename>")
def download(filename: str):
    path = os.path.join(app.config["OUTPUT_FOLDER"], filename)
    if not os.path.exists(path):
        return "Archivo no encontrado", 404
    return send_file(path, as_attachment=True)


if __name__ == "__main__":
    app.run(debug=True)
