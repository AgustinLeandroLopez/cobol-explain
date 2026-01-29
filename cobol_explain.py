import re
import json
from dataclasses import dataclass, asdict
from typing import List, Optional, Dict, Any


@dataclass
class CobolExplanation:
    program_id: Optional[str]
    ##displays: List[str]
    #has_stop: bool
    #has_goback: bool
    # [NUEVO]
    loops: List[Dict[str, Any]]
    conditions: List[Dict[str, Any]]
    moves: List[Dict[str, Any]]
    files: Dict[str, Any]   # <-- NUEVO

    # ---------- SALIDA TÉCNICA - ARCHIVOS - TABLAS - COMMIT - ROLLBACK ----------
    def to_text_technical(self) -> str:
        lines: List[str] = []
        lines.append("📄 Análisis COBOL")
        lines.append("-" * 30)

        if self.program_id:
            lines.append(f"✅ PROGRAM-ID detectado: {self.program_id}")
        else:
            lines.append("⚠️ No se encontró PROGRAM-ID.")

        """if self.displays:
            lines.append(f"✅ DISPLAY detectados: {len(self.displays)}")
        else:
            lines.append("ℹ️ DISPLAY detectados: 0")"""

        inputs = self.files.get("inputs", [])
        outputs = self.files.get("outputs", [])
        assigns = self.files.get("assign", {})
        tables = self.files.get("sql_tables", {})
        tx = self.files.get("tx", {})

        if inputs:
            lines.append(f"✅ LEE {len(inputs)} archivo(s) de entrada:")
            for f in inputs:
                phys = assigns.get(f)
                lines.append(f"   - {f}" + (f" (ASSIGN TO: {phys})" if phys else ""))
        else:
            lines.append("ℹ️ LEE 0 archivo(s) de entrada.")

        if outputs:
            lines.append(f"✅ ESCRIBE {len(outputs)} archivo(s) de salida:")
            for f in outputs:
                phys = assigns.get(f)
                lines.append(f"   - {f}" + (f" (ASSIGN TO: {phys})" if phys else ""))
        else:
            lines.append("ℹ️ ESCRIBE 0 archivo(s) de salida.")

        if tables:
            lines.append(f"✅ USA {len(tables)} tabla(s) SQL:")
            for table, ops in tables.items():
                modos = []
                for op in ops:
                    if op in ("INSERT", "UPDATE", "DELETE"):
                        modos.append(f"{op} (ESCRIBE)")
                    else:
                        modos.append(f"{op} (LEE)")
                lines.append(f"   - {table}: " + ", ".join(modos))
        else:
            lines.append("ℹ️ No se detectaron tablas SQL.")

        if tx.get("has_commit"):
            lines.append("✅ Se detecta COMMIT")
        else:
            lines.append("ℹ️ No se detecta COMMIT")

        if tx.get("has_rollback"):
            lines.append("✅ Se detecta ROLLBACK")
        else:
            lines.append("ℹ️ No se detecta ROLLBACK")


        """if self.has_stop or self.has_goback:
            end_type = []
            if self.has_stop:
                end_type.append("STOP RUN")
            if self.has_goback:
                end_type.append("GOBACK")
            lines.append(f"✅ Finaliza ejecución con: {', '.join(end_type)}")
        else:
            lines.append("⚠️ No se detectó STOP RUN ni GOBACK (fin del programa).")"""

        lines.append("")
        lines.append("⚠️ Reportes de bucles, condicionales en Analisis_Cobol_Funcional.txt")

        prog = self.program_id or "DESCONOCIDO"

        """display_count = len(self.displays)
        lines.append(
            f"Este programa COBOL se identifica como '{prog}'. "
            f"Se detectaron {display_count} sentencias DISPLAY (no se listan los mensajes)."
        )"""

        return "\n".join(lines)
    ############ SALIDA DETALLADAS DE CONDICIONALES, BUCLES, MOVES, ETC   ############
    def to_text_functional(self) -> str:
        lines: List[str] = []

        lines.append("Se detalla el código cobol, bucles, condicionales y asignaciones.")
        lines.append("-" * 40)

        # Bucles
        if self.loops:
            lines.append("Bucles detectados (PERFORM):")
            for lp in self.loops:
                if lp.get("type") == "perform_varying":
                    lines.append(
                        f" - VARYING {lp['var']} FROM {lp['from']} BY {lp['by']} "
                        f"UNTIL {lp['var']} > {lp['until_value']}"
                    )
                elif lp.get("type") == "perform_until":
                    lines.append(f" - UNTIL {lp['until']}")
        else:
            lines.append("No se detectaron bucles.")

        # IF
        if self.conditions:
            lines.append("\nCondiciones detectadas (IF):")
            seen = set()
            for c in self.conditions:
                key = (c["left"], c["op"], c["right"])
                if key in seen:
                    continue
                seen.add(key)
                if c["right_type"] == "string":
                    lines.append(f' - IF {c["left"]} {c["op"]} "{c["right"]}"')
                else:
                    lines.append(f' - IF {c["left"]} {c["op"]} {c["right"]}')
        else:
            lines.append("\nNo se detectaron condiciones IF.")

        # MOVE
        if self.moves:
            lines.append("\nAsignaciones detectadas (MOVE):")
            for mv in self.moves:
                if mv["value_type"] == "string":
                    lines.append(f' - MOVE "{mv["value"]}" TO {mv["target"]}')
                else:
                    lines.append(f' - MOVE {mv["value"]} TO {mv["target"]}')
        else:
            lines.append("\nNo se detectaron MOVE.")

        return "\n".join(lines)

    ##def to_json_dict(self) -> Dict[str, Any]:
        end_statements = []
        if self.has_stop:
            end_statements.append("STOP RUN")
        if self.has_goback:
            end_statements.append("GOBACK")

        prog = self.program_id or "UNKNOWN"

        ai_role = "Analista funcional bancario senior + arquitecto de modernización"
        ai_goal = "Explicar qué hace el programa COBOL en términos funcionales y técnicos, y sugerir mejoras o pasos de modernización."

        ai_sections = [
            "Resumen ejecutivo (1-3 líneas)",
            "Qué hace (funcional)",
            "Qué hace (técnico)",
            "Entradas/Salidas detectadas (si aplica)",
            "Mensajes al usuario (DISPLAY)",
            "Fin del programa (STOP RUN / GOBACK)",
            "Riesgos / dudas / supuestos",
            "Ideas de modernización (API / microservicio) - opcional",
        ]

        data: Dict[str, Any] = {
            "program_id": self.program_id,
            "statements": {
                "display": self.displays,
                "end": end_statements,
            },
            "flags": {
                "has_display": len(self.displays) > 0,
                "has_stop_run": self.has_stop,
                "has_goback": self.has_goback,
                "has_end_statement": len(end_statements) > 0,
            },
            "metrics": {
                "display_count": len(self.displays),
                "end_statement_count": len(end_statements),
            },
            "analysis": {
                "program_kind": "message_printer" if len(self.displays) > 0 else "unknown",
                "terminates_explicitly": len(end_statements) > 0,
            },
            "logic": {
            "loops": self.loops,
            "conditions": self.conditions,
            "moves": self.moves,
            },
            "ai": {
                "language": "es",
                "role": {
                    "name": ai_role,
                    "style": [
                        "claro",
                        "estructurado",
                        "sin humo",
                        "orientado a negocio y a riesgo",
                        "con ejemplos concretos",
                    ],
                    "tone": "profesional",
                },
                "task": {
                    "goal": ai_goal,
                    "audience": [
                        "analista funcional",
                        "desarrollador no-COBOL",
                        "líder técnico",
                    ],
                    "constraints": [
                        "No inventar reglas de negocio que no estén explícitas",
                        "Si falta información, marcarlo como 'asunción' o 'no detectado'",
                        "Mantener el resumen en español",
                    ],
                },
                "output_spec": {
                    "format": "markdown",
                    "sections": ai_sections,
                },
            },
            "summary": (
                f"Program '{prog}' displays {len(self.displays)} message(s) "
                f"and ends with {', '.join(end_statements) if end_statements else 'NO EXPLICIT END'}."
            ),
        }

        # Prompt listo para copiar/pegar a ChatGPT (sin API)
        data["ai_prompt"] = (
            f"Actuá como: {ai_role}\n"
            f"Tarea: {ai_goal}\n\n"
            f"Reglas:\n"
            f"- No inventes lógica.\n"
            f"- Si falta info, decilo explícitamente.\n"
            f"- Respondé en español.\n\n"
            f"Entregable en Markdown con estas secciones:\n"
            + "\n".join([f"- {s}" for s in ai_sections]) +
            "\n\n"
            "Datos estructurados del programa (JSON):\n"
            + json.dumps(
                {
                    "program_id": data["program_id"],
                    "statements": data["statements"],
                    "flags": data["flags"],
                    "metrics": data["metrics"],
                    "analysis": data["analysis"],
                },
                ensure_ascii=False,
                indent=2
            )
        )

        return data


def read_text(path: str) -> str:
    with open(path, "r", encoding="utf-8", errors="ignore") as f:
        return f.read()


def normalize(cobol: str) -> str:
    return cobol.replace("\r\n", "\n")


def extract_program_id(cobol: str) -> Optional[str]:
    m = re.search(r"\bPROGRAM-ID\s*\.\s*([A-Z0-9_-]+)\s*\.", cobol, flags=re.IGNORECASE)
    return m.group(1) if m else None

def extract_fds(cobol: str) -> List[str]:
    """
    Devuelve file-names declarados en FD.
    Soporta:
      FD INP-CLI.
      FD INP-CLI
         RECORD ...
    """
    fds = re.findall(
        r"^\s*FD\s+([A-Z0-9-]+)\b",
        cobol,
        flags=re.IGNORECASE | re.MULTILINE
    )
    return [x.upper() for x in fds]

def remove_cobol_comments(cobol: str) -> str:
    cleaned_lines = []
    for line in cobol.splitlines():
        stripped = line.lstrip()
        if stripped.startswith("*"):
            continue
        # elimina comentario inline estilo *>
        if "*>" in line:
            line = line.split("*>", 1)[0]
        cleaned_lines.append(line)
    return "\n".join(cleaned_lines)

def extract_data_record_name(fd_block: str) -> Optional[str]:
    # DATA RECORD IS REG-ENTRADA.
    m = re.search(r"\bDATA\s+RECORD\s+IS\s+([A-Z0-9-]+)\b", fd_block, flags=re.IGNORECASE)
    return m.group(1).upper() if m else None


def extract_01_pic_x_size_for_record(fd_block: str, record_name: str) -> Optional[int]:
    """
    Busca una línea que contenga: 01 <record_name> ... PIC ...
    y toma SOLO las que no tengan '*' antes del token 01 (para ignorar variantes/inactivas).
    """
    for line in fd_block.splitlines():
        up = line.upper()

        # Debe contener el 01 + nombre + PIC
        if f"01 {record_name.upper()}" not in up or " PIC " not in up:
            continue

        # Si hay '*' ANTES de '01', la ignoramos (ej: LTBUSD*01, FE0206*LTBUSD 01)
        idx01 = up.find("01")
        if idx01 != -1 and "*" in up[:idx01]:
            continue

        # Cortamos en '.' si existe (sentencia COBOL)
        if "." in line:
            line_cut = line.split(".", 1)[0]
        else:
            line_cut = line

        # Extraemos lo que venga después de PIC
        m = re.search(r"\bPIC\b\s+(.+)$", line_cut, flags=re.IGNORECASE)
        if not m:
            continue

        return pic_length(m.group(1))

    return None

def pic_length(pic: str) -> int:
    """
    Longitud aproximada (bytes) de una PIC DISPLAY.
    V1: soporta X(n), X, 9(n), 9, S9(n)V9(m)
    (COMP/COMP-3 los ignoramos por ahora)
    """
    p = pic.upper()
    p = re.sub(r"\s+", "", p)  # sin espacios

    # Ignorar binarios/packed por ahora (podés mejorarlo después)
    if "COMP-3" in p or re.search(r"\bCOMP\b", p):
        return 0

    # X(n)
    m = re.search(r"X\((\d+)\)", p)
    if m:
        return int(m.group(1))

    # X repetido (XXX)
    if "X" in p and "(" not in p:
        return p.count("X")

    # 9(n)
    m = re.search(r"9\((\d+)\)", p)
    base = int(m.group(1)) if m else p.count("9")

    # decimales: V99 o V9(n)
    dec = 0
    m = re.search(r"V9\((\d+)\)", p)
    if m:
        dec = int(m.group(1))
    elif "V" in p:
        dec = p.split("V", 1)[1].count("9")

    return base + dec

def extract_fd_record_sizes(cobol: str) -> Dict[str, Dict[str, Any]]:
    cobol_nc = remove_cobol_comments(cobol)
    blocks = extract_fd_blocks(cobol_nc)

    out: Dict[str, Dict[str, Any]] = {}

    for fd_name, fd_block in blocks.items():
        info: Dict[str, Any] = {"size": None, "source": None}

        # 1) Prioridad: RECORD CONTAINS
        rc = extract_record_contains_size(fd_block)
        if rc is not None:
            info["size"] = rc
            info["source"] = "RECORD CONTAINS"
            out[fd_name] = info
            continue

        # 2) DATA RECORD IS <name>  -> buscar ese 01
        dr = extract_data_record_name(fd_block)
        if dr:
            sz = extract_01_pic_x_size_for_record(fd_block, dr)
            if sz:
                info["size"] = sz
                info["source"] = f"DATA RECORD IS {dr}"
                out[fd_name] = info
                continue

        # 3) Fallback: primer 01 con PIC
        sz = extract_first_01_pic_x_size(fd_block)
        if sz:
            info["size"] = sz
            info["source"] = "FIRST 01"
        else:
            info["size"] = None
            info["source"] = "NO DETECTADO"

        out[fd_name] = info

    return out

def extract_record_contains_size(fd_block: str) -> Optional[int]:
    m = re.search(r"\bRECORD\s+CONTAINS\s+(\d+)\s+CHARACTERS\b", fd_block, re.IGNORECASE)
    return int(m.group(1)) if m else None

def extract_fd_blocks(cobol: str) -> Dict[str, str]:
    """
    Devuelve: FD_NAME -> bloque de texto del FD.
    """
    blocks: Dict[str, str] = {}
    pattern = re.compile(
        r"^\s*FD\s+([A-Z0-9-]+)\b(.*?)(?=^\s*FD\b|^\s*WORKING-STORAGE\b|^\s*LINKAGE\b|^\s*PROCEDURE\b|\Z)",
        re.IGNORECASE | re.MULTILINE | re.DOTALL
    )
    for m in pattern.finditer(cobol):
        blocks[m.group(1).upper()] = m.group(2)
    return blocks

def extract_first_01_pic_x_size(fd_block: str) -> Optional[int]:
    """
    Fallback: primer 01 con PIC, ignorando líneas con '*' antes del 01.
    """
    for line in fd_block.splitlines():
        up = line.upper()

        if " 01 " not in f" {up} " or " PIC " not in up:
            continue

        idx01 = up.find("01")
        if idx01 != -1 and "*" in up[:idx01]:
            continue

        if "." in line:
            line_cut = line.split(".", 1)[0]
        else:
            line_cut = line

        m = re.search(r"\bPIC\b\s+(.+)$", line_cut, flags=re.IGNORECASE)
        if not m:
            continue

        return pic_length(m.group(1))

    return None




def extract_select_assign(cobol: str) -> Dict[str, str]:
    """
    Mapping file-name -> asignación física.
    Soporta:
      SELECT A ASSIGN TO TRFIN
      SELECT A ASSIGN TO "TRFIN"
      SELECT A ASSIGN TO 'TRFIN'
    """
    mapping: Dict[str, str] = {}

    pattern = re.compile(
        r"^\s*SELECT\s+([A-Z0-9-]+)\s+ASSIGN\s+TO\s+("
        r"(['\"])(.*?)\3"         # con comillas
        r"|([A-Z0-9-]+)"          # sin comillas
        r")",
        flags=re.IGNORECASE | re.MULTILINE
    )

    for m in pattern.finditer(cobol):
        file_name = m.group(1).upper()
        if m.group(4) is not None:      # con comillas
            assign_to = m.group(4)
        else:                            # sin comillas
            assign_to = m.group(5)
        mapping[file_name] = assign_to

    return mapping

def extract_sql_table_usage(cobol: str) -> List[Dict[str, Any]]:
    """
    Devuelve lista de usos SQL encontrados:
    - INSERT INTO <TABLE>
    - UPDATE <TABLE>
    - DELETE FROM <TABLE>
    - SELECT ... FROM <TABLE>  (simple; puede haber joins)
    """
    usages: List[Dict[str, Any]] = []

    # INSERT INTO TABLA
    for m in re.finditer(r"\bEXEC\s+SQL\b.*?\bINSERT\s+INTO\s+([A-Z0-9_#$@-]+)\b.*?\bEND-EXEC\b",
                         cobol, flags=re.IGNORECASE | re.DOTALL):
        usages.append({"table": m.group(1).upper(), "op": "INSERT"})

    # UPDATE TABLA
    for m in re.finditer(r"\bEXEC\s+SQL\b.*?\bUPDATE\s+([A-Z0-9_#$@-]+)\b.*?\bEND-EXEC\b",
                         cobol, flags=re.IGNORECASE | re.DOTALL):
        usages.append({"table": m.group(1).upper(), "op": "UPDATE"})

    # DELETE FROM TABLA
    for m in re.finditer(r"\bEXEC\s+SQL\b.*?\bDELETE\s+FROM\s+([A-Z0-9_#$@-]+)\b.*?\bEND-EXEC\b",
                         cobol, flags=re.IGNORECASE | re.DOTALL):
        usages.append({"table": m.group(1).upper(), "op": "DELETE"})

    # SELECT ... FROM TABLA (simple)
    # Nota: si hay JOIN, va a tomar solo la primera tabla después de FROM.
    for m in re.finditer(r"\bEXEC\s+SQL\b.*?\bSELECT\b.*?\bFROM\s+([A-Z0-9_#$@-]+)\b.*?\bEND-EXEC\b",
                         cobol, flags=re.IGNORECASE | re.DOTALL):
        usages.append({"table": m.group(1).upper(), "op": "SELECT"})

    return usages

def extract_transaction_control(cobol: str) -> Dict[str, bool]:
    """
    Detecta COMMIT y ROLLBACK reales (no comentados).
    """
    cobol_nc = remove_cobol_comments(cobol)

    has_commit = re.search(
        r"\bEXEC\s+SQL\b.*?\bCOMMIT\b.*?\bEND-EXEC\b",
        cobol_nc,
        flags=re.IGNORECASE | re.DOTALL
    ) is not None

    has_rollback = re.search(
        r"\bEXEC\s+SQL\b.*?\bROLLBACK\b.*?\bEND-EXEC\b",
        cobol_nc,
        flags=re.IGNORECASE | re.DOTALL
    ) is not None

    return {
        "has_commit": has_commit,
        "has_rollback": has_rollback,
    }


def summarize_table_usage(usages: List[Dict[str, Any]]) -> Dict[str, List[str]]:
    """
    TRFIB -> ["INSERT", "SELECT"]
    """
    out: Dict[str, List[str]] = {}
    for u in usages:
        t = u["table"]
        op = u["op"]
        out.setdefault(t, [])
        if op not in out[t]:
            out[t].append(op)
    return out


def extract_open_modes(cobol: str) -> Dict[str, str]:
    """
    Mapping file-name -> modo de OPEN (INPUT/OUTPUT/I-O/EXTEND).
    Soporta:
      OPEN INPUT A B
      OPEN INPUT A
           B
      OPEN OUTPUT X Y
    """
    modes: Dict[str, str] = {}

    lines = cobol.splitlines()
    in_open = False
    current_mode: Optional[str] = None

    # Para cortar el bloque OPEN si empieza otra sentencia "fuerte"
    strong_stmt = re.compile(
        r"^\s*(PERFORM|READ|WRITE|CLOSE|IF|MOVE|ADD|SUBTRACT|DISPLAY|CALL|EXEC\s+SQL)\b",
        re.IGNORECASE
    )

    for line in lines:
        line_strip = line.strip()

        # Inicio de OPEN
        if re.match(r"^OPEN\b", line_strip, flags=re.IGNORECASE):
            in_open = True
            current_mode = None
            line_strip = re.sub(r"^OPEN\b", "", line_strip, flags=re.IGNORECASE).strip()

        if in_open:
            # Si arranca otra sentencia fuerte, terminamos el OPEN
            if strong_stmt.match(line_strip):
                in_open = False
                current_mode = None
                continue

            # Quitamos puntos para no romper tokens
            line_strip = line_strip.replace(".", " ")

            # Tokenizamos palabras (incluye I-O)
            tokens = re.findall(r"\bI-O\b|\b[A-Z0-9-]+\b", line_strip.upper())

            for t in tokens:
                if t in ("INPUT", "OUTPUT", "EXTEND", "I-O"):
                    current_mode = t
                else:
                    # Si ya hay modo, todo lo demás son archivos hasta que cambie el modo
                    if current_mode:
                        modes[t] = current_mode

            # Fin lógico del OPEN si la línea quedó vacía
            if line_strip.strip() == "":
                in_open = False
                current_mode = None

    return modes


"""def extract_displays(cobol: str) -> List[str]:
    # Captura DISPLAY "...." o DISPLAY '....' y devuelve SOLO el texto (sin comillas)
    results: List[str] = []
    for m in re.finditer(r"\bDISPLAY\s+(['\"])(.*?)\1", cobol, flags=re.IGNORECASE):
        text = m.group(2)
        results.append(text)
    return results"""

# [NUEVO] Detecta PERFORM VARYING (forma típica)
def extract_perform_varying(cobol: str) -> List[Dict[str, Any]]:
    loops: List[Dict[str, Any]] = []

    pattern = re.compile(
        r"\bPERFORM\s+VARYING\s+([A-Z0-9-]+)\s+FROM\s+(\d+)\s+BY\s+(\d+)\s+UNTIL\s+\1\s*>\s*(\d+)",
        flags=re.IGNORECASE
    )

    for m in pattern.finditer(cobol):
        loops.append({
            "type": "perform_varying",
            "var": m.group(1).upper(),
            "from": int(m.group(2)),
            "by": int(m.group(3)),
            "until_op": ">",
            "until_value": int(m.group(4)),
        })

    return loops


# [NUEVO] Detecta IF VAR = "LITERAL" o IF VAR = 3
def extract_if_conditions(cobol: str) -> List[Dict[str, Any]]:
    conds: List[Dict[str, Any]] = []

    # IF WS-PAIS = "FRANCIA"
    pattern_str = re.compile(
        r"\bIF\s+([A-Z0-9-]+)\s*=\s*(['\"])(.*?)\2",
        flags=re.IGNORECASE
    )
    for m in pattern_str.finditer(cobol):
        conds.append({
            "left": m.group(1).upper(),
            "op": "=",
            "right_type": "string",
            "right": m.group(3),
        })

    # IF WS-CONTADOR = 3
    pattern_num = re.compile(
        r"\bIF\s+([A-Z0-9-]+)\s*=\s*(\d+)\b",
        flags=re.IGNORECASE
    )
    for m in pattern_num.finditer(cobol):
        conds.append({
            "left": m.group(1).upper(),
            "op": "=",
            "right_type": "number",
            "right": int(m.group(2)),
        })

    return conds


def extract_moves(cobol: str) -> List[Dict[str, Any]]:
    moves: List[Dict[str, Any]] = []

    pattern = re.compile(
        r"\bMOVE\s+("
        r"\d+|"                 # numero
        r"['\"].*?['\"]|"       # string
        r"[A-Z0-9-]+"           # identificador
        r")\s+TO\s+([A-Z0-9-]+)\b",
        flags=re.IGNORECASE
    )

    for m in pattern.finditer(cobol):
        raw_value = m.group(1).strip()
        target = m.group(2).upper()

        if raw_value.isdigit():
            value_type = "number"
            value: Any = int(raw_value)
        elif raw_value.startswith(("'", '"')) and raw_value.endswith(("'", '"')):
            value_type = "string"
            value = raw_value.strip("'\"")
        else:
            value_type = "identifier"
            value = raw_value.upper()

        moves.append({
            "target": target,
            "value_type": value_type,
            "value": value,
        })

    return moves


def extract_perform_calls(cobol: str) -> List[Dict[str, Any]]:
    calls: List[Dict[str, Any]] = []
    # PERFORM 1000-INICIO
    pattern = re.compile(r"\bPERFORM\s+([0-9A-Z-]+)\b", flags=re.IGNORECASE)
    for m in pattern.finditer(cobol):
        name = m.group(1).upper()
        # evitamos contar PERFORM VARYING / PERFORM UNTIL como "call"
        if name in ("VARYING", "UNTIL"):
            continue
        calls.append({"type": "perform_call", "target": name})
    return calls


def extract_perform_until(cobol: str) -> List[Dict[str, Any]]:
    loops: List[Dict[str, Any]] = []
    # PERFORM UNTIL FIN-ENTRADA ... END-PERFORM
    pattern = re.compile(
        r"\bPERFORM\s+UNTIL\s+([A-Z0-9-]+)",
        flags=re.IGNORECASE
    )
    for m in pattern.finditer(cobol):
        loops.append({
            "type": "perform_until",
            "until": m.group(1).upper()
        })
    return loops

def extract_io(cobol: str) -> Dict[str, Any]:
    io = {"open": [], "read": [], "write": [], "close": []}

    for m in re.finditer(r"\bOPEN\s+(INPUT|OUTPUT|I-O|EXTEND)\s+([A-Z0-9-]+)", cobol, flags=re.IGNORECASE):
        io["open"].append({"mode": m.group(1).upper(), "file": m.group(2).upper()})

    for m in re.finditer(r"\bREAD\s+([A-Z0-9-]+)\b", cobol, flags=re.IGNORECASE):
        io["read"].append({"file": m.group(1).upper()})

    for m in re.finditer(r"\bWRITE\s+([A-Z0-9-]+)\b", cobol, flags=re.IGNORECASE):
        io["write"].append({"record": m.group(1).upper()})

    for m in re.finditer(r"\bCLOSE\s+([A-Z0-9-]+(?:\s+[A-Z0-9-]+)*)", cobol, flags=re.IGNORECASE):
        io["close"].append({"files": [x.upper() for x in m.group(1).split()]})

    return io



#def detect_stop_run(cobol: str) -> bool:
  #  return re.search(r"\bSTOP\s+RUN\b", cobol, flags=re.IGNORECASE) is not None


#def detect_goback(cobol: str) -> bool:
 #   return re.search(r"\bGOBACK\b", cobol, flags=re.IGNORECASE) is not None


def explain_cobol_file(path: str) -> CobolExplanation:
    raw = read_text(path)
    cobol = normalize(raw)

    loops = extract_perform_varying(cobol)
    loops.extend(extract_perform_until(cobol))   # <-- SUMA EL UNTIL
    
    fds = extract_fds(cobol)
    assigns = extract_select_assign(cobol)
    open_modes = extract_open_modes(cobol)
    tx = extract_transaction_control(cobol)
     # ✅ NUEVO: tamaños reales por FD
    fd_sizes = extract_fd_record_sizes(cobol)

    sql_usages = extract_sql_table_usage(cobol)
    tables = summarize_table_usage(sql_usages)

    # Clasificación por modo OPEN
    inputs = [f for f, m in open_modes.items() if m == "INPUT"]
    outputs = [f for f, m in open_modes.items() if m in ("OUTPUT", "EXTEND", "I-O")]

    files_info = {
        "fds": fds,
        "assign": assigns,
        "open_modes": open_modes,
        "inputs": inputs,
        "outputs": outputs,
        # NUEVO: SQL / tablas
        "sql_tables": tables, 
        "tx": tx,                 # <-- NUEVO
        "fd_sizes": fd_sizes,
    }

    return CobolExplanation(
        program_id=extract_program_id(cobol),
        #displays=extract_displays(cobol),
        #has_stop=detect_stop_run(cobol),
        #has_goback=detect_goback(cobol),
        loops=loops,
        conditions=extract_if_conditions(cobol),
        moves=extract_moves(cobol),
        files=files_info,
    )


def write_text(path: str, content: str) -> None:
    with open(path, "w", encoding="utf-8") as f:
        f.write(content)


def write_json(path: str, data: Dict[str, Any]) -> None:
    with open(path, "w", encoding="utf-8") as f:
        json.dump(data, f, ensure_ascii=False, indent=2)


if __name__ == "__main__":
    # cobol_file = "hola.cbl"
    #cobol_file = "SalidaF.cbl"
    cobol_file = "COBDB2.cbl"

    explanation = explain_cobol_file(cobol_file)

    tech = explanation.to_text_technical()
    func = explanation.to_text_functional()

    print(tech)
    print("\n" + "=" * 60 + "\n")
    print(func)

    write_text("Analisis_Cobol_Tecnico.txt", tech)
    write_text("Analisis_Cobol_Funcional.txt", func)

    print("\n✅ Archivos generados:")
    print(" - Analisis_Cobol_Tecnico.txt")
    print(" - Analisis_Cobol_Funcional.txt")

    ##write_json("salida.json", explanation.to_json_dict())