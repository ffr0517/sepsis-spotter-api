import React, { useMemo, useRef, useState } from "react";

/**
 * SpotSepsis Front-End (S1 wired to real API)
 * - Chat (left) + Results (right)
 * - Real /s1_infer via VITE_API_BASE
 * - Pasteable “Current Info Sheet” to rehydrate state
 */

// -----------------------------
// Types
// -----------------------------
type ClinicalFeatures = {
  age_months?: number;
  sex?: "M" | "F" | "O" | string;
  // Common fields from your S1 feature list; add as needed:
  "hr.all"?: number;
  "rr.all"?: number;
  "oxy.ra"?: number;
  fever_c?: number;
  cough?: boolean;
  lethargy?: boolean;
  // ... you can add more fields later or map synonyms in the API
};

type CurrentInfoSheet = {
  sheet_version: number;
  created_at: string;
  patient: { anon_id: string };
  context: Record<string, any>;
  features: { clinical: ClinicalFeatures; labs: Record<string, any> };
  s1?: {
    v1: { prob: number; thr: number; call: string };
    v2: { prob: number; thr: number; call: string };
    decision: string;
    model_hash: string;
  };
  notes?: string[];
};

type S1Response = {
  v1: { prob: number; thr: number; call: "Severe" | "Other" };
  v2: { prob: number; thr: number; call: "NOTSevere" | "Other" };
  s1_decision: "Severe" | "NOTSevere" | "Other";
  explanations: { feature: string; contribution: number }[];
  warnings: string[];
  current_info_sheet: CurrentInfoSheet;
  hash: string;
  timing_ms: number;
};

// -----------------------------
// API (S1 real, S2 placeholder)
// -----------------------------
const API = import.meta.env.VITE_API_BASE as string | undefined;

async function s1Infer(features: ClinicalFeatures): Promise<S1Response> {
  if (!API) throw new Error("VITE_API_BASE is not set");
  const r = await fetch(`${API}/s1_infer`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ features }),
  });
  if (!r.ok) {
    const txt = await r.text().catch(() => "");
    throw new Error(`S1 failed (${r.status}): ${txt}`);
  }
  return r.json();
}

// -----------------------------
// Small UI helpers (no Tailwind)
// -----------------------------
const card: React.CSSProperties = {
  background: "#fff",
  border: "1px solid #eee",
  borderRadius: 12,
  boxShadow: "0 1px 3px rgba(0,0,0,0.06)",
  padding: 12,
  marginBottom: 12,
};

function Pill({ text, color = "#666", bg = "#eee" }: { text: string; color?: string; bg?: string }) {
  return (
    <span
      style={{
        display: "inline-block",
        padding: "2px 8px",
        borderRadius: 999,
        fontSize: 12,
        color,
        background: bg,
      }}
    >
      {text}
    </span>
  );
}

// -----------------------------
// Main component
// -----------------------------
export default function App() {
  const [messages, setMessages] = useState<{ role: "user" | "assistant"; content: string }[]>([
    {
      role: "assistant",
      content:
        "Hi! Tell me about your patient. I’ll collect essentials and run S1. You can paste a Current Info Sheet JSON anytime.",
    },
  ]);

  const [clinical, setClinical] = useState<ClinicalFeatures>({});
  const [infoSheet, setInfoSheet] = useState<CurrentInfoSheet | null>(null);
  const [s1, setS1] = useState<S1Response | null>(null);
  const [busy, setBusy] = useState(false);
  const [err, setErr] = useState<string | null>(null);

  const inputRef = useRef<HTMLInputElement>(null);

  const decisionChip = useMemo(() => {
    const decision = s1?.s1_decision || "";
    if (!decision) return null;
    if (decision === "Severe") return <Pill text="Severe" color="#7a061f" bg="#ffe0e6" />;
    if (decision === "NOTSevere") return <Pill text="NOTSevere" color="#0a5" bg="#e6fff2" />;
    return <Pill text="Other" />;
  }, [s1]);

  function pushAssistant(text: string) {
    setMessages((m) => [...m, { role: "assistant", content: text }]);
  }

  // Very light NLP to extract a few fields; you can ignore this if you’ll type in the right-rail editors.
  function parseIntoClinical(text: string) {
    const next = { ...clinical };
    const fever = text.match(/(?:fever|temp)[: ]*\s*(\d{2}(?:\.\d)?)\s*c/i);
    if (fever) next.fever_c = parseFloat(fever[1]);
    const hr = text.match(/(?:hr|heart ?rate)[: ]*\s*(\d{2,3})/i);
    if (hr) next["hr.all"] = parseInt(hr[1]);
    const rr = text.match(/(?:rr|resp(?:iratory)? ?rate)[: ]*\s*(\d{2,3})/i);
    if (rr) next["rr.all"] = parseInt(rr[1]);
    const spo2 = text.match(/sp[o0]2[: ]*\s*(\d{2})/i);
    if (spo2) next["oxy.ra"] = parseInt(spo2[1]);
    if (/cough/i.test(text)) next.cough = true;
    if (/letharg/i.test(text)) next.lethargy = true;
    setClinical(next);
  }

  async function onSend(text: string) {
    if (!text.trim()) return;
    setMessages((m) => [...m, { role: "user", content: text }]);

    // Commands
    if (/^run s1$/i.test(text.trim())) {
      await runS1();
      return;
    }
    if (/^paste sheet:/i.test(text.trim())) {
      const json = text.replace(/^paste sheet:/i, "").trim();
      try {
        const parsed = JSON.parse(json) as CurrentInfoSheet;
        setInfoSheet(parsed);
        setClinical({ ...(parsed.features?.clinical ?? {}) });
        pushAssistant("Restored your Current Info Sheet. You can now run S1 again or add details.");
      } catch {
        pushAssistant("That didn’t parse as JSON. Try again or use the text area on the right.");
      }
      return;
    }

    // Otherwise try to pick up some values
    parseIntoClinical(text);
    pushAssistant("Thanks — updated snapshot. Type ‘run s1’ to triage or add more details.");
  }

  async function runS1() {
    setBusy(true);
    setErr(null);
    try {
      const res = await s1Infer(clinical);
      setS1(res);
      setInfoSheet(res.current_info_sheet);
      pushAssistant(`S1 result: ${res.s1_decision}. You can copy the Current Info Sheet and paste later.`);
    } catch (e: any) {
      const msg = e?.message || "S1 failed";
      setErr(msg);
      pushAssistant("S1 failed — check inputs or API URL.");
    } finally {
      setBusy(false);
    }
  }

  function copyInfoSheet() {
    if (!infoSheet) return;
    navigator.clipboard.writeText(JSON.stringify(infoSheet, null, 2));
    pushAssistant("Copied Current Info Sheet to clipboard. Paste later with ‘paste sheet: { ... }’");
  }

  return (
    <div style={{ background: "#f6f7f9", minHeight: "100vh", padding: 16 }}>
      <div style={{ maxWidth: 1200, margin: "0 auto", display: "grid", gap: 16, gridTemplateColumns: "1fr 1fr" }}>
        {/* Left: Chat */}
        <div style={{ ...card, height: "80vh", display: "flex", flexDirection: "column" }}>
          <div style={{ fontWeight: 600, marginBottom: 8 }}>SpotSepsis Assistant</div>
          <div style={{ flex: 1, overflow: "auto", display: "flex", flexDirection: "column", gap: 8 }}>
            {messages.map((m, i) => (
              <div
                key={i}
                style={{
                  alignSelf: m.role === "assistant" ? "flex-start" : "flex-end",
                  background: m.role === "assistant" ? "#f0f2f5" : "#2563eb",
                  color: m.role === "assistant" ? "#111" : "#fff",
                  padding: "8px 12px",
                  borderRadius: 12,
                  maxWidth: "80%",
                  whiteSpace: "pre-wrap",
                }}
              >
                {m.content}
              </div>
            ))}
          </div>
          <div style={{ display: "flex", gap: 8, marginTop: 8 }}>
            <input
              ref={inputRef}
              placeholder="Describe the patient, or type 'run s1' / 'paste sheet: {json}'"
              onKeyDown={(e) => {
                if (e.key === "Enter") {
                  const v = (e.target as HTMLInputElement).value;
                  (e.target as HTMLInputElement).value = "";
                  onSend(v);
                }
              }}
              style={{ flex: 1, padding: "8px 10px", borderRadius: 10, border: "1px solid #ddd" }}
            />
            <button
              onClick={() => {
                if (!inputRef.current) return;
                const v = inputRef.current.value;
                inputRef.current.value = "";
                onSend(v);
              }}
              style={{
                padding: "8px 12px",
                borderRadius: 10,
                border: "1px solid #2563eb",
                background: "#2563eb",
                color: "#fff",
                fontWeight: 600,
              }}
            >
              Send
            </button>
          </div>
          {err && (
            <div style={{ marginTop: 8, color: "#b00020", fontSize: 12 }}>
              {err} — is <code>VITE_API_BASE</code> set and CORS enabled?
            </div>
          )}
        </div>

        {/* Right: Context & Results */}
        <div style={{ height: "80vh", overflow: "auto" }}>
          <div style={card}>
            <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", marginBottom: 8 }}>
              <div style={{ fontWeight: 600 }}>Patient Snapshot</div>
              <button
                onClick={() => {
                  const age = prompt("Age (months)", clinical.age_months?.toString() || "");
                  const sex = prompt("Sex (M/F/O)", clinical.sex || "");
                  const hr = prompt("HR", clinical["hr.all"]?.toString() || "");
                  const rr = prompt("RR", clinical["rr.all"]?.toString() || "");
                  const spo2 = prompt("SpO2", clinical["oxy.ra"]?.toString() || "");
                  const fever = prompt("Fever °C", clinical.fever_c?.toString() || "");
                  setClinical({
                    ...clinical,
                    age_months: age ? Number(age) : clinical.age_months,
                    sex: sex || clinical.sex,
                    "hr.all": hr ? Number(hr) : clinical["hr.all"],
                    "rr.all": rr ? Number(rr) : clinical["rr.all"],
                    "oxy.ra": spo2 ? Number(spo2) : clinical["oxy.ra"],
                    fever_c: fever ? Number(fever) : clinical.fever_c,
                  });
                }}
                style={{ padding: "4px 8px", borderRadius: 8, border: "1px solid #ddd", background: "#f7f7f7" }}
              >
                Edit
              </button>
            </div>
            <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 8, fontSize: 14 }}>
              <KV label="Age (mo)" value={clinical.age_months ?? "-"} />
              <KV label="Sex" value={clinical.sex ?? "-"} />
              <KV label="HR" value={clinical["hr.all"] ?? "-"} />
              <KV label="RR" value={clinical["rr.all"] ?? "-"} />
              <KV label="SpO2" value={clinical["oxy.ra"] ?? "-"} />
              <KV label="Fever (°C)" value={clinical.fever_c ?? "-"} />
              <KV label="Cough" value={clinical.cough ? "Yes" : "No"} />
              <KV label="Lethargy" value={clinical.lethargy ? "Yes" : "No"} />
            </div>
          </div>

          <div style={card}>
            <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", marginBottom: 8 }}>
              <div style={{ fontWeight: 600 }}>Model Status</div>
              <div style={{ display: "flex", gap: 8 }}>
                <button
                  disabled={busy}
                  onClick={runS1}
                  style={{
                    padding: "4px 8px",
                    borderRadius: 8,
                    border: "1px solid #2563eb",
                    background: busy ? "#e9eefc" : "#eef4ff",
                    color: "#214",
                  }}
                >
                  {busy ? "Running S1…" : "Run S1"}
                </button>
              </div>
            </div>
            <div style={{ display: "flex", alignItems: "center", gap: 8, marginBottom: 8 }}>
              <div style={{ fontSize: 14, color: "#555" }}>Decision:</div>
              {decisionChip || <Pill text="—" />}
            </div>
            <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr 1fr", gap: 8, fontSize: 12 }}>
              <div style={{ background: "#fafafa", borderRadius: 8, padding: 8 }}>
                <div style={{ fontWeight: 600, color: "#666", marginBottom: 6 }}>S1</div>
                {s1 ? (
                  <div style={{ lineHeight: 1.6 }}>
                    <div>v1: prob {s1.v1.prob.toFixed(2)} thr {s1.v1.thr.toFixed(2)} → {s1.v1.call}</div>
                    <div>v2: prob {s1.v2.prob.toFixed(2)} thr {s1.v2.thr.toFixed(2)} → {s1.v2.call}</div>
                    <div>decision: {s1.s1_decision}</div>
                  </div>
                ) : (
                  <div style={{ color: "#777" }}>Not run</div>
                )}
              </div>
              <div style={{ background: "#fafafa", borderRadius: 8, padding: 8 }}>
                <div style={{ fontWeight: 600, color: "#666", marginBottom: 6 }}>Thresholds</div>
                <div>S1 thresholds come from your API.</div>
              </div>
              <div style={{ background: "#fafafa", borderRadius: 8, padding: 8 }}>
                <div style={{ fontWeight: 600, color: "#666", marginBottom: 6 }}>API</div>
                <div style={{ wordBreak: "break-all" }}>{API ? API : "VITE_API_BASE not set"}</div>
              </div>
            </div>
          </div>

          <div style={card}>
            <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", marginBottom: 8 }}>
              <div style={{ fontWeight: 600 }}>Current Info Sheet</div>
              <div style={{ display: "flex", gap: 8 }}>
                <button
                  disabled={!infoSheet}
                  onClick={copyInfoSheet}
                  style={{ padding: "4px 8px", borderRadius: 8, border: "1px solid #ddd", background: "#f7f7f7" }}
                >
                  Copy
                </button>
              </div>
            </div>
            <textarea
              placeholder="Paste a Current Info Sheet JSON here to restore…"
              defaultValue={infoSheet ? JSON.stringify(infoSheet, null, 2) : ""}
              onBlur={(e) => {
                try {
                  const parsed = JSON.parse(e.target.value) as CurrentInfoSheet;
                  setInfoSheet(parsed);
                  setClinical({ ...(parsed.features?.clinical ?? {}) });
                  pushAssistant("Sheet restored.");
                } catch {
                  // ignore
                }
              }}
              style={{
                width: "100%",
                height: 180,
                fontFamily: "ui-monospace, SFMono-Regular, Menlo, monospace",
                fontSize: 12,
                border: "1px solid #ddd",
                borderRadius: 8,
                padding: 8,
                background: "#fff",
              }}
            />
          </div>

          <div style={card}>
            <div style={{ fontWeight: 600, marginBottom: 6 }}>Safety & Disclaimer</div>
            <div style={{ fontSize: 12, color: "#666" }}>
              This tool is clinical decision support and not a diagnosis. Use clinical judgment and local guidelines.
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}

function KV({ label, value }: { label: string; value: React.ReactNode }) {
  return (
    <div style={{ display: "flex", justifyContent: "space-between" }}>
      <span style={{ color: "#666" }}>{label}</span>
      <span style={{ fontWeight: 600 }}>{value as any}</span>
    </div>
  );
}

