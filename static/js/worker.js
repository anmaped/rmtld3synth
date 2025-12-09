
var worker;
function runWorker() {
    lst_files = [];
    id = 0;
    sessions = [];
    document.getElementById('tab-bar').innerHTML = "";

    var argv = encodeURIComponent("rmtld3synth --out-src \".\" " + inputEditor.getValue().replaceAll("'", "\""));
    if (worker) {
        worker.terminate();
    }
    worker = new Worker("js/bindings.js?argv=" + argv);

    code.setValue("", 1);

    worker.onmessage = function (m) {
        if (typeof m.data == 'string') {
            if (m.data != "\0\n") {
                log(m.data);
            }
        }

        if (typeof m.data == 'object') {
            // m.data = {name: ..., content: ...}
            console.log(m.data);
            console.log("tab" + id);

            let mode = "ace/mode/text";
            if (m.data.name && m.data.name.endsWith(".c") || m.data.name.endsWith(".cpp") || m.data.name.endsWith(".h")) {
                mode = "ace/mode/c_cpp";
            }

            if (m.data.name && m.data.name.endsWith(".ml") || m.data.name.endsWith(".mli")) {
                mode = "ace/mode/ocaml";
            }

            sessions.push(ace.createEditSession("", mode));
            sessions[id].setMode(mode);

            if (m.data.name && m.data.name.endsWith(".c") || m.data.name.endsWith(".cpp") || m.data.name.endsWith(".h")) {

                const frozen_id = id;
                clangformat(m.data.name, m.data.content).then((formatted) => {
                    sessions[frozen_id].setValue(formatted, 1);
                });

            } else {
                sessions[id].setValue(m.data.content, 1);
            }

            document.getElementById('tab-bar').innerHTML += ("<div class=\"tab" + (id == 0 ? " active" : "") + "\" data-tab=\"tab" + id + "\" onclick=\"switchTab('" + id + "')\">" + m.data.name + "</div>");
            if (id == 0) { switchTab(0); }
            id = id + 1;

        }
    }

    worker.onerror = function (m) {
        if (typeof m.message == 'string') {
            console.log("error: " + m.message);
        }
    }
}


function parseCommandLine(cmd) {
  // 1. Tokenize respecting quotes, without unescaping
  const tokens = [];
  let current = "";
  let quote = null;

  for (let i = 0; i < cmd.length; i++) {
    const ch = cmd[i];

    if (quote) {
      if (ch === quote) {
        quote = null;
      } else {
        current += ch; // keep characters literally
      }
    } else {
      if (ch === "'" || ch === '"') {
        quote = ch;
      } else if (/\s/.test(ch)) {
        if (current.length) {
          tokens.push(current);
          current = "";
        }
      } else {
        current += ch;
      }
    }
  }

  if (current.length) tokens.push(current);

  // 2. Convert tokens into JSON
  const result = {};
  let i = 0;

  while (i < tokens.length) {
    const t = tokens[i];

    if (t.startsWith("--")) {
      // Convert flag names: '-' → '_'
      let key = t.slice(2).replace(/-/g, "_");

      if (tokens[i + 1] && !tokens[i + 1].startsWith("--")) {
        let val = tokens[++i];

        // numeric conversion only for pure numbers
        if (/^-?\d+(\.\d+)?$/.test(val)) {
          val = Number(val);
        }

        // Handle duplicate flags → array
        if (result[key] !== undefined) {
          if (!Array.isArray(result[key])) {
            result[key] = [result[key]];
          }
          result[key].push(val);
        } else {
          result[key] = val;
        }

      } else {
        result[key] = true;
      }
    }

    i++;
  }

  return result;
}



function setInput(cmd) {
    try {
        if (window.backendMode) {
            inputEditor.setValue(JSON.stringify(parseCommandLine(cmd), null, 2));
        } else {
            inputEditor.setValue(cmd);
        }
        console.log("Set Input:", window.schema);
    } catch (error) {
        console.error('Error setting input:', error);
    }

}
