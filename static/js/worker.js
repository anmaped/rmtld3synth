
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
      // set radio button
      document.getElementById("inputTypeJson").checked = true;
    } else {
      inputEditor.setValue(cmd);
    }
    console.log("Set Input:", window.schema);
  } catch (error) {
    console.error('Error setting input:', error);
  }

}

/**
 * Converts a JSON object to a CLI command string for rmtld3synth.
 * 
 * Parses the provided JSON string and constructs a command-line string by:
 * - Converting object keys from snake_case to kebab-case for CLI flags
 * - Handling boolean values by adding the flag only if true
 * - Handling array values by repeating the flag for each element
 * - Wrapping non-boolean values in single quotes
 * 
 * The resulting command is set as the value of the inputEditor if setInput is true.
 * 
 * @param {string} json - A JSON string representing command options
 * @param {boolean} [setInput=true] - Whether to update the inputEditor with the result
 * @returns {string} The CLI command string
 * @throws {Error} Logs error to console if JSON parsing fails
 * 
 * @example
 * // Input: '{"output_file": "result.txt", "verbose": true, "items": ["a", "b"]}'
 * // Output: "rmtld3synth --output-file 'result.txt' --verbose --items 'a' --items 'b'"
 */
function convertJsonToCli(json, setInput = true) {

  let cmd = "rmtld3synth";
  try {
    const obj = JSON.parse(json);
    for (const key in obj) {
      const value = obj[key];
      const flag = `--${key.replace(/_/g, "-")}`;
      if (typeof value === "boolean") {
        if (value) {
          cmd += ` ${flag}`;
        }
      } else if (Array.isArray(value)) {
        value.forEach((v) => {
          cmd += ` ${flag} '${v}'`;
        });
      } else {
        cmd += ` ${flag} '${value}'`;
      }
    }
    if (setInput)
      inputEditor.setValue(cmd);
  } catch (error) {
    console.error("Error converting JSON to CLI:", error);
  }

  return cmd;
}

/**
 * Converts a command-line interface string to JSON format and updates the input editor.
 * 
 * Parses the provided CLI command string and constructs a JSON object by:
 * - Extracting flags and their values from the command string
 * - Converting kebab-case flag names to snake_case for JSON keys
 * - Handling boolean flags (flags without values)
 * - Handling array values (repeated flags)
 * - Preserving numeric types
 * 
 * The resulting JSON is set as the value of the inputEditor if setInput is true.
 * 
 * @param {string} cmd - The command-line string to be converted to JSON
 * @param {boolean} [setInput=true] - Whether to update the inputEditor with the result
 * @returns {string} The JSON string representation of the parsed command
 * @throws {Error} Logs error to console if parsing or conversion fails
 * 
 * @example
 * // Input: "rmtld3synth --output-file 'result.txt' --verbose --items 'a' --items 'b'"
 * // Output: '{\n  "output_file": "result.txt",\n  "verbose": true,\n  "items": ["a", "b"]\n}'
 */
function convertCliToJson(cmd, setInput = true) {
  try {
    const obj = parseCommandLine(cmd);
    if (setInput) {
      inputEditor.setValue(JSON.stringify(obj, null, 2));
    }
    return JSON.stringify(obj, null, 2);

  } catch (error) {
    console.error("Error converting CLI to JSON:", error);
  }

}

/**
 * Checks if a given string is a valid JSON format.
 * @param {string} str - The string to be checked.
 * @returns {boolean} - Returns true if the string is valid JSON, false otherwise.
 */
function isJsonString(str) {
  try {
    JSON.parse(str);
  } catch (e) {
    return false;
  }
  return true;
}

function updateInputConversion(value) {

  if (!isJsonString(value)) {
    let x = convertCliToJson(inputEditor.getValue(), false);
    return x;
  }

  return value;
}
