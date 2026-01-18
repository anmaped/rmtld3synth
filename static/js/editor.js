// Initialize Ace Editors (including input editor, code editor with tabs, and console output editor)

// Input editor for DSL input
const inputEditor = ace.edit("input-editor");
inputEditor.setTheme("ace/theme/textmate");
inputEditor.session.setMode("ace/mode/text"); // plain text mode
inputEditor.setOptions({
    maxLines: 10,
    minLines: 4,
    autoScrollEditorIntoView: true,
    wrap: true,
    showPrintMargin: false,
    showGutter: false
});

inputEditor.setValue("");

// Trigger your 'go()' function on keyup (or debounce it for performance)
inputEditor.session.on('change', ev => {
    console.log("Editor change action: " + ev.action);
    if (ev.action === 'insert') {
        if (window.backendMode === false) {
            let cmd = convertInputFormat(inputEditor.getValue());
            // set code-gen-icon to spinning
            document.getElementById('code-gen-icon').classList.add('spin');
            runWorker(cmd);
            // stop spinning after 1 seconds
            setTimeout(() => {
                document.getElementById('code-gen-icon').classList.remove('spin');
            }, 1000);
        }
        else {
            // set code-gen-icon to spinning
            document.getElementById('code-gen-icon').classList.add('spin');
            // Handle backend mode logic here
            console.log("Backend mode is enabled; not running worker.");
            log(JSON.stringify({ input_dsl: inputEditor.getValue(), ocaml_language: true }))
            // update requested jobs count badge
            let requestedJobsCountElem = document.getElementById('requested-jobs-count');
            let requestedJobsCount = parseInt(requestedJobsCountElem.textContent);
            requestedJobsCount += 1;
            requestedJobsCountElem.textContent = requestedJobsCount;
            // do a post request to the server with the inputEditor content
            fetch('api/request', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: convertInputFormat(inputEditor.getValue())
            })
                .then(response => response.json())
                .then(requestdata => {
                    // Handle server response
                    console.log('Server response:', requestdata);

                    // update pending jobs count badge
                    updatePendingJobsCount(1);

                    // add pending jobs to modal  pendingJobsTableBody
                    addPendingJobToModalAndLocalStorage(requestdata.hash_id, 'Pending');

                    // set hash
                    window.location.hash = requestdata.hash_id;

                    // Start polling for the request status

                    // check the request every 2 seconds until we get a response with status 'completed'
                    const intervalId = setInterval(() => {
                        fetch('api/request/' + requestdata.hash_id)
                            .then(response => response.json())
                            .then(data => {
                                console.log('Status response:', data);
                                if (data.status === 'completed') {
                                    clearInterval(intervalId);

                                    let files = convertMultipartToJsonListAutoBoundary(data.result)

                                    // Log the files to console
                                    console.log("Files received from server:");
                                    files.forEach(file => {
                                        console.log("File:", file.name);
                                        console.log("Content:", file.content);
                                        // You can handle each file here, e.g., display in tabs
                                    });

                                    // Fill the text editor with the received files
                                    fill_text_editor(files);

                                    showFloatingAlert("Code generation completed successfully!", "success", 8000);

                                    log("Request completed successfully.", "text-success");

                                    // stop spinning after 1 seconds
                                    setTimeout(() => {
                                        document.getElementById('code-gen-icon').classList.remove('spin');
                                    }, 1000);

                                    // update pending jobs count badge
                                    updatePendingJobsCount(-1);

                                    // update pending jobs table in modal
                                    updatePendingJobsModalAndLocalStorage(requestdata.hash_id, 'Completed');
                                }
                                else if (data.status === 'error') {
                                    clearInterval(intervalId);
                                    console.error('Error processing request on server.');
                                    console.error('Error details:', data.result);

                                    // data.result may contain vt100 escape sequences, can you convert them to plain text with color or underline

                                    let errorMessage = data.result;

                                    showFloatingAlert("Error processing request on server.", "danger", 15000, ansiOctalToDom(errorMessage));

                                    log("Error processing request on server:\n" + ansiOctalToDom(errorMessage).textContent, "text-danger");

                                    clearEditorToDefault();

                                    // stop spinning after 1 seconds
                                    setTimeout(() => {
                                        document.getElementById('code-gen-icon').classList.remove('spin');
                                    }, 1000);

                                    // update pending jobs count badge
                                    updatePendingJobsCount(-1);

                                    // update pending jobs table in modal
                                    updatePendingJobsModalAndLocalStorage(requestdata.hash_id, 'Error');
                                }
                            })
                            .catch((error) => {
                                console.error('Error:', error);
                            });
                    }, 2000);

                })
                .catch((error) => {
                    console.error('Error:', error);
                });

        }
    }
});

// Optional: mimic textarea height resizing
inputEditor.on('input', () => {
    const lines = inputEditor.session.getLength();
    inputEditor.container.style.height = `${Math.max(60, lines * 20)}px`;
    inputEditor.resize();
});

// Optional: get value like from a textarea
function getInputValue() {
    return inputEditor.getValue();
}

// Main code editor with tabs
var code = ace.edit("code");

code.setOptions({
    maxLines: Infinity,
    autoScrollEditorIntoView: true,
    wrap: true,
    showPrintMargin: false
});

sessions = [
    ace.createEditSession("Run the tool first to see generated code here\nChoose an example on the left to get started.", "ace/mode/text"),
];

// Function to switch between tabs
function switchTab(tabName) {
    console.log(tabName);
    code.setSession(sessions[tabName]);
    code.getSession().setUseWrapMode(true);

    setActiveTab(tabName);
}

function setActiveTab(tabName) {
    // Remove 'active' class from all tabs
    document.querySelectorAll('.tab').forEach(tab => {
        tab.classList.remove('active');
    });

    // Add 'active' class to the selected tab
    const newActiveTab = document.querySelector(`.tab[data-tab="tab${tabName}"]`);
    if (newActiveTab) {
        newActiveTab.classList.add('active');
    }
}

// Update file count badge
function updateFileCount() {
    const tabs = document.querySelectorAll('.tab[data-tab]');
    const count = tabs.length > 0 && tabs[0].textContent.trim() === 'No generated files yet' ? 0 : tabs.length;
    const fileCountSpan = document.getElementById('file-count');
    if (fileCountSpan) {
        fileCountSpan.textContent = count;
    }
}

// Initialize with tab1
switchTab('0');
updateFileCount();


// floating alerts
(function initFloatingAlerts() {
    // Create container once
    if (!document.getElementById("floating-alert-container")) {
        const container = document.createElement("div");
        container.id = "floating-alert-container";
        document.body.appendChild(container);
    }
})();

// Show a floating alert message
function showFloatingAlert(message, type = "danger", timeout = 4000, object = null) {
    const container = document.getElementById("floating-alert-container");
    const template = document.getElementById("floating-alert-template");

    if (!container || !template) return;

    // Clone template
    const alert = template.cloneNode(true);
    alert.removeAttribute("id");

    // Set alert type
    alert.classList.remove("alert-danger", "d-none", "fade");
    alert.classList.add(`alert-${type}`, "floating-alert", "show");

    // Set message and append object if provided
    const alertMsg = alert.querySelector(".alert-msg");
    alertMsg.textContent = message;

    if (object) {
        alertMsg.appendChild(document.createElement("br"));
        alertMsg.appendChild(object);
        alertMsg.innerHTML = alertMsg.innerHTML.replace(/\\n/g, "<br>");
    }

    // Close button
    const closeBtn = alert.querySelector(".btn-close");
    closeBtn.addEventListener("click", () => alert.remove());

    // Add to stack
    container.appendChild(alert);

    // Auto-remove
    if (timeout) {
        setTimeout(() => alert.remove(), timeout);
    }
}

// Clear editor to default state
function clearEditorToDefault() {
    sessions = [
        ace.createEditSession("Run the tool first to see generated code here\nChoose an example on the left to get started.", "ace/mode/text"),
    ];
    document.getElementById('tab-bar').innerHTML = ""; // clear previous tabs
    // reset tab id
    let id = 0;
    const tabHTML = `<div class="tab active" data-tab="tab${id}" onclick="switchTab('${id}')">No generated files yet</div>`;
    document.getElementById('tab-bar').innerHTML += tabHTML;
    switchTab(0);
    updateFileCount();
}


// Server REST API multipart file handling

/**
 * Converts a multipart form data string into a JSON list of file objects.
 * 
 * @param {string} multipartStr - The raw multipart form data string containing file data
 * @param {string} boundary - The boundary string used to separate different parts in the multipart data
 * @returns {Array<{name: string, content: string}>} An array of objects, each containing a filename and its content
 * 
 * @example
 * const multipart = '--boundary\r\nContent-Disposition: form-data; name="file"; filename="test.txt"\r\n\r\nHello World\r\n--boundary--';
 * const files = convert_multipart_to_json_list(multipart, 'boundary');
 * // Returns: [{ name: 'test.txt', content: 'Hello World' }]
 */
function convert_multipart_to_json_list(multipartStr, boundary) {
    const files = [];

    // Split parts by boundary (ignore leading/trailing line breaks)
    const parts = multipartStr
        .split(`--${boundary}`)
        .map(p => p.trim())
        .filter(p => p && p !== '--'); // Remove empty or end marker

    for (const part of parts) {
        // Split headers and body
        const [rawHeaders, ...bodyLines] = part.split(/\r?\n\r?\n/);
        const body = bodyLines.join('\n'); // Rest is the content

        // Parse headers
        const headers = rawHeaders.split(/\r?\n/);
        let filename = null;

        for (const header of headers) {
            const match = header.match(/filename="(.+?)"/);
            if (match) {
                filename = match[1];
                break;
            }
        }

        if (filename) {
            files.push({ name: filename, content: body });
        }
    }

    return files;
}


/**
 * Infers and extracts the boundary string from a multipart message.
 * 
 * Parses a multipart string (e.g., multipart/form-data) to identify and extract
 * the boundary delimiter used to separate parts. Searches for lines starting with
 * "--" and returns the boundary value without leading/trailing dashes.
 * 
 * @param {string} multipartStr - The multipart message string to parse
 * @returns {string|null} The extracted boundary string without dashes, or null if no boundary is found
 * 
 * @example
 * const multipart = '--boundary123\r\nContent-Type: text/plain\r\n';
 * const boundary = inferBoundary(multipart); // Returns: "boundary123"
 */
function inferBoundary(multipartStr) {
    const lines = multipartStr.split(/\r?\n/);
    for (const line of lines) {
        const trimmed = line.trim();
        if (trimmed.startsWith("--") && trimmed.length > 2) {
            // remove the leading "--"
            return trimmed.slice(2).replace(/-+$/, ''); // remove trailing dashes
        }
    }
    return null;
}


/**
 * Converts a multipart string to a JSON list by automatically inferring the boundary.
 * 
 * @param {string} multipartStr - The multipart string to convert
 * @returns {Array} An array representation of the multipart data in JSON format
 * @throws {Error} If the boundary cannot be inferred from the multipart string
 */
function convertMultipartToJsonListAutoBoundary(multipartStr) {
    const boundary = inferBoundary(multipartStr);
    if (!boundary) {
        throw new Error("Could not infer boundary from multipart string");
    }
    return convert_multipart_to_json_list(multipartStr, boundary);
}

/**
 * Fill the Ace editor sessions with a list of files
 * @param {Array<{name: string, content: string}>} files
 */
/**
 * Fills the text editor with multiple file tabs, each configured with appropriate syntax highlighting.
 * Creates Ace editor sessions for each file, applies language-specific modes (C/C++, OCaml, or plain text),
 * and optionally formats C/C++ files using clangformat. Generates tab UI elements and activates the first tab.
 * 
 * @param {Array<{name: string, content: string}>} files - Array of file objects containing name and content properties
 * @returns {void}
 * 
 * @description
 * - Resets and clears existing tabs
 * - Detects file type by extension and sets appropriate Ace editor mode:
 *   - `.c`, `.cpp`, `.h` → C/C++ mode with clangformat
 *   - `.ml`, `.mli` → OCaml mode
 *   - Others → plain text mode
 * - Creates editor sessions stored in global `sessions` array
 * - Generates clickable tab UI in `tab-bar` element
 * - Automatically activates the first tab
 * 
 * @requires ace - Ace Editor library must be loaded
 * @requires clangformat - Function for formatting C/C++ code (async)
 * @requires switchTab - Function to switch between tabs
 * @requires sessions - Global array to store Ace editor sessions
 */
function fill_text_editor(files) {
    let id = 0; // reset id for tabs
    document.getElementById('tab-bar').innerHTML = ""; // clear previous tabs

    for (const file of files) {
        // Determine Ace mode based on filename
        let mode = "ace/mode/text";
        if (file.name && (file.name.endsWith(".c") || file.name.endsWith(".cpp") || file.name.endsWith(".h"))) {
            mode = "ace/mode/c_cpp";
        } else if (file.name && (file.name.endsWith(".ml") || file.name.endsWith(".mli"))) {
            mode = "ace/mode/ocaml";
        }

        // Create Ace session and set mode
        sessions.push(ace.createEditSession("", mode));
        sessions[id].setMode(mode);

        // Set content (optionally run clangformat for C/C++ files)
        if (file.name && (file.name.endsWith(".c") || file.name.endsWith(".cpp") || file.name.endsWith(".h"))) {
            const frozen_id = id;
            clangformat(file.name, file.content).then((formatted) => {
                sessions[frozen_id].setValue(formatted, 1);
            });
        } else {
            sessions[id].setValue(file.content, 1);
        }

        // Add tab to tab bar
        const tabHTML = `<div class="tab${id === 0 ? " active" : ""}" data-tab="tab${id}" onclick="switchTab('${id}')">${file.name}</div>`;
        document.getElementById('tab-bar').innerHTML += tabHTML;

        // Activate first tab by default
        if (id === 0) switchTab(0);

        id += 1;

        // update file count badge
        updateFileCount();

    }
}


function ansiOctalToDom(input) {
    if (!input) return document.createTextNode("");

    const ANSI_COLORS = {
        "30": "black",
        "31": "red",
        "32": "green",
        "33": "yellow",
        "34": "blue",
        "35": "magenta",
        "36": "cyan",
        "37": "white",
        "90": "gray",
        "91": "lightcoral",
        "92": "lightgreen",
        "93": "lightyellow",
        "94": "lightblue",
        "95": "plum",
        "96": "lightcyan",
        "97": "white"
    };

    const container = document.createDocumentFragment();
    const ansiRegex = /\\027\[([0-9:;]*)m/g;

    let lastIndex = 0;
    let match;
    let currentStyles = {};

    const pushTextNode = (text) => {
        if (!text) return;
        const span = document.createElement("span");
        // Apply current styles
        Object.assign(span.style, currentStyles);
        span.textContent = text;
        container.appendChild(span);
    };

    while ((match = ansiRegex.exec(input)) !== null) {
        // Text before this ANSI code
        pushTextNode(input.substring(lastIndex, match.index));

        const codesRaw = match[1].split(/[;:]/).filter(s => s !== "");
        const codes = codesRaw.map(Number).filter(n => !isNaN(n));

        codes.forEach(code => {
            switch (code) {
                case 0: // reset
                    currentStyles = {};
                    break;
                case 1: // bold
                    currentStyles.fontWeight = "bold";
                    break;
                case 3: // italic
                    currentStyles.fontStyle = "italic";
                    break;
                case 4: // underline
                    currentStyles.textDecoration = "underline";
                    break;
                case 21: // bold off
                    delete currentStyles.fontWeight;
                    break;
                case 24: // underline off
                    delete currentStyles.textDecoration;
                    break;
                default:
                    if (ANSI_COLORS[code]) {
                        currentStyles.color = ANSI_COLORS[code];
                    }
                // ignore unknown codes like 58:2 safely
            }
        });

        lastIndex = ansiRegex.lastIndex;
    }

    // Remaining text
    pushTextNode(input.substring(lastIndex));

    return container;
}
