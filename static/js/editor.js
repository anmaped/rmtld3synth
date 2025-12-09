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
            runWorker();
        }
        else {
            // Handle backend mode logic here
            console.log("Backend mode is enabled; not running worker.");
            log(JSON.stringify({ input_dsl: inputEditor.getValue(), ocaml_language: true }))
            // do a post request to the server with the inputEditor content
            fetch('/api/request', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: inputEditor.getValue()
            })
                .then(response => response.json())
                .then(data => {
                    // Handle server response
                    console.log('Server response:', data);


                    // check the request every 2 seconds until we get a response with status 'completed'
                    const intervalId = setInterval(() => {
                        fetch('/api/request/' + data.hash_id)
                            .then(response => response.json())
                            .then(data => {
                                console.log('Status response:', data);
                                if (data.status === 'completed') {
                                    clearInterval(intervalId);
                                    log("Request completed successfully.");

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
                                }
                                else if (data.status === 'error') {
                                    clearInterval(intervalId);
                                    console.error('Error processing request on server.');
                                }
                            })
                            .catch((error) => {
                                console.error('Error:', error);
                            });
                    }, 200);

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

// Store tab names corresponding to each session
const sessionNames = [
    "output.txt"
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

// Initialize with tab1
switchTab('0');


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
    }
}
