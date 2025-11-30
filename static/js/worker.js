
var worker;
function go() {
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
                csEditor.setValue(csEditor.getValue() + m.data, 1);


                const filteredLines = m.data
                    .split('\n')
                    .filter(line => line.startsWith('./'));
                if (filteredLines.length != 0) {
                    lst_files.push(filteredLines);
                    console.log(filteredLines);
                }

                for (let f of lst_files) {
                    worker.postMessage("get:file:" + f);
                }
                lst_files = [];

            }
        }

        if (typeof m.data == 'object') {
            console.log(m.data.content);
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

function setCommand(cmd) {
    inputEditor.setValue(cmd);
}
