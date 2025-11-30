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

inputEditor.setValue("--help");

// Trigger your 'go()' function on keyup (or debounce it for performance)
inputEditor.session.on('change', () => {
    go();
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

// Initialize csEditor for console output
var csEditor = ace.edit("cs-editor");
csEditor.session.setMode("ace/mode/ocaml");

csEditor.setOptions({
    maxLines: Infinity,
    autoScrollEditorIntoView: true,
    wrap: true,
    showPrintMargin: false,
    showGutter: false,
    readOnly: true
});

// Main code editor with tabs
var code = ace.edit("code");

code.setOptions({
    maxLines: Infinity,
    autoScrollEditorIntoView: true,
    wrap: true,
    showPrintMargin: false
});

sessions = [
    ace.createEditSession("// Code in Tab 1", "ace/mode/ocaml"),
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
