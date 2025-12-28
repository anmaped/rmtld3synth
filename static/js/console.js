const logConsole = document.getElementById("log-console");
const header = document.getElementById('headingLogConsole');
const collapseLogConsole = document.getElementById('collapseLogConsole');
const toggleBtn = document.getElementById('toggleConsoleBtn');

let isDragging = false;
let startY = 0, startHeight = 0;
let moved = false;
let isClick = false;

const DRAG_THRESHOLD = 5; // pixels
const CONSOLE_HEIGHT_KEY = 'logConsoleHeight';
const CONSOLE_COLLAPSED_KEY = 'logConsoleCollapsed';
const MIN_HEIGHT = 200;
const MAX_HEIGHT = 600;

// Restore saved height
const savedHeightRaw = localStorage.getItem(CONSOLE_HEIGHT_KEY);
const savedHeight = savedHeightRaw ? parseInt(savedHeightRaw, 10) : null;
if (savedHeight) {
    const height = Math.max(MIN_HEIGHT, Math.min(savedHeight, MAX_HEIGHT));
    logConsole.style.height = height + 'px';
    console.log("Restored log console height:", height);
}

// Initialize Bootstrap Collapse
const bsCollapse = new bootstrap.Collapse(collapseLogConsole, {
    toggle: localStorage.getItem(CONSOLE_COLLAPSED_KEY) !== 'true' // don't auto-toggle on init
});

// Toggle button click
toggleBtn.addEventListener('click', () => {
    if (!isClick) {
        // Prevent toggle if we were dragging
        console.log("Prevented toggle due to drag.");
        return;
    }

    bsCollapse.toggle();

    isClick = false;
});

// Update button class when collapse changes
collapseLogConsole.addEventListener('shown.bs.collapse', () => {
    toggleBtn.classList.remove('collapsed');
    toggleBtn.setAttribute('aria-expanded', 'true');
    localStorage.setItem(CONSOLE_COLLAPSED_KEY, 'false'); // save state
});

collapseLogConsole.addEventListener('hidden.bs.collapse', () => {
    toggleBtn.classList.add('collapsed');
    toggleBtn.setAttribute('aria-expanded', 'false');
    localStorage.setItem(CONSOLE_COLLAPSED_KEY, 'true'); // save state
});

// Drag start
header.addEventListener('mousedown', (e) => {
    isClick = true;

    // Do not allow drag if accordion is collapsed
    if (!collapseLogConsole.classList.contains('show')) {
        e.preventDefault();
        return;
    }

    //skip right click
    if (e.button !== 0) return;

    isDragging = true;
    moved = false;
    startY = e.clientY;
    startHeight = logConsole.offsetHeight;
    e.preventDefault(); // prevent text selection
});

// Dragging
document.addEventListener('mousemove', (e) => {
    if (!isDragging) return;

    const dy = startY - e.clientY; // moving up increases height

    // Only start counting as "moved" after threshold
    if (!moved && Math.abs(dy) < DRAG_THRESHOLD) return;
    moved = true;

    let newHeight = startHeight + dy;
    newHeight = Math.max(MIN_HEIGHT, Math.min(newHeight, MAX_HEIGHT));
    logConsole.style.height = newHeight + 'px';

    // set style="cursor: ns-resize;"
    toggleBtn.style.cursor = 'ns-resize';
});

// Drag end
document.addEventListener('mouseup', () => {
    if (!isDragging) return;

    if (moved) {
        isClick = false;

        console.log("Finished dragging log console.");
        localStorage.setItem(CONSOLE_HEIGHT_KEY, logConsole.offsetHeight);
        console.log("Saved log console height:", logConsole.offsetHeight);
        updateBodyPadding(); // sync after drag
    } else {
        console.log("Mouse up without drag.");
    }

    // Reset flags
    isDragging = false;
    moved = false;

    // reset cursor
    toggleBtn.style.cursor = '';
});



/* Typing animation */
function typeLine(text, colorClass = "", speed = 1) {
    return new Promise(resolve => {
        const line = document.createElement("div");
        logConsole.appendChild(line);
        let i = 0;
        function type() {
            line.innerHTML = `<span class="${colorClass}">${text.slice(0, i)}</span><span class="cursor"></span>`;
            logConsole.scrollTop = logConsole.scrollHeight;
            if (i < text.length) { i++; setTimeout(type, speed); }
            else { line.innerHTML = `<span class="${colorClass}">${text}</span>`; resolve(); }
        }
        type();
    });
}

const lineCountBadge = document.getElementById('log-console-line-count');

function updateLineCount() {
    const lineCount = logConsole.childElementCount;
    lineCountBadge.textContent = `${lineCount} line${lineCount === 1 ? '' : 's'}`;
}


async function terminalLog(text, colorClass = "") { await typeLine(text, colorClass); updateLineCount(); }



// Connection status badge
function setConnectionStatus(connected) {
    const statusBadge = document.getElementById('log-connection-status');

    statusBadge.classList.remove('d-none');

    if (!window.backendMode) {

        statusBadge.textContent = 'Browser-Only';
        statusBadge.classList.remove('bg-danger');
        statusBadge.classList.remove('bg-success');
        statusBadge.classList.add('bg-secondary');
        return;
    }

    if (connected) {
        statusBadge.textContent = 'Connected';
        statusBadge.classList.remove('bg-danger');
        statusBadge.classList.add('bg-success');
    } else {
        statusBadge.textContent = 'Disconnected';
        statusBadge.classList.remove('bg-success');
        statusBadge.classList.add('bg-danger');
    }
}

setConnectionStatus(false); // initially disconnected

// Accordion auto-height adjustment
// This ensures the accordion's height is always correct based on its content
// It handles dynamic content changes, animations, and window resizing
// It also ensures the body padding is adjusted to prevent content overlap
// This is necessary for the accordion to work properly with Bootstrap's collapse feature
const accordion = document.getElementById('logConsoleAccordion');

function updateBodyPadding() {
    const rect = accordion.getBoundingClientRect();
    document.body.style.paddingBottom = `${rect.height}px`;
}

// Debounce function to limit how often updateBodyPadding is called
let debounceTimeout;
function updateBodyPaddingDebounced() {
    clearTimeout(debounceTimeout);
    debounceTimeout = setTimeout(updateBodyPadding, 50);
}

// Bootstrap collapse events
accordion.addEventListener('shown.bs.collapse', updateBodyPaddingDebounced);
accordion.addEventListener('hidden.bs.collapse', updateBodyPaddingDebounced);

// Window resize
window.addEventListener('resize', updateBodyPaddingDebounced);

// Observe live size changes (animation, content growth, badges, etc.)
const resizeObserver = new ResizeObserver(updateBodyPaddingDebounced);
resizeObserver.observe(accordion);

// Initial
updateBodyPadding();