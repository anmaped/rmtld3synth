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

// click on pending jobs badge to show info
const pendingJobsBadge = document.getElementById('log-pending-jobs');

// Pending jobs badge click
pendingJobsBadge.addEventListener('click', () => {
    // show all pending jobs in a bootstrap table modal
    const bsModal = new bootstrap.Modal(document.getElementById('pendingJobsModal'));
    bsModal.show();
});

// Connection status badge click
const connectionStatusBadge = document.getElementById('log-connection-status');
connectionStatusBadge.addEventListener('click', () => {
    // show connection info modal
    const bsModal = new bootstrap.Modal(document.getElementById('connectionStatusModal'));
    bsModal.show();

    // update backend version and mode
    const backendVersionElem = document.getElementById('backend-version');
    const connectionModeElem = document.getElementById('connection-mode');

    if (!window.backendMode) {
        backendVersionElem.textContent = 'Not connected';
        connectionModeElem.textContent = 'Browser-Only';
    } else {
        // fetch version from backend
        fetch('api/version')
            .then(response => response.json())
            .then(data => {
                backendVersionElem.textContent = data.version || 'Unknown';
            })
            .catch(err => {
                backendVersionElem.textContent = 'Error fetching version';
                console.error('Error fetching backend version:', err);
            });

        connectionModeElem.textContent = 'Server-Backend';

        // set backend url as current url (from browser)
        const backendUrlElem = document.getElementById('backend-url');
        backendUrlElem.textContent = window.location.href;
        // remove hash
        backendUrlElem.textContent = backendUrlElem.textContent.split('#')[0] + "api/";


        // set connection status as connected or disconnected on the modal
        ping('api/status')
            .then(isAlive => {
                if (isAlive) {
                    // set badge text to connected connection-status-badge
                    document.getElementById('connection-status-badge').textContent = 'Connected';
                    // set badge class to bg-success
                    document.getElementById('connection-status-badge').classList.remove('bg-danger');
                    document.getElementById('connection-status-badge').classList.add('bg-success');
                } else {
                    document.getElementById('connection-status-badge').textContent = 'Disconnected';
                    document.getElementById('connection-status-badge').classList.remove('bg-success');
                    document.getElementById('connection-status-badge').classList.add('bg-danger');
                }
            });

        // update uptime based on window.firstConnectedTimestamp
        const uptimeElem = document.getElementById('uptime');
        if (window.firstConnectedTimestamp) {
            const uptimeMs = Date.now() - window.firstConnectedTimestamp;
            const seconds = Math.floor((uptimeMs / 1000) % 60);
            const minutes = Math.floor((uptimeMs / (1000 * 60)) % 60);
            const hours = Math.floor((uptimeMs / (1000 * 60 * 60)) % 24);
            const days = Math.floor(uptimeMs / (1000 * 60 * 60 * 24));

            let uptimeStr = '';
            if (days > 0) uptimeStr += `${days}d `;
            if (hours > 0 || days > 0) uptimeStr += `${hours}h `;
            if (minutes > 0 || hours > 0 || days > 0) uptimeStr += `${minutes}m `;
            uptimeStr += `${seconds}s`;

            uptimeElem.textContent = uptimeStr;
        } else {
            uptimeElem.textContent = 'N/A';
        }


    }
});

// do a watchdog to update connection status every 10 seconds
setInterval(() => {
    if (window.backendMode) {
        // ping backend to check connection
        // check connection with ping
        ping('api/status')
            .then(isAlive => {
                if (isAlive) {
                    // set connection status as connected
                    setConnectionStatus(true);
                    // store timestamp of last successful connection
                    window.lastConnectedTimestamp = Date.now();
                    // set first connection time if not set
                    if (!window.firstConnectedTimestamp) {
                        window.firstConnectedTimestamp = Date.now();
                    }
                } else {
                    // set connection status as disconnected
                    setConnectionStatus(false);
                    // reset first connection time if disconnected for more than 30 seconds
                    if (window.lastConnectedTimestamp &&
                        (Date.now() - window.lastConnectedTimestamp) > 30000) {
                        window.firstConnectedTimestamp = null;
                    }
                }
            });

    }
}, 10000); // every 10 seconds


// ping function to check if backend is reachable
function ping(url, timeout = 3000) {
    const controller = new AbortController();
    const timer = setTimeout(() => controller.abort(), timeout);

    return fetch(url, {
        method: "HEAD",        // minimal request
        cache: "no-store",
        signal: controller.signal
    })
        .then(() => true)          // server reachable
        .catch(err => {
            if (err.name === "AbortError") return false;
            return false;
        })
        .finally(() => clearTimeout(timer));
}


// Toggle button click
toggleBtn.addEventListener('click', (event) => {

    // check if click is inside pending jobs badge
    const rect = pendingJobsBadge.getBoundingClientRect();
    const mouseX = event.clientX;
    const mouseY = event.clientY;
    if (mouseX >= rect.left && mouseX <= rect.right &&
        mouseY >= rect.top && mouseY <= rect.bottom) {
        // Click was inside pending jobs badge, do not toggle
        console.log("Click inside pending jobs badge, not toggling console.");
        return;
    }

    // check if click is inside connection status badge
    const statusRect = connectionStatusBadge.getBoundingClientRect();
    if (mouseX >= statusRect.left && mouseX <= statusRect.right &&
        mouseY >= statusRect.top && mouseY <= statusRect.bottom) {
        // Click was inside connection status badge, do not toggle
        console.log("Click inside connection status badge, not toggling console.");
        return;
    }

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

// Terminal-like logging with typing effect
function typeLine(text, colorClass = "") {
    return new Promise(resolve => {
        const line = document.createElement("div");
        line.innerHTML = `<span class="${colorClass}">${text}</span>`;
        logConsole.appendChild(line);
        logConsole.scrollTop = logConsole.scrollHeight;
        resolve();
    });
}


const lineCountBadge = document.getElementById('log-console-line-count');

function updateLineCount() {
    const lineCount = logConsole.childElementCount;
    lineCountBadge.textContent = `${lineCount} line${lineCount === 1 ? '' : 's'}`;
}


let typingQueue = Promise.resolve();

function terminalLog(text, colorClass = "") {
    typingQueue = typingQueue.then(async () => {
        await typeLine(text, colorClass);
        updateLineCount();
    });
    return typingQueue;
}



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
        // set pending jobs badge visible
        pendingJobsBadge.classList.remove('d-none');
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