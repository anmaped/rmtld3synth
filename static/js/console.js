const logConsole = document.getElementById("log-console");
const header = document.getElementById('headingLogConsole');

let isDragging = false;
let startY, startHeight;

header.addEventListener('mousedown', (e) => {
    isDragging = true;
    startY = e.clientY;
    startHeight = logConsole.offsetHeight;
    e.preventDefault(); // prevent text selection
});

document.addEventListener('mousemove', (e) => {
    if (!isDragging) return;
    const dy = startY - e.clientY; // moving up increases height
    let newHeight = startHeight + dy;
    newHeight = Math.max(100, Math.min(newHeight, 600)); // min 100px, max 600px
    logConsole.style.height = newHeight + 'px';
});

document.addEventListener('mouseup', () => {
    isDragging = false;
});


/* Typing animation */
function typeLine(text, colorClass = "", speed = 1) {
    return new Promise(resolve => {
        const line = document.createElement("div");
        logConsole.appendChild(line);
        let i = 0;
        function type() {
            line.innerHTML = `<span class="${colorClass}">${text.slice(0,i)}</span><span class="cursor"></span>`;
            logConsole.scrollTop = logConsole.scrollHeight;
            if(i<text.length){i++; setTimeout(type,speed);} 
            else {line.innerHTML = `<span class="${colorClass}">${text}</span>`; resolve();}
        }
        type();
    });
}

const lineCountBadge = document.getElementById('log-console-line-count');

function updateLineCount() {
    const lineCount = logConsole.childElementCount;
    lineCountBadge.textContent = `${lineCount} line${lineCount === 1 ? '' : 's'}`;
}


async function terminalLog(text, colorClass = "") { await typeLine(text,colorClass); updateLineCount(); }


// Connection status badge
function setConnectionStatus(connected) {
    const statusBadge = document.getElementById('log-connection-status');

    if (window.backendMode) {
        statusBadge.classList.remove('d-none');
    }
    else {
        statusBadge.classList.remove('d-none');
        statusBadge.textContent = 'Browser-Only';
        statusBadge.classList.remove('bg-danger');
        statusBadge.classList.remove('bg-success');
        statusBadge.classList.add('bg-secondary');
        return;
    }

    if (connected) {
        statusBadge.textContent = 'Server-Backed Connected';
        statusBadge.classList.remove('bg-danger');
        statusBadge.classList.add('bg-success');
    } else {
        statusBadge.textContent = 'Server-Backed Disconnected';
        statusBadge.classList.remove('bg-success');
        statusBadge.classList.add('bg-danger');
    }
}

setConnectionStatus(false); // initially disconnected
