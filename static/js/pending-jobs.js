
// session storage helper
const pendingJobsKey = 'pendingJobs';

// Function to update pending jobs count badge
function updatePendingJobsCount(delta) {
    let pendingJobsCountElem = document.getElementById('pending-jobs-count');
    let pendingJobsCount = parseInt(pendingJobsCountElem.textContent);
    pendingJobsCount += delta;
    if (pendingJobsCount < 0) pendingJobsCount = 0;
    pendingJobsCountElem.textContent = pendingJobsCount;
}

// Function to update pending jobs modal table
function updatePendingJobsModalAndLocalStorage(hash_id, status) {

    let pendingJobsTableBody = document.getElementById('pendingJobsTableBody');
    let rows = pendingJobsTableBody.getElementsByTagName('tr');
    for (let i = 0; i < rows.length; i++) {
        let cells = rows[i].getElementsByTagName('td');

        if (cells[0].textContent === hash_id) {
            log("Update row " + i + " with content " + cells[0].innerText + " time " + cells[2].innerText + " for hash_id " + hash_id);

            cells[1].textContent = status;
            cells[1].style.color = status === 'Completed' ? 'green' : (status === 'Error' ? 'red' : '');
            // update timestamp to current time minus the start time cells[2].textContent eg. 1/17/2026, 6:06:51 PM (10s later)
            let startTime = new Date(cells[2].innerText);
            let endTime = new Date();
            let durationSeconds = Math.round((endTime - startTime));
            cells[2].textContent = endTime.toLocaleString() + ` (${durationSeconds} ms)`;
            updatePendingJobsModalLocalStorage(hash_id, status, cells[2].textContent);
            break;
        }
    }
}

// add session storage entry
function addPendingJobToModalLocalStorage(hash_id, status) {
    // store all info in session storage if does not exists
    let pendingJobs = JSON.parse(localStorage.getItem(pendingJobsKey) || '[]');
    pendingJobs.push({ hash_id: hash_id, status: status, created: new Date().toLocaleString() });
    localStorage.setItem(pendingJobsKey, JSON.stringify(pendingJobs));
}

// update session storage
function updatePendingJobsModalLocalStorage(hash_id, status, timestamp) {
    let pendingJobs = JSON.parse(localStorage.getItem(pendingJobsKey) || '[]');
    for (let i = 0; i < pendingJobs.length; i++) {
        if (pendingJobs[i].hash_id === hash_id) {
            pendingJobs[i].status = status;
            pendingJobs[i].created = timestamp;
            break;
        }
    }
    localStorage.setItem(pendingJobsKey, JSON.stringify(pendingJobs));
}

function initFromPendingJobsLocalStorage() {
    let pendingJobs = JSON.parse(localStorage.getItem(pendingJobsKey) || '[]');
    for (let job of pendingJobs) {
        addPendingJobToModal(job.hash_id, job.status, job.created);
    }
    // update requested jobs count badge
    let requestedJobsCountElem = document.getElementById('requested-jobs-count');
    requestedJobsCountElem.textContent = pendingJobs.length;
}


// Function to add a pending job to the modal table
function addPendingJobToModal(hash_id, status, timestamp = null) {
    let pendingJobsTableBody = document.getElementById('pendingJobsTableBody');

    // Remove the "No pending jobs" placeholder row if it exists
    const existingRows = pendingJobsTableBody.getElementsByTagName('tr');
    if (existingRows.length === 1) {
        const firstCell = existingRows[0].getElementsByTagName('td')[0];
        if (firstCell && firstCell.textContent.trim() === 'No pending jobs') {
            existingRows[0].remove();
        }
    }

    // add new row
    let newRow = document.createElement('tr');
    if (!timestamp) {
        timestamp = new Date().toLocaleString();
    }
    const cells = [hash_id, status, timestamp, '--'];
    cells.forEach(text => {
        const td = document.createElement('td');
        td.textContent = text;
        if (text === status) {
            td.style.color = status === 'Completed' ? 'green' : (status === 'Error' ? 'red' : '');
        }
        if (text === hash_id) {
            td.style.fontFamily = 'monospace';
            // add link to view details
            const link = document.createElement('a');
            link.href = '#' + hash_id;
            link.textContent = hash_id;
            link.onclick = function (e) {
                // Fetch request details from API
                fetch('api/request/' + hash_id)
                    .then(response => response.json())
                    .then(data => {
                        console.log('Job details:', data);

                        openNewPageTab(`Request ${hash_id}`, hash_id, data);

                    }
                    )
                    .catch(error => {
                        console.error('Error fetching details:', error);
                        alert('Error fetching details for ' + hash_id);
                    });
            };
            td.textContent = ''; // clear text
            td.appendChild(link);
        }
        newRow.appendChild(td);
    });
    pendingJobsTableBody.appendChild(newRow);

    return timestamp;
}

// Function to add a pending job to the modal table
function addPendingJobToModalAndLocalStorage(hash_id, status) {
    timestamp = addPendingJobToModal(hash_id, status);

    addPendingJobToModalLocalStorage(hash_id, status, timestamp);
}

function openNewPageTab(name, hash_id, data) {

    // open a new window
    const detailsWindow = window.open('', '_blank');

    if (!detailsWindow) {
        alert('Please allow popups for this site to view job details.');
        return;
    }

    const doc = detailsWindow.document;

    // Basic document setup
    doc.title = name || 'Job Details';
    doc.documentElement.lang = 'en';

    // Meta tags
    const metaCharset = doc.createElement('meta');
    metaCharset.setAttribute('charset', 'UTF-8');

    const metaViewport = doc.createElement('meta');
    metaViewport.name = 'viewport';
    metaViewport.content = 'width=device-width, initial-scale=1.0, maximum-scale=1.0';

    doc.head.append(metaCharset, metaViewport);

    // Copy styles
    document
        .querySelectorAll('link[rel="stylesheet"], style')
        .forEach(node => {
            doc.head.appendChild(node.cloneNode(true));
        });

    // ---- Body content ----
    const container = doc.createElement('div');
    container.className = 'container p-4';

    const title = doc.createElement('h5');
    title.innerHTML = `Job Hash ID: <code>${hash_id}</code>`;

    const status = doc.createElement('p');
    const statusColor =
        data.status === 'completed'
            ? 'green'
            : data.status === 'error'
                ? 'red'
                : 'gray';

    const statusText = doc.createElement('strong');
    statusText.textContent = 'Status: ';
    const statusSpan = doc.createElement('span');
    statusSpan.style.color = statusColor;
    statusSpan.textContent = data.status;
    status.textContent = '';
    status.appendChild(statusText);
    status.appendChild(statusSpan);

    const resultLabel = doc.createElement('p');
    resultLabel.innerHTML = '<strong>Result:</strong>';

    const pre = doc.createElement('pre');
    pre.style.cssText =
        'overflow-y:auto;background:#f5f5f5;padding:10px;border-radius:4px;';
    pre.textContent = data.result; // SAFE (no XSS)

    container.append(title, status, resultLabel, pre);
    doc.body.appendChild(container);
}

// initialize from local storage on page load
initFromPendingJobsLocalStorage();