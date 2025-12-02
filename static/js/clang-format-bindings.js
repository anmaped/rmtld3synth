// ------------------------
// Download .tgz
// ------------------------
async function download(url) {
    const res = await fetch(url);
    if (!res.ok) throw new Error("Download failed: " + res.status);
    return new Uint8Array(await res.arrayBuffer());
}

// ------------------------
// Extract .tgz → file map
// ------------------------
async function extractTGZ(tgzBytes) {
    const tarData = await new Promise((resolve, reject) => {
        fflate.gunzip(tgzBytes, (err, data) => err ? reject(err) : resolve(data));
    });

    const entries = await untar(tarData.buffer);
    const files = {};

    for (const entry of entries) {
        files[entry.name] = new Uint8Array(entry.buffer);
    }
    return files;
}

// ------------------------
// Load JS + WASM URLs
// ------------------------
let blobURLStore = {};

async function load(files, dirname = "package/") {
    for (const [name, data] of Object.entries(files)) {
        const type = name.endsWith(".js") ? "application/javascript"
            : name.endsWith(".wasm") ? "application/wasm"
                : null;

        if (!type) continue;

        // Remove dirname prefix from name
        const cleanName = name.replace(/^package\//, '');

        // Store blob URL for ALL files
        const blobURL = URL.createObjectURL(new Blob([data], { type }));
        blobURLStore[cleanName] = blobURL;

        log("Blob URL created:", cleanName);
    }
}

// ------------------------
// Cleanup all Blob URLs
// ------------------------
function cleanupBlobs() {
    for (const [name, url] of Object.entries(blobURLStore)) {
        log("Revoking:", name);
        URL.revokeObjectURL(url);
    }
    blobURLStore = {}; // clear object
    wasmURL = undefined;
    log("All Blob URLs revoked");
}

// ------------------------
// Fetch interceptor for WASM
// ------------------------
const originalFetch = window.fetch;
window.fetch = async function (resource, options) {
    console.log("Fetch:", resource);

    let urlStr = resource;
    if (resource instanceof Request) urlStr = resource.url;

    if (blobURLStore[urlStr]) {
        console.log("Serving \"" + urlStr + "\" from blob URL:", blobURLStore[urlStr]);
        return originalFetch(blobURLStore[urlStr], options);
    }

    return originalFetch(resource, options);
};

// ------------------------
// Load clang-format WASM bundle
// ------------------------
async function loadClangFormatWasm() {
    try {
        const VERSION = "20.1.5";
        const TGZ_URL = `bundles/wasm-fmt-clang-format-${VERSION}.tgz`;

        log("Downloading clang-format WASM bundle...");
        const tgzBytes = await download(TGZ_URL);

        log("Extracting...");
        const files = await extractTGZ(tgzBytes);
        log("Files:\n  " + Object.keys(files).join("\n  "));

        await load(files);

        // only process main clang-format files
        if (blobURLStore["clang-format.js"]) {
            const { default: init, format } = await import(blobURLStore["clang-format.js"]);

            window.clangformat = async (filename, code) => {
                await init("clang-format.wasm");
                log("Initialized WASM");
                return format(code, filename, "Chromium").toString();
            };
        }

        const formatted = await window.clangformat(
            "test.c",
            "int x () { return 42; }"
        );
        log("Test Formatted code:\n" + formatted);

        cleanupBlobs();

        log("Done!");
    } catch (err) {
        log("Error:", err.message);
        console.error(err);
    }
}