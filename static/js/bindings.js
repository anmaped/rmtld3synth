
/**
 * Message event handler for the web worker.
 * Processes incoming messages and handles file retrieval requests.
 * 
 * @param {MessageEvent} e - The message event object
 * @param {string} e.data - The message data, expected to be a command string
 * 
 * @description
 * - If message starts with "get:file:", extracts filename and reads the file
 * - Posts back a message with the filename and file content
 * - File path is constructed as "/static/" + filename
 */
self.onmessage = function (e) {
  if (e.data.startsWith("get:file:")) {

    x = e.data.substring(11, e.data.length)

    //self.postMessage(readFile("/static/config/default"));
    self.postMessage({ name: x, content: readFile("/static/" + x) });

  }
};

/**
 * Original console.log function reference
 * @type {Function}
 */
original = console.log;

/**
 * Overridden console.log function that forwards logs to the main thread.
 * 
 * @param {...*} args - Arguments to be logged
 * 
 * @description
 * - Calls the original console.log with all arguments
 * - Posts a message to the main thread with "[Worker]" prefix and joined arguments
 */
console.log = function (...args) {
  original(...args);
  self.postMessage("[Worker] " + args.join(" ") + "\n");
};

// Check if rmtld3synth.js exists, fallback to rmtld3synth.bc.js
try {
  importScripts("../bundles/rmtld3synth.js");
} catch (e) {
  console.log("Failed to load rmtld3synth.js, trying rmtld3synth.bc.js");
  importScripts("../bundles/rmtld3synth.bc.js");
}
