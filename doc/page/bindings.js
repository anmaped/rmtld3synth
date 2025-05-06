
// Respond to a request
self.onmessage = function (e) {
  if (e.data.startsWith("get:file:")) {

    x = e.data.substring(11, e.data.length)

    //self.postMessage(readFile("/static/config/default"));
    self.postMessage({name: x, content: readFile("/static/"+x)});

  }
};

importScripts("rmtld3synth.js")
