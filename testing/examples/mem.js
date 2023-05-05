const fs = require('fs');
const bytes = fs.readFileSync(__dirname + '/mem.wasm');

let importObject = {
    env: {
        log_i32: (value) => { console.log ("logging i32: ", value) },
        log_i64: (value) => { console.log ("logging i64: ", value) },
    },
};

(async () => {
    const obj = await WebAssembly.instantiate(
        new Uint8Array (bytes), importObject
    );

    obj.instance.exports.write(42);
    obj.instance.exports.read();
  
})();
