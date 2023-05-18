const fs = require('fs');
const bytes = fs.readFileSync(__dirname + "/" + process.argv[2]);
const val_1 = process.argv[3];

let importObject = {
    env: {
        log_i32: (value) => { console.log("logging i32: ", value) },
        log_i64: (value) => { console.log("logging i64: ", value) },
    },
};

(async () => {
    const obj = await WebAssembly.instantiate(
        new Uint8Array(bytes), importObject
    );

    let res = obj.instance.exports.foo(val_1);
    console.log(`===> ${res}`);
})();
