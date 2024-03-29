const fs = require('fs');
const bytes = fs.readFileSync(__dirname + '/add.wasm');
const val_1 = 41;
const val_2 = 1;


let importObject = {
    env: {
        log_i32: (value) => { console.log ("logging i32: ", value) },
        log_i64: (value) => { console.log ("logging i64: ", value) },
    },
/*    env: {
        import_i32: 5_000_000_000, // _ is ignored in numbers in JS and WAT
        import_f32: 123.0123456789,
        import_f64: 123.0123456789,
    } */
};

(async () => {
    const obj = await WebAssembly.instantiate(
        new Uint8Array (bytes), importObject
    );

    let res = obj.instance.exports.add(val_1, val_2);
    console.log(`===> ${res}`);

    let resAlt = obj.instance.exports.addAlt(val_1, val_2);
    console.log(`===> ${resAlt}`);
})();
