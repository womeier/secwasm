const fs = require('fs');
const bytes = fs.readFileSync(__dirname + '/bubblesort.wasm');

let importObject = {
    env: {
        write_int: (value) => { process.stdout.write(value.toString()) },
        write_char: (value) => {
            var chr = String.fromCharCode(value);
            process.stdout.write(chr);
        },
        get_random: () => { return Math.floor(Math.random() * 5000) - 2500 },
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

    let len = 500;
    obj.instance.exports.init(len);
    obj.instance.exports.print(); console.log("\n===============================");

    // TODO measure time of sort();
    obj.instance.exports.sort();

    obj.instance.exports.print(); console.log("");
})();
