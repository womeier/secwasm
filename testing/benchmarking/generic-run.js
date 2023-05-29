const fs = require('fs');
const bytes = fs.readFileSync(__dirname + {{file}});

let importObject = {
    env: {
        write_int: (value) => { process.stdout.write(value.toString()) },
        write_char: (value) => {
            var chr = String.fromCharCode(value);
            process.stdout.write(chr);
        },
        get_random: () => { return Math.floor(Math.random() * 10000) - 5000 },
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

    //let len = 10000;
    let len = 1000;
    console.log("Running bubblesort benchmark on array (length = " + len + ") [" + {{file}} + "]")
    obj.instance.exports.init(len);
    obj.instance.exports.print(); console.log("\n===============================");

    const start = Date.now();
    obj.instance.exports.bubblesort();
    const stop = Date.now();
    obj.instance.exports.print(); console.log("\n===============================");
    console.log("Sorting array of length " + len + " with bubblesort took " + (stop - start) + "ms.");
})();
