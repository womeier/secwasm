(module
    (memory 1)
    (func (export "foo") (param i32) (result i32)
        i32.const 0
        local.get 0
        i32.store
        i32.const 1
        i32.load
    )
)
