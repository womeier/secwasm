(module
    (global (mut i32<Public>))
    (global (mut i32<Secret>))

    (func (param i32<Public> i32<Secret>)
        (; Ok! ;)
        local.get 0
        global.set 0
    )
)
