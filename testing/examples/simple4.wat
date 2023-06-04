(module
(func (export "foo") (param i32) (result i32)
    (local i32)
    block
        loop
            local.get 0
            i32.const 0
            i32.le_s   ;; check if we're done
            br_if 1
            local.get 1 ;; result
            local.get 0
            local.get 0
            i32.mul     ;; square
            i32.add
            local.set 1 ;; increment result
            local.get 0
            i32.const 1
            i32.sub     ;; n = n - 1
            local.set 0
            br 0
        end
    end
    local.get 1
)
)
