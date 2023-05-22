(module
  (func (export "foo")
  (param i32) (result i32)
    block (result i32)
      block
        br 0
      end
      i32.const 1
    end
  )
)
