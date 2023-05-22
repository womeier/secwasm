(module
  (func (export "foo")
  (param i32) (result i32)
    block (result i32)
      block
        br 1
      end
      i32.const 1
    end
  )
)
