(module
  (func (export "foo")
  (param i32) (result i32)
      local.get 0
      block (param i32) (result i32)
        nop
      end
  )
)
