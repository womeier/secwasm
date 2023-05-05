(module
  ;; this is common when writing WASM by hand
  (func (export "add")
  (param $val_1 i32) (param $val_2 i32)
    (result i32)
      local.get $val_1
      local.get $val_2
      i32.add
  )
  ;; can also refer to variables by their index
  (func (export "addAlt")
  (param i32 i32) (result i32)
      local.get 0
      local.get 1
      i32.add
  )
)
