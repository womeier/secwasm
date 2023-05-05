(module
  ;; function import
  (import "env" "log_i32" (func $log_i32 (param i32)))
  ;; memory
  (memory 10) ;; times 64KB

  (func (export "read")
    i32.const 42
    i32.load
    call $log_i32
  )
  (func (export "write") (param $p i32)
    i32.const 42
    local.get $p
    i32.store
  )
)
