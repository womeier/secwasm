(module
  ;; function imports
  (import "env" "log_i32" (func $log_i32 ( param i32 )))
  (import "env" "write_char" (func $write_char( param i32 ))) ;; ascii decimal
  (import "env" "write_int" (func $write_int( param i32 )))
  (import "env" "get_random" (func $get_random (result i32)))

  ;; memory and global variables
  (memory 10) ;; times 64KB
  (global $length (mut i32) (i32.const 10))

  (func $init (export "init")
        (param $length i32)
        local.get $length
        global.set $length

        i32.const 0 ;; start ptr
        local.get $length
        call $init_vals
  )

  (func $init_vals (param $start_ptr i32) (param $remaining i32)
        local.get $remaining
        i32.const 0
        i32.eq
        if
          return
        end

        local.get $start_ptr
        call $get_random
        i32.store

        local.get $start_ptr
        i32.const 4
        i32.add

        local.get $remaining
        i32.const 1
        i32.sub

        call $init_vals
        )

  (func $print (export "print")
        i32.const 40 ;; (
        call $write_char

        i32.const 0
        call $print_nums

        i32.const 32 ;; space
        call $write_char
        i32.const 41 ;; )
        call $write_char
    )

  (func $print_nums (param $idx i32)
        local.get $idx
        global.get $length
        i32.ge_s
        if
          return
        end

        i32.const 32 ;; space
        call $write_char

        local.get $idx
        i32.const 4
        i32.mul
        i32.load
        call $write_int

        local.get $idx
        i32.const 1
        i32.add

        call $print_nums
  )

  (func $sort (export "sort")
        i32.const 0
        i32.const 0 ;; changed=false
        call $sort_helper

        ;; check if something changed
        i32.const 1
        i32.eq
        if
          call $sort
        end
  )

  (func $sort_helper (param $idx i32) (param $changed i32) (result i32) ;; ret: changed:1, unchanged:0
        (local $ptr_a i32) (local $ptr_b i32) (local $tmp i32)
      global.get $length
      i32.const 2
      i32.sub
      local.get $idx
      i32.lt_s

      ;; length - 2: last i to test and swap arr[i] and arr[i+1]
      if
        local.get $changed
        return
      end

      ;; swap if necessary
      ;; ptr_a -> idx*4
      local.get $idx
      i32.const 4
      i32.mul
      local.set $ptr_a

      ;; ptr_b -> (idx+1)*4
      local.get $idx
      i32.const 1
      i32.add
      i32.const 4
      i32.mul
      local.set $ptr_b

      local.get $ptr_a
      i32.load
      local.get $ptr_b
      i32.load
      i32.gt_s

      if
        ;; tmp := arr[a]
        local.get $ptr_a
        i32.load
        local.set $tmp

        ;; arr[a] = arr[b]
        local.get $ptr_a
        local.get $ptr_b
        i32.load
        i32.store

        ;; arr[b] = tmp
        local.get $ptr_b
        local.get $tmp
        i32.store

        ;; mark sth. changed
        i32.const 1
        local.set $changed
      end

      ;; call for index i+1
      local.get $idx
      i32.const 1
      i32.add
      local.get $changed
      call $sort_helper
  )
)
