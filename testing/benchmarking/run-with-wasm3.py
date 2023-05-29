# https://github.com/wasm3/pywasm3  see e.g. the examples folder
import wasm3 # install pywasm3
import sys
import random

module = open("./orig.wasm", "rb").read()
env = wasm3.Environment()
rt  = env.new_runtime(2048)
mod = env.parse_module(module)
rt.load(mod)

mod.link_function("env", "get_random", lambda: random.randint(0, 1000))
mod.link_function("env", "write_char", lambda x: sys.stdout.write(str(chr(x))))
mod.link_function("env", "write_int", lambda x: sys.stdout.write(str(x)))

wasm_init = rt.find_function("init")
wasm_init(20) # length

wasm_print = rt.find_function("print")
wasm_print()
print()

wasm_sort = rt.find_function("bubblesort")  # changed export name! (didn't like sort for some reason)
wasm_sort()
wasm_print()
print()
