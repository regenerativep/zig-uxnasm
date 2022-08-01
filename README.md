# zig-uxnasm

Zig translation of the [uxn assembler](https://git.sr.ht/~rabbits/uxn/tree/main/item/src/uxnasm.c).

Seems to work fine on the `blank.tal`, `console.tal`, and `mandelbrot.tal` example code in the uxn repository.

```
$ ./zig-out/bin/zig-uxnasm mandelbrot.tal output.rom
info: opened file mandelbrot.tal
info: assembled "output.rom". 317 bytes written
```

Example errors:

```
$ ./zig-out/bin/zig-uxnasm mandelbrot_broken.tal output.rom
info: opened file mandelbrot_broken.tal
error: mandelbrot_broken.tal:71:9 error.UnknownReference "draw-mandel/asdf"(16)
error: Assembler failure
```

```
./zig-out/bin/zig-uxnasm mandelbrot_broken2.tal output.rom
info: opened file mandelbrot_broken2.tal
info: on token: macro_call{"asdasdasdad"}
error: mandelbrot_broken2.tal:60:17 error.UnknownMacro "asdasdasdad"
error: Assembler failure
```

