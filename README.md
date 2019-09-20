# harputil

`harputil` is a silly little command line tool for converting notes into harmonica tabs.

## Usage

Notes should be written in the following format `[NoteName],[Octave]`. As an example, middle C is `C,4`.

```
Basic Usage: harputil [tuning] [baseNote] [notes]

# example

$ harputil richter "C,4" "G,4 C,5 G,4 Bb,5 G,4"

Output: 2d 4b 2d 3d' 2d
```

## Tabulature Reference

```
[Num]   - hole number
b       - blow
d       - draw
'       - 1/2 step bend
''      - whole step bend
'''     - 1 1/2 step bend
^       - over bend

# examples

3d'' - 3 draw whole-step bend
8b' - 8 half-step bend
6b^ - 6 overblow
7d^ - 7 overdraw
```
