# tap

https://radiotap.org/

Usage

```
tap --help
tap w | tap r
tap --name foo --num 0 --du8 --du8 w -f 0 --u8 0xab --u8 0xcd foo.bin
tap --name foo --num 0 --du8 --du8 r foo.bin
tap --name foo --num 0 --du8 --du8 --name bar --num 1 --ds16 w \
  -f 0 --u8 0xab --u8 0xcd \
  -f 1 --s16 100 \
  bar.bin
tap --name foo --num 0 --du8 --du8 --name bar --num 1 --ds16 r bar.bin
```

Install

```
cabal install tap
```

Development

```
cabal build
cabal haddock
cabal test
cabal run tap -- --help
```
