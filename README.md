# xfrp-ocaml
Xfrp for OCaml

## Installation

```
opam install menhir

make
```

## Test

```
./main.native test/test1.xfrp
```

```
./main.native test/test_butterfly.xfrp -t test/test_butterfly.tmpl -o erlang/butterfly.erl
cd erlang
./start.sh -nodebug butterfly
```
