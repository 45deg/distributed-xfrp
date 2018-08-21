# Distributed XFRP

A Distributed Functional Reactive Programming Language without Glitches

## Installation

```
opam install menhir

make
```

## Example (Real-time Collaborative Editor)

### Installation

Prerequisites: Erlang runtime, tmux

```
cd example
./setup_board.sh
```

### Run

```
cd example
./start_board.sh
```

See also: https://tmuxcheatsheet.com/

## Additional Examples

```
./main.native test/test1.xfrp
```

```
./main.native test/test_butterfly.xfrp -t test/test_butterfly.tmpl -o erlang/butterfly.erl
cd erlang
./start.sh butterfly
```

```
# output the node graph
./main.native test/test2.xfrp -dot | dot -Tpng > graph.png
```
