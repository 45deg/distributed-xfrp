#!/bin/bash

erlc board.erl
rm -rf out*

if [ -n "$SESSION_NAME" ];then
  session=$SESSION_NAME
else
  session=multi-ssh-`date +%s`
fi
window=multi-ssh

tmux new-session -d -n $window -s $session

tmux send-keys "erl -sname client1@localhost -noinput -pa cecho/_build/default/lib/cecho/ebin/ -eval 'board:start(client1@localhost)' +A 50 2>out.1" C-m

tmux split-window

tmux send-keys "erl -sname client2@localhost -noinput -pa cecho/_build/default/lib/cecho/ebin/ -eval 'board:start(client2@localhost)' +A 50 2>out.2" C-m

tmux split-window
tmux select-layout tiled

tmux send-keys "erl -sname server@localhost -noinput -pa cecho/_build/default/lib/cecho/ebin/ -eval 'board:start(server@localhost)' +A 50 2>out.0" C-m

tmux send-keys C-m

tmux select-pane -t 0

# tmux set-window-option synchronize-panes on

tmux attach-session -t $session
