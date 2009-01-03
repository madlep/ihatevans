#!/bin/sh
cd `dirname $0`
#-detached -mode embedded -noinput -noshell 
nohup erl -detached -sname mochiweb_ihatevans -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s ihatevans &
