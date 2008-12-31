#!/bin/sh
cd `dirname $0`
exec erl -sname mochiweb_ihatevans -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s ihatevans
