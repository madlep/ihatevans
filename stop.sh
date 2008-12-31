#!/bin/sh
HOST=`hostname -s`
erl -sname console -eval "spawn('mochiweb_ihatevans@$HOST', init, stop, []), halt()."