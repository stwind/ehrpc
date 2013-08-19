#!/bin/sh
APP=ehrpc
ERL=erl
COOKIE=erlang
NODE_NAME=$APP
CONFIG=priv/app.config
LIBS_DIR="deps"

exec $ERL -pa ebin \
    -boot start_sasl \
    -sname $NODE_NAME \
    -setcookie $COOKIE \
    -config $CONFIG \
    -env ERL_LIBS $LIBS_DIR \
    -eval "application:start(crypto)" \
    -eval "application:start(public_key)" \
    -eval "application:start(ssl)" \
    -eval "application:start(ranch)" \
    -eval "application:start(cowboy)" \
    -eval "application:start(lhttpc)" \
    -eval "application:start($APP)" \
    -s sync go
