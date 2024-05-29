#!/bin/bash

ALARM=/sys/class/rtc/rtc0/wakealarm

if pidof -q make-mkv; then
    svdrpsend mesg make-mkv is still running.
    exit 0
fi

if [ $# = 5 ]; then
    ethtool -s eth0 wol g
    echo 0 >$ALARM
    killall vdr
    sleep 10
    if [ $1 -ne 0 ]; then
        echo $(($1 - 420)) >$ALARM
        sleep 1
    fi
    poweroff
fi
