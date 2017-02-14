#!/bin/bash

run.sh sms send --number "$(lookup-number.sh "$1")" --message "$2"
