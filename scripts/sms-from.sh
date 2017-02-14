#!/bin/bash

run.sh sms list --thread "$(lookup-thread.sh "$1")"
