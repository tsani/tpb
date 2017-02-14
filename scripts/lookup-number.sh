#!/bin/bash

threads-involving.sh "$1" | jq -r '.recipients[0].number'
