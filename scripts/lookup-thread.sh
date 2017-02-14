#!/bin/bash

threads-involving.sh "$1" | jq -r '.id'
