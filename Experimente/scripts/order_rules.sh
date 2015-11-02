#!/bin/bash
$(awk -F' and ' '{ split($0, arr); asort(arr); for (i=1; i<=length(arr); i++) { printf "%s ", arr[i]}; printf RS; }')