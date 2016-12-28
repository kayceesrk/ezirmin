#!/bin/bash

rm -rf /tmp/ezirmin

if [ -z "$1" ]
then
	echo "Usage: $0 <_build/bench/bench_*.native>"
	exit 1
fi

for i in {1..25}
do
	{ time `for j in {1..100}; do echo "Hello, world!"; done | $1`; } 2>&1 | grep "real" | sed "s/real.*m\(.*\)s.*/\1/"
done
