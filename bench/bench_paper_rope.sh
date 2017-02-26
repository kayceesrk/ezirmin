#!/bin/bash

rm -rf /tmp/ezirminr

for i in {1..10}
do
	echo "num_ops=$i"
	time ../_build/bench/bench_paper_rope.native $(( $i * 100 ))
done
