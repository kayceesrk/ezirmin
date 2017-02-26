#!/bin/bash

for i in {1..10}
do
	rm -rf /tmp/ezirminr
	echo "num_ops=$i"
	time ../_build/bench/bench_paper_rope.native $(( $i * 100 ))
done
