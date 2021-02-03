#!/bin/bash

for i in {1..100}; do
echo "Rscript run_lb.R ${i} lm;" >> lb_sim
done

for i in {101..200}; do
echo "Rscript run_lb.R ${i} simp_lin_cpp;" >> lb_sim
done
