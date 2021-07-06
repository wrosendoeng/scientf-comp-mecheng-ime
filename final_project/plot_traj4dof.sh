#!/bin/bash

./execute.sh

frame ( )
  {
    echo "set terminal pngcairo size 960,640 font 'Arial, 14' enhanced"
    echo "set termoption enhanced"
    echo "set encoding iso_8859_1"
    echo "set output 'pic_traj4DOF.png'"
    echo "set xrange [0:30000] "
    echo "set yrange [0:14000]"
    echo "set xlabel 'downrange (meters)'"
    echo "set ylabel 'height (meters)'"
    echo "set title 'MPMTM c/ Base Bleed - dt = 0.005'"
    echo "plot 'results_bb.txt' u 2:3 w lines dt 2 lc rgb 'red' lw 1.5 title 'base bleed','results_inert.txt' u 2:3 w lines dt 6 lc rgb 'black' lw 1.5 title 'no base bleed'"
#    'results.txt' u 1:4 w lines dt 10 lc rgb 'blue' title "f\''""({/Symbol h})"
    return
  }

frame | gnuplot