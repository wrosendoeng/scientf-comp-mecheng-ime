#!/bin/bash
frame ( )
  {
    echo "set terminal pngcairo size 960,640 font 'Arial, 14' enhanced"
    echo "set termoption enhanced"
    echo "set encoding iso_8859_1"
    echo "set output 'pic_cd0.png'"
    echo "set xrange [0:3]"
    echo "set yrange [0.05:0.25]"
    echo "set xlabel 'Mach Number'"
    echo "set ylabel 'Cd_0'"
    echo "set title 'Drag Coefficient at 0 angle of attack'"
    echo "plot 'myfileactive.txt' u 15:17 w lines dt 6 lc rgb 'black' lw 1.5 title 'base bleed', \\
    'myfileinert.txt' u 15:17 w lines dt 4 lc rgb 'yellow' lw 1.5 title 'no base bleed'"
    return
  }

frame | gnuplot