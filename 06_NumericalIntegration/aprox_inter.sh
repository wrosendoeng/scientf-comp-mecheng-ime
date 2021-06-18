#!/bin/bash

frame ( )
  {
    echo "set term pdfcairo size 5,3 font 'Arial,14'"
    echo "set output 'pic_aprox_inter.pdf'"
    echo "set xlabel 'x'"
    echo -e "set format x \"%g\" "
    echo "set xrange [0:14]"
    echo "set ylabel 'y'"
    echo "set title 'Comparison Approximation x Interpolation'"
    echo -e "set format y \"%g\" "
    echo "set yrange [-250:250]"
    echo "plot 'result_aprox.dat' using 1:2 w points title 'trig mmq', 'result_interp.dat' using 1:2 w lines title 'cubic spline' "
    return
  }

frame | gnuplot