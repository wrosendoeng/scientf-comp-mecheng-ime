set term pdfcaior enhanced color dashed size 15cm, 30cm
set encoding iso_8859_15

set pm3d map
set colobor
set xlabel "x(cm)"
set ylabel "y(cm)"
set autoscale xy

splot 'temp_field.txt' title 'Temperature (K)'
