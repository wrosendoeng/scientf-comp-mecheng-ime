set term pdfcairo enhanced color dashed size 29.7cm, 21.0cm
set encoding iso_8859_15

set output 'temp_field.pdf'

set view map
set pm3d interpolate 200,200
set colorbox
set xlabel "x(cm)"
set ylabel "y(cm)"
set autoscale xy
set title "Temperature Field of 2D Heat Diffusion Problem with Heat Generation"

splot 'results.dat' using 2:3:4 title 'Temperature (K)' with pm3d