reset 
set term pngcairo enhanced size 1200, 960 
set encoding iso_8859_15

set output 'temp_field.png'

set pm3d map interpolate 20, 20 
set contour base
set isosamples 50, 50
set cntrparam levels incr -0.3,0.1,0.5
set palette maxcolors 10
# set palette defined (0 "#333399", 1 "#3333f7", 2 "#3373ff", 3 "#33c6ff", 4 "#5affd2", 5 "#9cff8f", 6 "#dfff4c", 7 "#ffc733", 8 "#ff7a33", 9 "#e53333")

set clabel '%.1f'
# unset surface
unset key

set xlabel "x (cm)"
set ylabel "y (cm)"
set autoscale xy

set title "Temperature Field of 2D Heat Diffusion Problem with Heat Generation" font 'arial, 14'
splot 'results.txt' using 2:3:4 notitle with lines lt -1, 'results.txt' using 2:3:4 notitle with pm3d