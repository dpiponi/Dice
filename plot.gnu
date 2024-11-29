set yrange [0:*]
set grid
plot "data.dat" using 1:3:xtic(2) with linespoints
pause mouse
