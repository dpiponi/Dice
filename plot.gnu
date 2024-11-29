set boxwidth 0.5
set style fill solid
plot "data.dat" using 1:3:xtic(2) with boxes
pause mouse
