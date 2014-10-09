#set log y
set out "o.png"
set term png
set grid
plot "M30.dat" u 1:2 w l, "M50.dat" u 1:2 w l
#pause -1