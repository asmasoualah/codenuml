set title "Double precision"
Aa = "#eeeeee"; Bb = "#aaaaaa"; Cc = "#cccccc"; 
set auto x
set style data histogram
set style histogram cluster gap 1
set style fill solid border -1
set boxwidth 0.9
set xtic scale 0
set term pdf font "times,12"
set output "single.pdf"
set key left top
# 2, 3, 4, 5 are the indexes of the columns; 'fc' stands for 'fillcolor'
plot 'single3' using 2:xtic(1) ti col lt rgb Aa, '' u 3 ti col lt rgb Bb, '' u 4 ti col lt rgb Cc 

