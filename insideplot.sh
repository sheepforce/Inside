# plotting values from a log file with Gnuplot. Give the log file as first
# argument and the devices as string as the second argument.
# ci = plot ColdIon
# ls = plot LakeShore
# g1 = plot GraphixThree1
# g2 = plot GraphixThree2
#
# therefore cig1 would plot the Data from the ColdIon and the GraphixThree1

#!/bin/sh

echo "usage: insideplot [filename] [ci][ls][g1][g2]"
echo "both arguments must be given"
echo "give the devices as a string without spaces in arbitrary combinations"
echo "ci = ColdIon, ls = LakeShore, g1 = GraphixThree1, g2 = GraphixThree2"

input=$1
opt=$2
filename=$(tempfile)

# remove CET from the time
sed "s/CET//g" $input > $filename

# write a gnuplot script
# set formating, data, labels etc.
echo "set xdata time"                      >  inside.gnuplot
echo "set timefmt \"%Y-%m-%d %H:%M:%S\""   >> inside.gnuplot
echo "set ylabel \"p / mbar\""             >> inside.gnuplot
echo "set y2tics"                          >> inside.gnuplot
echo "set y2tics nomirror"                 >> inside.gnuplot
echo "set ytics nomirror"                  >> inside.gnuplot
echo "set y2label \"T / K\""               >> inside.gnuplot
echo "set format y \"%g\""                 >> inside.gnuplot
echo "set xlabel \"date\""                 >> inside.gnuplot
echo "set terminal svg"                    >> inside.gnuplot
echo "set output \"inside.svg\""           >> inside.gnuplot
echo "set logscale y"                      >> inside.gnuplot

# plot command for gnuplot
printf "plot "                             >> inside.gnuplot
# ColdIon
if [[ $opt == *"ci"* ]]
  then
    printf "\"$filename\" using 1:(\$3 == 0 ? NaN : \$3) with lines axes x1y1 title \"ColdIon (Pressure)\", "\
                                           >> inside.gnuplot
fi

# LakeShore
if [[ $opt == *"ls"* ]]
  then
    printf "\"$filename\" using 1:(\$4 == 0 ? NaN : \$4) with lines axes x1y2 title \"LakeShore A (Temperature)\", \
            \"$filename\" using 1:(\$5 == 0 ? NaN : \$5) with lines axes x1y2 title \"LakeShore B (Temperature)\", "\
                                           >> inside.gnuplot
fi

# Graphix Three 1
if [[ $opt == *"g1"* ]]
  then
    printf "\"$filename\" using 1:(\$6 == 0 ? NaN : \$6) with lines axes x1y1 title \"GraphixThree1 A (Pressure)\",  \
            \"$filename\" using 1:(\$7 == 0 ? NaN : \$7) with lines axes x1y1 title \"GraphixThree1 B (Pressure)\",  \
            \"$filename\" using 1:(\$8 == 0 ? NaN : \$8) with lines axes x1y1 title \"GraphixThree1 C (Pressure)\", "\
                                           >> inside.gnuplot
fi

# Graphix Three 2
if [[ $opt == *"g2"* ]]
  then
    printf "\"$filename\" using 1:(\$9 == 0 ? NaN : \$9) with lines axes x1y1 title \"GraphixThree2 A (Pressure)\",    \
            \"$filename\" using 1:(\$10 == 0 ? NaN : \$10) with lines axes x1y1 title \"GraphixThree2 B (Pressure)\",  \
            \"$filename\" using 1:(\$11 == 0 ? NaN : \$11) with lines axes x1y1 title \"GraphixThree2 C (Pressure)\", "\
                                           >> inside.gnuplot
fi

gnuplot inside.gnuplot

rm $filename
