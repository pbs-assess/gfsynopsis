
setwd("/Volumes/Extreme-SSD/gfs/report/report-rmd/")
system("pdfcrop --margins '--2 -17 -3 -24' --clip resdoc.pdf resdoc-crop.pdf")
system("pdfseparate resdoc-crop.pdf %d.pdf")
system("rm 13.pdf")
system("rm 1.pdf")
system("rm 2.pdf")
system("rm 3.pdf")
system("rm 4.pdf")
