all:
	Rscript -e "source('report/make.R')"
	# cd report/report
	# make

one:
	Rscript -e "N <- 1:10; source('report/make.R')"

two:
	Rscript -e "N <- 11:20; source('report/make.R')"

three:
	Rscript -e "N <- 21:30; source('report/make.R')"

four:
	Rscript -e "N <- 32:60; source('report/make.R')"

five:
	Rscript -e "N <- 61:90; source('report/make.R')"

six:
	Rscript -e "N <- 91:108; source('report/make.R')"
