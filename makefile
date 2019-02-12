all:
	Rscript -e "source('report/make.R')"
	# cd report/report-rmd/
	# make

one:
	Rscript -e "N <- 1:10; source('report/make.R')"

two:
	Rscript -e "N <- 11:20; source('report/make.R')"

three:
	Rscript -e "N <- 21:30; source('report/make.R')"

four:
	Rscript -e "N <- 31:45; source('report/make.R')"

five:
	Rscript -e "N <- 46:70; source('report/make.R')"

six:
	Rscript -e "N <- 71:90; source('report/make.R')"

seven:
	Rscript -e "N <- 91:114; source('report/make.R')"
