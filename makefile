all:
	Rscript -e "source('report/make.R')"
	cd report/report
	make

one:
	Rscript -e "N <- 1:10; source('report/make.R')"

two:
	Rscript -e "N <- 11:20; source('report/make.R')"

three:
	Rscript -e "N <- 21:31; source('report/make.R')"
