rmarkdown::render("paper.Rmd")


system("cp paper/workflow.tiff paper/fig1.tiff")
system("convert paper/pages/silvergray-rockfish-1.png -compress zip paper/fig2.tiff")
system("convert paper/pages/silvergray-rockfish-2.png -compress zip paper/fig3.tiff")
system("convert paper/fig4.png -resize 1500x1500 -compress zip paper/fig4.tiff")


# d <- readLines("paper.tex")

# figure_line <- grep("^\\\\begin\\{figure\\}", d)[[1]]
# bib_beg_line <- grep("^\\\\hypertarget\\{references", d)[[1]]
# bib_end_line <- length(d) - 2

# d <- d[c(
#   1:(figure_line - 1),
#   bib_beg_line:bib_end_line,
#   c(figure_line:bib_beg_line - 1), length(d)
# )]
# writeLines(d, "paper.tex")

# tinytex::latexmk("paper.tex")
