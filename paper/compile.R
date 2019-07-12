rmarkdown::render("paper.Rmd", output_format = "latex_document")
d <- readLines("paper.tex")

figure_line <- grep("^\\\\begin\\{figure\\}", d)[[1]]
bib_beg_line <- grep("^\\\\hypertarget\\{references", d)[[1]]
bib_end_line <- length(d) - 2

d <- d[c(
  1:(figure_line - 1),
  bib_beg_line:bib_end_line,
  c(figure_line:bib_beg_line - 1), length(d)
)]
writeLines(d, "paper.tex")

tinytex::latexmk("paper.tex")
