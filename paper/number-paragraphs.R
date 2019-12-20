d <- readLines("paper.Rmd")
j <- 1
for (i in seq_along(d)) {
  p <- grepl("<!-- Paragraph [0-9]+", d[i])
  if (p) {
    d[i] <- gsub("(Paragraph) ([0-9]+):", paste0("\\1 ", j, ":"), d[i])
    j <- j + 1
  }
}
writeLines(d, "paper.Rmd")
