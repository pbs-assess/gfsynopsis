pdf2png <- function(path = ".", resolution = 200) {
  files <- list.files(path = path, pattern = "\\.pdf", full.names = TRUE)
  files_png <- sub("\\.pdf", ".png", files)
  for (i in seq_along(files)) {
    message(files_png[i])
    system(paste("convert -density", resolution, "-quality 100",
      files[i], files_png[i]))
  }
}
