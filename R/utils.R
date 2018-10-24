#' ggplot2-like colour scale in HCL space
#'
#' @param n Number of colours to return.
#' @param hue_min Minimum hue value in the range `(0,360)`
#' @param hue_max Maximum hue value in the range `(0,360)`
#' @param l Luminance in the range `(0,100)`
#' @param c Chroma of the colour.
#' @details See the [grDevices::hcl()] function for details.
#' @export
#' @examples
#' gg_color_hue(10)
#' plot(1:6, col = gg_color_hue(6), pch = 20, cex = 3)
gg_color_hue <- function(n, hue_min = 8, hue_max = 290, l = 52, c = 100) {
  hues <- rev(seq(hue_min, hue_max, length = n + 1))
  grDevices::hcl(h = hues, l = l, c = c)[seq_len(n)]
}

clean_name <- function(x) gsub("/", "-", gsub(" ", "-", x))

all_cap <- function(x) toupper(x)

emph <- function(x) paste0("\\emph{", x, "}")

first_cap <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
    {s <- substring(s, 2); if(strict) tolower(s) else s},
    sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

#' @export
rougheye_split <- function(x) {
  spl <- strsplit(x, "/")[[1]]
  first <- strsplit(spl, " ")[[1]][[1]]
  second <- strsplit(spl, " ")[[1]][[2]]
  third <- strsplit(spl, " ")[[2]][[1]]
  c(paste(first, second, sep = "-"), paste(first, third, sep = "-"))
}
#' Fourth route power transformation
#'
#' @export
fourth_root_power_trans <- function() {
  scales::trans_new(
    name = "fourth root power",
    transform = function(x) x^0.25,
    inverse = function(x) x^4,
    domain = c(0, Inf))
}

find_length_outliers <- function(xx) {
  yy <-  stats::pnorm(xx, mean = stats:mean(xx, na.rm = TRUE),
    sd = stats::sd(xx, na.rm = TRUE), log.p = TRUE)
  zz <- stats::qnorm(yy, log.p = TRUE)
  out <- zz[zz > 4 & !is.na(zz)]
  xx[zz == out & !is.na(zz)]
}
