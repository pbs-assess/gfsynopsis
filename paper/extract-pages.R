# install poppler on homebrew
# and:
# brew link --overwrite poppler

# pdftoppm input.pdf outputname -png -f {page} -singlefile -rx 300 -ry 300
# convert -density 150 input.pdf[666] -quality 90 output.png


get_page <- function(filename, page, crop_x = 250, crop_y = 260) {
  Y <- 3301
  X <- 2500
  call <- paste0("pdftoppm report/report-rmd/_book/resdoc.pdf paper/pages/",
   filename, " -png -f ", page + 7, " -singlefile -rx 300 -ry 300 -x ",
    crop_x, " -y ", crop_y, " -W ", X - crop_x * 2, " -H ", Y - crop_y * 2)
  system(call)
}

get_page("silvergray-rockfish-1", 120)
get_page("silvergray-rockfish-2", 121)

get_page("quillback-rockfish-1", 142)
get_page("quillback-rockfish-2", 143)

get_page("canary-rockfish-1", 158)
get_page("canary-rockfish-2", 159)

get_page("redstripe-rockfish-1", 160)
get_page("redstripe-rockfish-2", 161)

get_page("yelloweye-rockfish-1", 164)
get_page("yelloweye-rockfish-2", 165)

get_page("lingcod-1", 184)
get_page("lingcod-2", 185)

get_page("arrowtooth-flounder-1", 220)
get_page("arrowtooth-flounder-2", 221)

get_page("petrale-sole-1", 224)
get_page("petrale-sole-2", 225)

get_page("sand-sole-1", 248)
get_page("sand-sole-2", 249)

get_page("black-eelpout-1", 76)
get_page("black-eelpout-2", 77)

get_page("walleye-pollock-1", 68)
get_page("walleye-pollock-2", 69)

get_page("spotted-ratfish-1", 56)
get_page("spotted-ratfish-2", 57)

get_page("north-pacific-spiny-dogfish-1", 38)
get_page("north-pacific-spiny-dogfish-2", 39)


system("convert paper/pages/silvergray-rockfish-1.png -compress zip paper/fig1.tiff")
system("convert paper/pages/silvergray-rockfish-2.png -compress zip paper/fig2.tiff")
system("convert paper/fig3.png -resize 1500x1500 -compress zip paper/fig3.tiff")
