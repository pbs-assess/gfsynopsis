# Generate plot-pages.Rmd ---------------------------------------------
# This is the guts of where the .tex / .Rmd figure page code gets made
temp <- lapply(spp$species_common_name, \(x) generate_plotpages_Rmd(x = x, spp = spp))
temp <- lapply(temp, function(x) paste(x, collapse = "\n"))
temp <- paste(temp, collapse = "\n")
temp <- c("<!-- This page has been automatically generated: do not edit by hand -->\n", temp)
con <- file(file.path(build_dir, "plot-pages.Rmd"), encoding = "UTF-8")
writeLines(temp, con = con)

