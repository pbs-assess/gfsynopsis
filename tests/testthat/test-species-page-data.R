test_that("species page data contains the fields needed by both outputs", {
  spp <- data.frame(
    species_common_name = c("pacific cod", "north pacific spiny dogfish"),
    species_science_name = c("Gadus macrocephalus", "Squalus suckleyi"),
    species_code = c("222", "044"),
    spp_w_hyphens = c("pacific-cod", "north-pacific-spiny-dogfish"),
    order = c("Gadiformes", "Squaliformes"),
    family = c("Gadidae", "Squalidae"),
    worms_id = c("126436", "299224"),
    resdoc = c("@forrest2020pcod", "@galluci2011dogfish, @anderson2025dogfish"),
    sar = c("@dfo2019pcod3cd5abcd", "@dfo2010dogfish"),
    other_ref_cite = c("Last Science Response: @dfo2021pcod", ""),
    cosewic_status_reports = c("", "cosewic2011dogfish"),
    cosewic_status = c(NA, "Special Concern"),
    sara_status = c(NA, "No Status")
  )

  cod <- gfsynopsis:::species_page_data("pacific cod", spp)
  expect_equal(cod$slug, "pacific-cod")
  expect_equal(cod$common_name, "Pacific Cod")
  expect_equal(cod$images, file.path(
    "figure-pages", c("pacific-cod-1.png", "pacific-cod-2.png")
  ))
  expect_equal(cod$links[[2L]]$label, "WoRMS")

  dogfish <- gfsynopsis:::species_page_data(
    "north pacific spiny dogfish", spp
  )
  expect_equal(dogfish$common_name, "Pacific Spiny Dogfish")
  expect_equal(dogfish$slug, "north-pacific-spiny-dogfish")

  rmd <- generate_plotpages_Rmd("pacific cod", spp)
  expect_true(any(grepl("## Pacific Cod", rmd, fixed = TRUE)))
  expect_true(any(grepl("figure-pages/pacific-cod-2.png", rmd, fixed = TRUE)))

  expect_length(gfsynopsis:::species_pages_data(spp), 2L)
})

test_that("missing or placeholder WoRMS identifiers do not create links", {
  spp <- data.frame(
    species_common_name = "test fish",
    species_science_name = "Piscis testus",
    species_code = "999",
    spp_w_hyphens = "test-fish",
    order = "Testiformes",
    family = "Testidae",
    worms_id = NA_character_
  )

  for (worms_id in c(NA_character_, "", "unknown")) {
    spp$worms_id <- worms_id
    page <- gfsynopsis:::species_page_data("test fish", spp)
    expect_equal(vapply(page$links, `[[`, character(1), "label"), "FishBase")
  }
})
