test_that("web references resolve bibliography keys and note citations", {
  bibliography_file <- here::here(
    "report", "tech-report-main", "bib", "spp-refs.bib"
  )
  legacy_bibliography_file <- here::here(
    "report", "report-rmd", "bib", "spp-refs.bib"
  )
  skip_if_not(file.exists(bibliography_file))

  bibliography <- RefManageR::ReadBib(bibliography_file, check = FALSE)
  urls <- gfsynopsis:::web_reference_urls(
    bibliography_file,
    legacy_bibliography_file = legacy_bibliography_file
  )
  page <- list(
    references = list(
      research_documents = "@forrest2020pcod",
      science_advisory_reports = "@dfo2019pcod3cd5abcd",
      other = "Last Science Response: @dfo2021pcod, @dfo2024pcod",
      cosewic_status_report = ""
    ),
    notes = c("The source is\n [@lacko2023].", "A plain\n note.")
  )

  references <- gfsynopsis:::web_reference_records(
    page,
    bibliography = bibliography,
    urls = urls
  )
  expect_true(is.list(references))
  expect_equal(
    vapply(references, `[[`, character(1), "label")[[1L]],
    "R. E. Forrest, S. C. Anderson, C. J. Grandin et al. (2020)"
  )
  expect_match(references[[1L]]$citation, "Assessment of Pacific Cod")
  expect_match(references[[1L]]$url, "40952290.pdf")

  notes <- gfsynopsis:::web_resolve_note_citations(
    page$notes,
    reference_records = references,
    bibliography = bibliography,
    urls = urls
  )
  expect_match(notes[[1L]], "Lacko")
  expect_match(notes[[1L]], "https://publications.gc.ca")
  expect_false(grepl("[\r\n]", notes[[1L]]))
  expect_equal(notes[[2L]], "A plain note.")
  expect_false(grepl("@[A-Za-z][A-Za-z0-9_-]*", paste(notes, collapse = " ")))
})
