# The first tests reproduce the REPRO2IMPACT study's own tests of detector
# version 1.1.0, so the port cannot drift from the study.
one <- function(text) as.list(rr_detect(text)[1, ])

test_that("data shared in a repository is detected with its evidence", {
  txt <- paste("Method. Participants completed the task.",
               "Data Availability Statement. All data and analysis scripts are openly available at https://osf.io/ab3cd/.",
               "Results. The effect was large.")
  p <- one(txt)
  expect_equal(p$data_statement, "yes")
  expect_equal(p$data_shared, "yes")
  expect_equal(p$code_shared, "yes")
  expect_match(p$data_shared_evidence, "osf.io/ab3cd")
})

test_that("data on request is a statement but not sharing", {
  p <- one("Data availability. The data that support the findings are available from the corresponding author upon reasonable request. Results follow.")
  expect_equal(p$data_statement, "yes")
  expect_equal(p$data_shared, "no")
  expect_equal(p$code_shared, "no")
})

test_that("missing full text is unverifiable, never no", {
  p <- rr_detect(c(NA, "  "))
  values <- unlist(p[, c("data_statement", "data_shared", "code_shared", "preregistered", "materials_shared")])
  expect_true(all(values == "unverifiable"))
})

test_that("preregistrations are recognised", {
  expect_equal(one("The study was preregistered at https://osf.io/x7k2p before data collection.")$preregistered, "yes")
  expect_equal(one("Hypotheses were registered at AsPredicted (aspredicted.org/blind.php?x=ab12cd).")$preregistered, "yes")
  expect_equal(one("This trial is registered as NCT01234567.")$preregistered, "yes")
  expect_equal(one("We did not register the study. Data were analysed in R.")$preregistered, "no")
})

test_that("code on GitHub is detected and naming software alone is not", {
  expect_equal(one("The analysis code is available at https://github.com/lab/study.")$code_shared, "yes")
  expect_equal(one("Analyses were run in R 4.1 using the lme4 package.")$code_shared, "no")
})

test_that("Spanish statements are recognised", {
  p <- one("Disponibilidad de los datos. Los datos y el c\u00f3digo est\u00e1n disponibles en https://zenodo.org/records/123456.")
  expect_equal(p$data_statement, "yes")
  expect_equal(p$data_shared, "yes")
  expect_equal(p$code_shared, "yes")
})

test_that("markup keeps link targets and drops scripts", {
  x <- rr_text_from_html("<p>Data at <a href=\"https://osf.io/qwe12/\">OSF</a>.</p><script>var a=1;</script>")
  expect_match(x, "osf.io/qwe12")
  expect_false(grepl("var a", x))
})

test_that("JATS front matter and publisher menus do not produce indicators", {
  xml <- paste0("<article><front><custom-meta-group><custom-meta>pmc-status-live yes</custom-meta>",
                "<custom-meta>registered report</custom-meta></custom-meta-group></front>",
                "<body><p>We analysed survey data.</p></body></article>")
  expect_false(grepl("registered report", rr_text_from_jats(xml)))
  html <- paste0("<html><nav><ul><li>Registered reports</li><li>Open research</li></ul></nav>",
                 "<article>", strrep("<p>Participants read texts and answered questions.</p>", 200),
                 "</article></html>")
  expect_equal(one(rr_text_from_html(html))$preregistered, "no")
})

test_that("results carry ids and the detector version", {
  d <- rr_detect(c("a", "b"), id = c("10.1/a", "10.1/b"))
  expect_equal(d$id, c("10.1/a", "10.1/b"))
  expect_true(all(d$detector_version == "1.1.0"))
  expect_error(rr_detect("a", id = 1:2), "one value per text")
  path <- tempfile(fileext = ".json")
  rr_write_json(d, path)
  expect_equal(jsonlite::read_json(path)$schema, "rr-detect/1")
})
