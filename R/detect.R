# Transparency detectors of the REPRO2IMPACT study (protocol section 3.2),
# ported without changing their rules: detector_version is the same and the
# tests reproduce the study's own tests. Each indicator is "yes", "no" or
# "unverifiable"; it is never summed with the others. The rules are explicit
# so that every "yes" can be traced to its evidence sentence.

detector_version <- "1.1.0"

detector_indicators <- c("data_statement", "data_shared", "code_shared", "preregistered", "materials_shared")

repo_pattern <- paste0(
  "(osf\\.io/[a-z0-9]{4,}|zenodo\\.org|doi\\.org/10\\.5281/zenodo|datadryad\\.org|",
  "doi\\.org/10\\.5061/dryad|figshare\\.com|doi\\.org/10\\.6084/m9\\.figshare|",
  "dataverse|icpsr\\.umich\\.edu|openicpsr|ukdataservice|reshare\\.ukdataservice|",
  "psycharchives|data\\.mendeley\\.com|doi\\.org/10\\.17632|gesis\\.org|",
  "doi\\.org/10\\.7910/dvn|zenodo|open science framework)"
)
code_host_pattern <- "(github\\.com/[A-Za-z0-9_.-]+|gitlab\\.com/|bitbucket\\.org/|codeocean\\.com|software heritage)"
data_words <- "\\b(data|dataset|datasets|datos|raw data|replication (files|package))\\b"
code_words <- "\\b(code|codes|script|scripts|syntax|analysis files|replication (files|package)|c\u00f3digo)\\b"
material_words <- paste0("\\b(materials|stimuli|questionnaires?|instruments?|survey items|protocols?|codebook|",
                         "materiales|est\u00edmulos|cuestionarios?)\\b")
statement_pattern <- paste0(
  "(data availability( statement)?|availability of data|data accessibility statement|",
  "data sharing statement|open practices statement|disponibilidad de (los )?datos)"
)
on_request_pattern <- "(up)?on (reasonable )?request|from the corresponding author|bajo petici\u00f3n|previa solicitud"
prereg_pattern <- paste0(
  "(pre-?regist\\w*.{0,120}(osf\\.io/[a-z0-9]{4,}|aspredicted|clinicaltrials\\.gov|socialscienceregistry)|",
  "(osf\\.io/[a-z0-9]{4,}|aspredicted\\.org/[a-z0-9]+|aspredicted #?\\d+).{0,120}pre-?regist\\w*|",
  "aspredicted\\.org|\\bNCT\\d{8}\\b|\\bCRD42\\d{6,}\\b|AEARCTR-\\d+|registered report)"
)

split_sentences <- function(text) {
  s <- unlist(strsplit(text, "(?<=[.!?;])\\s+(?=[A-Z\u00c1\u00c9\u00cd\u00d3\u00da(\\[])", perl = TRUE))
  # Very long "sentences" are menus or metadata dumps, not prose.
  s[nchar(s) > 0 & nchar(s) <= 800]
}

# Sentences matching all patterns, case-insensitive.
find_sentences <- function(sentences, ...) {
  pats <- list(...)
  hit <- rep(TRUE, length(sentences))
  for (p in pats) hit <- hit & grepl(p, sentences, ignore.case = TRUE, perl = TRUE)
  sentences[hit]
}

detect_one <- function(text) {
  empty <- list(value = "unverifiable", evidence = NA_character_)
  if (is.na(text) || !nzchar(trimws(text))) {
    return(stats::setNames(rep(list(empty), length(detector_indicators)), detector_indicators))
  }
  s <- split_sentences(text)
  verdict <- function(hits, exclude = NULL) {
    if (!is.null(exclude)) hits <- hits[!grepl(exclude, hits, ignore.case = TRUE, perl = TRUE) |
                                          grepl(repo_pattern, hits, ignore.case = TRUE, perl = TRUE)]
    if (length(hits)) list(value = "yes", evidence = substr(hits[1], 1, 400))
    else list(value = "no", evidence = NA_character_)
  }
  list(
    data_statement = verdict(find_sentences(s, statement_pattern)),
    data_shared = verdict(find_sentences(s, data_words, repo_pattern), exclude = on_request_pattern),
    code_shared = verdict(c(find_sentences(s, code_words, repo_pattern),
                            find_sentences(s, code_words, code_host_pattern)),
                          exclude = on_request_pattern),
    preregistered = verdict(find_sentences(s, prereg_pattern)),
    materials_shared = verdict(c(find_sentences(s, material_words, repo_pattern),
                                 find_sentences(s, material_words, code_host_pattern)))
  )
}

#' Detect transparency practices in article full texts
#'
#' Applies explicit, versioned rules to the full text of each article and
#' reports five indicators, each with the sentence that supports it:
#'
#' * `data_statement`: a data availability statement is present;
#' * `data_shared`: data are in a repository (statements that data are
#'   available "on request" do not count);
#' * `code_shared`: code is in a repository or code host;
#' * `preregistered`: a preregistration or registered report is cited;
#' * `materials_shared`: materials, stimuli or instruments are shared.
#'
#' Each indicator is `"yes"`, `"no"` or `"unverifiable"` (no text to read).
#' The indicators are reported separately and never added up. English and
#' Spanish statements are recognised. Convert HTML or JATS XML first with
#' [rr_text_from_html()] or [rr_text_from_jats()].
#'
#' @param text A character vector of full texts; `NA` for articles whose text
#'   could not be obtained.
#' @param id Optional identifiers (for example DOIs), one per text.
#' @return A data frame with one row per text: `id`, `detector_version`, and
#'   for each indicator its value and an `_evidence` column.
#' @references Detectors from the REPRO2IMPACT study protocol,
#'   <https://github.com/pablosancheznunez/repro2impact>.
#' @export
#' @examples
#' rr_detect(c(
#'   "Data Availability Statement. All data and scripts are at https://osf.io/ab3cd/.",
#'   "Data are available from the corresponding author upon reasonable request.",
#'   NA
#' ), id = c("10.1/a", "10.1/b", "10.1/c"))
rr_detect <- function(text, id = NULL) {
  if (!is.character(text) && !all(is.na(text))) stop("`text` must be a character vector.", call. = FALSE)
  text <- as.character(text)
  id <- id %||% seq_along(text)
  if (length(id) != length(text)) stop("`id` must have one value per text.", call. = FALSE)
  rows <- lapply(seq_along(text), function(i) {
    p <- detect_one(text[i])
    row <- data.frame(id = as.character(id[i]), detector_version = detector_version, stringsAsFactors = FALSE)
    for (k in names(p)) {
      row[[k]] <- p[[k]]$value
      row[[paste0(k, "_evidence")]] <- p[[k]]$evidence
    }
    row
  })
  do.call(rbind, rows)
}

#' Extract readable text from HTML or JATS XML
#'
#' `rr_text_from_html()` keeps the article region of a publisher page (dropping
#' navigation, headers, footers and menus), keeps the targets of links
#' (repository URLs often appear only there) and ends a sentence at every
#' block element so that headings do not merge with the next sentence.
#' `rr_text_from_jats()` drops the JATS front matter, whose metadata would
#' otherwise trigger indicators.
#'
#' @param x A character vector of HTML pages or JATS XML documents.
#' @return A character vector of plain texts.
#' @export
#' @examples
#' rr_text_from_html('<article><h2>Data availability</h2><p>Data at
#'   <a href="https://osf.io/qwe12/">OSF</a>.</p></article>')
rr_text_from_html <- function(x) vapply(x, function(h) strip_markup(article_region(h)), "", USE.NAMES = FALSE)

#' @rdname rr_text_from_html
#' @export
rr_text_from_jats <- function(x) {
  vapply(x, function(xml) {
    xml <- gsub("(?s)<front>.*?</front>", " ", xml, perl = TRUE)
    xml <- gsub("(?s)<custom-meta-group>.*?</custom-meta-group>", " ", xml, perl = TRUE)
    strip_markup(xml)
  }, "", USE.NAMES = FALSE)
}

strip_markup <- function(x) {
  # Pages in legacy encodings would otherwise yield NA lengths downstream.
  raw <- x
  x <- iconv(raw, from = "UTF-8", to = "UTF-8", sub = " ")
  if (is.na(x)) x <- iconv(raw, from = "latin1", to = "UTF-8", sub = " ")
  if (is.na(x)) return("")
  x <- gsub("(?s)<(script|style)[^>]*>.*?</\\1>", " ", x, perl = TRUE)
  # Keep link targets: repository URLs often live only in href attributes.
  x <- gsub("<a[^>]+href=\"([^\"]+)\"[^>]*>", " \\1 ", x, perl = TRUE)
  x <- gsub("<ext-link[^>]+xlink:href=\"([^\"]+)\"[^>]*>", " \\1 ", x, perl = TRUE)
  # Block-level tags end a sentence, so headings and menu items do not merge.
  x <- gsub("</?(p|div|li|h[1-6]|sec|title|td|tr|br)\\b[^>]*>", " . ", x, perl = TRUE)
  x <- gsub("<[^>]+>", " ", x)
  x <- gsub("&nbsp;|&#160;", " ", x)
  x <- gsub("&amp;", "&", x)
  trimws(gsub("\\s+", " ", x))
}

article_region <- function(html) {
  html <- gsub("(?s)<(nav|header|footer|aside|form)\\b[^>]*>.*?</\\1>", " ", html, perl = TRUE)
  m <- regmatches(html, regexpr("(?s)<article\\b.*</article>", html, perl = TRUE))
  if (length(m) && nchar(m) > 5000) return(m)
  m <- regmatches(html, regexpr("(?s)<main\\b.*</main>", html, perl = TRUE))
  if (length(m) && nchar(m) > 5000) return(m)
  html
}
