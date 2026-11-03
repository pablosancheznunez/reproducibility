`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

utc_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

audit_statuses <- c("met", "not_met", "not_applicable", "unverifiable")

code_ext <- "\\.(R|r|Rmd|rmd|qmd|py|jl|do|sas|m|sh)$"
skip_dirs <- "(^|/)(\\.git|renv/library|renv/staging|node_modules|\\.Rproj\\.user|packrat/lib[^/]*|__pycache__|\\.venv|venv)(/|$)"

#' The reproducibility checklist
#'
#' The items [rr_audit()] checks, with the reason for each and the standard or
#' guide it comes from. `critical` items are the minimum for anyone else to
#' rerun a project.
#'
#' @return A data frame with columns `id`, `category`, `item`, `rationale`,
#'   `reference` and `critical`.
#' @export
#' @examples
#' rr_checklist()[, c("id", "critical")]
rr_checklist <- function() {
  x <- utils::read.csv(system.file("extdata", "checklist.csv", package = "reproducibility"),
                       stringsAsFactors = FALSE, encoding = "UTF-8")
  x$critical <- as.logical(x$critical)
  x
}

#' Audit a research project directory
#'
#' Inspects the files of a project, without running any of its code, and
#' checks each item of [rr_checklist()]. Every result carries its evidence:
#' the files found, or the lines that break a rule (for example an absolute
#' path, reported as `file:line`).
#'
#' Statuses: `met`, `not_met`, `not_applicable` (for example seeds in a
#' project that uses no randomness) and `unverifiable`. The audit describes
#' conditions for reproducibility; it does not rerun the analysis, and a
#' project that meets every item can still fail to reproduce.
#'
#' @param path The project directory.
#' @param large_file_mb Files above this size (in MB) are reported by the
#'   `no_large_files` item.
#' @return An object of class `rr_audit`: a list with `checks` (a data frame
#'   with the checklist columns plus `status` and `evidence`), `path`,
#'   `project_type`, `audited_at`, `n_files`, `n_code_files` and `version`.
#'   In an R package (a `DESCRIPTION` with a `Package` field), `R CMD check` is
#'   the entry point and `inst/extdata` holds package files, so the pipeline
#'   and data items are `not_applicable` unless `data/` or a pipeline exists.
#' @export
#' @examples
#' dir <- file.path(tempdir(), "demo-project")
#' rr_template(dir, title = "Demo project")
#' audit <- rr_audit(dir)
#' audit
#' as.data.frame(audit)[, c("id", "status")]
rr_audit <- function(path = ".", large_file_mb = 50) {
  if (!is.character(path) || length(path) != 1L || !dir.exists(path)) {
    stop("`path` must be an existing directory.", call. = FALSE)
  }
  all_files <- list.files(path, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  files <- all_files[!grepl(skip_dirs, all_files)]
  top <- list.files(path, all.files = TRUE, no.. = TRUE)
  code <- files[grepl(code_ext, files)]
  has <- function(pattern, where = files) where[grepl(pattern, where, ignore.case = TRUE, perl = TRUE)]
  lines_of <- function(f) {
    tryCatch(suppressWarnings(readLines(file.path(path, f), warn = FALSE, encoding = "UTF-8")),
             error = function(e) character())
  }
  code_lines <- lapply(stats::setNames(code, code), lines_of)
  grep_code <- function(pattern) {
    hits <- unlist(lapply(names(code_lines), function(f) {
      l <- code_lines[[f]]
      i <- grep(pattern, l, perl = TRUE)
      if (length(i)) paste0(f, ":", i) else NULL
    }))
    hits %||% character()
  }
  readme_files <- has("^readme(\\.[a-z]+)?$", top)
  readme_text <- tolower(paste(unlist(lapply(readme_files, lines_of)), collapse = " "))
  result <- function(found, evidence_found, evidence_missing = NA_character_) {
    list(status = if (found) "met" else "not_met",
         evidence = if (found) paste(utils::head(evidence_found, 5), collapse = "; ") else evidence_missing)
  }

  checks <- list()
  checks$readme <- result(length(readme_files) > 0, readme_files, "No README at the top level.")
  lic <- has("^(licen[cs]e|copying)(\\.[a-z]+)?$", top)
  desc <- if (file.exists(file.path(path, "DESCRIPTION"))) lines_of("DESCRIPTION") else character()
  lic_desc <- grep("^License:", desc, value = TRUE)
  is_package <- any(grepl("^Package:", desc))
  checks$license <- result(length(lic) > 0 || length(lic_desc) > 0, c(lic, lic_desc), "No LICENSE file or License field.")
  cit <- has("^(citation\\.cff|codemeta\\.json|\\.zenodo\\.json|inst/citation)$")
  checks$citation <- result(length(cit) > 0, cit, "No CITATION.cff, codemeta.json or .zenodo.json.")
  checks$version_control <- result(dir.exists(file.path(path, ".git")), ".git",
                                   "No .git directory (the project may be a downloaded copy).")
  deps <- has("(^|/)(renv\\.lock|DESCRIPTION|requirements[^/]*\\.txt|environment\\.ya?ml|pyproject\\.toml|Pipfile(\\.lock)?|poetry\\.lock|Project\\.toml|Manifest\\.toml|packrat/packrat\\.lock)$")
  checks$dependencies_declared <- result(length(deps) > 0, deps, "No dependency file (renv.lock, DESCRIPTION, requirements.txt, environment.yml...).")
  pinned <- has("(^|/)(renv\\.lock|Pipfile\\.lock|poetry\\.lock|Manifest\\.toml|packrat/packrat\\.lock|conda-lock\\.ya?ml)$")
  reqs <- has("(^|/)requirements[^/]*\\.txt$")
  pinned_reqs <- reqs[vapply(reqs, function(f) {
    l <- trimws(lines_of(f))
    l <- l[nzchar(l) & !startsWith(l, "#")]
    length(l) > 0 && all(grepl("==", l, fixed = TRUE))
  }, TRUE)]
  checks$dependencies_pinned <- result(length(c(pinned, pinned_reqs)) > 0, c(pinned, pinned_reqs),
                                       "No lock file with exact versions.")
  cont <- has("(^|/)(Dockerfile|Containerfile|[^/]*\\.def|\\.devcontainer/[^/]+|binder/[^/]+|apptainer[^/]*)$")
  checks$container <- result(length(cont) > 0, cont, "No container or environment specification.")
  pipe <- has("(^|/)(Makefile|_targets\\.R|Snakefile|dvc\\.yaml|run_all[^/]*|main\\.(R|py)|run\\.(R|py|sh)|_quarto\\.yml)$")
  checks$pipeline <- if (is_package && !length(pipe)) {
    list(status = "not_applicable", evidence = "R package: R CMD check is the entry point.")
  } else {
    result(length(pipe) > 0, pipe, "No single entry point (Makefile, _targets.R, Snakefile, run_all...).")
  }
  ci <- has("^(\\.github/workflows/[^/]+\\.ya?ml|\\.gitlab-ci\\.yml|\\.circleci/config\\.yml|azure-pipelines\\.yml)$")
  checks$ci <- result(length(ci) > 0, ci, "No continuous integration configuration.")
  # In an R package, inst/extdata holds package files, documented in its help
  # pages; only data/ and data-raw/ count as research data.
  data_files <- has(if (is_package) "^(data|data-raw)/" else "^(data|data-raw|raw_data|inst/extdata)/")
  data_ref <- grepl("(doi\\.org/|zenodo|osf\\.io|figshare|dryad|dataverse|10\\.\\d{4,9}/)", readme_text, perl = TRUE)
  if (is_package && !length(data_files)) {
    checks$data_available <- list(status = "not_applicable", evidence = "R package without research data.")
    checks$data_documented <- checks$data_available
  } else checks$data_available <- result(length(data_files) > 0 || data_ref,
                                  c(if (length(data_files)) paste(length(data_files), "file(s) in data directories"),
                                    if (data_ref) "README cites a repository or DOI"),
                                  "No data directory and no repository link or DOI in the README.")
  if (is_package && !length(data_files)) {
    # Already set above.
  } else if (!length(data_files) && !data_ref) {
    checks$data_documented <- list(status = "not_applicable", evidence = "No data found to document.")
  } else {
    dd <- has("(codebook|data[_-]?dictionary|data[_-]?description|variables|(data|data-raw)/readme[^/]*|schema\\.json)")
    checks$data_documented <- result(length(dd) > 0, dd, "No codebook, data dictionary or data README.")
  }
  if (!length(code)) {
    checks$no_absolute_paths <- list(status = "not_applicable", evidence = "No code files.")
    checks$seeds_set <- list(status = "not_applicable", evidence = "No code files.")
  } else {
    abs_hits <- grep_code("(setwd\\s*\\(|[\"'][A-Za-z]:[/\\\\]|[\"']/(Users|home)/)")
    checks$no_absolute_paths <- list(
      status = if (length(abs_hits)) "not_met" else "met",
      evidence = if (length(abs_hits)) paste(utils::head(abs_hits, 10), collapse = "; ")
                 else paste(length(code), "code file(s) without absolute paths or setwd()"))
    random <- grep_code("\\b(sample|rnorm|runif|rbinom|rpois|sample_n|slice_sample|np\\.random\\.|random\\.(random|choice|shuffle|randint)|torch\\.rand|bootstrap|boot\\()")
    seeds <- grep_code("(set\\.seed\\s*\\(|np\\.random\\.seed|random\\.seed\\s*\\(|torch\\.manual_seed|default_rng\\s*\\(\\s*\\d|tar_option_set\\([^)]*seed)")
    checks$seeds_set <- if (!length(random)) {
      list(status = "not_applicable", evidence = "No random number generation found.")
    } else {
      result(length(seeds) > 0, seeds, paste("Randomness without a seed:", paste(utils::head(random, 5), collapse = "; ")))
    }
  }
  sizes <- file.size(file.path(path, files))
  big <- files[!is.na(sizes) & sizes > large_file_mb * 1024^2]
  checks$no_large_files <- list(status = if (length(big)) "not_met" else "met",
                                evidence = if (length(big)) paste(utils::head(big, 5), collapse = "; ")
                                           else paste0("No file above ", large_file_mb, " MB"))

  cl <- rr_checklist()
  cl$status <- vapply(cl$id, function(i) checks[[i]]$status %||% "unverifiable", "")
  cl$evidence <- vapply(cl$id, function(i) checks[[i]]$evidence %||% NA_character_, "")
  rownames(cl) <- NULL
  structure(list(checks = cl, path = normalizePath(path, winslash = "/"),
                 project_type = if (is_package) "r-package" else "research-project", audited_at = utc_now(),
                 n_files = length(files), n_code_files = length(code),
                 version = as.character(utils::packageVersion("reproducibility"))),
            class = "rr_audit")
}

#' @export
as.data.frame.rr_audit <- function(x, ...) x$checks

#' @export
print.rr_audit <- function(x, ...) {
  ch <- x$checks
  cat("Reproducibility audit of ", x$path, " (", x$project_type, ")\n", x$n_files, " files, ",
      x$n_code_files, " code files\n\n", sep = "")
  mark <- c(met = "[x]", not_met = "[ ]", not_applicable = "[-]", unverifiable = "[?]")
  for (i in seq_len(nrow(ch))) {
    cat(sprintf("%s %-22s %s%s\n", mark[[ch$status[i]]], ch$id[i], ch$status[i], if (ch$critical[i]) " (critical)" else ""))
  }
  crit <- ch$critical & ch$status == "not_met"
  cat("\n", sum(ch$status == "met"), " met, ", sum(ch$status == "not_met"), " not met (", sum(crit),
      " critical). Conditions for reproducibility, not a reproduction.\n", sep = "")
  invisible(x)
}

#' Write an audit or detection result
#'
#' `rr_report()` writes a self-contained HTML report of an audit, with the
#' evidence and reference of every item. `rr_write_json()` writes an audit or
#' a [rr_detect()] result as JSON for other tools.
#'
#' @param x An `rr_audit` (both functions) or a data frame from [rr_detect()]
#'   (`rr_write_json()` only).
#' @param path File to write.
#' @return `path`, invisibly.
#' @export
#' @examples
#' dir <- file.path(tempdir(), "demo-project")
#' rr_template(dir, title = "Demo project")
#' audit <- rr_audit(dir)
#' rr_report(audit, tempfile(fileext = ".html"))
#' rr_write_json(audit, tempfile(fileext = ".json"))
rr_report <- function(x, path) {
  if (!inherits(x, "rr_audit")) stop("`x` must come from rr_audit().", call. = FALSE)
  esc <- function(s) {
    s <- gsub("&", "&amp;", as.character(s), fixed = TRUE)
    s <- gsub("<", "&lt;", s, fixed = TRUE)
    gsub(">", "&gt;", s, fixed = TRUE)
  }
  ch <- x$checks
  rows <- paste0("<tr class=\"", ch$status, "\"><td>", esc(ch$category), "</td><td>", esc(ch$item),
                 if (length(ch$id)) ifelse(ch$critical, " <strong>(critical)</strong>", ""), "</td><td>", ch$status,
                 "</td><td>", esc(ifelse(is.na(ch$evidence), "", ch$evidence)), "</td><td>", esc(ch$reference),
                 "</td></tr>", collapse = "\n")
  html <- paste0(
    "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\"><title>Reproducibility audit</title>",
    "<style>body{font-family:system-ui,sans-serif;max-width:1100px;margin:2rem auto;padding:0 1rem;color:#1d2433}",
    "table{border-collapse:collapse;width:100%;font-size:.9rem}td,th{border:1px solid #d5d9e0;padding:.4rem;text-align:left;vertical-align:top}",
    ".met td:nth-child(3){color:#1b7a3d}.not_met td:nth-child(3){color:#b42318;font-weight:600}",
    ".note{background:#eef3fb;border-left:4px solid #3267b7;padding:.8rem 1rem}</style></head><body>",
    "<h1>Reproducibility audit</h1><p>", esc(x$path), " &middot; ", x$n_files, " files, ", x$n_code_files,
    " code files &middot; audited ", x$audited_at, " with reproducibility ", x$version, "</p>",
    "<p class=\"note\">This audit checks the <em>conditions</em> for reproducing the project from its files. ",
    "It does not run the analysis, so it cannot show that the results reproduce.</p>",
    "<table><tr><th>Category</th><th>Item</th><th>Status</th><th>Evidence</th><th>Reference</th></tr>", rows,
    "</table></body></html>")
  writeLines(html, path, useBytes = TRUE)
  invisible(path)
}

#' @rdname rr_report
#' @export
rr_write_json <- function(x, path) {
  obj <- if (inherits(x, "rr_audit")) {
    c(list(schema = "rr-audit/1"), unclass(x)[c("path", "project_type", "audited_at", "version", "n_files", "n_code_files")],
      list(checks = x$checks))
  } else if (is.data.frame(x) && "detector_version" %in% names(x)) {
    list(schema = "rr-detect/1", detector_version = unique(x$detector_version), articles = x)
  } else {
    stop("`x` must come from rr_audit() or rr_detect().", call. = FALSE)
  }
  jsonlite::write_json(obj, path, auto_unbox = TRUE, pretty = TRUE, na = "null", dataframe = "rows")
  invisible(path)
}
