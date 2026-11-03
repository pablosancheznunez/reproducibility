write_new <- function(path, file, lines) {
  target <- file.path(path, file)
  if (file.exists(target)) return(NULL)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, target, useBytes = TRUE)
  file
}

#' Create a reproducible project skeleton
#'
#' Creates the files and folders that cover most of [rr_checklist()]: a README
#' with the sections a reader needs, a data README to document variables,
#' `CITATION.cff`, a single entry point (`run_all.R`) and `.gitignore`.
#' Existing files are never overwritten. No licence is written, because the
#' choice of licence is the authors' decision; the README says where to add
#' it.
#'
#' @param path Directory of the project; created if needed.
#' @param title Project title.
#' @return The paths of the files created, invisibly.
#' @export
#' @examples
#' dir <- file.path(tempdir(), "my-study")
#' rr_template(dir, title = "My study")
#' list.files(dir, recursive = TRUE, all.files = TRUE)
rr_template <- function(path, title = basename(path)) {
  if (!is.character(path) || length(path) != 1L) stop("`path` must be a single directory path.", call. = FALSE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  for (d in c("data/raw", "data/processed", "R", "outputs")) {
    dir.create(file.path(path, d), recursive = TRUE, showWarnings = FALSE)
  }
  created <- c(
    write_new(path, "README.md", c(
      paste("#", title), "",
      "## What this is", "", "One paragraph: question, design and main output.", "",
      "## How to reproduce", "", "1. Install the dependencies (`renv::restore()` if `renv.lock` exists).",
      "2. Run `source(\"run_all.R\")` from the project root.", "3. Results are written to `outputs/`.", "",
      "## Data", "", "Where the data come from, their licence and persistent identifier (DOI). See `data/README.md`.", "",
      "## Licence", "", "Add a LICENSE file for the code and state the licence of the data here.", "",
      "## Citation", "", "See `CITATION.cff`.")),
    write_new(path, "data/README.md", c(
      "# Data", "", "| File | Source | Licence | Identifier |", "| --- | --- | --- | --- |", "",
      "## Variables", "", "| Variable | Type | Unit | Description |", "| --- | --- | --- | --- |")),
    write_new(path, "CITATION.cff", c(
      "cff-version: 1.2.0", "message: \"If you use this work, please cite it as below.\"", "type: software",
      paste0("title: \"", gsub("\"", "'", title), "\""), "authors:", "  - family-names: \"\"", "    given-names: \"\"")),
    write_new(path, "run_all.R", c(
      "# Runs the whole analysis from a clean session, from the project root.",
      "# Use relative paths only; set a seed with set.seed() before any random step.",
      "for (f in sort(list.files(\"R\", pattern = \"\\\\.R$\", full.names = TRUE))) source(f)")),
    write_new(path, ".gitignore", c(".Rproj.user", ".Rhistory", ".RData", "renv/library/", "outputs/*.tmp"))
  )
  invisible(file.path(path, created))
}

#' Run the audit on every change with GitHub Actions
#'
#' Writes `.github/workflows/reproducibility-audit.yml`, which audits the
#' repository on every push and pull request, uploads the HTML report as an
#' artifact and fails when a critical item is not met. An existing workflow
#' file is never overwritten.
#'
#' @param path The project directory.
#' @return The path of the workflow file, invisibly; `NULL` if it already
#'   existed.
#' @export
#' @examples
#' dir <- file.path(tempdir(), "my-study")
#' rr_template(dir)
#' rr_use_github_action(dir)
rr_use_github_action <- function(path = ".") {
  if (!dir.exists(path)) stop("`path` must be an existing directory.", call. = FALSE)
  file <- write_new(path, ".github/workflows/reproducibility-audit.yml", c(
    "name: Reproducibility audit", "", "on:", "  push:", "  pull_request:", "  workflow_dispatch:", "",
    "permissions:", "  contents: read", "", "jobs:", "  audit:", "    runs-on: ubuntu-latest", "    steps:",
    "      - uses: actions/checkout@v4", "      - uses: r-lib/actions/setup-r@v2", "        with:",
    "          use-public-rspm: true",
    "      - name: Install reproducibility",
    "        run: Rscript -e 'install.packages(\"remotes\"); remotes::install_github(\"pablosancheznunez/reproducibility\")'",
    "      - name: Audit",
    "        run: |",
    "          Rscript -e '",
    "            a <- reproducibility::rr_audit(\".\")",
    "            print(a)",
    "            reproducibility::rr_report(a, \"reproducibility-audit.html\")",
    "            ch <- a$checks",
    "            if (any(ch$critical & ch$status == \"not_met\")) quit(status = 1)",
    "          '",
    "      - uses: actions/upload-artifact@v4", "        if: always()", "        with:",
    "          name: reproducibility-audit", "          path: reproducibility-audit.html"))
  if (is.null(file)) {
    message("Workflow already exists; not overwritten.")
    return(invisible(NULL))
  }
  invisible(file.path(path, file))
}
