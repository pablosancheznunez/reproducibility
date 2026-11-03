# reproducibility <img src="man/figures/logo.svg" align="right" height="139" alt="reproducibility logo" />

Website: <https://pablosancheznunez.com/reproducibility/>

Reproducibility audits of research projects and articles. Both describe the *conditions* for reproducing research; neither reruns an analysis.

Status: **prototype**. Part of the [metascience](https://github.com/pablosancheznunez/metascience) ecosystem.

## Audit a project

```r
# install.packages("remotes")
remotes::install_github("pablosancheznunez/reproducibility")
library(reproducibility)

a <- rr_audit("path/to/project")
a
rr_report(a, "audit.html")         # every item with its evidence and reference
rr_write_json(a, "audit.json")
```

The 14 items in `rr_checklist()` cover documentation, licence, citation metadata, version control, declared and pinned dependencies, containers, pipelines, CI, data availability and documentation, absolute paths, random seeds and large files. Each one names the guide or standard it comes from (The Turing Way, FAIR4RS, TOP Guidelines, the Software Citation Principles). Five items are critical: the minimum for anyone else to rerun the project.

The audit only reads files; it never runs project code. Statuses are `met`, `not_met`, `not_applicable` and `unverifiable`. R packages are recognised: for them `R CMD check` is the entry point, so pipeline and data items do not apply.

```r
rr_template("my-study", title = "My study")   # README, data README, CITATION.cff, run_all.R, .gitignore
rr_use_github_action("my-study")               # audit on every push; fails on critical items
```

Neither function overwrites existing files. The template writes no licence, because the authors choose it.

## Detect transparency in articles

```r
rr_detect(texts, id = dois)
rr_detect(rr_text_from_html(pages))
rr_detect(rr_text_from_jats(xml))
```

Reports five indicators: data availability statement, data shared, code shared, preregistration and materials shared. Each one is `yes`, `no` or `unverifiable` and comes with the sentence that supports it. Text that cannot be read is `unverifiable`, never `no`. The indicators are never summed. English and Spanish statements are recognised.

These are the detectors of the [REPRO2IMPACT](https://github.com/pablosancheznunez/repro2impact) study (detector version 1.1.0), ported without changing their rules. The package runs the study's own tests to make sure they stay identical.

## License

MIT.
