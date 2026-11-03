## Submission

First submission of researchreproducibility.

## Test environments

* GitHub Actions: Ubuntu (R devel, release, oldrel-1), macOS (release), Windows (release).
* Local: Windows 11 x64, R 4.6.2 ucrt.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Notes for CRAN

* The package makes no network requests and never runs the code of the projects it audits.
* `rr_template()` and `rr_use_github_action()` write only to the directory given by the user, never overwrite existing files, and their examples use `tempdir()`.
* Examples and tests write only to `tempdir()`.
