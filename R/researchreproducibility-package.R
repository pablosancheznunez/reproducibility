#' researchreproducibility: reproducibility audits of projects and articles
#'
#' Two kinds of audit, both describing *conditions* for reproducibility, not
#' whether a result was reproduced:
#'
#' * [rr_audit()]: a project directory, against the checklist in
#'   [rr_checklist()];
#' * [rr_detect()]: the full text of articles, with the transparency detectors
#'   of the REPRO2IMPACT study (detector version 1.1.0).
#'
#' Results use four statuses: `met`, `not_met`, `not_applicable` and
#' `unverifiable`. Something that cannot be checked is `unverifiable`, never
#' `not_met`. [rr_report()] and [rr_write_json()] write the results;
#' [rr_template()] and [rr_use_github_action()] help a project meet the
#' checklist and keep meeting it.
#'
#' @keywords internal
"_PACKAGE"
