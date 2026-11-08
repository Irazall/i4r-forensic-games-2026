## ============================================================================================
## CLAIMS SCRIPT — verify every claim in the AI report, one numbered block at a time.
## Claim numbers match the [claim N] tags in the report, the Appendix-A findings table, and the
## rows of the verdict survey.
##
## THE RULE: if you cannot verify a claim marked RETRACTION or CORRIGENDUM MATERIAL after
## honest study, RETURN THE PAPER and take another. Do not skip it. Do not let AI decide.
##
## NEVER contact the paper's authors or the journal. I4R handles all correspondence.
##
## 10 claims: 1 retraction material, 7 corrigendum material, 2 comment material. File language: .R (the deposit's own).
## ============================================================================================

## ---- standalone check harness: the verification notebook's setup chunk -------------------------
## How to run it: Rscript claims_script.R from any folder, with the deposit's folder in I4R_PKG_DIR (or a pkg/
## folder beside the working directory). Checks that read the article find it in paper/ beside pkg/ (or in
## I4R_AUDIT_DIR/paper/): the PDF as paper/paper.pdf and its text as paper/paper.txt, made with
##     pdftotext -layout paper/paper.pdf paper/paper.txt
## Run it in a UTF-8 locale. Every check passed, with these helpers, in the audit's verification notebook.
if (!isTRUE(l10n_info()[["UTF-8"]]))
  message("This R session is not in a UTF-8 locale; checks on text with non-ASCII characters may FAIL. ",
          "Run it with LANG=en_US.UTF-8 (or C.UTF-8).")

## The packages these checks use. A missing one is named here; the claims that need it record a FAIL.
.I4R_NEED <- c("haven", "afex", "emmeans", "car")
.I4R_MISSING <- .I4R_NEED[!vapply(.I4R_NEED, requireNamespace, logical(1), quietly = TRUE)]
if (length(.I4R_MISSING)) message(
  "These R packages are used below but not installed: ", paste(.I4R_MISSING, collapse = ", "),
  "\nInstall them, then run this file again: install.packages(", deparse(.I4R_MISSING), ")",
  "\n(a Bioconductor package installs with BiocManager::install()). Until then the claims that need them FAIL.")
suppressPackageStartupMessages(for (.p in c("knitr", "haven")) if (requireNamespace(.p, quietly = TRUE)) library(.p, character.only = TRUE))

## Resolve the deposit WITHOUT hardcoding the auditor's machine: I4R_PKG_DIR, else a pkg/ folder beside the
## working directory (or the audit layout's), else stop.
PKG_DIR <- local({
  e <- Sys.getenv("I4R_PKG_DIR", "")
  if (nzchar(e) && dir.exists(e)) return(normalizePath(e))
  for (cand in c(file.path(getwd(), "pkg"), file.path(dirname(getwd()), "audit", "pkg"), "pkg"))
    if (dir.exists(cand)) return(normalizePath(cand))
  stop("cannot locate the deposit: set I4R_PKG_DIR to the extracted package directory (the pkg/ folder)")
})
AUDIT_DIR <- local({
  e <- Sys.getenv("I4R_AUDIT_DIR", "")
  if (nzchar(e) && dir.exists(e)) return(normalizePath(e))
  normalizePath(dirname(PKG_DIR))
})
SNIPS_DIR <- local({
  e <- Sys.getenv("I4R_SNIPS_DIR", "")
  if (nzchar(e) && dir.exists(e)) return(e)
  cand <- file.path(AUDIT_DIR, "verification_snippets")
  if (dir.exists(cand)) normalizePath(cand) else cand
})
## The notebook's chunks run from the audit folder; so do these checks.
setwd(AUDIT_DIR)
if (!file.exists(file.path(AUDIT_DIR, "paper", "paper.txt"))) message(
  "Some checks read the article at ", file.path(AUDIT_DIR, "paper", "paper.txt"), ": put the PDF at ",
  "paper/paper.pdf beside the deposit folder and run pdftotext -layout paper/paper.pdf paper/paper.txt ",
  "(or set I4R_AUDIT_DIR to the folder holding paper/). Until then those claims FAIL.")

## ---- read-only guard: pkg/ is the authors' deposit. Fingerprint on entry, re-check in the summary; scratch goes
## to tempdir(), never beside the deposit.
.pkg_manifest <- function() {
  fs <- list.files(PKG_DIR, recursive = TRUE, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  data.frame(file = fs, size = file.size(fs), md5 = unname(tools::md5sum(fs)), stringsAsFactors = FALSE)
}
.PKG_ON_ENTRY <- .pkg_manifest()
SCRATCH <- tempfile("verif_"); dir.create(SCRATCH)

## ---- deposit loader: always R, whatever the package was written in.
read_deposit <- function(file, ...) {
  path <- if (file.exists(file.path(PKG_DIR, file))) file.path(PKG_DIR, file) else file
  if (!file.exists(path)) stop("not in the deposit: ", file)
  ext <- tolower(tools::file_ext(path))
  switch(ext,
         xlsx = , xls  = readxl::read_excel(path, ...),
         dta          = haven::read_dta(path, ...),
         sav = , por  = haven::read_sav(path, ...),
         sas7bdat     = haven::read_sas(path, ...),
         omv          = read_omv_deposit(path, ...),
         csv          = utils::read.csv(path, ...),
         tsv = , txt  = utils::read.delim(path, ...),
         rds          = readRDS(path),
         rdata = , rda = { e <- new.env(); load(path, envir = e); as.list(e) },
         parquet      = arrow::read_parquet(path, ...),
         stop("no loader for .", ext, " -- add one to read_deposit()")
  )
}
## A jamovi .omv is a zip: prefer the package; fall back to the zip so a missing jmvReadWrite does not block a check.
read_omv_deposit <- function(path, ...) {
  if (requireNamespace("jmvReadWrite", quietly = TRUE))
    return(jmvReadWrite::read_omv(path, ...))
  message("jmvReadWrite unavailable; reading the .omv as a zip archive")
  ex <- file.path(SCRATCH, paste0("omv_", basename(path)))
  dir.create(ex, showWarnings = FALSE); utils::unzip(path, exdir = ex)
  list(parts = list.files(ex, recursive = TRUE, full.names = TRUE))
}

## ---- expectation / check machinery: the notebook's verdict rules, one printed line per value.
.V <- new.env(parent = emptyenv())
.V$exp <- list(); .V$obs <- list(); .V$state <- list(); .V$order <- character(0); .V$current <- NULL
## A claim's block may record outcomes only under its own claim id.
.bind_claim <- function(id) {
  if (!is.null(.V$current) && !identical(id, .V$current))
    stop(sprintf("the %s block recorded an outcome under %s", .V$current, id))
}
.fmt <- function(x) {
  v <- if (is.numeric(x)) {
    ifelse(is.na(x), "NA",
           ifelse(x == round(x) & abs(x) < 1e15, format(x, big.mark = ",", scientific = FALSE),
                  format(round(x, 6), nsmall = 0, scientific = FALSE)))
  } else as.character(x)
  paste(v, collapse = ", ")
}
.show <- function(x) if (is.null(x)) "NULL" else paste(format(x), collapse = ", ")
## expect() runs BEFORE anything is computed and prints the prediction.
expect <- function(id, ..., tol = 0, note = NULL) {
  .bind_claim(id)
  vals <- list(...); if (is.null(names(vals)) || any(names(vals) == "")) stop("expect(): every value needs a name")
  for (k in names(vals)) {
    key <- paste(id, k, sep = "|")
    .V$exp[[key]] <- list(id = id, key = k, expected = vals[[k]], tol = tol, note = note)
    if (!key %in% .V$order) .V$order <- c(.V$order, key)
    cat(sprintf("[%-6s %-34s] EXPECTED  %s\n", id, k, .show(vals[[k]])))
  }
  invisible(TRUE)
}
## check() prints the observation and the verdict, and never stops the run.
check <- function(id, ...) {
  .bind_claim(id)
  vals <- list(...); if (is.null(names(vals)) || any(names(vals) == "")) stop("check(): every value needs a name")
  for (k in names(vals)) {
    key <- paste(id, k, sep = "|"); e <- .V$exp[[key]]; ob <- vals[[k]]
    if (is.null(e)) {
      .V$obs[[key]] <- list(id = id, key = k, observed = vals[[k]], verdict = "FAIL", detail = "never declared with expect()")
      if (!key %in% .V$order) .V$order <- c(.V$order, key)
      cat(sprintf("[%-6s %-34s] FAIL  undeclared\n", id, k)); next
    }
    ex <- e$expected; tol <- e$tol
    if (is.numeric(ex) && is.numeric(ob) && length(ex) == length(ob)) {
      d <- max(abs(ex - ob)); ok <- isTRUE(all(!is.na(d)) && d <= tol + 1e-12)
    } else ok <- identical(as.character(ex), as.character(ob))
    .V$obs[[key]] <- list(id = id, key = k, observed = ob, verdict = if (ok) "PASS" else "FAIL")
    cat(sprintf("[%-6s %-34s] OBSERVED  %s  %s\n", id, k, .show(ob), if (ok) "PASS" else "FAIL"))
  }
  invisible(TRUE)
}
## Reach back into an earlier claim's observation.
observed_of <- function(claim, key) {
  o <- .V$obs[[paste(claim, key, sep = "|")]]
  if (is.null(o)) NA else o$observed
}
## The claim rests on an assessment, stated so a replicator can reject it on the merits.
judgement <- function(id, statement, basis = "") {
  .bind_claim(id)
  .V$state[[id]] <- list(id = id, verdict = "JUDGEMENT")
  cat(sprintf("[%-6s %-34s] JUDGEMENT  %s\n", id, "-", statement))
  if (nzchar(basis)) cat(sprintf("[%-6s %-34s] basis      %s\n", id, "-", basis))
  invisible(TRUE)
}
## The deposit cannot settle it; the reason is given.
unverifiable <- function(id, reason, observed = NULL) {
  .bind_claim(id)
  .V$state[[id]] <- list(id = id, verdict = "UNVERIFIABLE")
  cat(sprintf("[%-6s %-34s] UNVERIFIABLE  %s\n", id, "-", reason))
  invisible(TRUE)
}
## A runtime error inside a claim's block is that claim's FAIL, never the end of the run.
runtime_failure <- function(id, error) {
  .V$state[[id]] <- list(id = id, verdict = "FAIL")
  cat(sprintf("[%-6s %-34s] FAIL  runtime error: %s\n", id, "-", conditionMessage(error)))
  invisible(FALSE)
}
claims_summary <- function(claims = character(0)) {
  for (key in .V$order) if (is.null(.V$obs[[key]]))
    .V$obs[[key]] <- list(id = .V$exp[[key]]$id, key = .V$exp[[key]]$key, verdict = "FAIL")
  verdicts <- vapply(.V$obs, function(o) o$verdict, character(1))
  states <- vapply(.V$state, function(s) s$verdict, character(1))
  recorded <- unique(c(vapply(.V$obs, function(o) o$id, character(1)), names(.V$state)))
  silent <- setdiff(claims, recorded)
  foreign <- if (length(claims)) setdiff(recorded, claims) else character(0)
  after <- .pkg_manifest()
  sorted <- function(m) { m <- m[order(enc2utf8(m$file), method = "radix"), c("file", "size", "md5")]; rownames(m) <- NULL; m }
  same <- identical(sorted(.PKG_ON_ENTRY), sorted(after))
  prelude_ok <- isTRUE(get0(".I4R_PRELUDE_OK", ifnotfound = TRUE))
  n_pass <- sum(verdicts == "PASS")
  n_fail <- sum(verdicts != "PASS") + sum(states == "FAIL") + length(silent) + length(foreign) + (!same) + (!prelude_ok)
  cat(sprintf("\n%d checks, %d PASS, %d FAIL\n", n_pass + n_fail, n_pass, n_fail))
  cat(sprintf("claims marked JUDGEMENT: %d; UNVERIFIABLE: %d\n", sum(states == "JUDGEMENT"), sum(states == "UNVERIFIABLE")))
  if (!prelude_ok) cat("the shared helpers (_prelude.R) stopped with an error; claims that use them FAIL\n")
  if (any(states == "FAIL")) cat("claims that stopped with an error:", paste(names(states)[states == "FAIL"], collapse = ", "), "\n")
  if (length(silent)) cat("claims that recorded no outcome:", paste(silent, collapse = ", "), "\n")
  if (length(foreign)) cat("outcomes recorded under ids that are not claims here:", paste(foreign, collapse = ", "), "\n")
  cat(sprintf("deposit unmodified: %s\n", if (same) "yes" else "NO -- INVESTIGATE"))
  invisible(n_fail)
}
## ---- audit-specific helpers (verification_snippets/_prelude.R) ----
.I4R_PRELUDE_OK <- tryCatch({
  ## Audit-specific helpers: Altermann & Gröpel (2025), International Journal of Sport and Exercise Psychology 23(3), 487-500, doi 10.1080/1612197X.2024.2332280.
  ## The package holds one SPSS file (120 recruited students: 96 completers and 24 dropouts) and the authors' SPSS syntax. Completers are selected with
  ## the syntax's own filter (Concentration_KL_POST > 0). Snippets print counts, aggregate statistics and model estimates only (no ID, individual
  ## value, row position, extreme or range) and never modify the shared objects built here.
  suppressPackageStartupMessages({ library(haven); library(afex); library(emmeans); library(car) })
  AT_PKG <- normalizePath(PKG_DIR)
  AT_SAV <- file.path(AT_PKG, "Altermann_LongtermExercise&Attention.sav")
  AT_SYNTAX <- readLines(file.path(AT_PKG, "Study_Syntax.txt"), warn = FALSE, encoding = "UTF-8")
  AT_RAW <- haven::read_sav(AT_SAV)
  AT_D0 <- haven::zap_labels(AT_RAW)
  AT_GN <- c("endurance", "strength", "coordination", "control")
  AT_D0$G <- factor(AT_D0$Intervention, levels = 1:4, labels = AT_GN)
  AT_DONE <- !is.na(AT_D0$Concentration_KL_POST) & AT_D0$Concentration_KL_POST > 0
  AT_D <- AT_D0[AT_DONE, ]
  AT_OUT <- c(KL = "Concentration_KL", F = "ErrorScore", BZO = "Speed_BZO")
  AT_W <- list(ctrl = c(1, 1, 1, -3), end = c(2, -1, -1, 0), str = c(-1, 2, -1, 0), coo = c(-1, -1, 2, 0))
  AT_CHANGE <- function(o, data = AT_D) data[[paste0(o, "_POST")]] - data[[paste0(o, "_PRE")]]
  ## an LMATRIX contrast with MMATRIX pre -1 post 1 = the contrast of the post-minus-pre change, against the change-score model's residual
  AT_CON <- function(o, w, data = AT_D) { ch <- AT_CHANGE(o, data); fit <- stats::lm(ch ~ data$G); sse <- sum(stats::resid(fit)^2); mse <- sse / fit$df.residual
  m <- tapply(ch, data$G, mean); n <- tapply(ch, data$G, length); est <- sum(w * m); ss <- est^2 / sum(w^2 / n)
  c(estimate = est, F = ss / mse, df2 = fit$df.residual, p = stats::pf(ss / mse, 1, fit$df.residual, lower.tail = FALSE), pes = ss / (ss + sse)) }
  ## the 4 x 2 repeated-measures ANOVA (type III) of one outcome
  AT_ANOVA <- function(o, data = AT_D) {
    long <- rbind(data.frame(ID = data$ID, G = data$G, P = "pre", y = data[[paste0(o, "_PRE")]]), data.frame(ID = data$ID, G = data$G, P = "post", y = data[[paste0(o, "_POST")]]))
    suppressMessages(afex::aov_ez("ID", "y", long, between = "G", within = "P", type = 3, anova_table = list(es = "pes")))$anova_table }
  ## pairwise comparisons of change (pooled SD of the change-score model) and the ANCOVA of the posttest on the pretest (sum-to-zero contrasts set on a copy)
  AT_PAIRS <- function(o, data = AT_D) { dd <- data.frame(G = data$G, ch = AT_CHANGE(o, data)); summary(emmeans::contrast(emmeans::emmeans(stats::lm(ch ~ G, dd), "G"), "pairwise", adjust = "none")) }
  AT_ANCOVA <- function(o, data = AT_D) { dd <- data.frame(G = data$G, pre = data[[paste0(o, "_PRE")]], post = data[[paste0(o, "_POST")]]); contrasts(dd$G) <- stats::contr.sum(4)
  stats::lm(post ~ G + pre, dd) }
  ## Article text: pdftotext -raw keeps sentences that the layout interleaves; fall back to paper.txt
  PAPER_TXT <- local({
    raw <- tryCatch(suppressWarnings(system2("pdftotext", c("-q", "-raw", shQuote(file.path(AUDIT_DIR, "paper", "paper.pdf")), "-"), stdout = TRUE)), error = function(e) character(0))
    if (!length(raw)) raw <- readLines(file.path(AUDIT_DIR, "paper", "paper.txt"), warn = FALSE)
    x <- paste(raw, collapse = " "); x <- gsub("­\\s*", "", x); x <- gsub("ﬁ", "fi", gsub("ﬂ", "fl", gsub("ﬀ", "ff", x)))
    x <- gsub("[–−]", "-", x); x <- gsub("​", "", x); gsub("\\s+", " ", x) })
  PAPER_NS <- gsub("\\s+", "", PAPER_TXT)
  paper_has <- function(s) as.integer(grepl(gsub("\\s+", "", s), PAPER_NS, fixed = TRUE))
  code_has <- function(s, lines = AT_SYNTAX) as.integer(any(grepl(s, lines, fixed = TRUE)))
  TRUE }, error = function(e) { cat(sprintf("[%-6s %-34s] FAIL  runtime error: %s\n", "prelude", "-", conditionMessage(e))); FALSE })
## ============================================================================================
## ============================================================================================
## CLAIM 1 [RETRACTION MATERIAL]  (finding F1, Section 3)
##   The Abstract, Discussion and Conclusion state that coordination training gave the highest
##   and strength training the lowest improvement in attention, but for the concentration score
##   the coordination contrast (coordination against the average of endurance and strength) has
##   p = .0177, above the article's own Bonferroni level of .0125; the article reports no direct
##   test of coordination against endurance (difference in change 6.05 points, 95% CI -4.95 to
##   17.05, p = .278 in the deposited data); and with the posttest adjusted for the pretest none
##   of the nine exercise-mode contrasts reaches .0125 (concentration: strength p = .0615,
##   coordination p = .0226; error score: strength p = .074, coordination p = .101).
## Check written by the audit (no author code covers this claim):

.V$current <- "C-1"
tryCatch({
  ## C-1: The Abstract, Discussion and Conclusion state that coordination training gave the highest and strength training the lowest improvement in attention, but for the concentration score the coordination contrast (coordination against the average of endurance and strength) has p = .0177, above the article's own Bonferroni level of .0125; the article reports no direct test of coordination against endurance (difference in change 6.05 points, 95% CI -4.95 to 17.05, p = .278 in the deposited data); and with the posttest adjusted for the pretest none of the nine exercise-mode contrasts reaches .0125 (concentration: strength p = .0615, coordination p = .0226; error score: strength p = .074, coordination p = .101).
  .kl <- "Concentration_KL"; .er <- "ErrorScore"; .sp <- "Speed_BZO"
  expect("C-1", coordination_contrast_p = 0.0177, tol = 0.00005, note = "LMATRIX -1 -1 2 0 with MMATRIX pre -1 post 1 on the concentration performance score (completers)")
  check("C-1", coordination_contrast_p = AT_CON(.kl, AT_W$coo)[["p"]])
  expect("C-1", coordination_contrast_above_0125 = 1, article_level_0125 = 1, article_marginally_significant = 1, tol = 0,
         note = "the contrast's p against the article's Bonferroni level, and the article's own level and wording")
  check("C-1", coordination_contrast_above_0125 = as.integer(AT_CON(.kl, AT_W$coo)[["p"]] > 0.0125), article_level_0125 = paper_has("we used the Bonferroni-adjusted p value of .0125"),
        article_marginally_significant = paper_has("marginally significant contrast for the coordination group"))
  .ch <- AT_CHANGE(.kl); .fit <- stats::lm(.ch ~ AT_D$G)
  .ce <- summary(emmeans::contrast(emmeans::emmeans(stats::lm(ch ~ G, data.frame(ch = .ch, G = AT_D$G)), "G"), list(coo_end = c(-1, 0, 1, 0))), infer = TRUE)
  expect("C-1", coordination_minus_endurance = 6.05, ci_lower = -4.95, ci_upper = 17.05, tol = 0.005, note = "difference in mean change, coordination minus endurance, pooled SD of the change-score model (df 92)")
  check("C-1", coordination_minus_endurance = .ce$estimate, ci_lower = .ce$lower.CL, ci_upper = .ce$upper.CL)
  expect("C-1", coordination_minus_endurance_p = 0.2777, tol = 0.00005); check("C-1", coordination_minus_endurance_p = .ce$p.value)
  expect("C-1", syntax_direct_coordination_endurance_contrasts = 0, tol = 0, note = "LMATRIX lines in the syntax that compare coordination with endurance alone (weights -1 0 1 0 or 1 0 -1 0)")
  check("C-1", syntax_direct_coordination_endurance_contrasts = sum(grepl("Intervention +(-1 +0 +1 +0|1 +0 +-1 +0)\\b", AT_SYNTAX)))
  .anc <- function(o, k) summary(emmeans::contrast(emmeans::emmeans(AT_ANCOVA(o), "G"), list(c = AT_W[[k]])))$p.value
  .np <- c(kl_str = .anc(.kl, "str"), kl_coo = .anc(.kl, "coo"), kl_end = .anc(.kl, "end"), er_str = .anc(.er, "str"), er_coo = .anc(.er, "coo"), er_end = .anc(.er, "end"),
           sp_str = .anc(.sp, "str"), sp_coo = .anc(.sp, "coo"), sp_end = .anc(.sp, "end"))
  expect("C-1", ancova_concentration_strength_p = 0.0615, ancova_concentration_coordination_p = 0.0226, ancova_error_strength_p = 0.0738, ancova_error_coordination_p = 0.1011, tol = 0.00005,
         note = "posttest regressed on pretest and arm (sum-to-zero coding): the article's mode contrasts")
  check("C-1", ancova_concentration_strength_p = .np[["kl_str"]], ancova_concentration_coordination_p = .np[["kl_coo"]], ancova_error_strength_p = .np[["er_str"]], ancova_error_coordination_p = .np[["er_coo"]])
  expect("C-1", ancova_mode_contrasts_below_0125 = 0, ancova_smallest_mode_p = 0.0226, tol = 0.00005, note = "the nine mode contrasts (three outcomes x three modes) after baseline adjustment")
  check("C-1", ancova_mode_contrasts_below_0125 = sum(.np < 0.0125), ancova_smallest_mode_p = min(.np))
  .pr <- AT_PAIRS(.kl); .pe <- AT_PAIRS(.er); .ps <- AT_PAIRS(.sp)
  expect("C-1", strength_vs_coordination_difference = 17.12, tol = 0.005); check("C-1", strength_vs_coordination_difference = -.pr$estimate[.pr$contrast == "strength - coordination"])
  expect("C-1", strength_vs_coordination_p = 0.0026, strength_vs_coordination_bonferroni6 = 0.0158, tol = 0.00005, note = "unadjusted and Bonferroni over the six pairs")
  check("C-1", strength_vs_coordination_p = .pr$p.value[.pr$contrast == "strength - coordination"], strength_vs_coordination_bonferroni6 = min(1, 6 * .pr$p.value[.pr$contrast == "strength - coordination"]))
  expect("C-1", error_coordination_minus_endurance = -2.43, speed_coordination_minus_endurance = 2.12, tol = 0.005)
  check("C-1", error_coordination_minus_endurance = -.pe$estimate[.pe$contrast == "endurance - coordination"], speed_coordination_minus_endurance = -.ps$estimate[.ps$contrast == "endurance - coordination"])
  expect("C-1", error_coordination_minus_endurance_p = 0.0856, speed_coordination_minus_endurance_p = 0.7103, tol = 0.00005)
  check("C-1", error_coordination_minus_endurance_p = .pe$p.value[.pe$contrast == "endurance - coordination"], speed_coordination_minus_endurance_p = .ps$p.value[.ps$contrast == "endurance - coordination"])
  .pm <- tapply(AT_D[[paste0(.kl, "_PRE")]], AT_D$G, mean)
  expect("C-1", pretest_strength = 162.25, pretest_endurance = 149.79, pretest_coordination = 149.92, tol = 0.005, note = "concentration performance pretest means of the analysed arms")
  check("C-1", pretest_strength = .pm[["strength"]], pretest_endurance = .pm[["endurance"]], pretest_coordination = .pm[["coordination"]])
  .bw <- stats::anova(stats::lm(AT_D[[paste0(.er, "_PRE")]] ~ AT_D$G))
  expect("C-1", pretest_error_F = 2.93, tol = 0.005); check("C-1", pretest_error_F = .bw[["F value"]][1])
  expect("C-1", pretest_error_p = 0.038, tol = 0.0005); check("C-1", pretest_error_p = .bw[["Pr(>F)"]][1])
  .r <- vapply(AT_OUT, function(o) stats::cor(AT_CHANGE(o), AT_D[[paste0(o, "_PRE")]]), numeric(1))
  expect("C-1", change_baseline_r_concentration = -0.69, change_baseline_r_error = -0.82, change_baseline_r_speed = -0.69, tol = 0.005, note = "correlation of change with the pretest (completers)")
  check("C-1", change_baseline_r_concentration = .r[["KL"]], change_baseline_r_error = .r[["F"]], change_baseline_r_speed = .r[["BZO"]])
  .wc <- function(o, w) { ch <- AT_CHANGE(o); m <- tapply(ch, AT_D$G, mean); v <- tapply(ch, AT_D$G, stats::var); n <- tapply(ch, AT_D$G, length)
  q <- w^2 * v / n; t <- sum(w * m) / sqrt(sum(q)); df <- sum(q)^2 / sum(q^2 / (n - 1)); 2 * stats::pt(-abs(t), df) }
  expect("C-1", separate_variance_strength_p = 0.0081, separate_variance_coordination_p = 0.039, tol = 0.0005, note = "the mode contrasts on concentration change with separate variances (Welch-Satterthwaite df)")
  check("C-1", separate_variance_strength_p = .wc(.kl, AT_W$str), separate_variance_coordination_p = .wc(.kl, AT_W$coo))
  .det <- function(a) stats::uniroot(function(d) { ncp <- d * sqrt(24 / 2); q <- stats::qt(1 - a / 2, 92); 1 - stats::pt(q, 92, ncp) + stats::pt(-q, 92, ncp) - 0.8 }, c(0.1, 3))$root
  expect("C-1", detectable_d_05 = 0.82, detectable_d_0125 = 0.98, tol = 0.005, note = "standardised two-arm difference detectable with 80% power, n = 24 per arm, error df 92")
  check("C-1", detectable_d_05 = .det(0.05), detectable_d_0125 = .det(0.0125))
  expect("C-1", observed_d_coordination_endurance = 0.32, tol = 0.005, note = "6.05 points over the residual SD of the change-score model")
  check("C-1", observed_d_coordination_endurance = .ce$estimate / summary(.fit)$sigma)
  .gp <- function(N) { lam <- 0.25^2 * N * 2 / (1 - 0.5); 1 - stats::pf(stats::qf(0.95, 3, (N - 4)), 3, (N - 4), ncp = lam) }
  expect("C-1", omnibus_interaction_power_n96 = 0.988, tol = 0.0005, note = "G*Power within-between interaction: f = .25, correlation .5, two measurements, four arms")
  check("C-1", omnibus_interaction_power_n96 = .gp(96))
  expect("C-1", endurance_contrast_p = 0.6014, tol = 0.00005); check("C-1", endurance_contrast_p = AT_CON(.kl, AT_W$end)[["p"]])
  expect("C-1", abstract_ranking_sentence = 1, abstract_greatest_effects = 1, abstract_pe_teachers = 1, discussion_high_power = 1, tol = 0, note = "article text")
  check("C-1", abstract_ranking_sentence = paper_has("The coordination group showed the highest, and the strength group the lowest, improvements in attentional performance"),
        abstract_greatest_effects = paper_has("with the greatest effects being observed in the coordination exercise group"),
        abstract_pe_teachers = paper_has("Physical education teachers are encouraged to enrich their lessons with coordinative tasks"),
        discussion_high_power = paper_has("high statistical power to detect predicted effects"))
  .pF <- function(gg) { m <- tapply(.ch, gg, mean); f <- stats::lm(.ch ~ gg); (sum(AT_W$coo * m))^2 / (sum(stats::resid(f)^2) / f$df.residual * sum(AT_W$coo^2 / 24)) }
  .pE <- function(gg) sum(AT_W$coo * tapply(.ch, gg, mean))
  set.seed(20261024); .perm <- replicate(20000, sample(AT_D$G)); .oF <- .pF(AT_D$G); .oE <- .pE(AT_D$G)
  .permF <- mean(apply(.perm, 2, function(gg) .pF(factor(gg, levels = AT_GN))) >= .oF); .permE <- mean(abs(apply(.perm, 2, function(gg) .pE(factor(gg, levels = AT_GN)))) >= abs(.oE))
  expect("C-1", permutation_p_contrast_F = 0.018, permutation_p_raw_estimate = 0.032, tol = 0.003, note = "20,000 permutations of the arm labels (seed 20261024): the coordination contrast's F statistic, and its raw estimate two-sided")
  check("C-1", permutation_p_contrast_F = .permF, permutation_p_raw_estimate = .permE)
  expect("C-1", strength_vs_endurance_p = 0.0485, strength_vs_endurance_bonferroni6 = 0.29, tol = 0.005); check("C-1", strength_vs_endurance_p = .pr$p.value[.pr$contrast == "endurance - strength"], strength_vs_endurance_bonferroni6 = min(1, 6 * .pr$p.value[.pr$contrast == "endurance - strength"]))
  .ex <- AT_D$G != "control"; .post <- tapply(AT_D$Concentration_KL_POST[.ex], droplevels(AT_D$G[.ex]), mean); .pre <- tapply(AT_D$Concentration_KL_PRE[.ex], droplevels(AT_D$G[.ex]), mean)
  expect("C-1", exercise_posttest_spread = 6.17, exercise_pretest_spread = 12.46, tol = 0.005, note = "largest minus smallest exercise-arm mean of the concentration score")
  check("C-1", exercise_posttest_spread = max(.post) - min(.post), exercise_pretest_spread = max(.pre) - min(.pre))
  .ec <- AT_CON(.er, AT_W$coo)
  expect("C-1", error_coordination_contrast_F = 8.79, tol = 0.005); check("C-1", error_coordination_contrast_F = .ec[["F"]])
  expect("C-1", error_coordination_contrast_p = 0.0038, tol = 0.00005, note = "the same coordination contrast on the error score passes .0125"); check("C-1", error_coordination_contrast_p = .ec[["p"]])
  .p12 <- unlist(lapply(AT_OUT, function(o) vapply(AT_W, function(w) AT_CON(o, w)[["p"]], numeric(1))))
  expect("C-1", contrasts_below_0125_of_12 = 6, holm12_significant = 6, bonferroni12_significant = 5, tol = 0, note = "the twelve contrasts (three outcomes x four): below .0125, and below .05 after Holm or Bonferroni over the twelve")
  check("C-1", contrasts_below_0125_of_12 = sum(.p12 < 0.0125), holm12_significant = sum(stats::p.adjust(.p12, "holm") < 0.05), bonferroni12_significant = sum(stats::p.adjust(.p12, "bonferroni") < 0.05))
  expect("C-1", error_strength_bonferroni12_p = 0.053, tol = 0.0005); check("C-1", error_strength_bonferroni12_p = min(1, 12 * AT_CON(.er, AT_W$str)[["p"]]))
  .chs <- AT_CHANGE(.sp); .cs <- summary(emmeans::contrast(emmeans::emmeans(stats::lm(ch ~ G, data.frame(ch = .chs, G = AT_D$G)), "G"), list(coo_end = c(-1, 0, 1, 0))), infer = TRUE)
  expect("C-1", speed_coordination_minus_endurance_ci_lower = -9.20, speed_coordination_minus_endurance_ci_upper = 13.45, tol = 0.005, note = "95% CI of the speed-change difference, coordination minus endurance")
  check("C-1", speed_coordination_minus_endurance_ci_lower = .cs$lower.CL, speed_coordination_minus_endurance_ci_upper = .cs$upper.CL)
  expect("C-1", article_speed_same_regardless = 1, tol = 0); check("C-1", article_speed_same_regardless = paper_has("indicating that the improvement in working speed was the same regardless of the type of intervention"))
  rm(.pF, .pE, .perm, .oF, .oE, .permF, .permE, .ex, .post, .pre, .ec, .p12, .chs, .cs)
  rm(.kl, .er, .sp, .ch, .fit, .ce, .anc, .np, .pr, .pe, .ps, .pm, .bw, .r, .wc, .det, .gp)
}, error = function(e) runtime_failure("C-1", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 2 [CORRIGENDUM MATERIAL]  (finding F2, Section 4)
##   The Abstract's and Discussion's statement that attention improved for all exercise groups
##   compared with control was tested only as the pooled contrast (+1, +1, +1, -3); in the
##   change-score model the strength arm's improvement over control has p = .0499 for the
##   concentration score (.15 after Bonferroni over three comparisons), .227 for the error score
##   and .164 for speed, while with the posttest adjusted for the pretest it has p = .0005 for
##   concentration, .0064 for speed and .012 for the error score.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-2"
tryCatch({
  ## C-2: The Abstract's and Discussion's statement that attention improved for all exercise groups compared with control was tested only as the pooled contrast (+1, +1, +1, -3); in the change-score model the strength arm's improvement over control has p = .0499 for the concentration score (.15 after Bonferroni over three comparisons), .227 for the error score and .164 for speed, while with the posttest adjusted for the pretest it has p = .0005 for concentration, .0064 for speed and .012 for the error score.
  .pk <- AT_PAIRS("Concentration_KL"); .pe <- AT_PAIRS("ErrorScore"); .ps <- AT_PAIRS("Speed_BZO")
  .sc <- function(p) p$p.value[p$contrast == "strength - control"]
  expect("C-2", strength_vs_control_concentration_p = 0.0499, strength_vs_control_error_p = 0.2273, strength_vs_control_speed_p = 0.1640, tol = 0.00005,
         note = "pairwise comparison of change, strength against control, pooled SD of the change-score model (df 92), unadjusted")
  check("C-2", strength_vs_control_concentration_p = .sc(.pk), strength_vs_control_error_p = .sc(.pe), strength_vs_control_speed_p = .sc(.ps))
  expect("C-2", strength_vs_control_concentration_bonferroni3 = 0.15, tol = 0.0005, note = "Bonferroni over the three arm-versus-control comparisons")
  check("C-2", strength_vs_control_concentration_bonferroni3 = min(1, 3 * .sc(.pk)))
  expect("C-2", endurance_vs_control_concentration_p = 0.0001, tol = 0.00005); check("C-2", endurance_vs_control_concentration_p = .pk$p.value[.pk$contrast == "endurance - control"])
  expect("C-2", coordination_vs_control_concentration_below_001 = 1, endurance_coordination_vs_control_all_outcomes_below_05 = 1, tol = 0)
  check("C-2", coordination_vs_control_concentration_below_001 = as.integer(.pk$p.value[.pk$contrast == "coordination - control"] < 0.001),
        endurance_coordination_vs_control_all_outcomes_below_05 = as.integer(all(vapply(list(.pk, .pe, .ps), function(p) all(p$p.value[p$contrast %in% c("endurance - control", "coordination - control")] < 0.05), logical(1)))))
  .ap <- function(o) { s <- summary(emmeans::contrast(emmeans::emmeans(AT_ANCOVA(o), "G"), "pairwise", adjust = "none")); s$p.value[s$contrast == "strength - control"] }
  expect("C-2", ancova_strength_vs_control_concentration_p = 0.0005, ancova_strength_vs_control_speed_p = 0.0064, ancova_strength_vs_control_error_p = 0.012, tol = 0.0005,
         note = "posttest adjusted for pretest, strength against control, unadjusted")
  check("C-2", ancova_strength_vs_control_concentration_p = .ap("Concentration_KL"), ancova_strength_vs_control_speed_p = .ap("Speed_BZO"), ancova_strength_vs_control_error_p = .ap("ErrorScore"))
  .lm <- grep("^\\s*/?LMATRIX", AT_SYNTAX, value = TRUE, ignore.case = TRUE); .lm <- gsub(".*Intervention\\s+", "", .lm)
  expect("C-2", syntax_distinct_contrast_vectors = 4, syntax_arm_versus_control_contrasts = 0, syntax_pooled_contrast = 1, tol = 0,
         note = "distinct LMATRIX weight vectors in the syntax; vectors that compare one exercise arm with control alone; the pooled 1 1 1 -3 vector")
  check("C-2", syntax_distinct_contrast_vectors = length(unique(trimws(.lm))), syntax_arm_versus_control_contrasts = sum(grepl("^(1 0 0 -1|0 1 0 -1|0 0 1 -1)$", trimws(unique(.lm)))),
        syntax_pooled_contrast = as.integer(any(trimws(.lm) == "1 1 1 -3")))
  expect("C-2", abstract_all_groups_sentence = 1, discussion_all_groups_sentence = 1, tol = 0, note = "article text")
  check("C-2", abstract_all_groups_sentence = paper_has("increased from before to after the exercise intervention for all exercise groups, as compared with the control group"),
        discussion_all_groups_sentence = paper_has("improved after the 8-week training intervention (two sessions/week) for all exercise groups, as compared with the control group"))
  rm(.pk, .pe, .ps, .sc, .ap, .lm)
}, error = function(e) runtime_failure("C-2", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 3 [CORRIGENDUM MATERIAL]  (finding F3, Section 5)
##   In three analysed rows the deposited concentration and error scores disagree with the row's
##   own omission and commission counts, and two of the stored concentration values are not
##   whole numbers, which a count cannot be; the published statistics use the deposited values,
##   and recomputing the three rows from their counts moves the coordination contrast on the
##   concentration score from p = .0177 to p = .0102, below the article's .0125 level, while no
##   other test changes its classification at .05 or .0125.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-3"
tryCatch({
  ## C-3: In three analysed rows the deposited concentration and error scores disagree with the row's own omission and commission counts, and two of the stored concentration values are not whole numbers, which a count cannot be; the published statistics use the deposited values, and recomputing the three rows from their counts moves the coordination contrast on the concentration score from p = .0177 to p = .0102, below the article's .0125 level, while no other test changes its classification at .05 or .0125.
  ## the d2-R identities: concentration performance = speed - omissions - commissions; error % = 100 (omissions + commissions) / speed
  .id <- function(ph, data = AT_D0) { kl <- data[[paste0("Concentration_KL_", ph)]]; b <- data[[paste0("Speed_BZO_", ph)]]; e <- data[[paste0("Ommisions_", ph)]] + data[[paste0("Commisions_", ph)]]
  f <- data[[paste0("ErrorScore_", ph)]]; ok <- !is.na(kl)
  list(n = sum(ok), kl_ok = sum(ok & abs(kl - (b - e)) < 1e-6), f_ok = sum(ok & abs(f - 100 * e / b) <= 0.005), nonint = sum(ok & abs(kl - round(kl)) > 1e-6),
       pair = sum(ok & abs(kl - (b - e)) >= 1e-6 & abs(f * b / 100 - (b - kl)) < 0.5)) }
  .pre <- .id("PRE"); .post <- .id("POST")
  expect("C-3", rows_checked = 216, concentration_identity_holds = 213, error_identity_holds = 213, tol = 0, note = "120 pretest and 96 posttest rows")
  check("C-3", rows_checked = .pre$n + .post$n, concentration_identity_holds = .pre$kl_ok + .post$kl_ok, error_identity_holds = .pre$f_ok + .post$f_ok)
  expect("C-3", inconsistent_rows = 3, non_integer_concentration_values = 2, stored_scores_agree_with_each_other = 3, tol = 0,
         note = "rows breaking the identities (pretest and posttest pooled; the occasion is not printed), non-integer concentration values, and rows whose stored concentration and error scores imply the same error count")
  check("C-3", inconsistent_rows = (.pre$n - .pre$kl_ok) + (.post$n - .post$kl_ok), non_integer_concentration_values = .pre$nonint + .post$nonint, stored_scores_agree_with_each_other = .pre$pair + .post$pair)
  ## recompute the three rows from their counts (a sensitivity analysis on a copy), and drop them
  .fix <- AT_D; .bad <- rep(FALSE, nrow(.fix))
  for (.ph in c("PRE", "POST")) { b <- .fix[[paste0("Speed_BZO_", .ph)]]; e <- .fix[[paste0("Ommisions_", .ph)]] + .fix[[paste0("Commisions_", .ph)]]
  w <- abs(.fix[[paste0("Concentration_KL_", .ph)]] - (b - e)) >= 1e-6; .bad <- .bad | w
  .fix[[paste0("Concentration_KL_", .ph)]][w] <- (b - e)[w]; .fix[[paste0("ErrorScore_", .ph)]][w] <- (100 * e / b)[w] }
  .drop <- AT_D[!.bad, ]
  expect("C-3", analysed_rows_affected = 3, tol = 0); check("C-3", analysed_rows_affected = sum(.bad))
  expect("C-3", coordination_p_deposited = 0.0177, coordination_p_recomputed = 0.0102, coordination_p_rows_dropped = 0.0115, tol = 0.00005, note = "coordination contrast on the concentration performance score")
  check("C-3", coordination_p_deposited = AT_CON("Concentration_KL", AT_W$coo)[["p"]], coordination_p_recomputed = AT_CON("Concentration_KL", AT_W$coo, .fix)[["p"]],
        coordination_p_rows_dropped = AT_CON("Concentration_KL", AT_W$coo, .drop)[["p"]])
  .cls <- function(data) { p <- c(); for (o in AT_OUT) { for (k in names(AT_W)) p <- c(p, AT_CON(o, AT_W[[k]], data)[["p"]]); p <- c(p, AT_ANOVA(o, data)["G:P", "Pr(>F)"]) }
  findInterval(p, c(0.0125, 0.05)) }
  .c0 <- .cls(AT_D); .c1 <- .cls(.fix); .c2 <- .cls(.drop)
  expect("C-3", tests_changing_class_recomputed = 1, tests_changing_class_dropped = 1, tol = 0, note = "of the 12 contrasts and 3 interactions, tests whose p changes side of .0125 or .05")
  check("C-3", tests_changing_class_recomputed = sum(.c0 != .c1), tests_changing_class_dropped = sum(.c0 != .c2))
  expect("C-3", error_mode_contrasts_below_0125_all_versions = 1, tol = 0, note = "error-score strength and coordination contrasts in the deposited, recomputed and dropped versions")
  check("C-3", error_mode_contrasts_below_0125_all_versions = as.integer(all(vapply(list(AT_D, .fix, .drop), function(dd) all(c(AT_CON("ErrorScore", AT_W$str, dd)[["p"]], AT_CON("ErrorScore", AT_W$coo, dd)[["p"]]) < 0.0125), logical(1)))))
  .nd <- function(x) vapply(x, function(v) { for (d in 0:10) if (abs(round(v, d) - v) < 1e-9) return(d); 11 }, numeric(1))
  expect("C-3", dropout_pretest_error_at_most_2_decimals = 24, completer_pretest_error_at_most_2_decimals = 2, tol = 0, note = "stored precision of the pretest error score")
  check("C-3", dropout_pretest_error_at_most_2_decimals = sum(.nd(AT_D0$ErrorScore_PRE[!AT_DONE]) <= 2), completer_pretest_error_at_most_2_decimals = sum(.nd(AT_D0$ErrorScore_PRE[AT_DONE]) <= 2))
  rm(.id, .pre, .post, .fix, .bad, .ph, .drop, .cls, .c0, .c1, .c2, .nd)
}, error = function(e) runtime_failure("C-3", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 4 [CORRIGENDUM MATERIAL]  (finding F4, Section 6)
##   The Abstract describes the 96 analysed students as randomised, but 120 were randomised (34,
##   31, 28 and 27 per arm) and 24 were lost (10, 7, 4 and 3; 29%, 23%, 14% and 11%) while
##   recruitment continued until each arm had 24 completers; every test is a completers-only
##   (per-protocol) analysis, no attrition or sensitivity analysis is reported, and the
##   dropouts' pretest error scores were lower than the completers' (8.59 against 11.30, Welch p
##   = .022, uncorrected).
## Check written by the audit (no author code covers this claim):

.V$current <- "C-4"
tryCatch({
  ## C-4: The Abstract describes the 96 analysed students as randomised, but 120 were randomised (34, 31, 28 and 27 per arm) and 24 were lost (10, 7, 4 and 3; 29%, 23%, 14% and 11%) while recruitment continued until each arm had 24 completers; every test is a completers-only (per-protocol) analysis, no attrition or sensitivity analysis is reported, and the dropouts' pretest error scores were lower than the completers' (8.59 against 11.30, Welch p = .022, uncorrected).
  .alloc <- table(AT_D0$G); .lost <- table(AT_D0$G[!AT_DONE]); .done <- table(AT_D$G)
  expect("C-4", randomised = 120, analysed = 96, lost = 24, tol = 0); check("C-4", randomised = nrow(AT_D0), analysed = nrow(AT_D), lost = sum(!AT_DONE))
  expect("C-4", allocated_endurance = 34, allocated_strength = 31, allocated_coordination = 28, allocated_control = 27, tol = 0)
  check("C-4", allocated_endurance = .alloc[["endurance"]], allocated_strength = .alloc[["strength"]], allocated_coordination = .alloc[["coordination"]], allocated_control = .alloc[["control"]])
  expect("C-4", lost_endurance = 10, lost_strength = 7, lost_coordination = 4, lost_control = 3, completers_per_arm_all_24 = 1, tol = 0)
  check("C-4", lost_endurance = .lost[["endurance"]], lost_strength = .lost[["strength"]], lost_coordination = .lost[["coordination"]], lost_control = .lost[["control"]], completers_per_arm_all_24 = as.integer(all(.done == 24)))
  .pct <- 100 * .lost / .alloc
  expect("C-4", lost_pct_endurance = 29, lost_pct_strength = 23, lost_pct_coordination = 14, lost_pct_control = 11, tol = 0.5)
  check("C-4", lost_pct_endurance = .pct[["endurance"]], lost_pct_strength = .pct[["strength"]], lost_pct_coordination = .pct[["coordination"]], lost_pct_control = .pct[["control"]])
  .cs <- suppressWarnings(stats::chisq.test(table(AT_D0$G, AT_DONE)))
  expect("C-4", attrition_chisq = 3.92, tol = 0.005); check("C-4", attrition_chisq = unname(.cs$statistic))
  expect("C-4", attrition_p = 0.27, tol = 0.005); check("C-4", attrition_p = .cs$p.value)
  .w <- function(o) stats::t.test(AT_D0[[paste0(o, "_PRE")]][!AT_DONE], AT_D0[[paste0(o, "_PRE")]][AT_DONE])
  .we <- .w("ErrorScore")
  expect("C-4", dropout_pretest_error_mean = 8.59, completer_pretest_error_mean = 11.30, tol = 0.005, note = "group means only")
  check("C-4", dropout_pretest_error_mean = unname(.we$estimate[1]), completer_pretest_error_mean = unname(.we$estimate[2]))
  expect("C-4", dropout_pretest_error_welch_p = 0.022, dropout_pretest_concentration_welch_p = 0.25, dropout_pretest_speed_welch_p = 0.77, tol = 0.005)
  check("C-4", dropout_pretest_error_welch_p = .we$p.value, dropout_pretest_concentration_welch_p = .w("Concentration_KL")$p.value, dropout_pretest_speed_welch_p = .w("Speed_BZO")$p.value)
  .exd <- !AT_DONE & AT_D0$G != "control"
  expect("C-4", exercise_arm_dropouts = 21, exercise_arm_dropouts_with_exertion = 0, tol = 0, note = "perceived exertion recorded for dropouts of the three exercise arms")
  check("C-4", exercise_arm_dropouts = sum(.exd), exercise_arm_dropouts_with_exertion = sum(.exd & !is.na(AT_D0$BORG)))
  expect("C-4", non_completers_hold_highest_ids = 1, tol = 0, note = "whether every non-completer's ID number exceeds every completer's (flag only; no ID is printed)")
  check("C-4", non_completers_hold_highest_ids = as.integer(min(AT_D0$ID[!AT_DONE]) > max(AT_D0$ID[AT_DONE])))
  expect("C-4", syntax_filter_completers = 1, abstract_96_randomised = 1, methods_24_lost = 1, methods_kept_recruiting = 1, tol = 0, note = "the syntax's filter and the article's wording")
  check("C-4", syntax_filter_completers = code_has("Concentration_KL_POST > 0"), abstract_96_randomised = paper_has("Adolescent students (N =96) aged 15-18 years were randomised"),
        methods_24_lost = paper_has("During the study, 24 persons (20% of all recruited persons)"), methods_kept_recruiting = paper_has("We kept recruiting until we reached 24 persons per group"))
  .bo <- AT_D0; for (.o in AT_OUT) { .m <- is.na(.bo[[paste0(.o, "_POST")]]) | !AT_DONE; .bo[[paste0(.o, "_POST")]][.m] <- .bo[[paste0(.o, "_PRE")]][.m] }
  expect("C-4", bocf_pooled_concentration_p = 0.0055, bocf_pooled_error_p = 0.0109, bocf_pooled_speed_p = 0.0438, bocf_coordination_concentration_p = 0.0061, tol = 0.00005,
         note = "each dropout's pretest carried forward as no change, all 120 randomised students, the article's contrasts on change")
  check("C-4", bocf_pooled_concentration_p = AT_CON("Concentration_KL", AT_W$ctrl, .bo)[["p"]], bocf_pooled_error_p = AT_CON("ErrorScore", AT_W$ctrl, .bo)[["p"]],
        bocf_pooled_speed_p = AT_CON("Speed_BZO", AT_W$ctrl, .bo)[["p"]], bocf_coordination_concentration_p = AT_CON("Concentration_KL", AT_W$coo, .bo)[["p"]])
  expect("C-4", participants_paragraph_total_sample_96 = 1, tol = 0); check("C-4", participants_paragraph_total_sample_96 = paper_has("The total sample thus included 96 students"))
  rm(.alloc, .lost, .done, .pct, .cs, .w, .we, .exd, .bo, .o, .m)
}, error = function(e) runtime_failure("C-4", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 5 [CORRIGENDUM MATERIAL]  (finding F5, Section 7)
##   The four analysed arms have an identical distribution of weekly PE hours (Table 1: 3.25
##   (1.33) in every arm, F = 0.00, p = 1.00) because every age-by-PE-hours-by-arm cell holds
##   exactly three students; permuting the completers' arm labels within the stated
##   age-by-gender strata gives such balance with p about .0005, a probability conditional on
##   completers being exchangeable within strata and on the observed arm margins that does not
##   model completion or the stopping and replacement of recruits, while the Table 1 note's
##   three women and three men per age in every arm holds in 28 of 32 cells; the allocation and
##   replacement procedure is not fully described, and the Table 1 PE test is not an independent
##   check of randomisation.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-5"
tryCatch({
  ## C-5: The four analysed arms have an identical distribution of weekly PE hours (Table 1: 3.25 (1.33) in every arm, F = 0.00, p = 1.00) because every age-by-PE-hours-by-arm cell holds exactly three students; permuting the completers' arm labels within the stated age-by-gender strata gives such balance with p about .0005, a probability conditional on completers being exchangeable within strata and on the observed arm margins that does not model completion or the stopping and replacement of recruits, while the Table 1 note's three women and three men per age in every arm holds in 28 of 32 cells; the allocation and replacement procedure is not fully described, and the Table 1 PE test is not an independent check of randomisation.
  .pt <- table(AT_D$G, AT_D$PE_week); .same <- as.integer(all(apply(.pt, 2, function(x) length(unique(x)) == 1)))
  expect("C-5", pe_distribution_identical_in_every_arm = 1, distinct_pe_values = 3, tol = 0, note = "weekly PE hours of the analysed arms: value counts per arm")
  check("C-5", pe_distribution_identical_in_every_arm = .same, distinct_pe_values = ncol(.pt))
  .ct <- sort(as.vector(.pt[1, ]), decreasing = TRUE)
  expect("C-5", pe_count_largest = 12, pe_count_second = 6, pe_count_third = 6, tol = 0, note = "students per PE value in each arm, sorted (the values themselves are not printed)")
  check("C-5", pe_count_largest = .ct[1], pe_count_second = .ct[2], pe_count_third = .ct[3])
  .pm <- tapply(AT_D$PE_week, AT_D$G, mean); .psd <- tapply(AT_D$PE_week, AT_D$G, stats::sd)
  expect("C-5", pe_mean_largest_gap_from_3_25 = 0, pe_sd_largest_gap_from_1_33 = 0, tol = 0.005, note = "Table 1 prints 3.25 (1.33) in every arm: largest distance of any arm's mean and SD from those values")
  check("C-5", pe_mean_largest_gap_from_3_25 = max(abs(.pm - 3.25)), pe_sd_largest_gap_from_1_33 = max(abs(.psd - 1.33)))
  .cell <- table(AT_D$Age, AT_D$PE_week, AT_D$G); .nz <- .cell[.cell > 0]
  expect("C-5", age_pe_arm_cells_all_three = 1, tol = 0, note = "every non-empty age x PE hours x arm cell of the completers holds three students")
  check("C-5", age_pe_arm_cells_all_three = as.integer(all(.nz == 3)))
  expect("C-5", pe_values_per_age = 2, pe_not_function_of_age_gender = 1, tol = 0)
  check("C-5", pe_values_per_age = max(tapply(AT_D$PE_week, AT_D$Age, function(x) length(unique(x)))), pe_not_function_of_age_gender = as.integer(any(tapply(AT_D$PE_week, paste(AT_D$Age, AT_D$Gender), function(x) length(unique(x))) > 1)))
  ## permutation: arm labels reshuffled within the age-by-gender strata (each arm keeps its stratum counts)
  set.seed(20261024); .st <- split(seq_len(nrow(AT_D)), paste(AT_D$Age, AT_D$Gender)); .g <- as.integer(AT_D$G); .pe <- AT_D$PE_week
  .ident <- function(g) { t <- table(factor(g, 1:4), factor(.pe)); all(t[2, ] == t[1, ]) && all(t[3, ] == t[1, ]) && all(t[4, ] == t[1, ]) }
  .B <- 100000L; .hits <- 0L; .gg <- .g
  for (.b in seq_len(.B)) { for (s in .st) .gg[s] <- .g[s][sample.int(length(s))]; if (.ident(.gg)) .hits <- .hits + 1L }
  expect("C-5", permutation_p_identical_pe = 0.0005, tol = 0.0003, note = "Monte Carlo share of 100,000 reshuffles of the completers' arm labels within the age-by-gender strata giving identical PE distributions in all four arms (seed 20261024): conditional on completer exchangeability within strata and the observed arm margins; completion and the replacement of recruits are not modelled")
  check("C-5", permutation_p_identical_pe = (.hits + 1) / (.B + 1))
  .c3 <- table(AT_D$G, AT_D$Gender, AT_D$Age)
  expect("C-5", arm_gender_age_cells = 32, cells_holding_three = 28, other_cells_two_or_four = 4, tol = 0)
  check("C-5", arm_gender_age_cells = length(.c3), cells_holding_three = sum(.c3 == 3), other_cells_two_or_four = sum(.c3 %in% c(2, 4)))
  expect("C-5", women_and_men_12_per_arm = 1, six_per_age_per_arm = 1, twentyfour_per_age = 1, tol = 0, note = "margins of the analysed sample")
  check("C-5", women_and_men_12_per_arm = as.integer(all(table(AT_D$G, AT_D$Gender) == 12)), six_per_age_per_arm = as.integer(all(table(AT_D$G, AT_D$Age) == 6)), twentyfour_per_age = as.integer(all(table(AT_D$Age) == 24)))
  .pt0 <- table(AT_D0$G, AT_D0$PE_week)
  expect("C-5", randomised_120_pe_identical = 0, tol = 0, note = "the same check on all 120 randomised students (proportions per arm)")
  check("C-5", randomised_120_pe_identical = as.integer(all(apply(prop.table(.pt0, 1), 2, function(x) diff(range(x)) < 1e-9))))
  .aov <- stats::anova(stats::lm(PE_week ~ G, AT_D))
  expect("C-5", table1_pe_F = 0, tol = 0.005); check("C-5", table1_pe_F = .aov[["F value"]][1])
  expect("C-5", table1_note_three_women_three_men = 1, methods_excel_randomisation = 1, tol = 0, note = "article text")
  check("C-5", table1_note_three_women_three_men = paper_has("each study group includes three women and three men of each age category"),
        methods_excel_randomisation = paper_has("Within each stratum, participants were then assigned to one of the four study groups by using randomisation function in Microsoft Excel"))
  rm(.pt, .same, .ct, .pm, .psd, .cell, .nz, .st, .g, .pe, .ident, .B, .hits, .gg, .b, .c3, .pt0, .aov)
}, error = function(e) runtime_failure("C-5", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 6 [COMMENT MATERIAL]  (finding F6, Section 8)
##   In the copy audited the data file holds, for all 120 recruited students aged 15 to 18
##   (including the 24 dropouts the article does not analyse), a pseudonymous ID number with
##   exact age, gender, arm, PE hours, leisure sport and three single-subject school grades with
##   their mean; age and gender alone leave no student unique in the file, but 112 of 120 are
##   unique on age, gender, PE hours, the leisure-sport answer and the three subject grades, and
##   the article makes no anonymisation claim and does not say whether consent covered public
##   release. This is within-file uniqueness, not a demonstrated re-identification.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-6"
tryCatch({
  ## C-6: In the copy audited the data file holds, for all 120 recruited students aged 15 to 18 (including the 24 dropouts the article does not analyse), a pseudonymous ID number with exact age, gender, arm, PE hours, leisure sport and three single-subject school grades with their mean; age and gender alone leave no student unique in the file, but 112 of 120 are unique on age, gender, PE hours, the leisure-sport answer and the three subject grades, and the article makes no anonymisation claim and does not say whether consent covered public release. This is within-file uniqueness, not a demonstrated re-identification.
  ## within-file uniqueness only (counts of rows alone in their combination class); no value, class or row is printed
  .u <- function(cols, dd = AT_D0) { k <- do.call(paste, c(lapply(cols, function(c) dd[[c]]), sep = "|")); sum(ave(rep(1, nrow(dd)), k, FUN = length) == 1) }
  .mn <- function(cols, dd = AT_D0) { k <- do.call(paste, c(lapply(cols, function(c) dd[[c]]), sep = "|")); min(table(k)) }
  expect("C-6", rows = 120, rows_of_dropouts = 24, columns = 22, string_columns = 0, tol = 0, note = "the deposited file: all recruited students, including the 24 not analysed")
  check("C-6", rows = nrow(AT_D0), rows_of_dropouts = sum(!AT_DONE), columns = ncol(AT_RAW), string_columns = sum(vapply(AT_RAW, is.character, logical(1))))
  expect("C-6", unique_age_gender = 0, smallest_age_gender_class = 12, tol = 0); check("C-6", unique_age_gender = .u(c("Age", "Gender")), smallest_age_gender_class = .mn(c("Age", "Gender")))
  .q7 <- c("Age", "Gender", "PE_week", "Sport_leisure", "German", "Math", "English"); .q5 <- c("Age", "Gender", "PE_week", "Sport_leisure", "SchoolGrade")
  expect("C-6", unique_seven_columns = 112, unique_with_mean_grade = 83, tol = 0, note = "age, gender, PE hours, leisure-sport answer and the three subject grades; the same with the mean grade instead")
  check("C-6", unique_seven_columns = .u(.q7), unique_with_mean_grade = .u(.q5))
  .c <- AT_D0; .c$GW <- round(.c$SchoolGrade); .lo <- min(.c$PE_week); .c$PB <- as.integer(.c$PE_week > .lo)
  expect("C-6", unique_whole_grade_mean = 35, unique_whole_grade_and_pe_band = 33, tol = 0, note = "mean grade rounded to whole grades; PE hours also in two bands (the lowest value against the higher two)")
  check("C-6", unique_whole_grade_mean = .u(c("Age", "Gender", "PE_week", "Sport_leisure", "GW"), .c), unique_whole_grade_and_pe_band = .u(c("Age", "Gender", "PB", "Sport_leisure", "GW"), .c))
  .nm <- tolower(names(AT_RAW))
  expect("C-6", identifier_like_column_names = 0, date_columns = 0, tol = 0, note = "column names suggesting a name, birth date, contact, address, school or class; date-typed columns")
  check("C-6", identifier_like_column_names = sum(grepl("name|birth|dob|mail|phone|address|school_?name|class", .nm)), date_columns = sum(vapply(AT_RAW, function(x) inherits(x, c("Date", "POSIXt")), logical(1))))
  expect("C-6", syntax_uses_id = 0, syntax_uses_subject_grades = 0, tol = 0, note = "the analysis syntax references neither the ID nor the three subject grades")
  check("C-6", syntax_uses_id = as.integer(any(grepl("\\bID\\b", AT_SYNTAX))), syntax_uses_subject_grades = as.integer(any(grepl("\\b(German|Math|English)\\b", AT_SYNTAX))))
  expect("C-6", article_anonymisation_terms = 0, article_consent_sentence = 1, article_age_range = 1, tol = 0, note = "article text: no anonymisation claim; consent of participants and parents; ages 15 to 18")
  check("C-6", article_anonymisation_terms = as.integer(grepl("anonym|de-identif|pseudonym", PAPER_TXT, ignore.case = TRUE)),
        article_consent_sentence = paper_has("Participants and their parents signed an informed consent before taking part in the study"), article_age_range = paper_has("aged 15-18 years"))
  .used <- vapply(names(AT_RAW), function(v) any(grepl(paste0("\\b", v, "\\b"), AT_SYNTAX)), logical(1))
  expect("C-6", syntax_referenced_variables = 11, syntax_uses_age_gender_leisure_flag = 0, tol = 0, note = "data-file variables named in the syntax; and whether Age, Gender or Sport_leisure is among them")
  check("C-6", syntax_referenced_variables = sum(.used), syntax_uses_age_gender_leisure_flag = sum(.used[c("Age", "Gender", "Sport_leisure")]))
  .g1 <- stats::anova(stats::lm(SchoolGrade ~ G, AT_D)); .g2 <- stats::anova(stats::lm(round(2 * SchoolGrade) / 2 ~ G, AT_D))
  expect("C-6", table1_grade_F = 1.57, table1_grade_F_half_grades = 1.97, tol = 0.005, note = "Table 1's grade test with the exact mean grade and with the mean grade rounded to half grades")
  check("C-6", table1_grade_F = .g1[["F value"]][1], table1_grade_F_half_grades = .g2[["F value"]][1])
  rm(.u, .mn, .q7, .q5, .c, .lo, .nm, .used, .g1, .g2)
}, error = function(e) runtime_failure("C-6", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 7 [CORRIGENDUM MATERIAL]  (finding F7, Section 9)
##   Table 2 prints the control arm's pretest means as 157.40 for the concentration score and
##   174.52 for speed, but the 24 deposited control completers give 157.00 and 174.42, and no
##   mean of 24 integer scores can equal either printed value; the printed SDs and the test
##   statistics built on these cells reproduce.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-7"
tryCatch({
  ## C-7: Table 2 prints the control arm's pretest means as 157.40 for the concentration score and 174.52 for speed, but the 24 deposited control completers give 157.00 and 174.42, and no mean of 24 integer scores can equal either printed value; the printed SDs and the test statistics built on these cells reproduce.
  .ctl <- AT_D[AT_D$G == "control", ]
  expect("C-7", control_completers = 24, tol = 0); check("C-7", control_completers = nrow(.ctl))
  expect("C-7", control_pretest_concentration_mean = 157.00, control_pretest_speed_mean = 174.42, control_pretest_concentration_sd = 23.26, control_pretest_speed_sd = 20.81, tol = 0.005,
         note = "the 24 control completers (Table 2 prints 157.40 (23.26) and 174.52 (20.81))")
  check("C-7", control_pretest_concentration_mean = mean(.ctl$Concentration_KL_PRE), control_pretest_speed_mean = mean(.ctl$Speed_BZO_PRE),
        control_pretest_concentration_sd = stats::sd(.ctl$Concentration_KL_PRE), control_pretest_speed_sd = stats::sd(.ctl$Speed_BZO_PRE))
  expect("C-7", table2_prints_157_40 = 1, table2_prints_174_52 = 1, tol = 0, note = "the article's Table 2 pretest rows")
  check("C-7", table2_prints_157_40 = paper_has("Pre 149.79 (20.69) 162.25 (23.52) 149.92 (22.49) 157.40 (23.26)"), table2_prints_174_52 = paper_has("Pre 169.54 (23.05) 179.33 (22.64) 173.71 (21.31) 174.52 (20.81)"))
  ## GRIM-style test: can a mean of 24 whole-number scores round to the printed value?
  .grim <- function(m, n = 24) { lo <- ceiling((m - 0.005) * n - 1e-9); hi <- floor((m + 0.005) * n - 1e-9); as.integer(hi >= lo) }
  expect("C-7", integer_scores_all = 1, printed_157_40_possible = 0, printed_174_52_possible = 0, tol = 0, note = "the pretest scores are whole numbers; whether any sum of 24 whole numbers gives a mean that rounds to the printed value")
  check("C-7", integer_scores_all = as.integer(all(abs(c(.ctl$Concentration_KL_PRE, .ctl$Speed_BZO_PRE) - round(c(.ctl$Concentration_KL_PRE, .ctl$Speed_BZO_PRE))) < 1e-9)),
        printed_157_40_possible = .grim(157.40), printed_174_52_possible = .grim(174.52))
  .all <- AT_D0[AT_D0$G == "control", ]
  expect("C-7", all_controls = 27, all_controls_concentration_mean = 157.67, all_controls_speed_mean = 176.00, tol = 0.005, note = "all 27 allocated controls (a tested alternative source)")
  check("C-7", all_controls = nrow(.all), all_controls_concentration_mean = mean(.all$Concentration_KL_PRE), all_controls_speed_mean = mean(.all$Speed_BZO_PRE))
  .a <- AT_ANOVA("Concentration_KL"); .b <- AT_ANOVA("Speed_BZO")
  expect("C-7", interaction_F_concentration = 10.07, interaction_F_speed = 3.96, tol = 0.005, note = "the printed test statistics reproduce from the deposited values")
  check("C-7", interaction_F_concentration = .a["G:P", "F"], interaction_F_speed = .b["G:P", "F"])
  rm(.ctl, .grim, .all, .a, .b)
}, error = function(e) runtime_failure("C-7", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 8 [CORRIGENDUM MATERIAL]  (finding F8, Section 9)
##   The syntax requests homogeneity-of-variance tests in five commands and Bonferroni post hoc
##   output for the group effect in two GLM commands that the article does not report; the
##   posttest variances differ across arms (Levene p = .012 for the concentration score and
##   .0002 for the error score; largest-to-smallest SD ratio about 2), and with separate
##   variances the coordination contrast has p = .039 for the concentration score and .018 for
##   the error score.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-8"
tryCatch({
  ## C-8: The syntax requests homogeneity-of-variance tests in five commands and Bonferroni post hoc output for the group effect in two GLM commands that the article does not report; the posttest variances differ across arms (Levene p = .012 for the concentration score and .0002 for the error score; largest-to-smallest SD ratio about 2), and with separate variances the coordination contrast has p = .039 for the concentration score and .018 for the error score.
  expect("C-8", syntax_homogeneity_requests = 5, syntax_glm_bonferroni_posthoc_requests = 2, tol = 0, note = "syntax commands requesting HOMOGENEITY output, and GLM commands requesting Bonferroni post hoc tests for the group effect")
  check("C-8", syntax_homogeneity_requests = sum(grepl("HOMOGENEITY", AT_SYNTAX)), syntax_glm_bonferroni_posthoc_requests = sum(grepl("POSTHOC *= *Intervention\\(BONFERRONI\\)", AT_SYNTAX)))
  expect("C-8", article_levene_mentions = 0, tol = 0, note = "the article reports no variance-homogeneity test")
  check("C-8", article_levene_mentions = as.integer(grepl("Levene|homogeneity of variance|homoscedastic", PAPER_TXT, ignore.case = TRUE)))
  .lv <- function(y) car::leveneTest(y, AT_D$G, center = mean)[["Pr(>F)"]][1]
  expect("C-8", levene_post_concentration_p = 0.012, levene_post_error_p = 0.0002, tol = 0.0005, note = "mean-centred Levene tests (as SPSS) on the posttest")
  check("C-8", levene_post_concentration_p = .lv(AT_D$Concentration_KL_POST), levene_post_error_p = .lv(AT_D$ErrorScore_POST))
  expect("C-8", levene_change_concentration_p = 0.024, levene_change_error_p = 0.029, levene_change_speed_p = 0.047, tol = 0.0005, note = "the same on the change scores")
  check("C-8", levene_change_concentration_p = .lv(AT_CHANGE("Concentration_KL")), levene_change_error_p = .lv(AT_CHANGE("ErrorScore")), levene_change_speed_p = .lv(AT_CHANGE("Speed_BZO")))
  .sd <- tapply(AT_D$Concentration_KL_POST, AT_D$G, stats::sd)
  expect("C-8", post_concentration_sd_ratio = 2.06, tol = 0.005, note = "largest over smallest arm SD of the posttest concentration score")
  check("C-8", post_concentration_sd_ratio = max(.sd) / min(.sd))
  .wc <- function(o, w) { ch <- AT_CHANGE(o); m <- tapply(ch, AT_D$G, mean); v <- tapply(ch, AT_D$G, stats::var); n <- tapply(ch, AT_D$G, length)
  q <- w^2 * v / n; t <- sum(w * m) / sqrt(sum(q)); df <- sum(q)^2 / sum(q^2 / (n - 1)); 2 * stats::pt(-abs(t), df) }
  expect("C-8", separate_variance_coordination_concentration_p = 0.039, separate_variance_coordination_error_p = 0.018, tol = 0.0005, note = "coordination contrast with separate variances (Welch-Satterthwaite df)")
  check("C-8", separate_variance_coordination_concentration_p = .wc("Concentration_KL", AT_W$coo), separate_variance_coordination_error_p = .wc("ErrorScore", AT_W$coo))
  rm(.lv, .sd, .wc)
}, error = function(e) runtime_failure("C-8", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 9 [CORRIGENDUM MATERIAL]  (finding F9, Section 9)
##   Table 1's leisure-hours row is computed on the 77 students active outside school (19, 18,
##   22 and 18 per arm; df 3 and 73) although its header gives n = 24 per arm; the text prints
##   'p < .018' for a Bonferroni p of .0183; and four printed statistics differ by one unit in
##   the last digit, consistent with rounding SPSS's three-decimal output twice.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-9"
tryCatch({
  ## C-9: Table 1's leisure-hours row is computed on the 77 students active outside school (19, 18, 22 and 18 per arm; df 3 and 73) although its header gives n = 24 per arm; the text prints 'p < .018' for a Bonferroni p of .0183; and four printed statistics differ by one unit in the last digit, consistent with rounding SPSS's three-decimal output twice.
  .lh <- !is.na(AT_D$Sport_leisure_week); .n <- table(AT_D$G[.lh])
  expect("C-9", leisure_hours_n_endurance = 19, leisure_hours_n_strength = 18, leisure_hours_n_coordination = 22, leisure_hours_n_control = 18, leisure_hours_n_total = 77, tol = 0,
         note = "students with leisure-sport hours (those active outside school), per analysed arm")
  check("C-9", leisure_hours_n_endurance = .n[["endurance"]], leisure_hours_n_strength = .n[["strength"]], leisure_hours_n_coordination = .n[["coordination"]], leisure_hours_n_control = .n[["control"]], leisure_hours_n_total = sum(.n))
  .a1 <- stats::anova(stats::lm(Sport_leisure_week ~ G, AT_D[.lh, ]))
  expect("C-9", leisure_F = 1.29, leisure_df2 = 73, tol = 0.005, note = "Table 1 prints F = 1.29, p = .283 under a header of n = 24 per arm")
  check("C-9", leisure_F = .a1[["F value"]][1], leisure_df2 = .a1[["Df"]][2])
  .z <- AT_D; .z$LZ <- ifelse(is.na(.z$Sport_leisure_week), 0, .z$Sport_leisure_week); .a2 <- stats::anova(stats::lm(LZ ~ G, .z))
  expect("C-9", leisure_zero_coded_F = 0.62, leisure_zero_coded_p = 0.60, tol = 0.005, note = "inactive students coded as zero hours")
  check("C-9", leisure_zero_coded_F = .a2[["F value"]][1], leisure_zero_coded_p = .a2[["Pr(>F)"]][1])
  expect("C-9", table1_header_n24 = 1, table1_leisure_row = 1, tol = 0)
  check("C-9", table1_header_n24 = paper_has("Table 1. Characteristics of the study groups. Endurance (n=24) Strength (n=24) Coordination (n =24) Control (n=24)"),
        table1_leisure_row = paper_has("Hours of sport in leisure time per week, M (SD) 6.05 (1.81) 6.22 (2.29) 6.27 (2.86) 7.44 (2.43) 1.29 .283"))
  .bd <- AT_D[AT_D$G != "control", ]; .bd$G <- droplevels(.bd$G); .bt <- stats::pairwise.t.test(.bd$BORG, .bd$G, p.adjust.method = "bonferroni", pool.sd = TRUE)$p.value
  expect("C-9", exertion_endurance_strength_bonferroni_p = 0.0183, tol = 0.00005, note = "Bonferroni post hoc test of perceived exertion, endurance against strength"); check("C-9", exertion_endurance_strength_bonferroni_p = .bt["strength", "endurance"])
  expect("C-9", text_prints_p_below_018 = 1, tol = 0); check("C-9", text_prints_p_below_018 = paper_has("than during the strength intervention (p<.018)"))
  .ce <- AT_CON("Concentration_KL", AT_W$end); .ec <- AT_CON("ErrorScore", AT_W$coo); .es <- AT_CON("ErrorScore", AT_W$str); .ga <- AT_ANOVA("ErrorScore")
  expect("C-9", endurance_contrast_F = 0.2748, error_coordination_contrast_F = 8.795, error_strength_contrast_pes = 0.0849, error_group_pes = 0.0349, tol = 0.0005,
         note = "the four statistics printed one unit higher (0.28, 8.80, .09, 0.04): values agree with the print after rounding to three decimals and then to two")
  check("C-9", endurance_contrast_F = .ce[["F"]], error_coordination_contrast_F = .ec[["F"]], error_strength_contrast_pes = .es[["pes"]], error_group_pes = .ga["G", "pes"])
  .dr <- function(x) round(round(x, 3) + 1e-9, 2)
  expect("C-9", double_rounding_reproduces_all_four = 1, single_rounding_reproduces = 0, tol = 0)
  check("C-9", double_rounding_reproduces_all_four = as.integer(all(abs(c(.dr(.ce[["F"]]), .dr(.ec[["F"]]), .dr(.es[["pes"]]), .dr(.ga["G", "pes"])) - c(0.28, 8.80, 0.09, 0.04)) < 1e-9)),
        single_rounding_reproduces = sum(abs(round(c(.ce[["F"]], .ec[["F"]], .es[["pes"]], .ga["G", "pes"]), 2) - c(0.28, 8.80, 0.09, 0.04)) < 1e-9))
  expect("C-9", article_prints_0_28 = 1, article_prints_8_80 = 1, tol = 0)
  check("C-9", article_prints_0_28 = paper_has("non-significant contrast for the endurance group, F(1, 92)=0.28"), article_prints_8_80 = paper_has("F(1, 92)=8.80, p= .004"))
  .act <- table(AT_D$G, AT_D$Sport_leisure); .sh <- 100 * prop.table(.act, 1)[, "1"]
  expect("C-9", share_active_endurance = 79, share_active_strength = 75, share_active_coordination = 92, share_active_control = 75, tol = 0.5)
  check("C-9", share_active_endurance = .sh[["endurance"]], share_active_strength = .sh[["strength"]], share_active_coordination = .sh[["coordination"]], share_active_control = .sh[["control"]])
  expect("C-9", share_active_chisq_p = 0.42, tol = 0.005); check("C-9", share_active_chisq_p = suppressWarnings(stats::chisq.test(.act))$p.value)
  rm(.act, .sh)
  rm(.lh, .n, .a1, .z, .a2, .bd, .bt, .ce, .ec, .es, .ga, .dr)
}, error = function(e) runtime_failure("C-9", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 10 [COMMENT MATERIAL]  (finding F10, Section 9)
##   In the copy audited the deposit has no README, codebook or licence; the syntax does not
##   open the data file or encode the .0125 level; no SPSS output, item-level d2 data,
##   attendance record or copy of the preregistration is deposited; and the data file's header
##   names SPSS 27.0 while the article names SPSS 26.0.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-10"
tryCatch({
  ## C-10: In the copy audited the deposit has no README, codebook or licence; the syntax does not open the data file or encode the .0125 level; no SPSS output, item-level d2 data, attendance record or copy of the preregistration is deposited; and the data file's header names SPSS 27.0 while the article names SPSS 26.0.
  .fl <- list.files(AT_PKG, recursive = TRUE, all.files = FALSE)
  expect("C-10", deposit_files = 2, readme_codebook_licence_files = 0, output_files = 0, tol = 0, note = "files in the copy audited")
  check("C-10", deposit_files = length(.fl), readme_codebook_licence_files = sum(grepl("readme|codebook|licen|dictionary", .fl, ignore.case = TRUE)), output_files = sum(grepl("\\.(spv|out|pdf|html?)$", .fl, ignore.case = TRUE)))
  expect("C-10", syntax_loads_data = 0, syntax_encodes_0125 = 0, tol = 0, note = "a GET FILE (or similar) command, and the .0125 level, in the syntax")
  check("C-10", syntax_loads_data = as.integer(any(grepl("GET +FILE|GET +SAS|GET +DATA", AT_SYNTAX, ignore.case = TRUE))), syntax_encodes_0125 = as.integer(any(grepl("0125", AT_SYNTAX, fixed = TRUE))))
  .hd <- rawToChar(readBin(AT_SAV, "raw", 64)[5:64])
  expect("C-10", sav_header_names_spss_27 = 1, article_names_spss_26 = 1, tol = 0, note = "the product string in the data file's header (no time stamp is read) and the article's Methods")
  check("C-10", sav_header_names_spss_27 = as.integer(grepl("27.0", .hd, fixed = TRUE)), article_names_spss_26 = paper_has("All analyses were performed with SPSS 26.0"))
  .vl <- names(attr(AT_RAW$Intervention, "labels"))
  expect("C-10", misspelled_value_label = 1, misspelled_variable_names = 4, file_name_has_ampersand = 1, tol = 0, note = "the 'Strenght' value label, the Ommisions/Commisions variable names, the data file name")
  check("C-10", misspelled_value_label = sum(grepl("Strenght", .vl, fixed = TRUE)), misspelled_variable_names = sum(grepl("^(Ommisions|Commisions)_", names(AT_RAW))), file_name_has_ampersand = as.integer(grepl("&", basename(AT_SAV), fixed = TRUE)))
  expect("C-10", article_data_statement_figshare = 1, article_preregistration_aspredicted = 1, tol = 0)
  check("C-10", article_data_statement_figshare = paper_has("Data are available on figshare"), article_preregistration_aspredicted = paper_has("preregistrations of sample size and primary analyses are avail- able on AsPredicted"))
  expect("C-10", id_variable_labelled = 0, tol = 0); check("C-10", id_variable_labelled = as.integer(!is.null(attr(AT_RAW$ID, "label"))))
  rm(.fl, .hd, .vl)
}, error = function(e) runtime_failure("C-10", e))
.V$current <- NULL

## ---- summary of every audit-written check above ----
claims_summary(c("C-1", "C-2", "C-3", "C-4", "C-5", "C-6", "C-7", "C-8", "C-9", "C-10"))
