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
## 8 claims: 0 retraction material, 5 corrigendum material, 3 comment material. File language: .R (the deposit's own).
## ============================================================================================

## ---- standalone check harness (same contract as verification.Rmd) ----------------------------
PKG_DIR <- local({
  e <- Sys.getenv("I4R_PKG_DIR", "")
  if (nzchar(e) && dir.exists(e)) return(normalizePath(e))
  for (cand in c(file.path(getwd(), "pkg"), file.path(dirname(getwd()), "audit", "pkg"), "pkg"))
    if (dir.exists(cand)) return(normalizePath(cand))
  stop("cannot locate the deposit: set I4R_PKG_DIR to the extracted package directory (the pkg/ folder)")
})
AUDIT_DIR <- Sys.getenv("I4R_AUDIT_DIR", dirname(PKG_DIR))
read_deposit <- function(file, ...) {
  path <- if (file.exists(file)) file else file.path(PKG_DIR, file)
  if (!file.exists(path)) stop("not in the deposit: ", file)
  ext <- tolower(tools::file_ext(path))
  switch(ext, csv = utils::read.csv(path, ...), tsv = utils::read.delim(path, ...), txt = utils::read.delim(path, ...),
         rds = readRDS(path), stop("no loader for .", ext, " in this claims script; open the file with the matching R package"))
}
.V <- new.env(); .V$exp <- list(); .V$res <- list()
expect <- function(id, ..., tol = 0, note = NULL) {
  vals <- list(...); if (is.null(names(vals)) || any(names(vals) == "")) stop("expect(): every value needs a name")
  for (k in names(vals)) { .V$exp[[paste(id, k, sep = "|")]] <- list(id = id, key = k, expected = vals[[k]], tol = tol, note = note)
    cat(sprintf("[%-6s %-34s] EXPECTED  %s
", id, k, format(vals[[k]]))) }
  invisible(TRUE)
}
check <- function(id, ...) {
  vals <- list(...); if (is.null(names(vals)) || any(names(vals) == "")) stop("check(): every value needs a name")
  for (k in names(vals)) { key <- paste(id, k, sep = "|"); e <- .V$exp[[key]]; ob <- vals[[k]]
    if (is.null(e)) { .V$res[[key]] <- "FAIL (never declared with expect())"; cat(sprintf("[%-6s %-34s] FAIL  undeclared
", id, k)); next }
    ok <- if (is.numeric(e$expected)) isTRUE(length(ob) == 1 && !is.na(ob) && abs(e$expected - ob) <= e$tol + 1e-12) else identical(e$expected, ob)
    .V$res[[key]] <- if (ok) "PASS" else "FAIL"
    cat(sprintf("[%-6s %-34s] OBSERVED  %s  %s
", id, k, format(ob), if (ok) "PASS" else "FAIL")) }
  invisible(TRUE)
}
claims_summary <- function() {
  declared <- names(.V$exp); checked <- names(.V$res)
  for (k in setdiff(declared, checked)) .V$res[[k]] <- "FAIL (declared but never checked)"
  fails <- sum(!vapply(.V$res, function(x) identical(x, "PASS"), logical(1)))
  cat(sprintf("
%d checks, %d PASS, %d FAIL
", length(.V$res), length(.V$res) - fails, fails))
  invisible(fails)
}
## ---- audit-specific helpers (verification_snippets/_prelude.R) ----
## Shared loaders and helpers for the checks of the Altermann and Groepel (2025) comment.
## The deposit is one SPSS data file and one SPSS syntax file; the data file is read with haven.
suppressMessages(library(haven))

GROUPS <- c("Endurance", "Strength", "Coordination", "Control")
CONTRASTS <- list(
  all_vs_control = c(1, 1, 1, -3),
  endurance      = c(2, -1, -1, 0),
  strength       = c(-1, 2, -1, 0),
  coordination   = c(-1, -1, 2, 0)
)

load_all <- function() {
  d <- haven::read_sav(file.path(PKG_DIR, "Altermann_LongtermExercise&Attention.sav"))
  d <- as.data.frame(lapply(d, function(x) as.numeric(unclass(x))))
  d$Intervention <- factor(d$Intervention, levels = 1:4, labels = GROUPS)
  d
}

## the deposited syntax keeps the cases with Concentration_KL_POST > 0 (system-missing cases drop out)
load_completers <- function() {
  d <- load_all()
  d[!is.na(d$Concentration_KL_POST) & d$Concentration_KL_POST > 0, ]
}

## rows (positions in the full file) whose stored scores break the d2 identities
## KL = BZO - omissions - commissions and F% = (omissions + commissions) / BZO * 100
identity_flags <- function(a) {
  pre <- abs(a$Speed_BZO_PRE - a$Ommisions_PRE - a$Commisions_PRE - a$Concentration_KL_PRE) > 1e-6 |
    abs((a$Ommisions_PRE + a$Commisions_PRE) / a$Speed_BZO_PRE * 100 - a$ErrorScore_PRE) >= 0.01
  post <- abs(a$Speed_BZO_POST - a$Ommisions_POST - a$Commisions_POST - a$Concentration_KL_POST) > 1e-6 |
    abs((a$Ommisions_POST + a$Commisions_POST) / a$Speed_BZO_POST * 100 - a$ErrorScore_POST) >= 0.01
  which(pre | (post %in% TRUE))
}

## the four scores recomputed from the stored BZO, omission and commission counts
identity_implied <- function(a) {
  a$Concentration_KL_PRE <- a$Speed_BZO_PRE - a$Ommisions_PRE - a$Commisions_PRE
  a$Concentration_KL_POST <- a$Speed_BZO_POST - a$Ommisions_POST - a$Commisions_POST
  a$ErrorScore_PRE <- (a$Ommisions_PRE + a$Commisions_PRE) / a$Speed_BZO_PRE * 100
  a$ErrorScore_POST <- (a$Ommisions_POST + a$Commisions_POST) / a$Speed_BZO_POST * 100
  a
}

## the article's text with line breaks and ligatures normalised
paper_text <- function() {
  p <- file.path(AUDIT_DIR, "paper", "paper.txt")
  x <- paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  x <- gsub("ﬁ", "fi", x, fixed = TRUE)
  x <- gsub("ﬂ", "fl", x, fixed = TRUE)
  x <- gsub("−", "-", x, fixed = TRUE)
  x
}

## every "F(df1, df2) = value, p < or = value" statement in the Results section, in order of appearance
paper_F_tests <- function() {
  x <- paper_text()
  a <- regexpr("\nResults\n", x, fixed = TRUE)
  b <- regexpr("\nDiscussion\n", x, fixed = TRUE)
  r <- gsub("\\s+", " ", substr(x, a, b))
  pat <- "F ?\\( ?([0-9]+) ?, ?([0-9]+) ?\\) ?= ?([0-9.]+), ?p ?([=<]) ?(\\.[0-9]+)"
  m <- regmatches(r, gregexpr(pat, r, perl = TRUE))[[1]]
  parts <- regmatches(m, regexec(pat, m, perl = TRUE))
  data.frame(df1 = as.numeric(sapply(parts, `[`, 2)), df2 = as.numeric(sapply(parts, `[`, 3)),
             F = as.numeric(sapply(parts, `[`, 4)), op = sapply(parts, `[`, 5),
             p = as.numeric(sapply(parts, `[`, 6)), stringsAsFactors = FALSE)
}

## Table 2 of the article: rows Pre and Post for each score, four groups, mean and SD
paper_table2 <- function() {
  x <- paper_text()
  lines <- strsplit(x, "\n", fixed = TRUE)[[1]]
  start <- grep("^Table 2\\. Means", lines)[1]
  blk <- lines[start:(start + 14)]
  out <- list()
  score <- NA
  for (ln in blk) {
    if (grepl("^\\s*Concentration score", ln)) score <- "KL"
    if (grepl("^\\s*Error score", ln)) score <- "F"
    if (grepl("^\\s*Speed score", ln)) score <- "BZO"
    ph <- sub("^\\s*(Pre|Post)\\s.*$", "\\1", ln)
    if (ph %in% c("Pre", "Post")) {
      mm <- regmatches(ln, gregexpr("[0-9]+\\.[0-9]+ \\([0-9]+\\.[0-9]+\\)", ln))[[1]]
      mm <- mm[1:4]
      m <- as.numeric(sub(" .*", "", mm)); s <- as.numeric(gsub(".*\\(|\\)", "", mm))
      out[[length(out) + 1]] <- data.frame(score = score, phase = ph, group = GROUPS, mean = m, sd = s)
    }
  }
  do.call(rbind, out)
}

## balanced or unbalanced 4 x 2 mixed ANOVA on (pre, post), computed from the change score
rm_anova <- function(pre, post, g) {
  g <- factor(g); n <- as.numeric(table(g)); G <- nlevels(g)
  ch <- post - pre; av <- (pre + post) / 2
  fit <- lm(ch ~ g); df2 <- fit$df.residual; mse <- sum(resid(fit)^2) / df2
  gm <- tapply(ch, g, mean); w <- rep(1 / G, G)
  F_phase <- sum(w * gm)^2 / (mse * sum(w^2 / n))
  F_int <- anova(fit)[["F value"]][1]
  F_grp <- anova(lm(av ~ g))[["F value"]][1]
  eta <- function(F, d1) F * d1 / (F * d1 + df2)
  list(F_phase = F_phase, F_group = F_grp, F_inter = F_int, df2 = df2,
       p_phase = pf(F_phase, 1, df2, lower.tail = FALSE), p_group = pf(F_grp, G - 1, df2, lower.tail = FALSE),
       p_inter = pf(F_int, G - 1, df2, lower.tail = FALSE),
       eta_phase = eta(F_phase, 1), eta_group = eta(F_grp, G - 1), eta_inter = eta(F_int, G - 1), mse = mse)
}

## planned contrast on the pre-to-post change (SPSS LMATRIX with MMATRIX -1 +1), pooled error term
contrast_F <- function(pre, post, g, w) {
  g <- factor(g); n <- as.numeric(table(g)); ch <- post - pre
  fit <- lm(ch ~ g); df2 <- fit$df.residual; mse <- sum(resid(fit)^2) / df2
  gm <- tapply(ch, g, mean)
  F <- sum(w * gm)^2 / (mse * sum(w^2 / n))
  list(F = F, df2 = df2, p = pf(F, 1, df2, lower.tail = FALSE), eta = F / (F + df2), est = sum(w * gm))
}

## the three scores analysed in the article: pretest and posttest column names
SCORES <- list(KL = c("Concentration_KL_PRE", "Concentration_KL_POST"),
               F = c("ErrorScore_PRE", "ErrorScore_POST"), BZO = c("Speed_BZO_PRE", "Speed_BZO_POST"))

## round half up, as the article's printed values are
rhu <- function(x, d = 2) floor(x * 10^d + 0.5 + 1e-9) / 10^d

## GRIM: can a mean of n integer scores print as m?
grim_ok <- function(m, n, d = 2) {
  s <- floor(m * n) + (-1:2)
  any(abs(rhu(s / n, d) - m) < 1e-9)
}

## the analysed sample under three treatments of the rows whose scores break the d2 identities
variant_data <- function(kind) {
  a <- load_all(); done <- !is.na(a$Concentration_KL_POST); flag <- identity_flags(a)
  switch(kind, published = a[done, ], recomputed = identity_implied(a)[done, ],
         dropped = a[done & !(seq_len(nrow(a)) %in% flag), ])
}

## separate-variance (Welch-Satterthwaite) p of a planned contrast on the change score
welch_contrast <- function(ch, g, w) {
  m <- tapply(ch, g, mean); v <- tapply(ch, g, var); n <- as.numeric(table(g))
  se2 <- sum(w^2 * v / n)
  2 * pt(-abs(sum(w * m) / sqrt(se2)), se2^2 / sum((w^2 * v / n)^2 / (n - 1)))
}

## p of a planned contrast on baseline-adjusted posttest means (common slope; first group is the reference)
ancova_contrast <- function(pre, post, g, w) {
  fit <- lm(post ~ pre + g); co <- coef(fit); idx <- grep("^g", names(co)); V <- vcov(fit)
  b <- c(0, co[idx]); Vf <- matrix(0, 4, 4); Vf[2:4, 2:4] <- V[idx, idx]
  2 * pt(-abs(sum(w * b) / sqrt(as.numeric(t(w) %*% Vf %*% w))), fit$df.residual)
}

## permutation p of a planned contrast on the change score (group labels shuffled over all analysed rows)
perm_contrast <- function(ch, g, w, B = 20000, seed = 1) {
  set.seed(seed); obs <- sum(w * tapply(ch, g, mean))
  perm <- replicate(B, sum(w * tapply(ch, sample(g), mean)))
  mean(abs(perm) >= abs(obs) - 1e-9)
}

## the deposited syntax file as lines
syntax_lines <- function() readLines(file.path(PKG_DIR, "Study_Syntax.txt"), warn = FALSE)

## one-way ANOVA: F, p and eta squared (SSB / SST)
oneway <- function(y, g) {
  ok <- !is.na(y); y <- y[ok]; g <- droplevels(factor(g[ok]))
  a <- anova(lm(y ~ g))
  list(F = a[["F value"]][1], df1 = a[["Df"]][1], df2 = a[["Df"]][2], p = a[["Pr(>F)"]][1],
       eta = a[["Sum Sq"]][1] / sum(a[["Sum Sq"]]))
}

## one-way ANOVA p-value of Bonferroni-adjusted pairwise comparisons using the pooled error term
bonf_pairs <- function(y, g) {
  ok <- !is.na(y); y <- y[ok]; g <- droplevels(factor(g[ok]))
  fit <- lm(y ~ g); df2 <- fit$df.residual; mse <- sum(resid(fit)^2) / df2
  m <- tapply(y, g, mean); n <- as.numeric(table(g)); L <- levels(g); out <- c()
  for (i in 1:(length(L) - 1)) for (j in (i + 1):length(L)) {
    t <- (m[i] - m[j]) / sqrt(mse * (1 / n[i] + 1 / n[j]))
    out[paste(L[i], L[j], sep = "-")] <- min(1, 2 * pt(-abs(t), df2) * choose(length(L), 2))
  }
  out
}
## ============================================================================================
## ============================================================================================
## CLAIM 1 [CORRIGENDUM MATERIAL]  (finding F1, Section 3)
##   The claim that coordination training has the greatest effect rests on a contrast that fails
##   the article's own threshold and on no direct comparison with endurance.
## Check written by the audit (no author code covers this claim):

expect("C-1", threshold = 0.0125, printed_coordination_p = 0.018, tol = 0,
       note = "Both are printed in the article: the Methods' Bonferroni-adjusted p value for the contrast tests and the Results' p for the coordination contrast on concentration performance; read from the article text, so exact")
expect("C-1", pooled_p = 0.0177, welch_p = 0.0392, ancova_p = 0.0226, tol = 0.00005,
       note = "Deterministic least-squares quantities printed to four decimals in the comment; half a unit of the last digit. The pooled value reproduces the article's p = .018")
expect("C-1", perm_p = 0.033, tol = 0.005,
       note = "Permutation p with 20000 shuffles and a fixed seed; Monte-Carlo standard error is about 0.0013 at this p, so 0.005 is about four standard errors")
expect("C-1", recomputed_pooled_p = 0.0102, recomputed_welch_p = 0.0270, recomputed_ancova_p = 0.0139,
       dropped_pooled_p = 0.0115, dropped_welch_p = 0.0306, dropped_ancova_p = 0.0215, tol = 0.00005,
       note = "Same deterministic quantities after recomputing the three inconsistent rows' scores from their counts, or dropping the rows; four decimals printed")
expect("C-1", recomputed_perm_p = 0.021, dropped_perm_p = 0.026, tol = 0.005,
       note = "Permutation p, 20000 shuffles, fixed seed; tolerance as for the published-data permutation p")
expect("C-1", coord_minus_endurance_change = 6.05, tol = 0.005,
       note = "Difference in mean concentration gain (coordination minus endurance), printed to two decimals")
expect("C-1", pooled_pair_p_published = 0.32, pooled_pair_p_recomputed = 0.25, pooled_pair_p_dropped = 0.25,
       ancova_pair_p_published = 0.09, ancova_pair_p_recomputed = 0.07, ancova_pair_p_dropped = 0.11, tol = 0.005,
       note = "Coordination versus endurance, two decimals printed; half a unit of the last digit")
expect("C-1", three_group_F = 4.11, tol = 0.005, note = "F(2, 69) for equal change in the three exercise groups, two decimals printed")
expect("C-1", three_group_p = 0.0205, coord_vs_strength_p = 0.0061, tol = 0.00005, note = "Four decimals printed")
expect("C-1", strength_ancova_p = 0.0615, tol = 0.00005, note = "Strength contrast on baseline-adjusted posttest means, four decimals printed")
expect("C-1", within_group_r_pre_change = -0.72, tol = 0.005, note = "Correlation of pretest with change after removing group means, two decimals printed")
expect("C-1", all_vs_control_max_p_below_001 = TRUE, note = "The exercise-versus-control contrast on concentration stays below .001 under the four specifications on the published data and under the pooled test after either treatment of the three records")

x <- paper_text(); ft <- paper_F_tests()
thr <- as.numeric(sub(".*of (\\.[0-9]+)$", "\\1", regmatches(x, regexpr("Bonferroni-adjusted p value of \\.[0-9]+", x))))
coord_printed <- ft$p[which(abs(ft$F - 5.84) < 1e-9)][1]
cc <- function(kind, w, B = 20000, seed = 20240103) {
  dd <- variant_data(kind); pre <- dd$Concentration_KL_PRE; post <- dd$Concentration_KL_POST; g <- dd$Intervention; ch <- post - pre
  ex <- g != "Control"; ge <- droplevels(g[ex]); che <- ch[ex]; fit3 <- lm(che ~ ge); mse3 <- sum(resid(fit3)^2) / fit3$df.residual
  ce <- che[ge == "Coordination"]; ee <- che[ge == "Endurance"]
  list(pooled = contrast_F(pre, post, g, w)$p, welch = welch_contrast(ch, g, w), ancova = ancova_contrast(pre, post, g, w),
       perm = perm_contrast(ch, g, w, B, seed), diff = mean(ce) - mean(ee),
       pair_pooled = 2 * pt(-abs((mean(ce) - mean(ee)) / sqrt(mse3 * 2 / length(ce))), fit3$df.residual),
       pair_ancova = summary(lm(post[ex] ~ pre[ex] + ge))$coefficients["geCoordination", 4])
}
pub <- cc("published", CONTRASTS$coordination); rec <- cc("recomputed", CONTRASTS$coordination); drp <- cc("dropped", CONTRASTS$coordination)
dd <- variant_data("published"); g <- dd$Intervention; pre <- dd$Concentration_KL_PRE; post <- dd$Concentration_KL_POST; ch <- post - pre
ex <- g != "Control"; ge <- droplevels(g[ex]); a3 <- anova(lm(ch[ex] ~ ge))
fit3 <- lm(ch[ex] ~ ge); mse3 <- sum(resid(fit3)^2) / fit3$df.residual; m3 <- tapply(ch[ex], ge, mean)
p_cs <- 2 * pt(-abs((m3["Coordination"] - m3["Strength"]) / sqrt(mse3 * 2 / 24)), fit3$df.residual)
sa <- ancova_contrast(pre, post, g, CONTRASTS$strength)
vp <- function(kind) { v <- variant_data(kind); contrast_F(v$Concentration_KL_PRE, v$Concentration_KL_POST, v$Intervention, CONTRASTS$all_vs_control)$p }
allc <- c(contrast_F(pre, post, g, CONTRASTS$all_vs_control)$p, welch_contrast(ch, g, CONTRASTS$all_vs_control),
          ancova_contrast(pre, post, g, CONTRASTS$all_vs_control), perm_contrast(ch, g, CONTRASTS$all_vs_control, 20000, 20240101),
          vp("recomputed"), vp("dropped"))
check("C-1", threshold = thr, printed_coordination_p = coord_printed,
      pooled_p = pub$pooled, welch_p = pub$welch, ancova_p = pub$ancova, perm_p = pub$perm,
      recomputed_pooled_p = rec$pooled, recomputed_welch_p = rec$welch, recomputed_ancova_p = rec$ancova,
      dropped_pooled_p = drp$pooled, dropped_welch_p = drp$welch, dropped_ancova_p = drp$ancova,
      recomputed_perm_p = rec$perm, dropped_perm_p = drp$perm, coord_minus_endurance_change = pub$diff,
      pooled_pair_p_published = pub$pair_pooled, pooled_pair_p_recomputed = rec$pair_pooled, pooled_pair_p_dropped = drp$pair_pooled,
      ancova_pair_p_published = pub$pair_ancova, ancova_pair_p_recomputed = rec$pair_ancova, ancova_pair_p_dropped = drp$pair_ancova,
      three_group_F = a3[["F value"]][1], three_group_p = a3[["Pr(>F)"]][1], coord_vs_strength_p = unname(p_cs),
      strength_ancova_p = sa, within_group_r_pre_change = cor(pre - ave(pre, g), ch - ave(ch, g)),
      all_vs_control_max_p_below_001 = isTRUE(max(allc) < 0.001))

## ============================================================================================
## CLAIM 2 [CORRIGENDUM MATERIAL]  (finding F2, Section 4)
##   Three records have concentration and error scores that contradict their own omission and
##   commission counts, two with non-integer concentration scores.
## Check written by the audit (no author code covers this claim):

expect("C-2", rows_in_file = 120, rows_failing_identity = 3, pretest_rows_failing = 1, posttest_rows_failing = 2,
       non_integer_KL_post = 2, non_integer_KL_pre = 0, tol = 0, note = "Counts of rows and values; exact")
expect("C-2", post_rows_share_one_non_integer_implied_error_count = TRUE, implied_by_KL_equals_implied_by_Fpct = TRUE,
       stored_counts_exceed_implied_in_all_three_rows = TRUE, all_three_rows_analysed = TRUE, tol = 0,
       note = "Error count implied by BZO minus the stored concentration score, and by the stored error percentage times BZO, against the stored omission plus commission counts; only logical summaries over the three rows are printed")
expect("C-2", discrepancy_min = 10.85, discrepancy_max = 19.85, tol = 0.005,
       note = "Range over the three rows of the stored omission-plus-commission count minus the count implied by the scores, two decimals printed; a range across rows, not a row value")
expect("C-2", KL_interaction_F_published = 10.07, KL_interaction_F_recomputed = 11.22, KL_interaction_F_dropped = 10.94,
       F_interaction_F_published = 8.06, F_interaction_F_recomputed = 8.23, F_interaction_F_dropped = 7.47, tol = 0.005,
       note = "Phase-by-group F(3, 92) (or F(3, 89) with three rows dropped), two decimals printed; half a unit of the last digit")
expect("C-2", control_post_KL_mean_published = 175.99, control_post_KL_mean_recomputed = 175.17, tol = 0.005,
       note = "Control group posttest concentration mean, two decimals printed")
expect("C-2", interaction_p_below_001_in_all_variants = TRUE, note = "The phase-by-group p stays below .001 for concentration and for the error score in all three treatments")

a <- load_all(); done <- !is.na(a$Concentration_KL_POST)
pre_bad <- abs(a$Speed_BZO_PRE - a$Ommisions_PRE - a$Commisions_PRE - a$Concentration_KL_PRE) > 1e-6 |
  abs((a$Ommisions_PRE + a$Commisions_PRE) / a$Speed_BZO_PRE * 100 - a$ErrorScore_PRE) >= 0.01
post_bad <- (abs(a$Speed_BZO_POST - a$Ommisions_POST - a$Commisions_POST - a$Concentration_KL_POST) > 1e-6 |
  abs((a$Ommisions_POST + a$Commisions_POST) / a$Speed_BZO_POST * 100 - a$ErrorScore_POST) >= 0.01) %in% TRUE
flag <- identity_flags(a); bp <- which(post_bad); bq <- which(pre_bad)
imp_kl <- (a$Speed_BZO_POST - a$Concentration_KL_POST)[bp]; imp_f <- (a$ErrorScore_POST / 100 * a$Speed_BZO_POST)[bp]
imp_pre_kl <- (a$Speed_BZO_PRE - a$Concentration_KL_PRE)[bq]; imp_pre_f <- (a$ErrorScore_PRE / 100 * a$Speed_BZO_PRE)[bq]
gap <- c((a$Ommisions_POST + a$Commisions_POST)[bp] - imp_kl, (a$Ommisions_PRE + a$Commisions_PRE)[bq] - imp_pre_kl)
nonint <- function(x) sum(abs(x - round(x)) > 1e-9, na.rm = TRUE)
fa <- function(kind, s) { dd <- variant_data(kind); rm_anova(dd[[SCORES[[s]][1]]], dd[[SCORES[[s]][2]]], dd$Intervention) }
ctl <- function(kind) mean(variant_data(kind)$Concentration_KL_POST[variant_data(kind)$Intervention == "Control"])
check("C-2", rows_in_file = nrow(a), rows_failing_identity = length(flag), pretest_rows_failing = sum(pre_bad), posttest_rows_failing = sum(post_bad),
      non_integer_KL_post = nonint(a$Concentration_KL_POST), non_integer_KL_pre = nonint(a$Concentration_KL_PRE),
      post_rows_share_one_non_integer_implied_error_count = isTRUE(abs(diff(imp_kl)) < 1e-9 && all(abs(imp_kl - round(imp_kl)) > 1e-9)),
      implied_by_KL_equals_implied_by_Fpct = isTRUE(all(abs(c(imp_kl - imp_f, imp_pre_kl - imp_pre_f)) < 1e-6)),
      stored_counts_exceed_implied_in_all_three_rows = isTRUE(all(gap > 0)),
      discrepancy_min = min(gap), discrepancy_max = max(gap),
      all_three_rows_analysed = isTRUE(all(flag %in% which(done))),
      KL_interaction_F_published = fa("published", "KL")$F_inter, KL_interaction_F_recomputed = fa("recomputed", "KL")$F_inter,
      KL_interaction_F_dropped = fa("dropped", "KL")$F_inter, F_interaction_F_published = fa("published", "F")$F_inter,
      F_interaction_F_recomputed = fa("recomputed", "F")$F_inter, F_interaction_F_dropped = fa("dropped", "F")$F_inter,
      control_post_KL_mean_published = ctl("published"), control_post_KL_mean_recomputed = ctl("recomputed"),
      interaction_p_below_001_in_all_variants = isTRUE(all(c(sapply(c("published", "recomputed", "dropped"), function(k) fa(k, "KL")$p_inter),
                                                              sapply(c("published", "recomputed", "dropped"), function(k) fa(k, "F")$p_inter)) < 0.001)))

## ============================================================================================
## CLAIM 3 [CORRIGENDUM MATERIAL]  (finding F3, Section 5)
##   Two control-group pretest means in Table 2 (concentration 157.40, speed 174.52) cannot
##   arise from 24 integer scores; the data give 157.00 and 174.42.
## Check written by the audit (no author code covers this claim):

expect("C-3", table2_means_compared = 24, table2_means_matching = 22, table2_SDs_matching = 24, tol = 0,
       note = "Table 2 has 3 scores x 2 phases x 4 groups = 24 means and 24 SDs; the comparison rounds half up to the printed two decimals, so exact")
expect("C-3", printed_KL_pre_control = 157.40, printed_BZO_pre_control = 174.52, tol = 0, note = "Read from the Table 2 text of the article")
expect("C-3", deposit_KL_pre_control = 157.00, deposit_BZO_pre_control = 174.42, tol = 0.005,
       note = "Deposited control-group pretest means, two decimals printed; half a unit of the last digit")
expect("C-3", KL_control_attainable_with_24_integers = FALSE, BZO_control_attainable_with_24_integers = FALSE,
       other_integer_means_attainable = 10, tol = 0,
       note = "GRIM: for n = 24 integer scores a mean prints as 157.40 or 174.52 for no integer sum; the 10 other integer-valued Table 2 means (concentration pretest, speed pretest and posttest) are attainable")
expect("C-3", one_unit_differences_explained_by_double_rounding = 3, tol = 0,
       note = "Three printed values (concentration endurance contrast F, error-score coordination contrast F, error-score group eta squared) differ by one unit in the last digit; each equals the deposit value rounded to three decimals and then to two")

d <- load_completers(); t2 <- paper_table2(); mm <- 0; sm <- 0; gm <- c(); cnt <- 0
for (s in names(SCORES)) for (ph in c("Pre", "Post")) for (g in GROUPS) {
  cnt <- cnt + 1
  y <- d[[SCORES[[s]][if (ph == "Pre") 1 else 2]]][d$Intervention == g]; ref <- t2[t2$score == s & t2$phase == ph & t2$group == g, ]
  mm <- mm + (rhu(mean(y), 2) == ref$mean); sm <- sm + (rhu(sd(y), 2) == ref$sd)
  if (s == "BZO" || (s == "KL" && ph == "Pre")) gm[paste(s, ph, g)] <- grim_ok(ref$mean, 24)   # integer-valued scores only
}
kl_c <- t2$mean[t2$score == "KL" & t2$phase == "Pre" & t2$group == "Control"]
bz_c <- t2$mean[t2$score == "BZO" & t2$phase == "Pre" & t2$group == "Control"]
ft <- paper_F_tests(); x <- paper_text(); lines <- strsplit(x, "\n", fixed = TRUE)[[1]]
blk <- lines[grep("^Table 2\\. Means", lines)[1] + 0:14]
gm_rows <- regmatches(blk, regexec("G\\s+([0-9.]+)\\s+(\\.[0-9]+)\\s+([0-9.]+)\\s*$", blk)); gm_rows <- gm_rows[lengths(gm_rows) > 0]
eta_err_printed <- as.numeric(gm_rows[[2]][4])
f_end <- contrast_F(d$Concentration_KL_PRE, d$Concentration_KL_POST, d$Intervention, CONTRASTS$endurance)$F
f_co <- contrast_F(d$ErrorScore_PRE, d$ErrorScore_POST, d$Intervention, CONTRASTS$coordination)$F
eta_err <- rm_anova(d$ErrorScore_PRE, d$ErrorScore_POST, d$Intervention)$eta_group
dr <- function(v) rhu(rhu(v, 3), 2)
expl <- (dr(f_end) == ft$F[which(abs(ft$F - 0.28) < 1e-9)][1]) + (dr(f_co) == ft$F[which(abs(ft$F - 8.80) < 1e-9)][1]) + (dr(eta_err) == eta_err_printed)
check("C-3", table2_means_compared = cnt, table2_means_matching = mm, table2_SDs_matching = sm,
      printed_KL_pre_control = kl_c, printed_BZO_pre_control = bz_c,
      deposit_KL_pre_control = mean(d$Concentration_KL_PRE[d$Intervention == "Control"]),
      deposit_BZO_pre_control = mean(d$Speed_BZO_PRE[d$Intervention == "Control"]),
      KL_control_attainable_with_24_integers = isTRUE(gm[["KL Pre Control"]]), BZO_control_attainable_with_24_integers = isTRUE(gm[["BZO Pre Control"]]),
      other_integer_means_attainable = sum(gm), one_unit_differences_explained_by_double_rounding = unname(expl))

## ============================================================================================
## CLAIM 4 [CORRIGENDUM MATERIAL]  (finding F4, Section 6)
##   The analysed groups have identical weekly PE-hour splits, which the described
##   stratification does not explain, and the Table 1 note misstates the coordination group's
##   strata.
## Check written by the audit (no author code covers this claim):

expect("C-4", PE_split_per_group = "12/6/6 | 12/6/6 | 12/6/6 | 12/6/6", PE_identical_in_all_four_groups = TRUE, PE_identical_in_all_120 = FALSE,
       strata_with_both_PE_values = 8, tol = 0,
       note = "Counts of participants with 2, 4 and 5 weekly PE hours per group among the 96 analysed (and the identical-split test on all 120 randomised); exact counts")
expect("C-4", P_identical_split_complete_randomisation = 0.000188, tol = 0.0000005,
       note = "Exact multivariate-hypergeometric probability that four random groups of 24 drawn from the 96 analysed participants all show the 12/6/6 split; three significant figures printed")
expect("C-4", P_identical_split_within_strata = 0.0005, tol = 0.0001,
       note = "Monte-Carlo probability under random assignment within the eight age-by-gender strata with the realised group sizes; one million shuffles with a fixed seed, so the standard error is about 0.00002 and the tolerance about four standard errors; printed as about 5 in 10,000")
expect("C-4", cells_not_three = 4, cells_with_exactly_three = 28, cells_total = 32, tol = 0,
       note = "Group-by-age-by-gender cells of the 96 analysed participants; Table 1 note says each holds three")
expect("C-4", coordination_age15_men_women = "2/4", coordination_age16_men_women = "4/2", other_groups_all_cells_three = TRUE, tol = 0, note = "Exact counts")
expect("C-4", note_text_present = TRUE, stratification_stated_on_age_and_gender_only = TRUE, note = "The Table 1 note and the Methods wording are read from the article text")

d <- load_completers(); a <- load_all()
tpe <- table(d$Intervention, d$PE_week); tpa <- table(a$Intervention, a$PE_week)
n2 <- sum(d$PE_week == 2); n4 <- sum(d$PE_week == 4); n5 <- sum(d$PE_week == 5)
lp <- 0; for (k in 0:2) lp <- lp + lchoose(n2 - 12 * k, 12) + lchoose(n4 - 6 * k, 6) + lchoose(n5 - 6 * k, 6) - lchoose(96 - 24 * k, 24)
st <- interaction(d$Age, d$Gender); gl <- as.integer(d$Intervention); pe <- match(d$PE_week, c(2, 4, 5)); idx <- split(seq_along(gl), st)
set.seed(20240102); B <- 1000000; hit <- 0L
for (b in 1:B) {
  g2 <- gl; for (ii in idx) g2[ii] <- gl[ii][sample.int(length(ii))]
  tb <- tabulate((g2 - 1L) * 3L + pe, 12)
  if (all(tb[1:3] == tb[4:6]) && all(tb[1:3] == tb[7:9]) && all(tb[1:3] == tb[10:12])) hit <- hit + 1L
}
ct <- table(d$Intervention, d$Age, d$Gender); co <- d[d$Intervention == "Coordination", ]
x <- paper_text(); xx <- gsub("\\s+", " ", x)
check("C-4", PE_split_per_group = paste(apply(tpe, 1, paste, collapse = "/"), collapse = " | "),
      PE_identical_in_all_four_groups = isTRUE(all(apply(tpe, 1, function(r) all(r == tpe[1, ])))),
      PE_identical_in_all_120 = isTRUE(all(apply(tpa, 1, function(r) all(r == tpa[1, ])))),
      strata_with_both_PE_values = sum(apply(table(d$Age, d$Gender, d$PE_week), c(1, 2), function(v) sum(v > 0) > 1)),
      P_identical_split_complete_randomisation = exp(lp), P_identical_split_within_strata = hit / B,
      cells_not_three = sum(ct != 3), cells_with_exactly_three = sum(ct == 3), cells_total = length(ct),
      coordination_age15_men_women = paste(sum(co$Age == 15 & co$Gender == 0), sum(co$Age == 15 & co$Gender == 1), sep = "/"),
      coordination_age16_men_women = paste(sum(co$Age == 16 & co$Gender == 0), sum(co$Age == 16 & co$Gender == 1), sep = "/"),
      other_groups_all_cells_three = isTRUE(all(ct[c("Endurance", "Strength", "Control"), , ] == 3)),
      note_text_present = grepl("each study group includes three women and three men of each age category", xx, fixed = TRUE),
      stratification_stated_on_age_and_gender_only = grepl("grouped into strata according to their age (15, 16, 17, and 18 years) and gender (male, female)", xx, fixed = TRUE))

## ============================================================================================
## CLAIM 5 [CORRIGENDUM MATERIAL]  (finding F5, Section 7)
##   The Abstract's N = 96 is the analysed sample; 120 were randomised, 24 were lost (29%, 23%,
##   14%, 11% by group), and the analysis uses completers only.
## Check written by the audit (no author code covers this claim):

expect("C-5", abstract_N = 96, rows_in_deposit = 120, analysed_rows = 96, tol = 0,
       note = "The Abstract's N is read from the article text; the other two are row counts of the deposited data file")
expect("C-5", randomised_per_group = "34/31/28/27", lost_per_group = "10/7/4/3", analysed_per_group = "24/24/24/24", tol = 0,
       note = "Group sizes in the deposit; they equal the counts printed in the article's Figure 1 flow chart (read from the rendered figure, which is an image and not in the extracted text)")
expect("C-5", loss_rate_pct_per_group = "29.4/22.6/14.3/11.1", dropout_share_pct = 20, tol = 0, note = "Percent lost per group, one decimal; share of all 120 randomised; the article states 20%")
expect("C-5", fisher_p_group_by_loss = 0.30, pretest_error_completers_vs_lost_p = 0.022, tol = 0.005,
       note = "Fisher exact p printed to two decimals (half a unit of the last digit); Welch p of pretest error score, completers versus lost participants, printed to three decimals and compared at 0.005 to cover the rounding")
expect("C-5", abstract_says_randomised_N96 = TRUE, tol = 0, note = "The Abstract gives N = 96 and says the students were randomised to one of three exercise groups or the control group; the two phrases are searched separately because the extracted Abstract text interleaves a second column between them")

a <- load_all(); d <- load_completers(); lost <- is.na(a$Concentration_KL_POST)
x <- gsub("\\s+", " ", paper_text())
abs_n <- as.numeric(sub(".*Adolescent students \\(N = ([0-9]+)\\).*", "\\1", regmatches(x, regexpr("Adolescent students \\(N = [0-9]+\\)", x))))
tb <- table(a$Intervention, lost)
tt <- t.test(a$ErrorScore_PRE[!lost], a$ErrorScore_PRE[lost])
check("C-5", abstract_N = abs_n, rows_in_deposit = nrow(a), analysed_rows = nrow(d),
      randomised_per_group = paste(table(a$Intervention), collapse = "/"), lost_per_group = paste(tb[, "TRUE"], collapse = "/"),
      analysed_per_group = paste(table(d$Intervention), collapse = "/"),
      loss_rate_pct_per_group = paste(round(100 * tb[, "TRUE"] / rowSums(tb), 1), collapse = "/"),
      dropout_share_pct = round(100 * mean(lost), 1), fisher_p_group_by_loss = fisher.test(tb)$p.value,
      pretest_error_completers_vs_lost_p = tt$p.value,
      abstract_says_randomised_N96 = isTRUE(grepl("Adolescent students \\(N = 96\\) aged", x) && grepl("randomised to one of three exercise intervention groups", x)))

## ============================================================================================
## CLAIM 6 [COMMENT MATERIAL]  (finding F6, Section 8)
##   Homogeneity tests and baseline group differences computed in the syntax are not reported;
##   posttest variances differ and pretest error scores differ across groups.
## Check written by the audit (no author code covers this claim):

expect("C-6", syntax_HOMOGENEITY_requests = 5, syntax_POSTHOC_requests = 4, tol = 0,
       note = "Lines of the deposited syntax that request homogeneity tests (two one-way analyses and the three repeated-measures models) and Bonferroni post-hoc comparisons (two one-way analyses and the concentration and error-score models); exact counts")
expect("C-6", article_mentions_Levene = FALSE, article_mentions_Boxs_M = FALSE, tol = 0,
       note = "Case-insensitive search of the article text for the two homogeneity tests the syntax requests")
expect("C-6", levene_post_concentration_p = 0.012, levene_post_error_below_001 = TRUE, tol = 0.0005,
       note = "Levene test (mean-centred, as the syntax's HOMOGENEITY option) on posttest scores across the four analysed groups; p printed to three decimals")
expect("C-6", post_SD_ratio_concentration = 2.06, post_SD_ratio_error = 2.01, tol = 0.005, note = "Largest to smallest group SD of the posttest score, two decimals printed")
expect("C-6", pretest_error_group_F = 2.93, tol = 0.005,
       note = "One-way F(3, 92) of the pretest error score across the four analysed groups, two decimals printed; half a unit of the last digit")
expect("C-6", pretest_error_group_p = 0.038, tol = 0.0005, note = "Its p, three decimals printed; half a unit of the last digit")
expect("C-6", pretest_concentration_group_p = 0.168, pretest_speed_group_p = 0.497, tol = 0.0005, note = "Same test for the other two scores, p to three decimals")

d <- load_completers(); sy <- syntax_lines(); x <- paper_text()
lev <- function(y, g) { z <- abs(y - ave(y, g)); anova(lm(z ~ g))[["Pr(>F)"]][1] }
o <- function(s) oneway(d[[SCORES[[s]][1]]], d$Intervention)
sdr <- function(col) { v <- tapply(d[[col]], d$Intervention, sd); max(v) / min(v) }
check("C-6", syntax_HOMOGENEITY_requests = sum(grepl("HOMOGENEITY", sy)), syntax_POSTHOC_requests = sum(grepl("POSTHOC", sy)),
      article_mentions_Levene = grepl("Levene", x, ignore.case = TRUE), article_mentions_Boxs_M = grepl("Box'?s", x),
      levene_post_concentration_p = lev(d$Concentration_KL_POST, d$Intervention),
      levene_post_error_below_001 = isTRUE(lev(d$ErrorScore_POST, d$Intervention) < 0.001),
      post_SD_ratio_concentration = sdr("Concentration_KL_POST"), post_SD_ratio_error = sdr("ErrorScore_POST"),
      pretest_error_group_F = o("F")$F, pretest_error_group_p = o("F")$p,
      pretest_concentration_group_p = o("KL")$p, pretest_speed_group_p = o("BZO")$p)

## ============================================================================================
## CLAIM 7 [COMMENT MATERIAL]  (finding F7, Section 8)
##   The deposit has no README, codebook or licence, its syntax opens no data file, and the data
##   file records a different SPSS version from the article.
## Check written by the audit (no author code covers this claim):

expect("C-7", files_in_deposit = 2, readme_or_codebook_or_licence_files = 0, tol = 0,
       note = "Non-hidden files directly in the deposit folder; a name search for README, codebook, licence or license files")
expect("C-7", syntax_lines_loading_data = 0, syntax_lines = 226, tol = 0,
       note = "Lines of the syntax file that open a data file (GET FILE, GET, DATASET or a FILE= option); the syntax file has 226 lines")
expect("C-7", data_file_name_has_ampersand = TRUE, sav_header_product_is_spss_27 = TRUE, article_names_spss_26 = TRUE, tol = 0,
       note = "The data file name is read from the deposit folder; the product string is the first 68 bytes of the .sav header; the article's version is read from the Statistical analysis paragraph")
expect("C-7", value_label_typos_present = TRUE, note = "The deposited value label for the strength group and two variable names carry spelling variants")

fl <- list.files(PKG_DIR); sy <- syntax_lines()
hd <- readBin(file.path(PKG_DIR, "Altermann_LongtermExercise&Attention.sav"), "raw", 68)
prod <- gsub("[^ -~]", "", rawToChar(hd[5:68]))
x <- gsub("\\s+", " ", paper_text())
a <- haven::read_sav(file.path(PKG_DIR, "Altermann_LongtermExercise&Attention.sav"))
vl <- names(attr(a$Intervention, "labels"))
check("C-7", files_in_deposit = length(fl), readme_or_codebook_or_licence_files = sum(grepl("readme|codebook|licen[cs]e", fl, ignore.case = TRUE)),
      syntax_lines_loading_data = sum(grepl("^\\s*(GET\\s|DATASET|.*FILE\\s*=)", sy, ignore.case = TRUE)), syntax_lines = length(sy),
      data_file_name_has_ampersand = any(grepl("&", fl, fixed = TRUE)),
      sav_header_product_is_spss_27 = grepl("SPSS STATISTICS.* 27\\.", prod),
      article_names_spss_26 = grepl("SPSS 26\\.0", x),
      value_label_typos_present = isTRUE("Strenght" %in% vl && all(c("Ommisions_PRE", "Commisions_PRE") %in% names(a))))

## ============================================================================================
## CLAIM 8 [COMMENT MATERIAL]  (finding F8, Section 8)
##   The data file holds individual school grades, age, gender and PE hours that make 107 of 120
##   rows unique; no anonymisation or consent for open release is stated.
## Check written by the audit (no author code covers this claim):

expect("C-8", columns_in_file = 22, rows_in_file = 120, tol = 0, note = "Dimensions of the deposited data file; exact")
expect("C-8", unique_rows_age_gender = 0, unique_rows_age_gender_PE = 1, unique_rows_with_three_grades = 107, tol = 0,
       note = "Rows that are the only row with their combination of values (counts only): age and gender; adding weekly PE hours; adding the German, mathematics and English grades. Exact counts")
expect("C-8", unique_rows_after_dropping_subject_grades = 69, unique_rows_with_mean_grade_rounded = 21, tol = 0,
       note = "Same count with the mean grade (kept) in place of the three subject grades, and with the mean grade rounded to a whole grade")
expect("C-8", subject_grade_columns_used_by_syntax = 0, tol = 0, note = "The syntax names none of the three subject-grade columns")
expect("C-8", table1_grade_F = 1.57, tol = 0.005, note = "Table 1 grade row F, computed from the mean-grade column; two decimals printed, half a unit of the last digit")
expect("C-8", table1_grade_p = 0.201, tol = 0.0005, note = "Table 1 grade row p, computed from the mean-grade column; three decimals printed, half a unit of the last digit")
expect("C-8", email_like_strings_in_file = 0, anonymisation_claimed_in_article = FALSE, tol = 0,
       note = "A pattern search of the binary data file, and a search of the article text for the word anonymous or anonymised")

a <- load_all(); sy <- syntax_lines(); x <- paper_text()
uq <- function(df, cols) { k <- do.call(paste, c(df[cols], sep = "|")); t <- table(k); sum(t[k] == 1) }
b <- a; b$SchoolGrade <- round(b$SchoolGrade)
d <- load_completers(); o <- oneway(d$SchoolGrade, d$Intervention)
raw <- readBin(file.path(PKG_DIR, "Altermann_LongtermExercise&Attention.sav"), "raw", file.size(file.path(PKG_DIR, "Altermann_LongtermExercise&Attention.sav")))
txt <- rawToChar(raw[raw >= as.raw(32) & raw < as.raw(127)])
check("C-8", columns_in_file = ncol(a), rows_in_file = nrow(a), unique_rows_age_gender = uq(a, c("Age", "Gender")),
      unique_rows_age_gender_PE = uq(a, c("Age", "Gender", "PE_week")),
      unique_rows_with_three_grades = uq(a, c("Age", "Gender", "PE_week", "German", "Math", "English")),
      unique_rows_after_dropping_subject_grades = uq(a, c("Age", "Gender", "PE_week", "SchoolGrade")),
      unique_rows_with_mean_grade_rounded = uq(b, c("Age", "Gender", "PE_week", "SchoolGrade")),
      subject_grade_columns_used_by_syntax = sum(grepl("\\b(German|Math|English)\\b", sy)),
      table1_grade_F = o$F, table1_grade_p = o$p,
      email_like_strings_in_file = length(regmatches(txt, gregexpr("[A-Za-z0-9._-]+@[A-Za-z0-9-]+[.][A-Za-z]{2,}", txt))[[1]]),
      anonymisation_claimed_in_article = grepl("anonym", x, ignore.case = TRUE))

## ---- summary of every audit-written check above ----
claims_summary()
