####################################################################################################
# RUN THE claims_script.R UNTIL ROW 236 BEFORE EXEUCTING THIS TEST
# ALSO MAKE SURE TO RUN THE FIRST PART OF THIS SCRIPT TO LOAD ALL VARIABLES AND HELPER FUNCTIONS
####################################################################################################

# Reset work space
rm(list = ls())

# Libraries
library(here)
library(dplyr)

# load util functions
source(here::here("pkg", "utils.R"))
source2(here::here("pkg", "claims_script.R"), 1, 236)

# Data 
AT_SAV <- file.path(here::here("pkg", "Altermann_LongtermExercise&Attention.sav"))
AT_RAW <- haven::read_sav(AT_SAV)
AT_D0 <- haven::zap_labels(AT_RAW)
AT_DONE <- !is.na(AT_D0$Concentration_KL_POST) & AT_D0$Concentration_KL_POST > 0
AT_D <- AT_D0[AT_DONE, ]
# Ensure AT_D has the data column G
AT_D$G <- factor(
  AT_D$Intervention,
  levels = 1:4,
  labels = c("endurance", "strength", "coordination", "control")
)

####################################################################################################
# Claims checks
####################################################################################################

#### Pretest consistency check
AT_D0$Concentration_KL_PRE_recalculated <- AT_D0$Speed_BZO_PRE - AT_D0$Ommisions_PRE - AT_D0$Commisions_PRE
AT_D0$ErrorScore_PRE_recalculated       <- (AT_D0$Ommisions_PRE + AT_D0$Commisions_PRE) / AT_D0$Speed_BZO_PRE * 100
AT_D0$diff_Concentration_KL_PRE       <- AT_D0$Concentration_KL_PRE - AT_D0$Concentration_KL_PRE_recalculated
AT_D0$diff_ErrorScore_PRE             <- AT_D0$ErrorScore_PRE - AT_D0$ErrorScore_PRE_recalculated

#### Posttest consistency check
AT_D0$Concentration_KL_POST_recalculated <- AT_D0$Speed_BZO_POST - AT_D0$Ommisions_POST - AT_D0$Commisions_POST
AT_D0$ErrorScore_POST_recalculated       <- (AT_D0$Ommisions_POST + AT_D0$Commisions_POST) / AT_D0$Speed_BZO_POST * 100
AT_D0$diff_Concentration_KL_POST       <- AT_D0$Concentration_KL_POST - AT_D0$Concentration_KL_POST_recalculated
AT_D0$diff_ErrorScore_POST             <- AT_D0$ErrorScore_POST - AT_D0$ErrorScore_POST_recalculated

#### Check age x gender strata per study group
df_aggregate <- AT_D |> dplyr::group_by(Intervention, Age, Gender) |> 
  dplyr::summarise(n = dplyr::n(), .groups = "drop_last")

#### Check average PE hours per study group
df_pe_hours <- AT_D |> dplyr::group_by(Intervention) |> 
  dplyr::summarise(mean_pe_hours = mean(PE_week))

#### Check average weekly leisure sport per study group
df_pe_hours <- AT_D |>
  dplyr::group_by(Intervention) |>
  dplyr::summarise(
    mean_pe_hours = mean(dplyr::coalesce(Sport_leisure_week, 0))
  )

#### Check group count from initial sample
df_dist_init <- AT_D0 |> dplyr::group_by(Intervention) |> 
  dplyr::summarise(n = dplyr::n(), .groups = "drop_last")

#### Recalculate main results while dropping three implausible rows
# Remove data points
AT_D_wo <- AT_D[!(AT_D$ID %in% c(10, 78, 96)),]
# Recalculate data values
AT_D_recalc <- AT_D
for (ph in c("PRE", "POST")) {
  speed  <- AT_D_recalc[[paste0("Speed_BZO_", ph)]]
  errors <- AT_D_recalc[[paste0("Ommisions_", ph)]] + AT_D_recalc[[paste0("Commisions_", ph)]]
  AT_D_recalc[[paste0("Concentration_KL_", ph)]] <- speed - errors
  AT_D_recalc[[paste0("ErrorScore_", ph)]]       <- 100 * errors / speed
}

# Build and print the complete LaTeX table
outcomes  <- c("Concentration_KL", "ErrorScore", "Speed_BZO")
out_names <- c("Concentration Score", "Error Score", "Speed Score")
contrasts <- c("ctrl", "end", "str", "coo")

datasets <- list(
  "Reported"      = AT_D,
  "Removal"       = AT_D_wo,
  "Recalculation" = AT_D_recalc
)

lines <- c(
  "\\begin{table}[htbp]",
  "    \\centering",
  "    \\caption{Main results recalculated based on two different approaches for data correction}",
  "    \\label{tab:rec}",
  "    \\begin{tabular}{l cccc}",
  "        \\toprule",
  "        & \\textbf{Control} & \\textbf{Endurance} & \\textbf{Strength} & \\textbf{Coordination} \\\\",
  "        \\textbf{Contrast} & $(1, 1, 1, -3)$ & $(2, -1, -1, 0)$ & $(-1, 2, -1, 0)$ & $(-1, -1, 2, 0)$ \\\\",
  "        \\midrule"
)

for (k in seq_along(outcomes)) {
  lines <- c(lines, sprintf("        \\multicolumn{5}{l}{\\textbf{%s}} \\\\", out_names[k]))
  for (d_name in names(datasets)) {
    cells <- vapply(contrasts, function(j) {
      fmt_cell(AT_CON(outcomes[k], AT_W[[j]], datasets[[d_name]]))
    }, character(1))
    lines <- c(lines, sprintf("        \\quad %-13s & %s \\\\", d_name, paste(cells, collapse = " & ")))
  }
  if (k < length(outcomes)) {
    lines <- c(lines, "        \\addlinespace")
  }
}

lines <- c(
  lines,
  "        \\bottomrule",
  "        \\addlinespace[4pt]",
  "        \\multicolumn{5}{p{0.88\\textwidth}}{\\footnotesize \\textit{Note:} Each cell reports the $F$- and $p$-value ($F; p$). ``Removal'' indicates that observations with IDs 10, 78, and 96 are excluded, whereas ``Recalculation'' indicates that test scores are recomputed from the underlying raw counts. Note that for speed score only the control contrast is reported in the paper while the other contrasts are still available in the authors' code.}",
  "    \\end{tabular}",
  "\\end{table}"
)

cat(paste(lines, collapse = "\n"), "\n")

#### Recalculate the main results using winsorization as described in the prergistration
# Define helper function
winsorize_3sd <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  pmin(pmax(x, m - 3 * s), m + 3 * s)
}
# Create winsorized data frame
AT_D_win <- AT_D
for (i in c("Concentration_KL", "ErrorScore", "Speed_BZO")) {
  for (ph in c("PRE", "POST")) {
    col <- paste0(i, "_", ph)
    AT_D_win[[col]] <- winsorize_3sd(AT_D_win[[col]])
  }
}
# Build and print LaTeX table
outcomes  <- c("Concentration_KL", "ErrorScore", "Speed_BZO")
out_names <- c("Concentration Score", "Error Score", "Speed Score")
contrasts <- c("ctrl", "end", "str", "coo")

datasets <- list(
  "Reported"   = AT_D,
  "Winsorized" = AT_D_win
)

lines <- c(
  "\\begin{table}[htbp]",
  "    \\centering",
  "    \\caption{Main results after winsorizing pretest and posttest scores at $\\pm 3$ SD}",
  "    \\label{tab:win}",
  "    \\begin{tabular}{l cccc}",
  "        \\toprule",
  "        & \\textbf{Control} & \\textbf{Endurance} & \\textbf{Strength} & \\textbf{Coordination} \\\\",
  "        \\textbf{Contrast} & $(1, 1, 1, -3)$ & $(2, -1, -1, 0)$ & $(-1, 2, -1, 0)$ & $(-1, -1, 2, 0)$ \\\\",
  "        \\midrule"
)

for (k in seq_along(outcomes)) {
  lines <- c(lines, sprintf("        \\multicolumn{5}{l}{\\textbf{%s}} \\\\", out_names[k]))
  for (d_name in names(datasets)) {
    cells <- vapply(contrasts, function(j) {
      fmt_cell(AT_CON(outcomes[k], AT_W[[j]], datasets[[d_name]]))
    }, character(1))
    lines <- c(lines, sprintf("        \\quad %-10s & %s \\\\", d_name, paste(cells, collapse = " & ")))
  }
  if (k < length(outcomes)) {
    lines <- c(lines, "        \\addlinespace")
  }
}

lines <- c(
  lines,
  "        \\bottomrule",
  "        \\addlinespace[4pt]",
  "        \\multicolumn{5}{p{0.88\\textwidth}}{\\footnotesize \\textit{Note:} Each cell reports the $F$- and $p$-value ($F; p$). ``Winsorized'' indicates that pretest and posttest values beyond $\\pm 3$ SD from the sample mean are capped at the respective boundary. Note that for speed score only the control contrast is reported in the paper while the other contrasts are still available in the authors' code.}",
  "    \\end{tabular}",
  "\\end{table}"
)

cat(paste(lines, collapse = "\n"), "\n")

#### Calculate effect sizes
f_to_pes <- function(f)   f^2 / (1 + f^2)
pes_to_f <- function(pes) sqrt(pes / (1 - pes))
printed_pes <- c(
  Speed_BZO        = 0.09,
  ErrorScore       = 0.12,
  Concentration_KL = 0.18
)
print(data.frame(
  printed_pes = printed_pes,
  cohen_f     = round(pes_to_f(printed_pes), 2)
))


#### Recalculate main results using baseline adjustment and considering unequal variances
# Define analysis function
AT_CON_ANCOVA_HC3 <- function(o, w, data = AT_D) {
  dd <- data.frame(
    G    = factor(data$G),
    pre  = data[[paste0(o, "_PRE")]],
    post = data[[paste0(o, "_POST")]]
  )
  # Cell-means ANCOVA: post = mu_1*I(G=1) + ... + mu_4*I(G=4) + beta_pre*pre + e
  fit <- stats::lm(post ~ 0 + G + pre, data = dd)
  X   <- stats::model.matrix(fit)
  h   <- stats::hatvalues(fit)
  u2  <- stats::resid(fit)^2 / (1 - h)^2
  
  # HC3 covariance matrix 
  XtX_inv <- solve(crossprod(X))
  V_hc3   <- XtX_inv %*% crossprod(X, X * u2) %*% XtX_inv
  
  # Contrast across the 4 adjusted group means (weight 0 on pre)
  R     <- c(w, 0)
  est   <- sum(R * stats::coef(fit))
  se2   <- as.numeric(t(R) %*% V_hc3 %*% R)
  F_val <- est^2 / se2
  df2   <- fit$df.residual
  
  c(
    estimate = est,
    F        = F_val,
    df2      = df2,
    p        = stats::pf(F_val, 1, df2, lower.tail = FALSE)
  )
}

# Build and print the LaTeX table
outcomes  <- c("Concentration_KL", "ErrorScore", "Speed_BZO")
out_names <- c("Concentration Score", "Error Score", "Speed Score")
contrasts <- c("ctrl", "end", "str", "coo")

lines <- c(
  "\\begin{table}[htbp]",
  "    \\centering",
  "    \\caption{Main results with baseline adjustment and heteroscedasticity-consistent standard errors}",
  "    \\label{tab:ancova_hc3}",
  "    \\begin{tabular}{l cccc}",
  "        \\toprule",
  "        & \\textbf{Control} & \\textbf{Endurance} & \\textbf{Strength} & \\textbf{Coordination} \\\\",
  "        \\textbf{Contrast} & $(1, 1, 1, -3)$ & $(2, -1, -1, 0)$ & $(-1, 2, -1, 0)$ & $(-1, -1, 2, 0)$ \\\\",
  "        \\midrule"
)

for (k in seq_along(outcomes)) {
  lines <- c(lines, sprintf("        \\multicolumn{5}{l}{\\textbf{%s}} \\\\", out_names[k]))
  
  cells_rep <- vapply(contrasts, function(j) {
    fmt_cell(AT_CON(outcomes[k], AT_W[[j]], AT_D))
  }, character(1))
  lines <- c(lines, sprintf("        \\quad %-14s & %s \\\\", "Reported", paste(cells_rep, collapse = " & ")))
  
  cells_adj <- vapply(contrasts, function(j) {
    fmt_cell(AT_CON_ANCOVA_HC3(outcomes[k], AT_W[[j]], AT_D))
  }, character(1))
  lines <- c(lines, sprintf("        \\quad %-14s & %s \\\\", "ANCOVA (HC3)", paste(cells_adj, collapse = " & ")))
  
  if (k < length(outcomes)) {
    lines <- c(lines, "        \\addlinespace")
  }
}

lines <- c(
  lines,
  "        \\bottomrule",
  "        \\addlinespace[4pt]",
  "        \\multicolumn{5}{p{0.88\\textwidth}}{\\footnotesize \\textit{Note:} Each cell reports the $F$- and $p$-value ($F; p$). ``Reported'' reproduces the pooled-variance change-score contrasts ($df_2 = 92$). ``ANCOVA (HC3)'' adjusts posttest scores for baseline pretest scores via ANCOVA ($df_2 = 91$) and accounts for unequal group variances using HC3 heteroscedasticity-consistent standard errors. Note that for speed score only the control contrast is reported in the paper while the other contrasts are still available in the authors' code.}",
  "    \\end{tabular}",
  "\\end{table}"
)

cat(paste(lines, collapse = "\n"), "\n")