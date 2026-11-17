# Helper to format a single cell as "F; p" (2 decimals for F, 3 for p, no leading 0 if < 1)
fmt_cell <- function(res) {
  f_str <- sub("^0\\.", ".", sprintf("%.2f", res[["F"]]))
  p_val <- res[["p"]]
  p_str <- if (p_val < 0.001) {
    "$< .001$"
  } else {
    sub("^0\\.", ".", sprintf("%.3f", p_val))
  }
  paste0(f_str, "; ", p_str)
}

# Helper to load source file only until specified line
# Source - https://stackoverflow.com/a/12215731
# Posted by Matthew Plourde
# Retrieved 2026-11-17, License - CC BY-SA 3.0
source2 <- function(file, start, end, ...) {
  file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed), ...)
}
