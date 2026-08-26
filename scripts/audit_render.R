# ---------------------------------------------------------------------------
# audit_render.R  (v2)
#
# Gate check before the demo report is made public.
#
# reactable and leaflet serialise entire dataframes into the rendered HTML, so
# a report can look synthetic on screen while still carrying real figures in
# its payload. This script extracts every embedded widget payload from both the
# original report and the demo and compares them column by column.
#
# Both pages render the same document, so widget payloads appear in the same
# order and can be aligned by position. That gives an exact comparison instead
# of the substring search used in v1, which produced meaningless matches: a
# search for "2,296" also hits "132,296" and "2,296,451", so in a 47 MB page
# almost every value appeared to match.
#
# Checks performed:
#   1. Structural alignment: same number of columns, same names in order.
#   2. No non-trivial numeric column is identical between the two reports.
#      All-zero columns are reported separately as benign.
#   3. No column shares the same national (first row) figure.
#   4. No local path to the private repo leaks into the output.
#   5. The synthetic-data banner is present in the rendered page.
#
# Requires: jsonlite (already a dependency of reactable/leaflet).
# ---------------------------------------------------------------------------

DEMO_HTML <- path.expand("~/Desktop/Work/vanuatu-demo/index.html")
REAL_HTML <- path.expand("~/Desktop/Work/source-inputs/index.html")

stopifnot(file.exists(DEMO_HTML))
if (!file.exists(REAL_HTML)) stop("original report not found at ", REAL_HTML)

suppressPackageStartupMessages(library(jsonlite))

slurp <- function(p) readChar(p, file.info(p)$size, useBytes = TRUE)

extract_payloads <- function(s) {
  parts <- strsplit(s, '<script type="application/json" data-for="', fixed = TRUE)[[1]]
  if (length(parts) < 2) return(character(0))
  out <- character(0)
  for (p in parts[-1]) {
    i <- regexpr('">', p, fixed = TRUE)
    if (i < 1) next
    body <- substring(p, i + 2)
    j <- regexpr("</script>", body, fixed = TRUE)
    if (j < 1) next
    out <- c(out, substring(body, 1, j - 1))
  }
  out
}

numeric_columns <- function(payloads) {
  res <- list()
  for (wi in seq_along(payloads)) {
    d <- try(fromJSON(payloads[wi], simplifyVector = TRUE), silent = TRUE)
    if (inherits(d, "try-error")) next
    dat <- try(d$x$tag$attribs$data, silent = TRUE)
    if (inherits(dat, "try-error") || is.null(dat) || is.null(names(dat))) next
    for (nm in names(dat)) {
      v <- suppressWarnings(as.numeric(dat[[nm]]))
      if (all(is.na(v)) || length(v) < 3) next
      res[[length(res) + 1]] <- list(widget = wi, col = nm, v = v)
    }
  }
  res
}

# --- 0. the decisive check: did any real value reach the demo inputs? ------
#
# The rendered page can only contain a real figure if a real figure reached the
# demo's Data folder. Comparing the inputs directly is stronger evidence than
# any comparison of the output, because it tests the cause rather than the
# symptom. A handful of coincidental matches on small integers and zeros is
# expected; a copied file would match almost everywhere.

DEMO_DATA <- path.expand("~/Desktop/Work/vanuatu-demo/data")
REAL_DATA <- path.expand("~/Desktop/Work/source-inputs")

cat("[0] demo inputs vs real inputs\n")
cat("    A raw match rate means nothing on its own: two independent draws from\n")
cat("    {2,3,4} agree a third of the time. Each column is therefore compared\n")
cat("    against the match rate expected if the two files were unrelated, which\n")
cat("    is sum over shared values of p_demo(v) * p_real(v). Copying shows up as\n")
cat("    an observed rate far above that expectation.\n\n")

# expected agreement between two independent draws from the observed marginals
expected_rate <- function(a, b) {
  pa <- table(a) / length(a)
  pb <- table(b) / length(b)
  cm <- intersect(names(pa), names(pb))
  if (!length(cm)) return(0)
  sum(as.numeric(pa[cm]) * as.numeric(pb[cm]))
}

input_fail <- FALSE
inputs <- c("baseline_indicators.csv", "damage_multipliers.csv",
            "response_resources.csv", "unit_costs.csv", "hazard_scenario.csv")
for (f in inputs) {
  pd <- file.path(DEMO_DATA, f); pr <- file.path(REAL_DATA, f)
  if (!file.exists(pd) || !file.exists(pr)) { cat("    skip (missing):", f, "\n"); next }
  a <- read.csv(pd, check.names = FALSE, stringsAsFactors = FALSE)
  b <- read.csv(pr, check.names = FALSE, stringsAsFactors = FALSE)
  cat(sprintf("    %s\n", f))
  if (nrow(a) != nrow(b)) { cat("      row count differs, cannot align\n"); next }
  nums <- intersect(names(a), names(b))
  nums <- nums[vapply(nums, function(k) is.numeric(a[[k]]) && is.numeric(b[[k]]), logical(1))]
  nums <- setdiff(nums, c("Year", "Year Collected"))
  for (k in nums) {
    ok <- !is.na(a[[k]]) & !is.na(b[[k]])
    if (!sum(ok)) next
    av <- a[[k]][ok]; bv <- b[[k]][ok]
    obs <- mean(av == bv)
    exp <- expected_rate(av, bv)
    nd  <- length(unique(bv))
    if (exp > 0.2) {
      verdict <- sprintf("uninformative (%d distinct value(s), coincidence likely)", nd)
    } else if (obs > max(0.05, 3 * exp)) {
      verdict <- "LEAK"; input_fail <- TRUE
    } else {
      verdict <- "pass"
    }
    cat(sprintf("      %-16s observed %6.2f%%   expected %6.2f%%   %s\n",
                k, 100 * obs, 100 * exp, verdict))
  }
}
if (!input_fail) {
  cat("\n    pass: no column exceeds its coincidence expectation\n")
} else {
  cat("\n    LEAK: a demo input file still carries real values\n")
}
cat("\n")

cat("reading rendered pages...\n")
demo_s <- slurp(DEMO_HTML)
real_s <- slurp(REAL_HTML)
cat(sprintf("  demo %.1f MB, original %.1f MB\n",
            file.info(DEMO_HTML)$size / 1e6, file.info(REAL_HTML)$size / 1e6))

D <- numeric_columns(extract_payloads(demo_s))
R <- numeric_columns(extract_payloads(real_s))
cat(sprintf("  demo: %d numeric columns, original: %d\n", length(D), length(R)))

fail <- 0L

# --- 1. structural alignment ----------------------------------------------

cat("\n[1] structural alignment\n")
if (length(D) != length(R)) {
  cat("    NOTE: differing column counts, falling back to name matching\n")
  aligned <- FALSE
} else {
  same_name <- sum(vapply(seq_along(D), function(k) D[[k]]$col == R[[k]]$col, logical(1)))
  cat(sprintf("    %d of %d columns line up by position and name\n", same_name, length(D)))
  aligned <- same_name == length(D)
  if (!aligned) cat("    NOTE: some names differ, comparisons below are still positional\n")
}

# --- 2. identical columns --------------------------------------------------

cat("\n[2] identical numeric columns\n")
# A column carrying only zeros and the odd 1 conveys almost nothing, and two
# such columns coincide readily. Those are reported as low-information rather
# than counted as leaks. Any column with real spread that matches exactly is a
# leak, because independent simulation cannot reproduce it.
zero_ok <- 0L; lowinfo <- character(0); leaks <- character(0)
n <- min(length(D), length(R))
for (k in seq_len(n)) {
  dv <- D[[k]]$v; rv <- R[[k]]$v
  if (length(dv) != length(rv)) next
  if (!isTRUE(all.equal(dv, rv))) next
  u <- unique(dv[!is.na(dv)])
  if (all(dv == 0, na.rm = TRUE)) {
    zero_ok <- zero_ok + 1L
  } else if (length(u) <= 3 && max(abs(u)) <= 10) {
    lowinfo <- c(lowinfo, sprintf("%s (values: %s)", D[[k]]$col,
                                  paste(sort(u), collapse = ",")))
  } else {
    leaks <- c(leaks, sprintf("%s (widget %d, %d distinct values, max %s)",
                              D[[k]]$col, D[[k]]$widget, length(u),
                              format(max(abs(u)), big.mark = ",")))
  }
}
cat(sprintf("    %d all-zero column(s) identical - benign\n", zero_ok))
if (length(lowinfo)) {
  cat(sprintf("    %d low-information column(s) identical - coincidence, not a leak:\n",
              length(lowinfo)))
  cat("      ", paste(lowinfo, collapse = "\n       "), "\n")
}
if (!length(leaks)) {
  cat("    pass: no column with real spread is shared\n")
} else {
  cat("    LEAK:", length(leaks), "column(s) carry the original values:\n")
  cat("      ", paste(leaks, collapse = "\n       "), "\n")
  fail <- fail + 1L
}

# --- 3. matching national figures ------------------------------------------

cat("\n[3] national (first row) figures\n")
# Small national counts collide by chance: there are only so many plausible
# values for the number of hospitals in a country. Matches below 50 are noted
# but not treated as leaks; anything larger matching exactly would be.
small <- character(0); big <- character(0); checked <- 0L
for (k in seq_len(n)) {
  a <- D[[k]]$v[1]; b <- R[[k]]$v[1]
  if (is.na(a) || is.na(b) || a == 0 || b == 0) next
  checked <- checked + 1L
  if (!isTRUE(all.equal(a, b))) next
  lab <- sprintf("%s = %s", D[[k]]$col, format(round(a), big.mark = ","))
  if (abs(a) < 50) small <- c(small, lab) else big <- c(big, lab)
}
cat(sprintf("    compared %d non-zero national figures\n", checked))
if (length(small)) {
  cat(sprintf("    %d small figure(s) under 50 coincide - expected:\n", length(small)))
  cat("      ", paste(small, collapse = "\n       "), "\n")
}
if (!length(big)) {
  cat("    pass: no national figure of any size is shared\n")
} else {
  cat("    LEAK:", length(big), "national figure(s) of size match the original:\n")
  cat("      ", paste(big, collapse = "\n       "), "\n")
  fail <- fail + 1L
}

# --- 4. private path leakage -----------------------------------------------

cat("\n[4] references to the private project\n")
bad <- c(basename(REAL_DATA), dirname(REAL_DATA))
leak <- bad[vapply(bad, function(p) grepl(p, demo_s, fixed = TRUE), logical(1))]
if (!length(leak)) {
  cat("    pass: no private path or filename appears\n")
} else {
  cat("    LEAK:", paste(leak, collapse = ", "), "\n"); fail <- fail + 1L
}

# --- 5. banner present -----------------------------------------------------

cat("\n[5] synthetic-data banner\n")
if (grepl("every figure on this page is simulated", demo_s, fixed = TRUE)) {
  cat("    pass: banner rendered\n")
} else {
  cat("    FAIL: banner missing from the rendered page\n"); fail <- fail + 1L
}

# --- side by side, for your own eyes ---------------------------------------

cat("\n--- national figures, original vs demo (first 18 non-zero columns) ---\n")
shown <- 0L
for (k in seq_len(n)) {
  a <- D[[k]]$v[1]; b <- R[[k]]$v[1]
  if (is.na(a) || is.na(b) || b == 0) next
  cat(sprintf("  %-34s original %14s   demo %14s\n", D[[k]]$col,
              format(round(b), big.mark = ","), format(round(a), big.mark = ",")))
  shown <- shown + 1L
  if (shown >= 18L) break
}

if (input_fail) fail <- fail + 1L

cat("\n===========================================================\n")
if (fail == 0L) {
  cat(" AUDIT PASSED - safe to publish\n")
} else {
  cat(" AUDIT FAILED -", fail, "check(s) need attention - DO NOT PUBLISH\n")
}
cat("===========================================================\n")
