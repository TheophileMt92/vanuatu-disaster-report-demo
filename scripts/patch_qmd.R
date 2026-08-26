# ---------------------------------------------------------------------------
# patch_qmd.R
#
# Applies the demo-specific changes to index.qmd, in place. Idempotent: running
# it twice is harmless. This is the complete set of differences between the
# demonstration report and the original client report.
#
#   1. Adds a banner declaring that every figure is simulated.
#   2. Credits both authors.
#   3. Fixes one hard-coded lowercase "data/" path that only resolves on a
#      case-insensitive filesystem such as macOS.
# ---------------------------------------------------------------------------

QMD <- path.expand("~/Desktop/Work/vanuatu-demo/index.qmd")
stopifnot(file.exists(QMD))

x <- readLines(QMD, warn = FALSE)
n_before <- length(x)

# --- 1. author credit ------------------------------------------------------

i <- grep('^author:', x)
if (length(i) == 1 && !grepl("Holtz", x[i])) {
  x[i] <- 'author: "Théophile L. Mouton and Yan Holtz"'
  cat("  author line updated\n")
} else {
  cat("  author line already set, skipped\n")
}

# --- 2. case-sensitive path fix -------------------------------------------

j <- grep('read\\.csv\\("data/council_province_lookup\\.csv"\\)', x)
if (length(j)) {
  x[j] <- sub('read\\.csv\\("data/council_province_lookup\\.csv"\\)',
              'read.csv(here::here("data", "council_province_lookup.csv"))',
              x[j])
  cat("  council lookup path fixed on", length(j), "line(s)\n")
} else {
  cat("  council lookup path already fixed, skipped\n")
}

# --- 3. synthetic data banner ---------------------------------------------

MARK <- "<!-- synthetic-data-banner -->"

banner <- c(
  MARK,
  '<div style="border-left:5px solid #b3261e; background:#fdecea; color:#4a1210;',
  '            padding:1.1rem 1.35rem; border-radius:5px; margin:1.75rem 0;',
  '            line-height:1.55;">',
  '<strong style="display:block; font-size:1.08rem; margin-bottom:.45rem;">',
  'Demonstration report: every figure on this page is simulated',
  '</strong>',
  'This is a public demonstration of a disaster damage and response estimation',
  'pipeline originally built for the Government of Vanuatu. All values shown here,',
  'including baselines, damage estimates, resource requirements and financial',
  'figures, are <strong>synthetic data</strong>, generated from scratch by',
  '<code>simulate_data.R</code> in this repository. They do not describe Vanuatu',
  'and must not be cited or reused as if they did.',
  '<br><br>',
  'What is real is everything around the numbers: the geography, the indicator',
  'and attribute structure, the sector coverage, and every line of calculation,',
  'aggregation and presentation logic. The point of the demonstration is the',
  'pipeline, not the results.',
  '</div>',
  ''
)

if (any(grepl(MARK, x, fixed = TRUE))) {
  cat("  banner already present, skipped\n")
} else {
  k <- grep("^# Intro\\s*$", x)[1]
  if (is.na(k)) stop("could not find the '# Intro' heading to anchor the banner")
  x <- append(x, banner, after = k - 1)
  cat("  banner inserted before line", k, "\n")
}

writeLines(x, QMD)
cat(sprintf("\ndone: %d lines -> %d lines\n", n_before, length(x)))
