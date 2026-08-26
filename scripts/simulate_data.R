# ---------------------------------------------------------------------------
# simulate_data.R
#
# Generates a fully synthetic input dataset for the Vanuatu disaster damage
# and response estimation report.
#
# METHOD
#   Reads ONLY the structure of the original input files: geography, indicator
#   and attribute names, units, sources, years and the missing-value pattern.
#   Every numeric value is generated from scratch. No real figure is read,
#   perturbed, rescaled or carried through in any form.
#
#   Each area council is assigned a synthetic population weight. Each indicator
#   is given a plausible national total. That total is split across the
#   indicator's attributes using the share table below, then distributed across
#   councils by weight with lognormal noise.
#
# OUTPUT
#   Writes the four simulated input CSVs plus a simulated hazard scenario into
#   the demo project's data/ folder. Base R only, no package dependencies.
#
#   SOURCE points at a local, undistributed copy of the original client inputs,
#   already renamed to the descriptive filenames used throughout this project.
#   Only their structure is read; see METHOD above.
# ---------------------------------------------------------------------------

set.seed(20260826)

SOURCE <- path.expand("~/Desktop/Work/source-inputs")
DEMO <- path.expand("~/Desktop/Work/vanuatu-demo/data")

stopifnot(dir.exists(SOURCE), dir.exists(DEMO))

rd <- function(f) read.csv(file.path(SOURCE, f), check.names = FALSE, stringsAsFactors = FALSE)
wr <- function(d, f) {
  write.csv(d, file.path(DEMO, f), row.names = FALSE, na = "")
  cat(sprintf("  wrote  %-42s %6d rows\n", f, nrow(d)))
}
akey <- function(x) ifelse(is.na(x) | x == "", "__NA__", x)

# ---------------------------------------------------------------------------
# 1. Plausible national totals, one per indicator.
#    Vanuatu scale: ~272,000 people, ~62,000 households, 71 area councils.
# ---------------------------------------------------------------------------

TOTALS <- c(
  "Number Schools"           =      1900,
  "Number Teachers"          =      5200,
  "Number Students"          =     88000,
  "Number Towers"            =       420,
  "Household Electricity"    =     62000,
  "Household Cooking Fuel"   =     62000,
  "Energy Infrastructure"    =      9500,
  "Stable Crop Households"   =     46000,
  "Staple Crop Production"   =    210000,
  "Fishing Households"       =     14000,
  "Fishing Production"       =     38000,
  "Cash Crop Households"     =     33000,
  "Cash Crop Production"     =     96000,
  "Timber Households"        =      7500,
  "Timber Production"        = 480000000,
  "Population Sex"           =    272000,
  "Population Age"           =    272000,
  "Marital Status"           =    272000,
  "Functional Difficulties"  =     24000,
  "Employment Status"        =    272000,
  "Household Size"           =     62000,
  "Health Facility"          =       340,
  "Health Professionals"     =      1150,
  "Inpatient Beds"           =       620,
  "Public Health Staff"      =       900,
  "Hospital Staff"           =      1400,
  "Domestic Vessals"         =       260,
  "Household Transport"      =     62000,
  "Infrastructure"           =      1600,
  "Road Surface"             =      1150,
  "Household Type"           =    124000,
  "Household Appliances"     =     40000,
  "Household Roof Material"  =     62000,
  "Household Wall Material"  =     62000,
  "Rooms Per Household"      =     62000,
  "Household Drinking Water" =     62000,
  "Household Toilet"         =     62000,
  "Industry Type"            =      3100
)

# ---------------------------------------------------------------------------
# 2. How each indicator's total splits across its attributes.
#    Attribute strings must match the source data EXACTLY, typos included.
# ---------------------------------------------------------------------------

SHARES <- list(
  "Number Schools"  = c(ecce = .50, primary = .38, secondary = .12),
  "Number Teachers" = c(ecce = .18, primary = .55, secondary = .27),
  "Number Students" = c(ecce = .12, primary = .62, secondary = .26),

  "Number Towers"   = c(digicel = .55, vodafone = .45),

  "Household Electricity"  = c("battery lamp" = .30, "no access" = .12,
                               "solar system" = .22, "main grid" = .32,
                               "generator" = .04),
  "Household Cooking Fuel" = c("electricity" = .04, "bottle gas" = .18,
                               "open fire" = .58, "solar power" = .01,
                               "wood stove" = .19),
  "Energy Infrastructure"  = c("hydro" = .002, "solar" = .030,
                               "electrcity pole support" = .180,
                               "electrical terminals" = .090,
                               "high voltage poles" = .120,
                               "high voltage support poles" = .060,
                               "high voltage transformer substation" = .015,
                               "low voltage poles" = .400,
                               "low voltage street distrbution boxes" = .103),

  "Stable Crop Households" = c("island cabbage" = .22, banana = .20, taro = .17,
                               kumala = .16, manioc = .15, yam = .10),
  "Staple Crop Production" = c(taro = .24, kumala = .20, manioc = .19,
                               banana = .18, "island cabbage" = .11, yam = .08),
  "Fishing Households"     = c("coastal reefs" = .24, "inshore fishing" = .20,
                               lagoon = .13, "outer reefs" = .12, mangroves = .09,
                               "offshore fishing" = .09,
                               "pelagic/open ocean fish" = .08, freshwater = .05),
  "Fishing Production"     = c("reef fish" = .30, "deep bottom fish" = .20,
                               tuna = .18, "pelagic/open ocean fish" = .16,
                               invertebrates = .11, freshwater = .05),
  "Cash Crop Households"   = c(coconut = .30, kava = .26, cocoa = .14,
                               vanilla = .08, coffee = .07,
                               "tahitian lime" = .06, pepper = .05, noni = .04),
  "Cash Crop Production"   = c(coconut = .38, kava = .30, cocoa = .12,
                               coffee = .07, "tahitian lime" = .05,
                               vanilla = .04, noni = .03, pepper = .01),
  "Timber Households"      = c(whitewood = .20, natapoa = .16, mahogany = .14,
                               nangae = .13, "koyu/natora" = .12, pine = .11,
                               sandalwood = .08, kauri = .06),
  "Timber Production"      = c(whitewood = .24, mahogany = .20, kauri = .16,
                               pine = .14, natapoa = .11, "koyu/natora" = .09,
                               nangae = .06),

  "Population Sex"          = c(male = .51, female = .49),
  "Population Age"          = c("0-4" = .13, "5-11" = .17, "12-18" = .16,
                                "19-35" = .27, "36-54" = .18, "55+" = .09),
  "Marital Status"          = c("never married" = .44, married = .38,
                                defacto = .12, widowed = .04, separated = .02),
  "Functional Difficulties" = c(seeing = .28, walking = .22, hearing = .16,
                                remembering = .15, communication = .10,
                                selfcare = .09),
  "Employment Status"       = c("own consumption" = .34, "self-employed" = .18,
                                private = .16, unpaid = .12, government = .10,
                                voluntary = .06, employer = .04),
  "Household Size"          = c("1-3" = .30, "4-7" = .49, "8+" = .21),

  "Health Facility"      = c(aidpost = .58, dispensary = .28,
                             "health centre" = .12, hospital = .02),
  "Health Professionals" = c("nurse aid" = .30, "registered nurse" = .28,
                             "nurse practitioner" = .19, midwife = .17,
                             doctor = .06),
  "Inpatient Beds"       = c("health centre" = .62, dispensary = .38),
  "Public Health Staff"  = c("family health" = .17, administrative = .16,
                             "environmental health & sanitation" = .13,
                             "communicable diseases" = .12,
                             "noncommunicable diseases" = .11,
                             "health promotion" = .10,
                             "surveillance & emergency response" = .09,
                             "health centre" = .07, dispensary = .04,
                             "__NA__" = .01),
  "Hospital Staff"       = c("clinical nursing services" = .22,
                             "clinical medical services" = .17,
                             administrative = .14,
                             "nonclinical nursing services" = .11,
                             "allied services" = .09,
                             "nonclinical medical services" = .08,
                             "pharmacy services" = .07,
                             "dental services" = .06,
                             "biomedical services" = .05,
                             "__NA__" = .01),

  "Domestic Vessals"    = c(ships = 1),
  "Household Transport" = c(canoe = .38, boats = .26, "motor vehicle" = .22,
                            motorcycle = .14),
  "Infrastructure"      = c("fire hydrants" = .40, "main water valves" = .30,
                            "police stations" = .10, "permanent bridge" = .09,
                            "temporary bridge" = .06, wharf = .04,
                            airport = .02),
  "Road Surface"        = c(earth = .46, gravel = .28, "chips seal" = .13,
                            asphalt = .09, concrete = .04),

  "Household Type"          = c("number households" = .52,
                                "private households" = .48),
  "Household Appliances"    = c(radio = .34, tv = .28, refrigerator = .22,
                                freezer = .16),
  "Household Roof Material" = c(metal = .82, concrete = .10, wood = .08),
  "Household Wall Material" = c(metal = .38, wood = .34, concrete = .28),
  "Rooms Per Household"     = c("0-3" = .58, "4-6" = .33, "7+" = .09),

  "Household Drinking Water" = c(piped = .58, tank = .28, well = .14),
  "Household Toilet"         = c("pit latrine" = .52, "water sealed" = .20,
                                 flush = .18, vip = .10),

  "Industry Type" = c("wholesale and retail trade" = .24,
                      "accommodation and food" = .13,
                      "agriculture, forestry and fishing" = .10,
                      construction = .08, "other services" = .08,
                      transportation = .07, manufacturing = .06,
                      "administrative services" = .05,
                      "scientific and technical" = .04, education = .04,
                      "arts and entertainment" = .03, "real estate" = .03,
                      "financial and insurance" = .03,
                      "information and communication" = .02,
                      "health and social work" = .02,
                      "water supply and waste management" = .010,
                      "mining and quarrying" = .005,
                      "electricity and gas" = .005)
)

build_shares <- function(ind, atts) {
  k <- akey(atts)
  spec <- SHARES[[ind]]
  v <- rep(NA_real_, length(k))
  if (!is.null(spec)) v <- unname(spec[match(k, names(spec))])
  miss <- is.na(v)
  if (all(miss)) {
    v <- rep(1 / length(k), length(k))
  } else if (any(miss)) {
    v[miss] <- max(0.02, 1 - sum(v[!miss])) / sum(miss)
  }
  v <- v / sum(v)
  names(v) <- k
  v
}

# ---------------------------------------------------------------------------
# 3. Baseline file. Replace Value only, keep every structural column and the
#    missing-value pattern untouched.
# ---------------------------------------------------------------------------

cat("\nsimulating baseline...\n")
b  <- rd("baseline_indicators.csv")
ac <- b[["Area Council"]]

councils <- sort(unique(ac[!is.na(ac) & ac != ""]))
w  <- rlnorm(length(councils), 0, 0.55)
names(w) <- councils
mx <- max(w)
if ("Port Vila"  %in% councils) w["Port Vila"]  <- mx * 6.0   # urban centre
if ("Luganville" %in% councils) w["Luganville"] <- mx * 2.2   # second town

# The Area Council column mixes two granularities: the 71 area councils the
# report keeps, plus 17 island-level names (Malekula, Tanna, Espiritu Santo...)
# that index.qmd discards via its valid_councils filter. National totals must
# therefore be distributed across the counted councils only, otherwise a share
# of every total lands on rows the report throws away.
qmd   <- readLines(file.path(dirname(DEMO), "index.qmd"), warn = FALSE)
i0    <- grep("^region_order <- c\\(", qmd)[1]
i1    <- i0 + which(trimws(qmd[(i0 + 1):length(qmd)]) == ")")[1]
ro    <- gsub('"', "", unlist(regmatches(qmd[i0:i1], gregexpr('"[^"]+"', qmd[i0:i1]))))
valid <- setdiff(ro, c("National", "Torba", "Sanma", "Penama",
                       "Malampa", "Shefa", "Tafea"))
cat("  councils counted toward national totals:", length(valid), "\n")
if (length(valid) != 71) warning("expected 71 counted councils, got ", length(valid))

SH <- list()
for (ind in unique(b$Indicator)) {
  if (is.na(ind) || ind == "") next
  SH[[ind]] <- build_shares(ind, unique(b$Attribute[b$Indicator == ind]))
}

maxyear <- suppressWarnings(max(b$Year, na.rm = TRUE))
newval  <- rep(NA_real_, nrow(b))
groups  <- split(seq_len(nrow(b)), paste(b$Indicator, akey(b$Attribute), b$Year, sep = "\r"))

for (g in groups) {
  ind <- b$Indicator[g[1]]
  if (is.na(ind) || ind == "") next

  tot <- unname(TOTALS[ind])
  if (is.na(tot)) tot <- 1000

  sh <- SH[[ind]][akey(b$Attribute[g[1]])]
  if (is.na(sh)) sh <- 1 / max(1, length(SH[[ind]]))

  yr <- b$Year[g[1]]
  drift <- if (is.na(yr) || is.na(maxyear)) 1 else 1 + 0.02 * (yr - maxyear)
  target <- tot * unname(sh) * drift

  cw <- unname(w[b[["Area Council"]][g]])
  cw[is.na(cw)] <- stats::median(w)
  raw   <- cw * rlnorm(length(g), 0, 0.30)
  keep  <- b[["Area Council"]][g] %in% valid
  denom <- if (any(keep)) sum(raw[keep]) else sum(raw)
  newval[g] <- target * raw / denom
}

unit <- tolower(ifelse(is.na(b$Unit), "number", b$Unit))
newval <- ifelse(unit == "kilometer", round(newval, 1), round(newval))
newval[!is.na(newval) & newval < 0] <- 0
b$Value[!is.na(b$Value)] <- newval[!is.na(b$Value)]   # preserve the NA pattern
wr(b, "baseline_indicators.csv")

# ---------------------------------------------------------------------------
# 4. Damage multipliers. In the original these are a single constant per
#    intensity column, applied to every council and indicator, so the synthetic
#    version keeps that shape.
# ---------------------------------------------------------------------------

cat("\nsimulating damage multipliers...\n")
m <- rd("damage_multipliers.csv")
for (nm in c("Intensity 2", "Intensity 3", "Intensity 4", "Intensity 5")) {
  if (!nm %in% names(m)) next
  k <- switch(nm, "Intensity 2" = 0.10, "Intensity 3" = 0.25,
                  "Intensity 4" = 0.50, "Intensity 5" = 0.75)
  m[[nm]][!is.na(m[[nm]])] <- k
}
wr(m, "damage_multipliers.csv")

# ---------------------------------------------------------------------------
# 5. Response resources: quantity of each item per affected unit.
# ---------------------------------------------------------------------------

cat("\nsimulating response resources...\n")
r <- rd("response_resources.csv")

RES <- c(
  "Tent" = 1, "Solar Lamp" = 2, "Water" = 3, "Tin Fish" = 0.5, "Rice" = 0.4,
  "Low Voltage Poles Support" = 0.6, "High Voltage Pole Support" = 0.3,
  "Power Line Cable" = 25,
  "Island Cabbage Cuttings" = 20, "Taro Seedlings" = 15, "Kumala Cuttings" = 25,
  "Manioc Cuttings" = 18, "Yam Cuttings" = 12,
  "Medical Tent" = 1, "Emergency Health Kits" = 2, "Trauma and Surgical Kits" = 1,
  "Essential NCD Medicines" = 3, "Mosquito Nets" = 4,
  "Truck" = 0.05, "Fibreglass Boat" = 0.10, "Ship" = 0.02, "Fuel" = 40,
  "Chainsaw" = 0.20,
  "Kitchen Set" = 1, "Jerrycan 10L" = 2, "Sleeping Mat" = 3, "Blanket" = 3,
  "Tank" = 0.5, "Water Purifier Tablets" = 30, "Hygiene Kit" = 1
)

rv <- unname(RES[r$Indicator])
rv[is.na(rv)] <- 1
r$Value[!is.na(r$Value)] <- rv[!is.na(r$Value)]
wr(r, "response_resources.csv")

# ---------------------------------------------------------------------------
# 6. Unit replacement costs, in vatu.
# ---------------------------------------------------------------------------

cat("\nsimulating financial estimates...\n")
f <- rd("unit_costs.csv")

COST <- list(
  "Number Schools" = c(ecce = 3500000, primary = 8000000, secondary = 20000000),
  "Number Towers"  = c(digicel = 12000000, vodafone = 12000000),
  "Household Electricity" = c("battery lamp" = 4500, generator = 65000,
                              "main grid" = 120000, "solar system" = 85000,
                              "no access" = 0),
  "Energy Infrastructure" = c(hydro = 45000000, solar = 5000000,
                              "electrcity pole support" = 35000,
                              "electrical terminals" = 20000,
                              "high voltage poles" = 180000,
                              "high voltage support poles" = 90000,
                              "high voltage transformer substation" = 3500000,
                              "low voltage poles" = 75000,
                              "low voltage street distrbution boxes" = 40000),
  "Staple Crop Production" = c("island cabbage" = 180, banana = 120, taro = 260,
                               kumala = 150, manioc = 110, yam = 320),
  "Cash Crop Production"   = c(kava = 1200, coconut = 55, cocoa = 320,
                               coffee = 480, vanilla = 25000,
                               "tahitian lime" = 210, pepper = 900, noni = 90),
  "Timber Production"      = c(whitewood = 1, mahogany = 1, kauri = 1, pine = 1,
                               natapoa = 1, "koyu/natora" = 1, nangae = 1),
  "Health Facility" = c(hospital = 250000000, "health centre" = 40000000,
                        dispensary = 8000000, aidpost = 2500000),
  "Infrastructure"  = c(airport = 60000000, wharf = 25000000,
                        "permanent bridge" = 35000000,
                        "temporary bridge" = 6000000,
                        "police stations" = 12000000,
                        "fire hydrants" = 350000,
                        "main water valves" = 250000),
  "Road Surface"    = c(asphalt = 45000000, "chips seal" = 28000000,
                        concrete = 55000000, earth = 3500000, gravel = 7000000),
  "Household Roof Material" = c(concrete = 900000, metal = 450000, wood = 250000),
  "Household Wall Material" = c(concrete = 1400000, metal = 700000, wood = 400000),
  "Household Appliances"    = c(freezer = 95000, refrigerator = 65000,
                                tv = 40000, radio = 8000),
  "Household Drinking Water" = c(piped = 220000, tank = 180000, well = 90000),
  "Household Toilet"         = c(flush = 180000, "pit latrine" = 45000,
                                 vip = 90000, "water sealed" = 120000)
)

fv <- rep(NA_real_, nrow(f))
fk <- akey(f$Attribute)
for (i in seq_len(nrow(f))) {
  tab <- COST[[ f$Indicator[i] ]]
  if (!is.null(tab)) {
    hit <- tab[fk[i]]
    if (!is.na(hit)) fv[i] <- unname(hit)
  }
}
fv[is.na(fv)] <- 50000
f$Value[!is.na(f$Value)] <- fv[!is.na(f$Value)]
wr(f, "unit_costs.csv")

# ---------------------------------------------------------------------------
# 7. Hazard scenario. Councils kept so the maps stay geographically sensible,
#    cyclone intensities redrawn.
# ---------------------------------------------------------------------------

cat("\nsimulating hazard scenario...\n")
h <- rd("hazard_scenario.csv")
h$Intensity <- sample(2:4, nrow(h), replace = TRUE, prob = c(.35, .40, .25))
wr(h, "hazard_scenario.csv")

# ---------------------------------------------------------------------------
# 8. Sanity check: national totals in the latest year, for eyeballing.
# ---------------------------------------------------------------------------

# Each sector uses its own latest year, and only the counted councils, exactly
# as index.qmd does. Indicators recorded nationally (blank Area Council) are
# reported as such rather than as a misleading zero.
cat("\n--- synthetic national totals, as the report will compute them -----\n")
for (ind in sort(unique(b$Indicator))) {
  if (is.na(ind) || ind == "") next
  s <- b[b$Indicator == ind & !is.na(b$Value), ]
  if (!nrow(s)) next
  yr <- max(s$Year, na.rm = TRUE)
  s  <- s[s$Year == yr & s[["Area Council"]] %in% valid, ]
  if (!nrow(s)) {
    cat(sprintf("  %-28s yr %s  %16s\n", ind, yr, "national-level"))
  } else {
    cat(sprintf("  %-28s yr %s  %16s\n", ind, yr,
                format(round(sum(s$Value, na.rm = TRUE)), big.mark = ",")))
  }
}
cat("-------------------------------------------------------------------\n")
cat("\nDone. Synthetic inputs written to:\n  ", DEMO, "\n\n")
