###############################################################################
#                                                                             #
#  Inequality in Exposure to Air Pollution in France                          #
#  Bringing Pollutant Cocktails into the Picture                              #
#  Camille Salesse                                                            #
#                                                                             #
#  01_replication.R                                                           #
#                                                                             #
#  PURPOSE                                                                    #
#  Reproduce every table and figure of the paper from the distributed data.   #
#  Running this file from top to bottom is sufficient. No other input is      #
#  needed and nothing outside the package is read or written.                 #
#                                                                             #
#  REQUIREMENTS                                                               #
#  R 4.2 or later, and the packages arrow, data.table, fixest and ggplot2.    #
#  Install them with                                                          #
#    install.packages(c("arrow", "data.table", "fixest", "ggplot2"))          #
#                                                                             #
#  RUNNING TIME                                                               #
#  About two minutes on a standard laptop.                                    #
#                                                                             #
#  OUTPUT                                                                     #
#  output/tables, one csv per table of the paper                              #
#  output/figures, one pdf and one png per figure of the paper                #
#  The console also prints every table, with the paper numbering.             #
#                                                                             #
###############################################################################

## ----------------------------------------------------------------- 0. SETUP

rm(list = ls())

pkgs <- c("arrow", "data.table", "fixest", "ggplot2")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing))
  stop("Missing packages, install them with install.packages(c(\"",
       paste(missing, collapse = "\", \""), "\"))")

library(arrow)
library(data.table)
library(fixest)
library(ggplot2)

setFixest_notes(FALSE)

# The working directory must be the root of the replication package, the
# folder that contains code, data and output. If you opened the .Rproj file
# or used Session, Set Working Directory, To Source File Location, adjust the
# line below accordingly.
if (!dir.exists("data") && dir.exists(file.path("..", "data")))
  setwd("..")
stopifnot(dir.exists("data"))

dir_out <- "output"
dir_fig <- file.path(dir_out, "figures")
dir_tab <- file.path(dir_out, "tables")
for (d in c(dir_out, dir_fig, dir_tab))
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)


## ------------------------------------------------------- 1. LOAD THE DATA
#
# The database is distributed as five Parquet files, one per pair of years,
# so that every file stays small. They are simply stacked back together.

files <- list.files("data", pattern = "^exposure_.*\\.parquet$",
                    full.names = TRUE)
stopifnot(length(files) == 5)

db <- rbindlist(lapply(files, read_parquet))
setorder(db, annee, IRIS)

cat("\n=============================================================\n")
cat(" DATA LOADED\n")
cat("=============================================================\n")
cat("Neighbourhood-years   ", nrow(db), "\n")
cat("Neighbourhoods        ", uniqueN(db$IRIS), "\n")
cat("Urban attraction areas", uniqueN(db$AAV2020), "\n")
cat("Period                ", paste(range(db$annee), collapse = " to "), "\n\n")

# Two analysis samples. The paper reports results for urban attraction areas
# excluding Paris, and treats the Paris area separately in the appendix.
df_np    <- db[TAAV2017 %in% 1:4]
df_paris <- db[TAAV2017 == 5]

# The fifteen mixtures. Together they partition polluted days, which is what
# makes the decomposition of Table 4 exact.
mono_vars  <- c("no2_2", "pm25_2", "pm10_2", "o3_2")
multi_vars <- c("no2_pm25_2", "no2_pm10_2", "no2_o3_2",
                "pm25_pm10_2", "pm25_o3_2", "pm10_o3_2",
                "no2_pm25_pm10_2", "pm25_no2_o3_2",
                "no2_pm10_o3_2", "pm10_pm25_o3_2",
                "no2_pm25_pm10_o3")
mix_vars   <- c(mono_vars, multi_vars)

pollutants <- c("jours_no2", "jours_pm25", "jours_pm10", "jours_o3")
aggregates <- c("sum_cocktail_day", "cocktail_days", "cocktail_sans_no2")

LAB <- c(
  no2_2 = "NO2 only", pm25_2 = "PM2.5 only", pm10_2 = "PM10 only",
  o3_2 = "O3 only",
  no2_pm25_2 = "NO2 + PM2.5", no2_pm10_2 = "NO2 + PM10",
  no2_o3_2 = "NO2 + O3", pm25_pm10_2 = "PM2.5 + PM10",
  pm25_o3_2 = "PM2.5 + O3", pm10_o3_2 = "PM10 + O3",
  no2_pm25_pm10_2 = "NO2 + PM2.5 + PM10", pm25_no2_o3_2 = "NO2 + PM2.5 + O3",
  no2_pm10_o3_2 = "NO2 + PM10 + O3", pm10_pm25_o3_2 = "PM2.5 + PM10 + O3",
  no2_pm25_pm10_o3 = "All four",
  jours_no2 = "NO2 days", jours_pm25 = "PM2.5 days",
  jours_pm10 = "PM10 days", jours_o3 = "O3 days",
  sum_cocktail_day = "Polluted days", cocktail_days = "Compound days",
  cocktail_sans_no2 = "Compound days without NO2",
  jours_pollues_sans_o3 = "Polluted days excluding O3")

SIZE <- c("Small UAAs" = "1:2", "Medium UAAs" = "3", "Large UAAs" = "4")


## ------------------------------------------------------------ 2. FUNCTIONS

# Population-weighted mean
pw <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)

banner <- function(txt) {
  cat("\n\n=============================================================\n")
  cat(" ", txt, "\n")
  cat("=============================================================\n")
}

stars <- function(p) fifelse(is.na(p), "",
  fifelse(p < .01, "***", fifelse(p < .05, "**", fifelse(p < .1, "*", ""))))

# Equation (1) of the paper. Ventile indicators, urban area by year fixed
# effects, population weights, standard errors clustered by neighbourhood.
ventile_coefs <- function(d, y, fe = "AAV2020^annee") {
  x <- as.numeric(d[[y]])
  if (all(is.na(x)) || uniqueN(x[!is.na(x)]) <= 1) return(NULL)
  m <- tryCatch(feols(as.formula(paste0(y, " ~ i(vingtile, ref = 1) | ", fe)),
                      data = d, weights = ~p_pop, cluster = ~IRIS),
                error = function(e) NULL)
  if (is.null(m)) return(NULL)
  ct <- as.data.frame(coeftable(m))
  ct <- ct[grepl("^vingtile::", rownames(ct)), , drop = FALSE]
  cf <- data.table(vingtile = as.integer(sub("vingtile::", "", rownames(ct))),
                   estimate = ct[, 1], se = ct[, 2], p = ct[, 4])
  rbind(data.table(vingtile = 1L, estimate = 0, se = 0, p = 1),
        cf)[order(vingtile)]
}

# The gap of the paper. Average exposure of the richest 30 percent of
# neighbourhoods minus that of the poorest 30 percent, within urban area and
# year. Ventiles 7 to 14 are excluded from this specification.
TOP_BOTTOM <- 6

gap <- function(d, y, fe = "AAV2020^annee") {
  empty <- list(estimate = NA_real_, se = NA_real_, p = NA_real_)
  if (!y %in% names(d)) return(empty)
  x <- as.numeric(d[[y]])
  if (all(is.na(x)) || uniqueN(x[!is.na(x)]) <= 1) return(empty)
  dd <- d[vingtile <= TOP_BOTTOM | vingtile >= 21 - TOP_BOTTOM]
  dd[, rich := as.integer(vingtile >= 21 - TOP_BOTTOM)]
  if (uniqueN(dd$rich) < 2) return(empty)
  m <- tryCatch(feols(as.formula(paste0(y, " ~ rich | ", fe)),
                      data = dd, weights = ~p_pop, cluster = ~IRIS),
                error = function(e) NULL)
  if (is.null(m)) return(empty)
  ct <- as.data.frame(coeftable(m))
  if (!"rich" %in% rownames(ct)) return(empty)
  list(estimate = ct["rich", 1], se = ct["rich", 2], p = ct["rich", 4])
}

fmt <- function(r, k = 2) if (is.na(r$estimate)) NA_character_ else
  paste0(round(r$estimate, k), stars(r$p))

# Value residualised on urban area by year means, recentred on the overall
# mean. Any comparison of group averages must go through this, since
# affluent neighbourhoods are over-represented in larger and more polluted
# urban areas.
residualise <- function(d, v, by = NULL) {
  x <- data.table(y = as.numeric(d[[v]]), w = d$p_pop,
                  g = paste(d$AAV2020, d$annee),
                  h = if (is.null(by)) "all" else as.character(d[[by]]))
  x[, ybar := sum(y * w, na.rm = TRUE) / sum(w, na.rm = TRUE), by = g]
  x[, mu   := sum(y * w, na.rm = TRUE) / sum(w, na.rm = TRUE), by = h]
  x$y - x$ybar + x$mu
}

save_table <- function(x, name) {
  print(as.data.frame(x))
  fwrite(x, file.path(dir_tab, paste0(name, ".csv")))
  invisible(x)
}

theme_paper <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.caption = element_text(size = 8, colour = "grey35", hjust = 0))

save_figure <- function(p, name, w = 24, h = 11) {
  ggsave(file.path(dir_fig, paste0(name, ".png")), p,
         width = w, height = h, units = "cm", dpi = 300, bg = "white")
  ggsave(file.path(dir_fig, paste0(name, ".pdf")), p,
         width = w, height = h, units = "cm", bg = "white")
  cat("   figure saved,", name, "\n")
}

COL <- c("#1B3A5C", "#BC2830", "#2B70CF", "#E8A33D", "#7A9E7E", "#8B5E83")


## ----------------------------------------------- 3. TABLE 1, EXPOSURE LEVELS

banner("TABLE 1. Exposure levels")

t1 <- rbindlist(lapply(c(aggregates, pollutants), function(v) {
  out <- data.table(indicator = LAB[[v]])
  for (nm in names(SIZE)) {
    d <- db[TAAV2017 %in% eval(parse(text = SIZE[[nm]]))]
    out[[nm]] <- round(pw(d[[v]], d$p_pop), 2)
  }
  out[["Outside Paris"]] <- round(pw(df_np[[v]], df_np$p_pop), 2)
  out[["Paris UAA"]]     <- round(pw(df_paris[[v]], df_paris$p_pop), 2)
  out
}))
save_table(t1, "table01_exposure_levels")

cat("\nSample sizes reported in the table footer\n")
counts <- rbindlist(list(
  data.table(field = "All UAAs", units = uniqueN(db$IRIS),
             unit_years = nrow(db)),
  data.table(field = "Outside Paris", units = uniqueN(df_np$IRIS),
             unit_years = nrow(df_np)),
  data.table(field = "Paris UAA", units = uniqueN(df_paris$IRIS),
             unit_years = nrow(df_paris))))
save_table(counts, "table01b_sample_sizes")


## --------------------------------------------- 4. TABLE 2, THE FIFTEEN MIXES

banner("TABLE 2. The fifteen pollutant mixtures")

t2 <- rbindlist(lapply(mix_vars, function(v) {
  out <- data.table(mixture = LAB[[v]],
                    type = fifelse(v %in% mono_vars, "Single", "Compound"))
  for (nm in names(SIZE)) {
    d <- db[TAAV2017 %in% eval(parse(text = SIZE[[nm]]))]
    out[[nm]] <- round(pw(d[[v]], d$p_pop), 2)
  }
  out[["Outside Paris"]] <- round(pw(df_np[[v]], df_np$p_pop), 2)
  out[["Paris UAA"]]     <- round(pw(df_paris[[v]], df_paris$p_pop), 2)
  out
}))
setorder(t2, type, -`Outside Paris`)
save_table(t2, "table02_mixtures")

cat("\nCheck, the mixtures partition polluted days\n")
cat("  sum of the fifteen mixtures ",
    round(sum(sapply(mix_vars, function(v) pw(df_np[[v]], df_np$p_pop))), 3),
    "\n  polluted days               ",
    round(pw(df_np$sum_cocktail_day, df_np$p_pop), 3), "\n")


## --------------------------------------------- 5. TABLE 3, INCOME GRADIENT

banner("TABLE 3. Income gradient in exposure, UAAs excluding Paris")

gap_row <- function(d, v, label) {
  g <- gap(d, v)
  mu <- pw(d[[v]], d$p_pop)
  data.table(indicator = label, mean = round(mu, 2), gap = fmt(g),
             pct_of_mean = if (!is.na(g$estimate) && abs(mu) > .05)
               round(100 * g$estimate / mu, 1) else NA_real_)
}

t3 <- rbindlist(c(
  lapply(c("sum_cocktail_day", "jours_pollues_sans_o3", "cocktail_days",
           "cocktail_sans_no2"),
         function(v) if (v %in% names(df_np)) gap_row(df_np, v, LAB[[v]])),
  lapply(pollutants, function(v) gap_row(df_np, v, LAB[[v]])),
  lapply(names(SIZE), function(nm)
    gap_row(db[TAAV2017 %in% eval(parse(text = SIZE[[nm]]))],
            "sum_cocktail_day", nm))))
save_table(t3, "table03_income_gradient")

a <- gap(df_np, "sum_cocktail_day")$estimate
if ("jours_pollues_sans_o3" %in% names(df_np)) {
  b <- gap(df_np, "jours_pollues_sans_o3")$estimate
  cat("\nAttenuation of the traffic-related gradient by adding ozone, ",
      round(100 * (1 - abs(a) / abs(b)), 1), " percent\n", sep = "")
}


## --------------------------------- 6. TABLE 4, DECOMPOSITION BY MIXTURE

banner("TABLE 4. Decomposition of the income gap by mixture")

decompose <- function(d) {
  total <- gap(d, "sum_cocktail_day")
  parts <- rbindlist(lapply(mix_vars, function(v) {
    g <- gap(d, v)
    data.table(variable = v, mixture = LAB[[v]],
               type = fifelse(v %in% mono_vars, "Single", "Compound"),
               with_no2 = grepl("no2", v),
               days = pw(d[[v]], d$p_pop),
               contribution = fifelse(is.na(g$estimate), 0, g$estimate),
               se = g$se, p = g$p)
  }))
  parts[, total := total$estimate]
  parts[]
}

dec_np <- decompose(df_np)
t4 <- dec_np[order(contribution),
  .(mixture, days = round(days, 2),
    contribution = paste0(round(contribution, 2), stars(p)))]
t4 <- rbind(t4, data.table(mixture = "All polluted days",
                           days = round(sum(dec_np$days), 2),
                           contribution = paste0(round(dec_np$total[1], 2), "***")))
save_table(t4, "table04_decomposition")

cat("\nPartition check\n")
cat("  sum of the fifteen contributions ", round(sum(dec_np$contribution), 4),
    "\n  aggregate gap                    ", round(dec_np$total[1], 4), "\n")
stopifnot(abs(sum(dec_np$contribution) - dec_np$total[1]) < 1e-6)


## ---------------------------- 7. APPENDIX TABLE, MIXTURES WITH AND WITHOUT NO2

banner("APPENDIX TABLE. Mixtures with and without NO2")

groups <- rbindlist(c(
  list(decompose(df_np)[, field := "UAAs excluding Paris"]),
  lapply(names(SIZE), function(nm)
    decompose(db[TAAV2017 %in% eval(parse(text = SIZE[[nm]]))])[, field := nm]),
  list(decompose(df_paris)[, field := "Paris UAA"])))

t_groups <- groups[, .(days = round(sum(days), 1),
                       gap = round(sum(contribution), 2)),
                   by = .(field, group = fifelse(with_no2,
                                                 "Mixtures containing NO2",
                                                 "Mixtures without NO2"))]
t_groups[, pct := round(100 * gap / days, 1)]
save_table(t_groups, "tableA_mixture_groups")


## ------------------------------- 8. APPENDIX TABLE, COMPOSITION AT BOTH ENDS

banner("APPENDIX TABLE. Composition of polluted days at both ends")

MAIN_CUT <- 2   # mixtures below two days per year are grouped as Other
main_mix <- dec_np[days >= MAIN_CUT, variable]

composition <- function(d, dec) {
  dd <- d[vingtile <= TOP_BOTTOM | vingtile >= 21 - TOP_BOTTOM]
  dd[, rich := as.integer(vingtile >= 21 - TOP_BOTTOM)]
  s <- sum(dd[rich == 1]$p_pop) / sum(dd$p_pop)
  out <- rbindlist(lapply(mix_vars, function(v) {
    b  <- dec[variable == v, contribution]
    mu <- pw(dd[[v]], dd$p_pop)
    data.table(mixture = fifelse(v %in% main_mix, LAB[[v]], "Other mixtures"),
               poorest = mu - s * b, richest = mu + (1 - s) * b)
  }))
  out[, .(poorest = sum(poorest), richest = sum(richest)), by = mixture]
}

comp_np <- composition(df_np, dec_np)
comp_np[, gap := richest - poorest]
setorder(comp_np, -poorest)
comp_np <- rbind(comp_np, data.table(mixture = "Total",
                                     poorest = sum(comp_np$poorest),
                                     richest = sum(comp_np$richest),
                                     gap = sum(comp_np$gap)))
comp_np[, (2:4) := lapply(.SD, round, 2), .SDcols = 2:4]
save_table(comp_np, "tableA_composition")


## ------------------------------------------- 9. APPENDIX TABLE, PARIS PROFILE

banner("APPENDIX TABLE. The Paris urban area, income profile")

t_paris <- rbindlist(lapply(c("sum_cocktail_day", "cocktail_days",
                              "jours_no2", "jours_o3"), function(v) {
  cf <- ventile_coefs(df_paris, v)
  if (is.null(cf)) return(NULL)
  g <- function(k) paste0(round(cf[vingtile == k, estimate], 2),
                          stars(cf[vingtile == k, p]))
  data.table(indicator = LAB[[v]], V5 = g(5), V10 = g(10), V15 = g(15),
             V20 = g(20), top_bottom_30 = fmt(gap(df_paris, v)))
}))
save_table(t_paris, "tableA_paris_profile")

# The upturn at the top of the Paris distribution is not internal to the city
# of Paris. Re-estimating on the neighbourhoods of the municipality alone,
# with year fixed effects since there is a single area, it disappears.
paris_city <- df_paris[insee == "75056"]
if (nrow(paris_city) > 1000) {
  cat("\nParis municipality alone,", uniqueN(paris_city$IRIS),
      "neighbourhoods\n")
  cf <- ventile_coefs(paris_city, "sum_cocktail_day", fe = "annee")
  cat("  ventile 15 ", round(cf[vingtile == 15, estimate], 2),
      "\n  ventile 20 ", round(cf[vingtile == 20, estimate], 2), "\n")
}


## ------------------------------------------ 10. APPENDIX TABLE, ROBUSTNESS

banner("APPENDIX TABLE. Robustness")

db[, regime := fcase(annee <= 2015, "2012-2015, all at 4 km",
                     annee <= 2017, "2016-2017, NO2 at 1 km",
                     default = "2018-2021, NO2 1 km and PM 2 km")]
df_np[, regime := fcase(annee <= 2015, "2012-2015, all at 4 km",
                        annee <= 2017, "2016-2017, NO2 at 1 km",
                        default = "2018-2021, NO2 1 km and PM 2 km")]

gap_unweighted <- function(d, y) {
  dd <- d[vingtile <= TOP_BOTTOM | vingtile >= 21 - TOP_BOTTOM]
  dd[, rich := as.integer(vingtile >= 21 - TOP_BOTTOM)]
  m <- feols(as.formula(paste0(y, " ~ rich | AAV2020^annee")),
             data = dd, cluster = ~IRIS)
  ct <- as.data.frame(coeftable(m))
  paste0(round(ct["rich", 1], 2), stars(ct["rich", 4]))
}

# Ventiles recomputed within each urban area and year, as an alternative to
# the national ranking used in the paper
df_np[, n_units := .N, by = .(AAV2020, annee)]
df_np[, vingtile_uaa := NA_integer_]
df_np[n_units >= 40, vingtile_uaa := {
  r <- frank(mediane, ties.method = "first")
  as.integer(ceiling(20 * r / .N))
}, by = .(AAV2020, annee)]

gap_within <- function(d, y) {
  dd <- d[!is.na(vingtile_uaa)]
  dd <- dd[vingtile_uaa <= TOP_BOTTOM | vingtile_uaa >= 21 - TOP_BOTTOM]
  dd[, rich := as.integer(vingtile_uaa >= 21 - TOP_BOTTOM)]
  m <- feols(as.formula(paste0(y, " ~ rich | AAV2020^annee")),
             data = dd, weights = ~p_pop, cluster = ~IRIS)
  ct <- as.data.frame(coeftable(m))
  paste0(round(ct["rich", 1], 2), stars(ct["rich", 4]))
}

t_rob <- rbindlist(list(
  data.table(specification = "Baseline, population-weighted",
             gap = fmt(gap(df_np, "sum_cocktail_day"))),
  data.table(specification = "Unweighted",
             gap = gap_unweighted(df_np, "sum_cocktail_day")),
  data.table(specification = "Excluding 2020",
             gap = fmt(gap(df_np[annee != 2020], "sum_cocktail_day"))),
  data.table(specification = "2012 to 2016 only",
             gap = fmt(gap(df_np[annee <= 2016], "sum_cocktail_day"))),
  data.table(specification = "2017 to 2021 only",
             gap = fmt(gap(df_np[annee >= 2017], "sum_cocktail_day"))),
  data.table(specification = "Split municipalities only",
             gap = fmt(gap(df_np[nchar(IRIS) == 9], "sum_cocktail_day"))),
  data.table(specification = "Within-UAA income ventiles",
             gap = gap_within(df_np, "sum_cocktail_day")),
  data.table(specification = "Additive area and year fixed effects",
             gap = fmt(gap(df_np, "sum_cocktail_day", fe = "AAV2020 + annee")))))

if ("jours_pollues_sans_o3" %in% names(df_np))
  t_rob <- rbind(t_rob, data.table(
    specification = "Excluding O3 from the count",
    gap = fmt(gap(df_np, "jours_pollues_sans_o3"))))

t_rob <- rbind(t_rob, rbindlist(lapply(sort(unique(df_np$regime)), function(r)
  data.table(specification = r,
             gap = fmt(gap(df_np[regime == r], "sum_cocktail_day"))))))
save_table(t_rob, "tableA_robustness")


## ------------------------------------- 11. APPENDIX TABLE, THREE INCOME GROUPS

banner("APPENDIX TABLE. Two against three income groups")

three <- copy(df_np)
three[, grp := fcase(vingtile <= TOP_BOTTOM, "bottom",
                     vingtile >= 21 - TOP_BOTTOM, "top", default = "middle")]
three[, grp := factor(grp, levels = c("bottom", "middle", "top"))]

t_three <- rbindlist(lapply(c("sum_cocktail_day", "jours_no2", "jours_o3",
                              "cocktail_sans_no2"), function(v) {
  m <- feols(as.formula(paste0(v, " ~ grp | AAV2020^annee")),
             data = three, weights = ~p_pop, cluster = ~IRIS)
  ct <- as.data.frame(coeftable(m))
  data.table(indicator = LAB[[v]],
             two_groups = fmt(gap(df_np, v)),
             three_groups = paste0(round(ct["grptop", 1], 2),
                                   stars(ct["grptop", 4])))
}))
save_table(t_three, "tableA_three_groups")


## ------------------------------------------------- 12. TEMPORAL EVOLUTION

banner("EVOLUTION. Levels and gap, year by year")

evo <- rbindlist(lapply(list(list(df_np, "Outside Paris"),
                             list(df_paris, "Paris UAA")), function(z) {
  d0 <- z[[1]]
  rbindlist(lapply(sort(unique(d0$annee)), function(a) {
    d <- d0[annee == a]
    g <- gap(d, "sum_cocktail_day")
    gn <- gap(d, "jours_no2")
    mu <- pw(d$sum_cocktail_day, d$p_pop)
    data.table(field = z[[2]], year = a, level = round(mu, 1),
               gap_days = round(g$estimate, 2),
               gap_pct = round(100 * g$estimate / mu, 2),
               gap_no2 = round(gn$estimate, 2))
  }))
}))
save_table(evo, "table_evolution_by_year")

banner("EVOLUTION. Distribution of the improvement, UAAs excluding Paris")

gains <- copy(df_np[annee %in% c(2012, 2013, 2020, 2021)])
gains[, period := fifelse(annee <= 2013, "start", "end")]
gains[, res := residualise(gains, "sum_cocktail_day", by = "period")]
gg <- dcast(gains[, .(days = pw(res, p_pop)), by = .(vingtile, period)],
            vingtile ~ period, value.var = "days")
gg[, `:=`(decline = start - end, decline_pct = 100 * (start - end) / start)]
save_table(gg[, lapply(.SD, round, 1)], "table_distribution_of_gains")

banner("EVOLUTION. Decline by mixture, UAAs excluding Paris")

decline_mix <- rbindlist(lapply(mix_vars, function(v) {
  x <- gains[, .(j = pw(get(v), p_pop)), by = period]
  data.table(mixture = LAB[[v]], with_no2 = grepl("no2", v),
             start = x[period == "start", j], end = x[period == "end", j])
}))
decline_mix[, `:=`(decline = start - end,
                   decline_pct = fifelse(start > .05,
                                         100 * (start - end) / start, NA_real_))]
setorder(decline_mix, -start)
save_table(decline_mix[, lapply(.SD, function(z)
  if (is.numeric(z)) round(z, 2) else z)], "table_decline_by_mixture")

cat("\nBy group\n")
print(as.data.frame(decline_mix[, .(start = round(sum(start), 1),
                                    end = round(sum(end), 1),
                                    decline_pct = round(100 * (sum(start) -
                                      sum(end)) / sum(start), 1)),
                                by = .(group = fifelse(with_no2,
                                  "Mixtures containing NO2",
                                  "Mixtures without NO2"))]))


## ------------------------------------------------------------- 13. FIGURES

banner("FIGURES")

curve <- function(d, y, label, fe = "AAV2020^annee") {
  cf <- ventile_coefs(d, y, fe)
  if (is.null(cf)) return(NULL)
  cf[, `:=`(series = label, low = estimate - 1.96 * se,
            high = estimate + 1.96 * se)]
  cf[]
}

# Figure, income gradient in exposure
f1 <- rbindlist(lapply(aggregates, function(v) {
  a <- curve(df_np, v, "Outside Paris")
  b <- curve(df_paris, v, "Paris UAA")
  rbind(a, b, fill = TRUE)[, variable := LAB[[v]]]
}))
f1[, variable := factor(variable, levels = LAB[aggregates])]
save_figure(
  ggplot(f1, aes(vingtile, estimate, colour = series, fill = series)) +
    geom_hline(yintercept = 0, colour = "grey60") +
    geom_ribbon(aes(ymin = low, ymax = high), alpha = .15, colour = NA) +
    geom_line(linewidth = .8) + geom_point(size = 1.3) +
    facet_wrap(~ variable, scales = "free_y") +
    scale_colour_manual(values = COL[1:2]) +
    scale_fill_manual(values = COL[1:2], guide = "none") +
    labs(x = "Income ventile", y = "Estimate and 95% CI, days per year") +
    theme_paper,
  "figure_gradient", 24, 11)

# Figure, two gradients of opposite sign
f2 <- rbind(
  rbindlist(lapply(pollutants, function(v)
    curve(df_np, v, sub(" days", "", LAB[[v]]))[, field := "Outside Paris"])),
  rbindlist(lapply(pollutants, function(v)
    curve(df_paris, v, sub(" days", "", LAB[[v]]))[, field := "Paris UAA"])))
f2[, series := factor(series, levels = c("NO2", "PM2.5", "PM10", "O3"))]
f2[, field := factor(field, levels = c("Outside Paris", "Paris UAA"))]
save_figure(
  ggplot(f2, aes(vingtile, estimate, colour = series, fill = series)) +
    geom_hline(yintercept = 0, colour = "grey60") +
    geom_ribbon(aes(ymin = low, ymax = high), alpha = .12, colour = NA) +
    geom_line(linewidth = .8) +
    facet_wrap(~ field, scales = "free_y") +
    scale_colour_manual(values = COL[1:4]) +
    scale_fill_manual(values = COL[1:4], guide = "none") +
    labs(x = "Income ventile", y = "Estimate and 95% CI, days per year") +
    theme_paper,
  "figure_two_gradients", 24, 11)

# Figure, contribution of each mixture
f3 <- dec_np[days >= 0.05]
setorder(f3, contribution)
f3[, mix := factor(mixture, levels = mixture)]
f3[, `:=`(low = contribution - 1.96 * se, high = contribution + 1.96 * se)]
save_figure(
  ggplot(f3, aes(mix, contribution, colour = type)) +
    geom_hline(yintercept = 0, colour = "grey60") +
    geom_pointrange(aes(ymin = low, ymax = high), linewidth = .6, size = .4) +
    coord_flip() +
    scale_colour_manual(values = c(Compound = COL[2], Single = COL[1])) +
    labs(x = NULL, y = "Richest 30% minus poorest 30%, days per year") +
    theme_paper,
  "figure_mixture_contributions", 20, 13)

# Figure, composition at both ends of the income scale
f4 <- melt(comp_np[mixture != "Total", .(mixture, poorest, richest)],
           id.vars = "mixture", variable.name = "group", value.name = "days")
f4[, group := fifelse(group == "poorest", "Poorest 30%", "Richest 30%")]
ord <- comp_np[mixture != "Total"][order(-poorest), mixture]
f4[, mixture := factor(mixture, levels = rev(ord))]
f4[, lab := fifelse(days >= 4, sprintf("%.0f", days), "")]
pal <- setNames(colorRampPalette(COL)(length(ord)), ord)
pal["Other mixtures"] <- "grey70"
save_figure(
  ggplot(f4, aes(group, days, fill = mixture)) +
    geom_col(width = .55, colour = "white", linewidth = .2) +
    geom_text(aes(label = lab), position = position_stack(vjust = .5),
              colour = "white", size = 3) +
    scale_fill_manual(values = pal, breaks = ord) +
    labs(x = NULL, y = "Days per year") + theme_paper +
    theme(legend.text = element_text(size = 8)),
  "figure_composition", 18, 14)

# Figure, trends by pollutant
f5 <- melt(
  df_np[, .(`Polluted days` = pw(sum_cocktail_day, p_pop),
            `NO2 days` = pw(jours_no2, p_pop),
            `PM2.5 days` = pw(jours_pm25, p_pop),
            `O3 days` = pw(jours_o3, p_pop)), by = annee],
  id.vars = "annee", variable.name = "series", value.name = "days")
save_figure(
  ggplot(f5, aes(annee, days, colour = series)) +
    geom_line(linewidth = .8) + geom_point(size = 1.3) +
    scale_colour_manual(values = c("grey30", COL[2], COL[4], COL[3])) +
    scale_x_continuous(breaks = seq(2012, 2021, 3)) +
    labs(x = NULL, y = "Days per year") + theme_paper,
  "figure_trends", 18, 11)

# Figure, gap in days and in percent of the mean
f6 <- melt(evo[, .(year, field, `Gap in days` = gap_days,
                   `Gap as % of the mean` = gap_pct)],
           id.vars = c("year", "field"), variable.name = "measure",
           value.name = "value")
save_figure(
  ggplot(f6, aes(year, value, colour = field)) +
    geom_hline(yintercept = 0, colour = "grey60") +
    geom_line(linewidth = .8) + geom_point(size = 1.3) +
    facet_wrap(~ measure, scales = "free_y") +
    scale_colour_manual(values = COL[1:2]) +
    scale_x_continuous(breaks = seq(2012, 2021, 3)) +
    labs(x = NULL, y = NULL) + theme_paper,
  "figure_gap_evolution", 24, 11)

# Figure, who benefited from cleaner air
f7a <- melt(gg[, .(vingtile, `2012-2013` = start, `2020-2021` = end)],
            id.vars = "vingtile", variable.name = "period", value.name = "days")
save_figure(
  ggplot(f7a, aes(vingtile, days, colour = period)) +
    geom_line(linewidth = .9) + geom_point(size = 1.5) +
    scale_colour_manual(values = COL[1:2]) +
    labs(x = "Income ventile", y = "Polluted days per year") + theme_paper,
  "figure_distribution_of_gains", 18, 11)

# Figure, gradient by size of urban area
f8 <- rbindlist(lapply(c("sum_cocktail_day", "jours_no2", "jours_o3"),
                       function(v) {
  a <- rbindlist(lapply(names(SIZE), function(nm)
    curve(db[TAAV2017 %in% eval(parse(text = SIZE[[nm]]))], v, nm)))
  b <- curve(df_paris, v, "Paris UAA")
  rbind(a, b, fill = TRUE)[, variable := LAB[[v]]]
}))
f8[, series := factor(series, levels = c(names(SIZE), "Paris UAA"))]
save_figure(
  ggplot(f8, aes(vingtile, estimate, colour = series)) +
    geom_hline(yintercept = 0, colour = "grey60") +
    geom_line(linewidth = .8) +
    facet_wrap(~ variable, scales = "free_y") +
    scale_colour_manual(values = c(COL[4], COL[5], COL[1], COL[2])) +
    labs(x = "Income ventile", y = "Estimate, days per year") + theme_paper,
  "figure_gradient_by_size", 24, 11)


## ---------------------------------------------------------------- 14. DONE

banner("REPLICATION COMPLETE")
cat("Tables  ", length(list.files(dir_tab)), "files in", dir_tab, "\n")
cat("Figures ", length(list.files(dir_fig, pattern = "png$")),
    "figures in", dir_fig, "\n\n")
cat("Key numbers of the paper\n")
cat("  Gap in polluted days, UAAs excluding Paris   ",
    fmt(gap(df_np, "sum_cocktail_day")), "\n")
cat("  Gap in NO2 days                              ",
    fmt(gap(df_np, "jours_no2")), "\n")
cat("  Gap in O3 days                               ",
    fmt(gap(df_np, "jours_o3")), "\n")
cat("  Mixtures containing NO2, contribution        ",
    round(sum(dec_np[with_no2 == TRUE, contribution]), 2), "\n")
cat("  Mixtures without NO2, contribution           ",
    round(sum(dec_np[with_no2 == FALSE, contribution]), 2), "\n")

sessionInfo()
