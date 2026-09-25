###############################################################################
#                                                                             #
#  Inequality in Exposure to Air Pollution in France                          #
#  Bringing Pollutant Cocktails into the Picture                              #
#  Camille Salesse                                                            #
#                                                                             #
#  00_prepare_data.R                                                          #
#                                                                             #
#  PURPOSE                                                                    #
#  Turn the working database into the five compressed Parquet files that are  #
#  distributed with this replication package. This script is run once by the  #
#  author and is not needed to replicate the results. Users of the package    #
#  start directly from 01_replication.R.                                      #
#                                                                             #
#  INPUT                                                                      #
#  base_o3.rds, the analysis database built by the data construction scripts  #
#  from the INERIS reanalysis and the INSEE FILOSOFI files.                   #
#                                                                             #
#  OUTPUT                                                                     #
#  data/exposure_2012_2013.parquet through data/exposure_2020_2021.parquet    #
#  data/variable_dictionary.csv                                               #
#                                                                             #
###############################################################################

library(data.table)
library(arrow)

# ----------------------------------------------------------------- SETTINGS --

# Folder holding base_o3.rds
dir_source <- "C:/Users/camil/Desktop/code and data inequality in exposure to air pollution/revision_aes"

# Folder of the replication package, where data/ will be written
dir_package <- "C:/Users/camil/Desktop/replication_package"

dir_data <- file.path(dir_package, "data")
if (!dir.exists(dir_data)) dir.create(dir_data, recursive = TRUE)


# -------------------------------------------------------------------- INPUT --

db <- as.data.table(readRDS(file.path(dir_source, "base_o3.rds")))
cat("Source database,", nrow(db), "rows and", ncol(db), "columns\n")

# The analysis code refers to the count of polluted days as sum_cocktail_day
if ("jours_pollues" %in% names(db) && !"sum_cocktail_day" %in% names(db))
  setnames(db, "jours_pollues", "sum_cocktail_day")


# -------------------------------------------------------- VARIABLES TO KEEP --
#
# Only the variables that the replication code actually uses are distributed.
# Everything else is dropped, which keeps each file small enough for GitHub.

id_vars <- c("IRIS", "insee", "annee", "AAV2020", "TAAV2017",
             "pole_centre_air")

socio_vars <- c("mediane", "p_pop", "vingtile", "decile")

pollutant_vars <- c("jours_no2", "jours_pm25", "jours_pm10", "jours_o3")

aggregate_vars <- c("sum_cocktail_day", "cocktail_days", "cocktail_sans_no2",
                    "jours_pollues_sans_o3", "cocktail_sans_o3")

mono_vars  <- c("no2_2", "pm25_2", "pm10_2", "o3_2")
multi_vars <- c("no2_pm25_2", "no2_pm10_2", "no2_o3_2",
                "pm25_pm10_2", "pm25_o3_2", "pm10_o3_2",
                "no2_pm25_pm10_2", "pm25_no2_o3_2",
                "no2_pm10_o3_2", "pm10_pm25_o3_2",
                "no2_pm25_pm10_o3")

keep <- c(id_vars, socio_vars, pollutant_vars, aggregate_vars,
          mono_vars, multi_vars)
keep <- intersect(keep, names(db))

missing <- setdiff(c(id_vars, socio_vars, pollutant_vars, mono_vars,
                     multi_vars), names(db))
if (length(missing))
  warning("Variables absent from the source database, ",
          paste(missing, collapse = ", "))

out <- db[, ..keep]


# ------------------------------------------------------- TYPES AND CLEANING --
#
# Integer storage wherever possible, which roughly halves the file size for
# the day counts. Identifiers are stored as character so that leading zeros
# of the INSEE codes are preserved.

out[, IRIS  := as.character(IRIS)]
out[, insee := as.character(insee)]

count_vars <- intersect(c(pollutant_vars, aggregate_vars, mono_vars,
                          multi_vars), names(out))
for (v in count_vars) set(out, j = v, value = as.integer(round(out[[v]])))

for (v in intersect(c("annee", "TAAV2017", "vingtile", "decile",
                      "pole_centre_air"), names(out)))
  set(out, j = v, value = as.integer(out[[v]]))

if ("p_pop" %in% names(out))   out[, p_pop := round(p_pop, 1)]
if ("mediane" %in% names(out)) out[, mediane := as.integer(round(mediane))]

setorder(out, annee, IRIS)
cat("Distributed database,", nrow(out), "rows and", ncol(out), "columns\n")


# ---------------------------------------------------- SPLIT INTO FIVE FILES --
#
# The split is by pairs of years rather than by arbitrary row ranges, so that
# each file is meaningful on its own and the reassembly order does not matter.

periods <- list(c(2012, 2013), c(2014, 2015), c(2016, 2017),
                c(2018, 2019), c(2020, 2021))

# zstd gives the smallest files, gzip is the fallback if the arrow build
# was compiled without it
compression <- tryCatch(
  if (arrow::codec_is_available("zstd")) "zstd" else "gzip",
  error = function(e) "gzip")
cat("Parquet compression,", compression, "\n\n")

sizes <- rbindlist(lapply(periods, function(p) {

  chunk <- out[annee %in% p]
  f <- file.path(dir_data,
                 sprintf("exposure_%d_%d.parquet", p[1], p[2]))
  write_parquet(chunk, f, compression = compression, compression_level = 9)

  data.table(file = basename(f), years = paste(p, collapse = "-"),
             rows = nrow(chunk),
             size_mb = round(file.size(f) / 1024^2, 2))
}))

print(as.data.frame(sizes))
cat("\nTotal size,", round(sum(sizes$size_mb), 2), "MB\n")

if (max(sizes$size_mb) > 90)
  warning("At least one file exceeds 90 MB, which is close to the GitHub ",
          "limit of 100 MB per file. Consider splitting further.")


# --------------------------------------------------------- DATA DICTIONARY --

dict <- data.table(
  variable = c(
    "IRIS", "insee", "annee", "AAV2020", "TAAV2017", "pole_centre_air",
    "mediane", "p_pop", "vingtile", "decile",
    "jours_no2", "jours_pm25", "jours_pm10", "jours_o3",
    "sum_cocktail_day", "cocktail_days", "cocktail_sans_no2",
    "jours_pollues_sans_o3", "cocktail_sans_o3",
    "no2_2", "pm25_2", "pm10_2", "o3_2",
    "no2_pm25_2", "no2_pm10_2", "no2_o3_2", "pm25_pm10_2", "pm25_o3_2",
    "pm10_o3_2", "no2_pm25_pm10_2", "pm25_no2_o3_2", "no2_pm10_o3_2",
    "pm10_pm25_o3_2", "no2_pm25_pm10_o3"),
  description = c(
    "Neighbourhood identifier, IRIS code for split municipalities and INSEE code otherwise",
    "INSEE code of the municipality",
    "Year, 2012 to 2021",
    "Identifier of the urban attraction area, INSEE 2020 delineation",
    "Size class of the urban attraction area, 1 to 5, with 5 for the Paris area",
    "Equal to one when the unit belongs to the core of its urban attraction area",
    "Median standard of living of the unit, euros per year, FILOSOFI",
    "Population of the unit",
    "National income ventile, computed year by year over all units of the sample",
    "National income decile, computed year by year",
    "Days per year on which the daily mean of NO2 exceeds 25 microgrammes per cubic metre",
    "Days per year on which the daily mean of PM2.5 exceeds 15",
    "Days per year on which the daily mean of PM10 exceeds 45",
    "Days per year on which the daily maximum of O3 exceeds 100",
    "Polluted days, days on which at least one pollutant exceeds its guideline",
    "Compound days, days on which at least two pollutants exceed simultaneously",
    "Compound days on which NO2 does not exceed",
    "Polluted days counting only NO2, PM2.5 and PM10",
    "Compound days counting only NO2, PM2.5 and PM10",
    "Days on which NO2 alone exceeds",
    "Days on which PM2.5 alone exceeds",
    "Days on which PM10 alone exceeds",
    "Days on which O3 alone exceeds",
    "Days on which NO2 and PM2.5 exceed together and no other pollutant does",
    "Days on which NO2 and PM10 exceed together and no other pollutant does",
    "Days on which NO2 and O3 exceed together and no other pollutant does",
    "Days on which PM2.5 and PM10 exceed together and no other pollutant does",
    "Days on which PM2.5 and O3 exceed together and no other pollutant does",
    "Days on which PM10 and O3 exceed together and no other pollutant does",
    "Days on which NO2, PM2.5 and PM10 exceed together and O3 does not",
    "Days on which NO2, PM2.5 and O3 exceed together and PM10 does not",
    "Days on which NO2, PM10 and O3 exceed together and PM2.5 does not",
    "Days on which PM2.5, PM10 and O3 exceed together and NO2 does not",
    "Days on which all four pollutants exceed together"))

dict <- dict[variable %in% names(out)]
fwrite(dict, file.path(dir_data, "variable_dictionary.csv"))
cat("\nDictionary written,", nrow(dict), "variables described\n")


# -------------------------------------------------------------- FINAL CHECK --
#
# Read the five files back and verify that the reassembled database is
# identical to the one that was written.

back <- rbindlist(lapply(list.files(dir_data, "\\.parquet$", full.names = TRUE),
                         read_parquet))
setorder(back, annee, IRIS)
cat("\nReassembled,", nrow(back), "rows against", nrow(out), "in the source\n")
cat("Identical,", isTRUE(all.equal(as.data.frame(out), as.data.frame(back),
                                   check.attributes = FALSE)), "\n")
