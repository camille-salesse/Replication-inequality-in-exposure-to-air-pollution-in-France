# Inequality in Exposure to Air Pollution in France

Replication package for *Inequality in Exposure to Air Pollution in France, Bringing Pollutant Cocktails into the Picture*, by Camille Salesse, 2026

## Run it

```r
install.packages(c("arrow", "data.table", "fixest", "ggplot2"))
source("code/01_replication.R")
```

That single file reproduces every table and figure of the paper in about two minutes. Results are written to `output/tables` and `output/figures`. Requires R 4.2 or later.

## Contents

| Path | What it is |
|---|---|
| `code/01_replication.R` | Reproduces all tables and figures |
| `code/00_prepare_data.R` | Built the distributed data, not needed for replication |
| `data/` | Five Parquet files, one per pair of years, plus a variable dictionary |
| `output/` | Written by the replication script |

The database is split by pairs of years so that each file stays small. The replication script stacks them back together.

See `data/variable_dictionary.csv` for every variable.



Code under the MIT licence. Data derived from public sources and redistributed in aggregated form for replication purposes.
