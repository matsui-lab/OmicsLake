# OmicsLake

OmicsLake provides a versioned, on-disk omics data management system for Bioconductor.
It standardizes tables to Parquet and objects to RDS/QS, enabling reproducible pipelines with simple read/write APIs.

## Quick start

```r
library(OmicsLake)

ol_init("atlas")
ol_write("counts", counts_df)
ol_commit("raw import"); ol_label("raw")
se <- ol_read_se("counts", ref="@raw", backing="hdf5")
```
