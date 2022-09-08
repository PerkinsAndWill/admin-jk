library(tidyverse)
library(sf)
library(tidytransit)
library(lubridate)
library(readxl)

proj_dir <- "P:/M-R/PAAC General Planning Services 2021.0196"
proj_data_dir <- "06 Analysis/PAAC Origin-Destination Data"

## READ CSV ====
# read_csv()
# read_excel()
data_df <- read_csv(
  file.path(proj_dir,
            proj_data_dir,
            "paac-OD-data-dump.csv"))


## READ GTFS ====
gtfs <- read_gtfs("data/SunMetro/gtfs_v2.zip")
# Set servicepatern_id
gtfs <- set_servicepattern(gtfs)
