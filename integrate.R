# INSTRUCTIONS  INSTRUCTIONS  INSTRUCTIONS INSTRUCTIONS INSTRUCTIONS INSTRUCTIONS INSTRUCTIONS

# CODE AUTHOR:  Mark Allan Hatcher 7.7.2016 (see hello to me at markallanhatcher@gmail.com  :) )
# For some really cool machine learning / data visualisation examples please see:
# www.thefactmachine.com


# CODE PURPOSE: using raw export data, classify data according to the high tech / medium tech
# classifications contained in: "concordances/tech_lookups.csv"   These classifications
# map to basic ANZSIC codes. But the raw data contains no ANZSIC codes.   So, we have
# an HS10 to ANZSIC concordance mapping table here: "Concordance_HS_ANZSIC.csv"

# In the raw data (as at December, 2015) there were about 15977 HS10 Codes.
# Therefore we match these to "Concordance_HS_ANZSIC.csv" The HS10 codes
# that do not match are called "Unmatched"

# Next we try to match the ANZSIC codes in "Concordance_HS_ANZSIC.csv" to the
# high tech classifications in: "concordances/tech_lookups.csv" .  
# Some of these codes will not match.  These unmatched codes are called: "Other"

# CODE PLAN OF ATTACK:
# There are three files to be read. 1 of these is the raw data [ df_me_exports <- fn_read_me_data() ]
# The other two are concordances: a) HS10 to ANZSIC mapping b) ANZIC to High Tech mapping
# These three files are compared and eventually a final concordance file is produced. This is called:
# "df_master_lu"  This file is guaranteed to match to every row in the raw data.
# From this file, the raw data is classified into a data.frame called: "df_by_year_by_tech"  
# The structure of this data.frame is: year (columns) and by tech_type (23 rows).
# This data frame is then output as: "outputs/output.xlsx"


# INSTRUCTIONS:
# 1) COPY the raw data from TRED.  To do this. Please see a related repo
# stored in origin	P:/OTSP/export_intelligence
# run the script:  "load_from_tred.R" stored in the above network location
# this produces an rda file which will be saved in:
# "P:/OTSP/export_intelligence/inputs/Exports_By_Country[recent_date].rda"
# get this file (the raw data) and then copy this to this repo's location
# and store it in the location: /inputs/raw_data.rda  [note that you will have to change the file name]

# 2) RUN this script.  This script will produce a very basic (i.e. unformatted) file stored
# in outputs/output.xlsx

# 3) COPY the most (i.e rightmost) recent column of output.xlsx (see above) into a formatted
# version of the spreadsheet (for a formatted version of the spreadsheet see:
# "NZ_HighTech_Merchandise_Exports_2000-2014_as_at_20160513.xlsx" )


# 4) Send the formatted spreadsheet to Andrew MCCallum.

# 5) Relax - celebrate the completion of a task that will make an improvement to
# NZ's economy.

# TO DO
# 1) Incorporate the TRED script (mentioned above) into this repo
# 2) Add functionality to format the XL spreadsheet.

# =============================================================================


# clear working space
rm(list = ls())

# load libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(WriteXLS)

# set some options
options(stringsAsFactors = FALSE)
options(scipen=999)


## PPL using MAC, outside of MBIE envirnment does not need to run it.
if( Sys.info()['sysname'] == 'Windows' ){
  ## set up R environment ---------------------------------------------------
  source("P:/R/common.Rprofile")
}


# function to read in source (i.e. raw data)
source("R/fn_read_me_data.R")

# reads and prepares the HS10 to anzsic to concordance.
source("R/fn_read_hs10_to_anzsic.R")

# this function creates an HS10 to high tech / medium tech lookup
source("R/fn_create_HS10_tech_lu.R")

# this function creates a category called "Other" this is stuff
# in the HS10_to_ANZSIC mapping but not in the "tech_lookups.csv"
source("R/fn_create_df_other.R")

# this function creates a data.frame called "df_unclassified_lu"
# it contains all the HS10 codes in the raw data but not in "df_hS10_to_anzsic"
source("R/fn_create_unmatched_lu.R")

# ========================
# End of the starting stuff...wir lassen programming:
# =======================

# read in and process raw merchandise exports (me) data
df_me_exports <- fn_read_me_data()

# read in HS10 to ANZSIC mapping
df_hS10_to_anzsic <- fn_read_hs10_to_anzsic()
# ASSERT: primary key = HS10
stopifnot(length(unique(df_hS10_to_anzsic$HS10)) == nrow(df_hS10_to_anzsic))

# This maps HS10 to high tech / medium tech classification
df_HS10_tech_lu <- fn_create_HS10_tech_lu(df_hS10_to_anzsic)
# ASSERT: primary key = HS10
stopifnot(length(unique(df_HS10_tech_lu$HS10)) == nrow(df_HS10_tech_lu))

# create data.frame of in(df_hS10_to_anzsic) but_not_in(df_HS10_tech_lu)
df_other_lu <- fn_create_df_other(df_hS10_to_anzsic, df_HS10_tech_lu)
# ASSERT: primary key = HS10
stopifnot(length(unique(df_other_lu$HS10)) == nrow(df_other_lu))

# ASSERT: df_hS10_to_anzsic has been partitioned into: df_HS10_tech_lu + df_other_lu
stopifnot(nrow(df_hS10_to_anzsic) == (nrow(df_HS10_tech_lu) + nrow(df_other_lu)))

df_unclassified <- fn_create_unmatched_lu(df_me_exports, df_hS10_to_anzsic)
# ASSERT: primary key = HS10
stopifnot(length(unique(df_unclassified$HS10)) == nrow(df_unclassified))

# create a master lookup data.frame
df_master_lu <- bind_rows(df_HS10_tech_lu, df_other_lu, df_unclassified)

# ASSERT: df_master_lu contains all the HS10 codes in the original data
stopifnot(sum(!unique(df_me_exports$HS10) %in% df_master_lu$HS10) == 0)
# ASSERT: primary key = HS10
length(unique(df_master_lu$HS10)) == nrow(df_master_lu)

# NOW, that we have created df_master_lu, keep only the necessary stuff
rm(list = ls()[!(ls() %in% c('df_master_lu','df_me_exports'))])


# combine the original data with the lookup information
df_combined <- inner_join(df_me_exports, df_master_lu, by = c("HS10" = "HS10")) %>% 
                rename(date = Date, year = Year, total = Total) %>% ungroup()

# Assert that the value of all years before 2015 equals a known total
stopifnot(df_combined %>% filter(year < 2015) %>% summarise(total = sum(total)) %>% .$total == 572063404521)

df_by_year_by_tech <- df_combined %>% group_by(year, sort, tech_type, main, sub) %>% 
                      summarise(total = sum(total)) %>% 
                      arrange(year, sort) %>% select(-sort) %>%
                      spread(year, total) %>% as.data.frame()

WriteXLS("df_by_year_by_tech", "outputs/output.xlsx")

