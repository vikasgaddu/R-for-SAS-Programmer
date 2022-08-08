# R Programming for SAS Programmer ------

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^-----
# Importing Data -----
# First task we will learn is to bring data in R data structure.
# Just like in SAS we have datalines, libname statement, proc import etc

## Reading Raw data - SAS Dataline equivalent -----

library(tidyverse)

# SAS stores information in dataset, while R stores it in data frame.
# tibble is a upgraded data frame which overcomes some of the drawbacks 
# of data frame like inconsistent return values.

vs_raw <- tribble(~subjid, ~dbp, ~sbp, ~visit,
                  '1001', 88, 128, 'Screening',
                  '1001', 89, 130, 'Week 1',
                  '1001', 85, 125, 'Week 2',
                  '1001', NA, 135, 'Week 3',
                  )

typeof(vs_raw)
class(vs_raw)

### Printing data and metadata -----
# Looking at the contents of your data frame
str(vs_raw)
print(vs_raw)

# Checking out the metadata of the data frame.
attributes(vs_raw)

# Looking at both data and metadata
# Load a package before using function defined in that package.
library(logr)
put(vs_raw) # May give error of log not open. Ignore it for now.

## Reading from CSV/EXCEL file ----
raw_path <- "./rawdata"
sdtm_path <- "./sdtmdata"

### Read CSV -----
vs_raw_csv <- 
  read_csv(file.path(raw_path, "VS.csv"))

### Read Excel Method 1 -----
library(readxl)
vs_raw_xlsx <- 
  read_excel(file.path(raw_path, "VS.xlsx"))
vs_raw_xlsx 

# Returns tibble
typeof(vs_raw_xlsx)
class(vs_raw_xlsx)

### Real Excel Method 2----
library(openxlsx)
vs_raw_xlsx <- 
  read.xlsx(file.path(raw_path, "VS.xlsx"),1)

typeof(vs_raw_xlsx)
class(vs_raw_xlsx)
## Reading RDS file -----
#using forward pipe operator
library(logr)
ae_rds <- 
  read_rds(file.path(sdtm_path, "ae.rds")) %>% put()

## Reading SAS data set -----
library(haven)
dm_sas <- read_sas(file.path(raw_path,"dm.sas7bdat"))

## Reading TXT file -----

dm_txt <- read.table(file.path(raw_path,"dm_sas.txt"), header = TRUE)
dm_txt

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^-----
# Exporting Data ------

output_path <- './export'
output_path

## Writing to CSV/Excel -----
### Write to CSV file ------
write_csv(vs_raw_csv,file.path(output_path,'vs.csv'))

### Writing to Excel Method 1 ----
library(writexl)
write_xlsx(vs_raw_xlsx,file.path(output_path,'vs1.xlsx'))

### Writing to Excel Method 2 ----
library(openxlsx)
write.xlsx(vs_raw_xlsx,file.path(output_path,'vs2.xlsx'))

## Writing to RDS file -----
write_rds(vs_raw_cvs, file.path(output_path,'vs.rds'))

## Writing SAS Data set -----

library(haven)
write_sas(dm_sas, file.path(output_path,'dm_sas.sas7bdat'))

## Writing to TXT file ----
write.table(dm_sas, file.path(output_path,'dm_sas.txt'), row.names = FALSE, col.names = TRUE, sep = "\t")

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^-----
# Equivalent to DATA STEP###############################

#keep = select ------
