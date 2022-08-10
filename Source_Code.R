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

## PROC REPORT like Syntax to create RTF file -----
library(reporter)
dm_sdtm <- read.csv(file.path(sdtm_path,'dm.csv'), na = c("NA"))
attributes(dm_sdtm)                                                       
class(dm_sdtm)

tbl <-create_table(dm_sdtm, show_cols = c('USUBJID','AGE','SEX', 'RACE', 'ETHNIC'),
                   first_row_blank = TRUE) %>% 
  define(USUBJID,id_var = TRUE)
rpt <- create_report(file.path(output_path,"dm_listing.rtf"), orientation = "portrait" , output_type = "RTF") %>% 
  page_header("Client: ABC", "Study: XZY") %>%
  titles("Listing 1.0","Analysis Data Subject Listing", blank_row = "above") %>% 
  footnotes("M= Male, F= Female", "Age is calculated as Randomization date minus Birth date.") %>% 
  add_content(tbl,align = "left") %>% 
  page_footer(Sys.time(), "Confidential","Page [pg] of [tpg]")
  
write_report(rpt) %>% put()

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^-----
#Statments that are Equivalent to DATA STEP###############################

##keep statement equivalent is SELECT ------

dm_selected <- dm_sdtm %>% select(c('USUBJID','AGE','SEX', 'RACE', 'ETHNIC'))
dm_selected

##DROP statement equivalent is also SELECT  -----

dm_drop <- dm_selected %>% select(-c('RACE', 'ETHNIC'))
dm_drop

## WHERE statement equivalent is FILTER -----
# Note the double equal sign for comparision
dm_filtered <- dm_sdtm %>% filter(SEX == 'M')
dm_filtered

## PROC SORT equivalent is ARRANGE------
ex <- tribble(~subjid,~exstdtc,~exendtc,
              1001,'NA','2020-02-28',
              1001,'2020-01-01','2020-01-15',
              1002,'2020-03-15','2020-03-28',
              1002,'NA','2020-04-15'              
              
)
ex_sorted <- ex %>% arrange(subjid, exstdtc)

## Creating Variable --SEQ variable ----

exseq <- ex %>% group_by(subjid) %>% 
  mutate(exseq = seq_along(subjid))

exseq

## Transposing wide to long (PROC TRANSPOSE) -----
exlong <- exseq %>% 
  pivot_longer(cols=c(exstdtc,exendtc), values_to = "exdtc")

## Selecting FIRST. record for a by group -----
exfirst <- exlong %>% 
  arrange(subjid,exdtc) %>% 
  group_by(subjid) %>% 
  filter(exdtc != 'NA') %>% 
  select(subjid, exdtc) %>% 
  slice(1)
exfirst

## Selecting LAST. record for a by group ------

exlast <- exlong %>% 
  arrange(subjid,desc(exdtc)) %>% 
  group_by(subjid) %>% 
  filter(exdtc != 'NA') %>% 
  select(subjid,exdtc) %>% 
  slice(1)
exlast

## Renaming a Variable ----

exfirst_r <- exfirst %>% 
  rename(rfxstdtc = exdtc)
exlast_r <- exlast %>% 
  rename(rfxendtc = exdtc)

## Merging datasets (MERGE)  -----

by_group = c("subjid"= "subjid")
rfdates <- exfirst_r %>% 
  left_join(exlast_r, by = by_group)
rfdates

# Another way to do the same is by using bind_cols
# Number of rows should match to use above function
#Common variables are renamed so not ideal for this case unless we drop them

rfxendtc <- exlast_r %>% 
  ungroup() %>% 
  select(c(rfxendtc))
rfxendtc
rfdates_ <- bind_cols(exfirst_r, rfxendtc)
rfdates_

## Setting dataset one underneath other (SET) ----

exset <- bind_rows(exfirst %>% 
                     mutate(name = "RFXSTDTC"),
                   exlast %>% 
                     mutate(name = "RFXENDTC"))
exset

## Transposing long to wide (PROC TRANSPOSE) -----

rfdates1 <- exset %>% 
  pivot_wider(id_cols=c(subjid),names_from = name, values_from=exdtc)
rfdates1

## Creating new variable using IF THEN ELSE-----

### Using IFELSE function -----

dm_sdtm %>% slice_sample(n=5)

dm_cat <- dm_sdtm %>% 
  mutate(AGECAT = ifelse(AGE < 30,  "Group 1",
                  ifelse(AGE <= 60, "Group 2",
                                    "Group 3") )) %>% 
  select(USUBJID, AGE,AGECAT) 

dm_cat %>% 
  slice_sample(n=5)

### Using CASE_WHEN function -----

dm_cat1 <- dm_sdtm %>% 
  mutate(AGECAT = case_when(
    AGE < 30 ~ "Group 1",
    AGE <= 60~ "Group 2",
    TRUE~"Group 3"
  ))

dm_cat1 %>% 
  select(USUBJID,AGE,AGECAT) %>% 
  slice_sample(n=5)


### Using C-Style IF THEN ELSE -----
year <- sample(2001:2005,20,replace=TRUE)
year
month <- sample(1:12, 20,replace = TRUE)
month
day <- sample(1:15, 20,replace = TRUE)
day
#inserting some missing values
year[1] <- NA

df_date <- tibble(year,month,day)

isodt <- Vectorize(function(yy,mm,dd){
  if (!is.na(dd)) {
    if (!is.na(yy)){yyc = sprintf("%4.0f", yy)}
    else {  yyc = "-"}
    
    if (!is.na(mm)){mmc = sprintf("%02.0f", mm)}
    else {mmc = "-"}
    
    ddc = sprintf("%02.0f", dd)
    # we can use str_c function instead of paste
    result = paste(yyc,mmc,ddc,sep='-')
  }
  else if (!is.na(mm)) {
    if (!is.na(yy)){yyc = sprintf("%4.0f", yy)}
    else {yyc = "-"}
    mmc = sprintf("%02.0f", mm)
    result = paste(yyc,mmc,sep='-') 
    
  }
  else if (!is.na(yy)) {
    yyc = sprintf("%4.0f", yy)
    result = yyc
    
  }
  else {
    result = ''
  }
  return(result)
}
)
df_date$isodtc <- isodt(df_date$year,df_date$month,df_date$day)
print(df_date)


##Converting numeric to character -----

### Using as.character function -----

year = 2001:2020
yearc = as.character(year)
yearc
str(yearc)
typeof(yearc)

### Using sprintf function ------

sprintf("%f", pi)
sprintf("%.3f", pi)
sprintf("%1.0f", pi)
sprintf("%5.1f", pi)
sprintf("%05.1f", pi)
sprintf("%+f", pi)
sprintf("% f", pi)
sprintf("%-10f", pi) # left justified

### Creating formats using VALUE function equivalent to PROC FORMAT -----
library(fmtr)
fmt_sex <- value(
  condition(x =='Male', "M"),
  condition(x =='Female', "F")
)
subjid <- c(1001:1004)
gender <- c('Female','Male','Female','Female')
tb_dm <- tibble(subjid,gender)
tb_dm_sex <- tb_dm %>% 
  mutate(
    sex = fapply(gender,fmt_sex)
  )
tb_dm_sex


## Converting Character to Numeric ------

### Using as.integer or as.double functions ----

agec <- c("50","60","53","24")
tagec <- tibble(agec)
typeof(tagec$agec)
colnames(tagec)
agen <- as.integer(tagec$agec)
agen
typeof(agen)
class(agen)

# To create this variable inside in tibble we can use mutate function

tagec <- tagec %>% mutate(
  agen = as.integer(agec)
)
tagec

pct <- c("9.12","5.6","2.0","50.45342")
pctn <- as.double(pct)
pctn 

### Using formats ------





##String manipulation ----------

### equivalent to SUBSTR function--------


### equivalent to SCAN function -------


### length of a string -------

invname <- randomNames::randomNames(20)
invname
str_length(invname)

### Trim leading and trailing spaces -----

invname = "   Garcia,  Nicholas    "
invname_strip <- str_trim(invname)
invname_strip

#note that double spaces betweeen lastname and firstname is still there
# to remove consequitive multiple spaces we will use
### Equivalent to COMPBL function -----
invname_cmpbl <- str_squish(invname)
invname_cmpbl

### Making string upcase, lowcase , titlecase ------

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^------------------

#Working with Dates-------

## Year, month and day stored in separate variables ------
# Sample Formatting Codes
# %d = day as a number
# %a = abbreviated weekday
# %A = Un-abbreviated weekday
# %m = month as number
# %b = abbreviated month
# %B = un-abbreviated month
# %y = 2-digit year
# %Y = 4-digit year
# %H = hour
# %M = minute
# %S = second
library(tidyverse)


df_date <- df_date %>%
  mutate(
    iso_dtc = case_when(
      (!is.na(day)) ~ paste(ifelse((!is.na(year)),'-',sprintf("%4.0f", year)),
                          ifesle((!is.na(month)),'-',sprintf("%02.0f",month)),
                          sprintf("%02f",day)
                          )
      (!is.na(month)) ~ paste(ifelse((!is.na(year)),'-',sprintf("%4.0f", year)),
                          sprintf("%02.0f",month)
                          )
      (!is.na(year)) ~ sprintf("%4.0f",year)
      )
    )
df_date
df_date$indtc <- paste(year,month,day,sep='-')
df_date$indt_8601 <- as.character(as.Date(df_date$indtc, "%Y-%m-%d"))

df_date

#Checking attributes of our new variable
attributes(df_date)
str(df_date)
class(df_date$indt)
as.numeric(df_date$indt)

# date9 type of formatting - month not showing up in upper case
format(df_date$indt, "%d%b%Y")
df_date
df_date$indt <- format(df_date$indt, "%d%b%Y")
df_date

