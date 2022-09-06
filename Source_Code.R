# R Programming for SAS Programmer ------

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^-----

#Creating LOG file -------
# R does not create LOG file by default so we will need help from 
# logr package to store the log file.
if(!require('logr')){
  install.packages('logr')
  library(logr)
}
log_file <- file.path(".","Source_code.log")
log_file
lf <- log_open(log_file,logdir=F,autolog=T, show_notes = F)

sep("Example of autolog feature")
# log_close()
# writeLines(readLines(lf))

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^-----
# Importing Data -----
# First task we will learn is to bring data in R data structure.
# Just like in SAS we have datalines, libname statement, proc import etc

## Importing the required libraries -----
if(!require('tidyverse')){
  install.packages('tidyverse')
  library(tidyverse)
}
# library(tidyverse)

## Reading Raw data - SAS Dataline equivalent -----
# SAS stores information in dataset, while R stores it in data frame.
# tibble is a upgraded data frame which overcomes some of the drawbacks 
# of data frame like inconsistent return values.

vs_raw <- tribble(~subjid, ~dbp, ~sbp, ~visit,
                  '1001', 88, 128, 'Screening',
                  '1001', 89, 130, 'Week 1',
                  '1001', 85, 125, 'Week 2',
                  '1001', NA, 135, 'Week 3',
                  )
vs_raw



#What is the objects data type (low-level)?
# Returns values like NULL,logical, integer,double, complex, environment, symbol
typeof(vs_raw$dbp) 

#Mutually exclusive classification of objects according to their basic structure
# types "integer" and "double" are returned as "numeric"
# types "special" and "built in" are returned as "function"
mode(vs_raw$dbp)

# What kind of object is it(High level)?
# For Vectors Class will return atomic datatypes like character, numeric, integer etc
# For objects like list, matrix, data.frame (tibble) and array the structure name itself is returned.
# This helps us to figure out which methods/functions are available for us.
class(vs_raw$dbp) 


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

## Rearrange variables using RELOCATE -------

ex_arrange <- ex %>% relocate(exendtc, .before =exstdtc )
ex_arrange

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
  filter(exdtc != 'NA') %>% 
  group_by(subjid) %>% 
  select(subjid, exdtc) %>% 
  slice(1) # slice_head may also work here
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

### Using sprintf function (C function printf) ------

sprintf("%f", pi) # Floating point number
sprintf("%.3f", pi) # 3 decimal place
sprintf("%1.0f", pi) # first value left of decimal place
sprintf("%5.1f", pi) #fixed width
sprintf("%05.1f", pi) # zero padded
sprintf("%+f", pi) # Plus sign before number
sprintf("% f", pi) #adding one space before the number
sprintf("%-10f", pi) # left justified
sprintf("%5.1f%%",10) # Escaping % sign

patient <- c('1001','1002')
conmed <- c('Tylenol','Asprin')
cmdt <- c('2022-08-08','2022-08-09')

#potentially can be used for patient narrative?
sprintf("Patient %s took %s on %s", patient,conmed,cmdt)

# getting npct type of structure
count <- c(10,20)
pct <- c(50,100)
sprintf("%d (%5.1f%%)",count,pct)

# for 100% we don't want decimal place
npct <- function(count,pct){
  pctf <- ifelse(pct == 100, "100",sprintf("%5.1f",pct))
  npct_f <- sprintf("%d (%s%%)",count,pctf)
  return(npct_f)
}
print(npct(count,pct))


### Using Format function --------


today <- format(Sys.Date(),format="%d%b%Y")
today
class(today)

a <- 100.01
a_c <- format(a,digits=7, nsmall =2)
a_c
a_c <- format(a,digits=6, nsmall =3)
a_c


### Using FMT_ functions ------

# Once you have the count, you can format npct 
#using fmt_cnt_pct function.

v1 <- c(4, 3, 3)
# Format count and percent: This function is little different then other
# fmt_ function. As this one does not actually calculate counts, it will
# Calculate percentages and concatenate to the count. 
# eg 4/3 * 100 = 133.33 => 4 (133.33%)
# This is a wrong percentage, so to correct it, we can also give our
# own denominator vector.
fmt_cnt_pct(v1,c(10))

# Summary stats function
table(v1)
adae <- tribble(~aedecod,"Headache","Headache")
adae <- adae %>% group_by(aedecod)
adae %>% count(aedecod) %>% fmt_cnt_pct(c(10))
fmt_n(v1)
fmt_mean_sd(v1)
summarise(v1)


## Converting Character to character

### Creating formats using VALUE function -----
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

fmt_vis <- value(
  condition(x == "visit 1", 1),
  condition(x =='visit 2', 2)
)

vis <- c('visit 1','visit 2')
visn <- fapply(vis, fmt_vis)
visn
typeof(visn)

##Numeric Function -------
round(pi,1)
ceiling(pi)
floor(pi)
trunc(pi)
signif(pi,6)


##String manipulation ----------
# Table 5.1: Tidyverse String Functions
# Function	Description
# stringr::str_c()	Concatenate two strings, specifying sep as the separator
# stringr::str_dup()	Duplicate a string a specified number of times
# stringr::str_length()	Return the length of a string
# stringr::str_flatten()	Collapse a vector of character values
# stringr::str_split()	Split a string using the specified pattern. This returns a list, use unlist() to return a vector
# stringr::str_glue()	Replaces sprintf(), allows for complex string concatenation using {}
# stringr::str_to_lower(), stringr::str_to_upper(), stringr::str_to_title(), stringr::str_to_sentence()	Control the case of strings
# stringr::str_pad()	Pad a string with specified characters, on either the left, the right, or both sides of the string
# stringr::str_trunc()	Limit a string to a certain width
# stringr::str_wrap()	Wrap a string to another line after a certain length
# stringr::str_trim()	Remove whitespace from the left and right side
# stringr::str_squish()	Remove repeated whitespace from the entire string
# stringr::word()	Subset the string by characters separated by whitespace
# stringr::str_extract()	A generalized form of word to extract characters following regex patterns
# stringr::str_detect()	Return TRUE / FALSE if specified pattern is detected in string
# stringr::str_replace(), stringr::str_replace_all()	Replace character values in a string, either once, or for all occurrences
# stringr::str_remove(), stringr::str_remove_all()	Remove specified character values from string
# stringr::str_starts(), stringr::str_ends()	Return logical value when string starts/ends with specified characters
# stringr::str_which()	Returns index of vector that has a match
# stringr::str_subset()	Returns element of vector that has a match

### Concatenating strings -----
library(stringr)
siteid <- c('1001','1101','1323')
studyid_siteid <- str_c("ABC-102",siteid,sep='-')
studyid_siteid
subjid <- c('2400','2401','2402')
usubjid <- str_c(studyid_siteid,subjid,sep = '-')
usubjid

### equivalent to SUBSTR function--------
vis <- "Week 1"
visn <- substr(vis,5,6)
visn

### equivalent to SCAN function -------

vis_split <- strsplit(vis, split = "/s")
vis_split
visn <- vis_split[1][1]
visn
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

title <- "LISTING 10.1.1.1 Listing of Demographic"
str_to_upper(title) #upcase
str_to_lower(title) #lowcase
str_to_title(title) #propcase
str_to_sentence(title) #not sure if there is equivalent in SAS


### Wrap Function -----
long_string <- "This is a very long string that cannot be on one single line"
wrapped_string <- str_wrap(long_string,width = 20)
cat(wrapped_string )


### scales function -----
# scales::number()	Round a numeric value and convert to string, aruments control prefixes, suffixes, comma separators, and accuracy
# scales::dollar()	A shortcut to using number(), using $ as a prefix
# scales::percent()	A shortcut to using number(), using % as a suffix
# scales::pvalue()	A shortcut to using number(), in standard p-value format
# scales::scientific()	A shortcut to using number(), in standart scientific notation
# 


#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^------------------

#Working with Dates-------
## Date formatting codes ------
# Sample Formatting Codes 	
# %d = day as a number [01-31]
# %a = abbreviated weekday [Mon]
# %A = Un-abbreviated weekday [Monday]
# %m = month as number 	[00-12]
# %b = abbreviated month [Jan]
# %B = un-abbreviated month [January]
# %y = 2-digit year [07]
# %Y = 4-digit year [2007]
# %H = hour
# %M = minute
# %S = second

## Converting numeric value to date -------
library(lubridate)
library(tidyverse)

# A Date is stored as number of days since 1Jan1970
x <- 0
class(x)
xdt <- as_date(x)
xdt
class(xdt)
typeof(xdt)

tr_date <- tibble(vsdtn = c(-1,0,1))
tr_date


tr_daten <- tr_date %>% 
  mutate(vsdtc <- as_date(vsdtn))
tr_daten

## Converting string value to date -------
#When string is in standard form.
x <- "1970-01-01"
xdt <- as.Date(x)
xdt

#When string is not in standard form.
#Note month is capital.
x <- "1JAN1970"
xdt <- as.Date(x, format="%d%b%Y")
xdt
class(xdt)

## Converting Date to character  --------
xdtc <- format(xdt,"%d%b%Y")
xdtc
class(xdt)

## Converting Date to numeric -------
numx <- as.numeric(xdt)
numx
class(numx)

# Calculating number of days
basedt <- as.Date(c("2022-01-01","2022-01-01"))
basedt
adt <- as.Date(c("2022-01-15","2022-02-28"))
adt

lbdt <- tibble(basedt,adt)
lbdt

lbdt_ady <- lbdt %>% 
  mutate(ady = adt - basedt,
         ady1 = adt - basedt + 1)

lbdt_ady

## Extracting day, month and year from a date ------
year(adt)
month(adt)
day(adt)

# Working with DateTime ------

## Converting numeric value to time ------
x <- 75
xtm <- as_datetime(x)
# Datetime in R is stored as Portable operating system interface 
class(xtm)
as.numeric(xtm)
typeof(xtm)
isodt1 <- format(xtm,"%Y-%m-%dT%H:%M:%S")
class(isodt1)

## Converting individual datetime components into datetime ------
isodtc2 <- ISOdatetime(1970,1,1,1,15,0)
isodtc2
class(isodtc2)

## Converting string value to datetime -----
x <- "1970-01-01 2:10"
xtm <- as.POSIXct(x)
xtm
class(xtm)

y <- "1970-01-02 3:12"
ytm <- as.POSIXct(y)
ytm

difftm <- ytm - xtm
difftm
as.numeric(difftm,units="hours")

## Extracting datetime components -----
exttm <- as.POSIXlt("1970-01-01 2:10")
exttm
class(exttm)
exttm$min
exttm$mon # note month is counted from 0, so the value will be
# one less than shown in date
exttm$year
exttm$day
exttm$zone


#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^-----------
#Basic Statistics -------
library(tidyverse)
##Summarizing data PROC MEANS ----------
lb <- read_rds(file.path("./sdtmdata","lb.rds"))
lb
lb_bili <- lb %>% 
  filter(LBTESTCD == "BILI" & LBCAT =="CHEMISTRY")
lb_bili
summary(lb_bili$LBSTRESN)

# PROC MEANS ignores missing values by default but summarize does not.
lb_bili %>% 
  summarise(N = n(),
            mean = mean(LBSTRESN, na.rm=TRUE ),
            min = min(LBSTRESN, na.rm=TRUE),
            nmiss = sum(is.na(LBSTRESN))
            )

## Group by ------
lb %>% 
  filter(LBCAT %in% c('CHEMISTRY','HEMATOLOGY'))  %>% 
  group_by(LBCAT,LBTESTCD,VISITNUM,VISIT) %>% 
  summarise(N = n(),
            mean = mean(LBSTRESN, na.rm=TRUE ),
            min = min(LBSTRESN, na.rm=TRUE),
            nmiss = sum(is.na(LBSTRESN))
  )

#formatted stats
lb %>% 
  filter(LBCAT %in% ('CHEMISTRY')) %>% 
  group_by(LBCAT,LBTESTCD,VISITNUM,VISIT) %>% 
  summarise(N = fmt_n(LBSTRESN),
            `Mean (SD)` = fmt_mean_sd(LBSTRESN),
            Median = fmt_median(LBSTRESN),
            `Q1 - Q3` = fmt_quantile_range(LBSTRESN),
            Range  = fmt_range(LBSTRESN)
  ) %>% 
  pivot_longer(c('N','Mean (SD)','Median','Q1 - Q3','Range'), names_to = "Stat", values_to = "Col")

## Getting distinct Values --------
dm <- read_rds(file.path("./sdtmdata","dm.rds"))
dm
dm_arm <- dm %>% 
  select(ARMCD,ARM) %>% 
  distinct()
dm_arm

dm_race <- dm %>% 
  select(RACE) %>% 
  distinct()
dm_race

## Getting distinct counts -------
bign <- dm %>% 
  select(USUBJID,ARMCD,ARM) %>% 
  filter(ARMCD != "SCRNFAIL") %>% 
  group_by(ARMCD,ARM) %>% 
  summarize(n = n())
bign
bign$ARM
bign['ARMCD']

tbign <- bign %>% 
  select(ARMCD,n) %>% 
  pivot_wider(names_from = ARMCD,
              names_prefix='n',
              values_from = n)
tbign

tbign$n1

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^-------------------
#Comparing data - PROC COMPARE equivalent ------

#When things match up
vs_prod <- tribble(~usubjid, ~visitnum, ~vstestcd, ~vsorres,
              "1001",1,"SYSBP", "120",
              "1001",2,"SYSBP", "122")
vs_prod

vs_qc <- tribble(~usubjid, ~visitnum, ~vstestcd, ~vsorres,
                   "1001",1,"SYSBP", "120",
                   "1001",2,"SYSBP", "122")

vs_qc

if (as.character(all_equal(vs_prod,
                           vs_qc,  
                           ignore_col_order = FALSE,
                           ignore_row_order = FALSE)) == "TRUE" ) {
  print("No unequal values where found. All values matched exactly.")
}else{
  
  print("Unequal Values where found. Details below.")
  diff1 <- anti_join(vs_prod, vs_qc) 
  diff1
  
  library(diffdf)
  diff2 <- diffdf(vs_prod, vs_qc)
  diff2
  
  library(arsenal)
  summary(comparedf(vs_prod, vs_qc))
  
}






# When observation match but variable/value/observation does not match

vs_prod <- tribble(~usubjid, ~visitnum, ~vstestcd, ~vsorres,
                   "1001",1,"SYSBP", "120",
                   "1001",2,"SYSBP", "122")
vs_prod

vs_qc <- tribble(~usubjid, ~visitnum, ~vstestcd, ~vsorres, ~vsstresn,
                 "1001",1,"SYSBP", "120",120,
                 "1001",2,"SYSBP", "123",123,
                 "1001",3,"SYSBP", "123",123)

vs_qc

if (as.character(all_equal(vs_prod,
                          vs_qc,  
                          ignore_col_order = FALSE,
                          ignore_row_order = FALSE)) == "TRUE" ) {
  print("No unequal values where found. All values matched exactly.")
}else{
  print("Unequal Values where found. Details below.")
  
  diff1 <- anti_join(vs_prod, vs_qc) 
  diff1
  #class(diff1)
  
  diff2<- diffdf(vs_prod, vs_qc, keys=c('usubjid','visitnum','vstestcd'))
  diff2
  #class(diff2)
  
  diff3 <- summary(comparedf(vs_prod, vs_qc, by_group=c('usubjid','visitnum','vstestcd')))
  diff3     
  
  #class(diff3)
}


#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^-------------------

#HELP-------

## Vignettes ------
vignette("dplyr", package = "dplyr")
vignette("colwise",package="dplyr")

## Help pages ---------
help("summarise", package= "dplyr")

## Cheat sheet -------
#https://www.rstudio.com/resources/cheatsheets/

