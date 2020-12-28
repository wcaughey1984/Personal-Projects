# 
# Title:    nThrive data exploration
# Purpose:  (nThrive Presentation) Reviewing the NHANES data set 
# Author:   Billy Caughey
# Date:     2020.12.19 - Initial Build 
# 

##### Libraries #####

library(tidyverse)
library(haven)

##### Import Data #####

## Demographics

demo_url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT"
nhanes_demo <- read_xpt(file = demo_url)

## Playing with Demographics 

table(nhanes_demo$RIDSTATR)

table(nhanes_demo$RIDAGEYR >= 80)

table(nhanes_demo$RIDRETH1)
