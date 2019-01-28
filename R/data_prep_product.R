####################################################
##
##  Pharma MMC
##
##   - data prep: product lists by therapetic class
##   - file structure:
##       root
##        |--medtrack_data
##            |--firms
##            \--products
##                |--Medtrack_...Oncologics.xls
##                ...
##        \--R
####################################################

library(stringr)
library(readxl)

proj_dir <- "C:/Users/T430/Google Drive/PhD/Research/MMC/pharma_encounters/mmc-pharma"
medtrack_dir <- file.path(proj_dir,"medtrack_data")
medtrack_product_dir <- file.path(medtrack_dir,"products")

setwd(proj_dir)

## Excel files
files <- dir(medtrack_product_dir, pattern = "\\.xlsx{0,1}$")

## lines to skip for Medtrack header atop data table
skip.lines <- 11

## init combined dataframe
dfall <- data.frame()

## load files loop
for (file in files) {
  cat(sprintf("loading file: %s\n", file))
  
  ## extract category from filename 
  ## following last underscore "_"; preceding file extension ".xls|x"
  parts <- str_split(file,"[_]")[[1]]
  category <- str_split(parts[length(parts)],"[.]")[[1]][1]
  
  ## load data
  file_full_path <- file.path(medtrack_product_dir, file)
  df <- read_excel(file_full_path, sheet = 1, na=NA, skip = skip.lines)
  
  ## fix column names
  names(df) <- str_replace_all(names(df),"\\s+","_")
  
  ## add category
  df$therapeutic_class <- category
  
  ## append rows to combined dataframe
  dfall <- rbind(dfall, df)
}

## end