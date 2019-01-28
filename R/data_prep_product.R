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
library(dbplyr)

proj_dir <- "C:/Users/T430/Google Drive/PhD/Research/MMC/pharma_encounters/mmc-pharma"
medtrack_dir <- file.path(proj_dir,"medtrack_data")
medtrack_product_dir <- file.path(medtrack_dir,"products")

setwd(proj_dir)

## Excel files
files <- dir(medtrack_product_dir, pattern = "\\.xlsx{0,1}$")

# ## lines to skip for Medtrack header atop data table
# skip.lines <- 1

## get sheets
sheets <- excel_sheets(file.path(medtrack_product_dir, file))
sheets <- sheets[!grepl("^Sheet",sheets,ignore.case = T)]

## init list of combined dataframes
l <- list()
for (sheet in sheets) {
  l[[sheet]] <- data.frame()
}

## load files loop
for (file in files) {
  cat(sprintf("loading file: %s\n", file))
  
  ## extract category from filename 
  ## following last underscore "_"; preceding file extension ".xls|x"
  parts <- str_split(file,"[_]")[[1]]
  category <- str_split(parts[length(parts)],"[.]")[[1]][1]
  
  ## loops sheets in workbook
  for (sheet in sheets) {
    cat(sprintf("  sheet %s\n", sheet))
    
    ## load data
    file_full_path <- file.path(medtrack_product_dir, file)
    
    ## deleted header from Product Synopsis sheet but not from others
    ## skip 11 lines of header material in other sheets
    if (sheet == "Product Synopsis") {
      df <- read_excel(file_full_path, sheet = sheet, na="--")
    } else {
      df <- read_excel(file_full_path, sheet = sheet, na="--", skip = 11)
    }
    
    ## clean column names
    names(df) <- str_to_lower(str_replace_all(names(df),"[\\s\\/]+","_"))
    
    ## add category
    df$therapeutic_class <- category
    
    ## append rows to combined dataframe
    l[[sheet]] <- rbind(l[[sheet]], df)
  }
  
}

print(summary(l))

out_file <- file.path(medtrack_dir, "products_list.rds")
saveRDS(l, file = out_file)

## end




##------------------------ Descriptives ------------------
prcnt <- plyr::count(l$`Product Synopsis`$highest_phase_of_development)
prcnt <- prcnt[c(9,8,5,6,7,4,1,3,2),]
prcnt$pct <- round(100 * prcnt$freq / sum(prcnt$freq), 1)
print(prcnt)
