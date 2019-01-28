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
library(uuid)
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

##============================================
## add product_firm 1-to-many relation table
##--------------------------------------------
## add UUID to product
l$`Product Synopsis`$uuid <- apply(l$`Product Synopsis`, 1, function(x)UUIDgenerate())
## add split company name comma-separated list (but don't split company name ends in ", Inc.")
firm_name_list <- str_split(l$`Product Synopsis`$company_name, ",(?!\\s+Inc) ")
## create firm_product dataframe
l$company_product <- data.frame()
for (i in 1:length(firm_name_list)) {
  .tmp <- data.frame(product_uuid=l$`Product Synopsis`$uuid[i], company_name=firm_name_list[[i]])
  l$company_product <- rbind(l$company_product, .tmp)
  if (i %% 2000 == 0) cat(sprintf(" i = %s (%.3f%s)\n",i,100*i/length(firm_name_list),"%"))
}


## save data list
out_file <- file.path(medtrack_dir, "products_list.rds")
saveRDS(l, file = out_file)

## end




# ##------------------ Descriptives ------------------
# ## frequency of development phase
# prcnt <- plyr::count(l$`Product Synopsis`$highest_phase_of_development)
# prcnt <- prcnt[c(9,8,5,6,7,4,1,3,2),]
# prcnt$pct <- round(100 * prcnt$freq / sum(prcnt$freq), 1)
# print(prcnt)
# 
# ## frequency of product type
# tycnt <- plyr::count(l$`Product Synopsis`$product_type)
# tycnt$pct <- round(100 * tycnt$freq / sum(tycnt$freq), 1)
# print(tycnt)






