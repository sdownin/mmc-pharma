####################################################
##
##  Pharma MMC
##
##   - data prep: product lists by therapetic class
##
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
library(tibble)
library(uuid)
library(plyr)
library(dplyr)
library(reshape2)

## dir names
proj_dir <- "C:/Users/T430/Google Drive/PhD/Research/MMC/pharma_encounters/mmc-pharma"
medtrack_dir <- file.path(proj_dir,"medtrack_data")
medtrack_product_dir <- file.path(medtrack_dir,"products")
medtrack_sales_dir <- file.path(medtrack_dir,"sales")


## set working dir
setwd(proj_dir)

##============================================
## LOAD PRODUCT LIST
##--------------------------------------------

prod_list_file <- file.path(medtrack_dir, "products_list.rds")
l <- readRDS(prod_list_file)

##---end---
head(l$Sales)

##============================================
## LOAD YEARLY PRODUCT SALES
##--------------------------------------------

##============================================
## Initialize data list
##--------------------------------------------

## Excel files
files <- dir(medtrack_sales_dir, pattern = "\\.xlsx$")

if (length(files)==0) 
  stop(sprintf("no files in dir %s", medtrack_sales_dir))

# ## lines to skip for Medtrack header atop data table
# skip.lines <- 1

## get sheets (excluding unedited sheets with default name "Sheet__")
file_full_path <- file.path(medtrack_sales_dir, files[1])
sheets <- excel_sheets(file_full_path)
sheets <- sheets[!grepl("^Sheet",sheets)]

## load data
df <- read_excel(file_full_path, sheet = sheets[1], na="--", skip = 11)

## convert long to wide
id.vars <- c("Product Name", "Companies", "Currency", "Region")
measure.vars <- names(df)[ ! names(df) %in% id.vars ]
dfl <- as_tibble(reshape2::melt(df, id.vars, measure.vars, variable.name="year", value.name="sales"))

## Check regions frequency
region.summary <- dfl %>%
  filter(!is.na(Region) & (! Region %in% c("Total Global Sales", "Worldwide") )) %>%
  group_by(Region, Currency) %>%
  tally(sort = T) %>%
  ungroup() %>%
  arrange(desc(n))
print(region.summary)


## Firms by regional sphere_group of influence (coarse separation by continent)
firms.us <- unique(
  c("Johnson & Johnson",'Pfizer','Amgen','Biogen','Eli Lilly','AbbVie',
    'Merck','Abbott','Gilead Sciences','Bristol-Myers Squibb')
)
firms.eu <- unique(
  c('Novartis','GlaxoSmithKline','AstraZeneca','Roche',
    'Sanofi','Bayer','Novo Nordisk')
)
firms <- c(firms.us, firms.eu)

## firm IDs
firmIDs = list(
  pfizer= c('pfizer'),
  gsk= c('glaxo'),
  merck= c('merck','plough'),  ## 2009 merck merged with schering-plough
  jnj= c('johnson'),
  novartis= c('novartis'),
  roche= c('roche'),
  sanofi= c('sanofi'),
  astrazeneca= c('astrazeneca'),
  bms= c('bristol','squibb'),
  abbott= c('abbott'),
  teva= c('teva'),
  bayer= c('bayer'),
  lilly= c('lilly'),
  novonordisk= c('nordisk'),
  gilead= c('gilead'),
  amgen= c('amgen'),
  genzyme= c('genzyme'),
  biogen= c('biogen'),
  abbvie= c('abbvie'),
  mylan= c('mylan')
)


## flag is single company product sales (not multiple) 
dfl$is_single <- as.integer(sapply(dfl$Companies, function(x) length(strsplit(x,"[|]")[[1]])==1 ))

## add firm names  (separate multiples by pipe "|")
dfl$firm_name <- apply(dfl[,"Companies"], 1, function(product_comanies){
  x <- c()
  for (firmID in names(firmIDs)) {
    firmNames <- firmIDs[[firmID]]
    for (firmName in firmNames) {
      if (grepl(firmName, product_comanies, ignore.case = T, perl = T)) {
        x <- c(x, firmID)
      }
    }
  }
  if (length(x) > 0)
    return(paste(unique(x), collapse = "|"))
  return("OTHER")
})

## summarize firm frequency by product
dflglob <- dfl[which(dfl$Region=='Total Global Sales'), ]
pctsingle <- 100 * length(grep("[|]",dflglob$firm_name)) / nrow(dflglob)
pctsinglefc <- 100 * length(grep("[|]",dflglob$firm_name)) / sum(dflglob$firm_name!='OTHER')
cat(sprintf('Multi-Firm Product Sales Pct of ALL Global Totals:\n single co: %5.1f%s\n multi cos: %5.1f%s',pctsingle,'%', 100-pctsingle,'%'))
cat(sprintf('Multi-Firm Product Sales Pct of FOCAL COHORT Global Totals:\n single co: %5.1f%s\n multi cos: %5.1f%s',pctsinglefc,'%', 100-pctsinglefc,'%'))
pscnt <- plyr::count(dfl$firm_name[which(dfl$Region=='Total Global Sales')])
pscnt <- pscnt[order(pscnt$freq, decreasing = T), ]
print(pscnt)

##============================================
## Merge in product attributes
##-------------------------------------------
## therapeutic class, condition treated
# pscols <- c('product_name','active_ingredient','condition_treated',
#             'therapeutic_category','therapeutic_class',
#             'highest_phase_of_development','product_type','molecule_type',
#             'substance_of_origin','target','moa','mode_of_action',
#             'marketing_status','route_of_administration','dosage_form',
#             'ephmra_drug_class','chemical_biological_class','product_description',
#             'strength','drug_delivery_technology','uuid')
## Create product info table with one product per row
## multiple records (e.g., therapeutic categories) are concatenated by pipes ("|")
.pasteNA <- function(x) {
  if (all(is.na(x))) 
    return(NA)
  return(paste(unique(x), collapse = "|"))
}

prodinfo <- plyr::ddply(l$`Product Synopsis`, .(product_name), summarize,
                        active_ingredient = .pasteNA(active_ingredient),
                        condition_treated = .pasteNA(condition_treated),
                        therapeutic_category = .pasteNA(therapeutic_category),
                        therapeutic_class = .pasteNA(therapeutic_class),
                        highest_phase_of_development = .pasteNA(highest_phase_of_development),
                        product_type = .pasteNA(product_type),
                        molecule_type = .pasteNA(molecule_type),
                        substance_of_origin = .pasteNA(substance_of_origin),
                        target = .pasteNA(target),
                        moa = .pasteNA(moa),
                        mode_of_action = .pasteNA(mode_of_action),
                        marketing_status = .pasteNA(marketing_status),
                        route_of_administration = .pasteNA(route_of_administration),
                        dosage_form = .pasteNA(dosage_form),
                        ephmra_drug_class = .pasteNA(ephmra_drug_class),
                        chemical_biological_class = .pasteNA(chemical_biological_class),
                        product_description = .pasteNA(product_description),
                        strength = .pasteNA(strength),
                        drug_delivery_technology = .pasteNA(drug_delivery_technology),
                        uuid = .pasteNA(uuid)
                        )

## add id to check for duplicates
dfl$id <- 1:nrow(dfl)
## merge  product into into sales long data frame
dflm <- merge(x=dfl, y=prodinfo, by.x='Product Name', by.y='product_name', all.x=TRUE, all.y=FALSE)
#reorder and check duplicates
dflm <- dflm[order(dflm$id), ]
dim(dflm)

# ## check duplicates
# ck <- plyr::count(dflm$id)
# ck <- ck[order(ck$freq, decreasing = T),]
# ck[ck$freq > 1, ]

## cache full dataframe-long-merge df before subsetting to pharma cohort
if (!'dflm.all' %in% ls()) 
  dflm.all <- dflm

## FILTER ONLY FIRMS IN focal cohort
fidx <- which(sapply(dflm$firm_name, function(firmIDstr){
  for(id in names(firmIDs)) {
    if (any(grepl(id, firmIDstr, ignore.case = T, perl = T))) {
      return(TRUE)
    }
  }
  return(FALSE)
}))
dflm <- dflm[fidx, ]

##============================================
##  RENAME COLUMNS AND DATA FRAME
##--------------------------------------------

## mapping from old to new columns names
colmap <- list(
  PRODNAME = "Product Name",
  YEAR = "year",
  REGION = "Region",
  SALES = "sales",
  CURRENCY = "Currency",
  CONAME = "Companies",
  FIRMID = "firm_name",
  CONDITION = "condition_treated",
  THERACLS = "therapeutic_class",
  THERACAT = "therapeutic_category",
  DELTECH = "drug_delivery_technology",
  SINGLE = 'is_single',
  ACTVIGT = 'active_ingredient',
  # HIPHADEV = 'highest_phase_of_development',
  PRODTYPE = 'product_type',
  MOLTYPE = 'molecule_type',
  SUBORIG = 'substance_of_origin',
  TARGET = 'target',
  MOA = 'moa',
  MODEACT = 'mode_of_action',
  MKTSTAT = 'marketing_status',
  RTADMIN = 'route_of_administration',
  DOSFORM = 'dosage_form',
  EPDRCLS = 'ephmra_drug_class',
  CBIOCLS = 'chemical_biological_class',
  PRODESC = 'product_description',
  STRENGTH = 'strength'
)




## Rename columns
for (i in 1:length(colmap)) {
  new <- names(colmap)[i]
  old <- colmap[[i]]
  idx <- which(names(dflm)==old)
  if (length(idx)>0) {
    names(dflm)[idx] <- new
  }
}

## Saves sales file to csv
salesfilebase <- file.path(medtrack_dir, 'COMBINED','20190521','sales',
                           'medtrack_product_sales')
write.csv(dflm[,names(colmap)], file = sprintf('%s.csv',salesfilebase), row.names = F)

## dflm <- read.csv(sprintf('%s.csv',salesfilebase), na.strings=c('','--','NaN','NA'), stringsAsFactors=F)

##============================================
## SUMMARIZE 
##--------------------------------------------
dfsr <- dflm[which(dflm$YEAR==2017),]
dfs <- dflm[which(dflm$YEAR==2017 & dflm$REGION=='Total Global Sales'),]
dim(dfsr)
dim(dfs)

# dfsr %>% group_by(REGION) %>% tally(sort = T) %>% ungroup() %>% arrange(desc(n))

##-------------------------------
## PRINT REPORT
##-------------------------------
printReport <- function() {

  ## REGION -- Use dfsr (product-region level of analysis)
  tmp <- dfsr %>% count(REGION, sort = T)
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$REGION))   ## Missing
  midx <- which(grepl(',',tmp$REGION) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(REGION='Other Single', n=nsingle),
               data.frame(REGION='Multiple', n=nmulti),
               data.frame(REGION='None', n=nmissing),
               data.frame(REGION='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  
  ## FIRMID -- Use dfs (product level)
  tmp <- dfs %>% count(FIRMID, sort = T)
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$FIRMID))   ## Missing
  midx <- which(grepl('[|]',tmp$FIRMID) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(FIRMID='Other Single', n=nsingle),
               data.frame(FIRMID='Multiple', n=nmulti),
               data.frame(FIRMID='None', n=nmissing),
               data.frame(FIRMID='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  
  
  ## CONAME -- Use dfs (product level)
  tmp <- dfs %>% count(CONAME, sort = T)
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$CONAME))   ## Missing
  midx <- which(grepl(',',tmp$CONAME) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(CONAME='Other Single', n=nsingle),
               data.frame(CONAME='Multiple', n=nmulti),
               data.frame(CONAME='None', n=nmissing),
               data.frame(CONAME='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  
  ## ACTVIGT -- Use dfs (product level)
  tmp <- dfs %>% count(ACTVIGT, sort = T)
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$ACTVIGT))   ## Missing
  midx <- which(grepl(',',tmp$ACTVIGT) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(ACTVIGT='Other Single', n=nsingle),
               data.frame(ACTVIGT='Multiple', n=nmulti),
               data.frame(ACTVIGT='None', n=nmissing),
               data.frame(ACTVIGT='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  
  ##---------------------------------------------------------------
  ## CONDITION -- Use dfs (product level)
  ucond <- unique(unlist(str_split(dfs$CONDITION, "\\(\\w{0,5}\\)")))
  uconds <- ucond[which(ucond != '')]
  tmp <- data.frame()
  for (cond in uconds) {
    tmp <- rbind(tmp, data.frame(CONDITION=cond, n=length(grep(cond,dfs$CONDITION))))
  }
  tmp <- as_tibble(tmp[order(tmp$n, decreasing = T),])
  print(tmp)
  ##-------------------------------------------------------------------
  
  ## THERACAT -- Use dfs (product level)
  tmp <- dfs %>% count(THERACAT, sort = T)
  cat(sprintf('\n Num THERACAT: %s\n',length(unique(unlist(str_split(dfs$THERACAT,'[|]'))))))
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$THERACAT))   ## Missing
  midx <- which(grepl(',',tmp$THERACAT) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(THERACAT='Other Single', n=nsingle),
               data.frame(THERACAT='Multiple', n=nmulti),
               data.frame(THERACAT='None', n=nmissing),
               data.frame(THERACAT='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  
  
  ## THERACLS -- Use dfs (product level)
  tmp <- dfs %>% count(THERACLS, sort = T)
  cat(sprintf('\n Num THERACLS: %s\n',length(unique(unlist(str_split(dfs$THERACLS,'[|]'))))))
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$THERACLS))   ## Missing
  midx <- which(grepl('[|]',tmp$THERACLS) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(THERACLS='Other Single', n=nsingle),
               data.frame(THERACLS='Multiple', n=nmulti),
               data.frame(THERACLS='None', n=nmissing),
               data.frame(THERACLS='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  
  
  ## HIPHADEV -- Use dfs (product level)
  tmp <- dfs %>% count(HIPHADEV, sort = T)
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$HIPHADEV))   ## Missing
  midx <- which(grepl(',',tmp$HIPHADEV) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(HIPHADEV='Other Single', n=nsingle),
               data.frame(HIPHADEV='Multiple', n=nmulti),
               data.frame(HIPHADEV='None', n=nmissing),
               data.frame(HIPHADEV='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  
  
  ## PRODTYPE -- Use dfs (product level)
  tmp <- dfs %>% count(PRODTYPE, sort = T)
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$PRODTYPE))   ## Missing
  midx <- which(grepl(',',tmp$PRODTYPE) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(PRODTYPE='Other Single', n=nsingle),
               data.frame(PRODTYPE='Multiple', n=nmulti),
               data.frame(PRODTYPE='None', n=nmissing),
               data.frame(PRODTYPE='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  
  
  ## MOLTYPE -- Use dfs (product level)
  dfs$MOLTYPE[dfs$MOLTYPE=='NA'] <- NA
  tmp <- dfs %>% count(MOLTYPE, sort = T)
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$MOLTYPE))   ## Missing
  midx <- which(grepl(',',tmp$MOLTYPE) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(MOLTYPE='Other Single', n=nsingle),
               data.frame(MOLTYPE='Multiple', n=nmulti),
               data.frame(MOLTYPE='None', n=nmissing),
               data.frame(MOLTYPE='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  
  
  ## SUBORIG -- Use dfs (product level)
  tmp <- dfs %>% count(SUBORIG, sort = T)
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$SUBORIG))   ## Missing
  midx <- which(grepl(',',tmp$SUBORIG) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(SUBORIG='Other Single', n=nsingle),
               data.frame(SUBORIG='Multiple', n=nmulti),
               data.frame(SUBORIG='None', n=nmissing),
               data.frame(SUBORIG='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  
  
  ## TARGET -- Use dfs (product level)
  tmp <- dfs %>% count(TARGET, sort = T)
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$TARGET))   ## Missing
  midx <- which(grepl(',',tmp$TARGET) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(TARGET='Other Single', n=nsingle),
               data.frame(TARGET='Multiple', n=nmulti),
               data.frame(TARGET='None', n=nmissing),
               data.frame(TARGET='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  
  
  ## MOA : MOLECULE OF ACTION -- Use dfs (product level)
  tmp <- dfs %>% count(MOA, sort = T)
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$MOA))   ## Missing
  midx <- which(grepl(',',tmp$MOA) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(MOA='Other Single', n=nsingle),
               data.frame(MOA='Multiple', n=nmulti),
               data.frame(MOA='None', n=nmissing),
               data.frame(MOA='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  
  
  ## MODEACT -- Use dfs (product level)
  tmp <- dfs %>% count(MODEACT, sort = T)
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$MODEACT))   ## Missing
  midx <- which(grepl(',',tmp$MODEACT) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(MODEACT='Other Single', n=nsingle),
               data.frame(MODEACT='Multiple', n=nmulti),
               data.frame(MODEACT='None', n=nmissing),
               data.frame(MODEACT='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  
  
  ## MKTSTAT -- Use dfs (product level)
  tmp <- dfs %>% count(MKTSTAT, sort = T)
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$MKTSTAT))   ## Missing
  midx <- which(grepl(',',tmp$MKTSTAT) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(MKTSTAT='Other Single', n=nsingle),
               data.frame(MKTSTAT='Multiple', n=nmulti),
               data.frame(MKTSTAT='None', n=nmissing),
               data.frame(MKTSTAT='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  
  
  ## RTADMIN : Route of Administration -- Use dfs (product level)
  tmp <- dfs %>% count(RTADMIN, sort = T)
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$RTADMIN))   ## Missing
  midx <- which(grepl(',',tmp$RTADMIN) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(RTADMIN='Other Single', n=nsingle),
               data.frame(RTADMIN='Multiple', n=nmulti),
               data.frame(RTADMIN='None', n=nmissing),
               data.frame(RTADMIN='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  
  
  ## DOSFORM -- Use dfs (product level)
  dfs$DOSFORM <- str_to_lower(dfs$DOSFORM)
  tmp <- dfs %>% count(DOSFORM, sort = T)
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$DOSFORM))   ## Missing
  midx <- which(grepl('(;|,)',tmp$DOSFORM) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(DOSFORM='Other Single', n=nsingle),
               data.frame(DOSFORM='Multiple', n=nmulti),
               data.frame(DOSFORM='None', n=nmissing),
               data.frame(DOSFORM='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  
  
  ## EPDRCLS -- Use dfs (product level)
  tmp <- dfs %>% count(EPDRCLS, sort = T)
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$EPDRCLS))   ## Missing
  midx <- which(grepl(',',tmp$EPDRCLS) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(EPDRCLS='Other Single', n=nsingle),
               data.frame(EPDRCLS='Multiple', n=nmulti),
               data.frame(EPDRCLS='None', n=nmissing),
               data.frame(EPDRCLS='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  
  
  ## CBIOCLS -- Use dfs (product level)
  tmp <- dfs %>% count(CBIOCLS, sort = T)
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$CBIOCLS))   ## Missing
  midx <- which(grepl(',',tmp$CBIOCLS) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(CBIOCLS='Other Single', n=nsingle),
               data.frame(CBIOCLS='Multiple', n=nmulti),
               data.frame(CBIOCLS='None', n=nmissing),
               data.frame(CBIOCLS='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  
  
  #----------------------------------------------
  ## PRODESC -- Use dfs (product level)
  tmpidx <- which(!is.na(dfs$PRODESC) & dfs$PRODESC != '')
  cat(sprintf('\n\n Records with PRODESC:  %.2f%s\n\n\n',100*length(tmpidx)/nrow(dfs),'%'))
  #----------------------------------------------
  
  
  ## STRENGTH -- Use dfs (product level)
  tmp <- dfs %>% count(STRENGTH, sort = T)
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$STRENGTH))   ## Missing
  midx <- which(grepl(',',tmp$STRENGTH) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(STRENGTH='Other Single', n=nsingle),
               data.frame(STRENGTH='Multiple', n=nmulti),
               data.frame(STRENGTH='None', n=nmissing),
               data.frame(STRENGTH='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  
  
  ## DELTECH -- Use dfs (product level)
  tmp <- dfs %>% count(DELTECH, sort = T)
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$DELTECH))   ## Missing
  midx <- which(grepl(',',tmp$DELTECH) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(DELTECH='Other Single', n=nsingle),
               data.frame(DELTECH='Multiple', n=nmulti),
               data.frame(DELTECH='None', n=nmissing),
               data.frame(DELTECH='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  

}
printReport()


##============================================
##  Descriptives
##--------------------------------------------
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
#
# ## frequency of firm
# cocnt <- plyr::count(l$company_product$company_name)
# cocnt$pct <- round(100 * cocnt$freq / sum(cocnt$freq), 2)
# cocnt <- cocnt[order(cocnt$freq, decreasing = T), ]
# View(cocnt)
# 
# ## Frequency of treated condition
# z = plyr::count(l$`Product Synopsis`$condition_treated)
# z = z[order(z$freq, decreasing = T), ]
# z$pct <- round(100 * z$freq / sum(z$freq), 2)
# print(head(z,10))





