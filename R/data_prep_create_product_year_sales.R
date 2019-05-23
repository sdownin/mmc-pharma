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
  HIPHADEV = 'highest_phase_of_development',
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


##============================================
## SUMMARIZE 
##--------------------------------------------
dfsr <- dflm[which(dflm$YEAR==2017),]
dfs <- dflm[which(dflm$YEAR==2017 & dflm$REGION=='Total Global Sales'),]
dim(dfsr)
dim(dfs)

dfsr %>% group_by(REGION) %>% tally(sort = T) %>% ungroup() %>% arrange(desc(n))
dfs %>% group_by(FIRMID) %>% tally(sort = T) %>% ungroup() %>% arrange(desc(n))
dfs %>% group_by(CONAME) %>% tally(sort = T) %>% ungroup() %>% arrange(desc(n))
dfs %>% group_by(ACTVIGT) %>% tally(sort = T) %>% ungroup() %>% arrange(desc(n))
dfs %>% group_by(CONDITION) %>% tally(sort = T) %>% ungroup() %>% arrange(desc(n))
dfs %>% group_by(THERACAT) %>% tally(sort = T) %>% ungroup() %>% arrange(desc(n))
dfs %>% group_by(THERACLS) %>% tally(sort = T) %>% ungroup() %>% arrange(desc(n))
dfs %>% group_by(HIPHADEV) %>% tally(sort = T) %>% ungroup() %>% arrange(desc(n))
dfs %>% group_by(PRODTYPE) %>% tally(sort = T) %>% ungroup() %>% arrange(desc(n))
dfs %>% group_by(MOLTYPE) %>% tally(sort = T) %>% ungroup() %>% arrange(desc(n))
dfs %>% group_by(SUBORIG) %>% tally(sort = T) %>% ungroup() %>% arrange(desc(n))
dfs %>% group_by(TARGET) %>% tally(sort = T) %>% ungroup() %>% arrange(desc(n))
dfs %>% group_by(MOA) %>% tally(sort = T) %>% ungroup() %>% arrange(desc(n))
dfs %>% group_by(MODEACT) %>% tally(sort = T) %>% ungroup() %>% arrange(desc(n))
dfs %>% group_by(MKTSTAT) %>% tally(sort = T) %>% ungroup() %>% arrange(desc(n))
dfs %>% group_by(RTADMIN) %>% tally(sort = T) %>% ungroup() %>% arrange(desc(n))
dfs %>% group_by(DOSFORM) %>% tally(sort = T) %>% ungroup() %>% arrange(desc(n))
dfs %>% group_by(EPDRCLS) %>% tally(sort = T) %>% ungroup() %>% arrange(desc(n))
dfs %>% group_by(CBIOCLS) %>% tally(sort = T) %>% ungroup() %>% arrange(desc(n))
dfs %>% group_by(PRODESC) %>% tally(sort = T) %>% ungroup() %>% arrange(desc(n))
dfs %>% group_by(STRENGTH) %>% tally(sort = T) %>% ungroup() %>% arrange(desc(n))
dfs %>% group_by(DELTECH) %>% tally(sort = T) %>% ungroup() %>% arrange(desc(n))



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





