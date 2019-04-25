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
library(dbplyr)
library(reshape2)
library(mongolite)

## dir names
proj_dir <- "C:/Users/T430/Google Drive/PhD/Research/MMC/pharma_encounters/mmc-pharma"
medtrack_dir <- file.path(proj_dir,"medtrack_data")
medtrack_product_dir <- file.path(medtrack_dir,"products")
medtrack_sales_dir <- file.path(medtrack_dir,"sales")

## set working dir
setwd(proj_dir)

##============================================
## Initialize data list
##--------------------------------------------

## Excel files
salesfile <- 'Medtrack_Product_sales_analyzer_30Jan2019_1985to2019_Global_Sales.xlsx'

## get sheets (excluding unedited sheets with default name "Sheet__")
file_full_path <- file.path(medtrack_sales_dir, salesfile)
sheets <- excel_sheets(file_full_path)
sheets <- sheets[!grepl("^Sheet",sheets)]

## Sales data
sa <- read_excel(file_full_path, sheet = sheets[1], na="--", skip = 11)
names(sa)[1:4] <- c("product_name", "companies", "currency", "region")

## FIRMS
firmsmap = list(
  pfizer=  c('pfizer'),
  gsk=  c('glaxo'),
  merck=  c('merck','schering','plough'),  ## 2009 merck merged with schering-plough
  jnj=  c('johnson'),
  novartis=  c('novartis'),
  roche=  c('roche'),
  sanofi=  c('sanofi'),
  astrazeneca=  c('astrazeneca'),
  bms=  c('bristol','squibb'),
  abbott=  c('abbott'),
  teva=  c('teva'),
  bayer=  c('bayer'),
  lilly=  c('lilly'),
  novonordisk=  c('nordisk'),
  gilead=  c('gilead'),
  amgen=  c('amgen'),
  genzyme=  c('genzyme'),
  biogen=  c('biogen'),
  abbvie=  c('abbvie'),
  mylan=  c('mylan')
)
firms <- names(firmsmap)


# ##============================================
# ## Load products data list for MMC
# ##--------------------------------------------
# products_file <- file.path(medtrack_dir, "products_list.rds")
# l <- readRDS(products_file)
# 
# ## Update company product uuid map
# cp <- as_tibble(l$company_product)
# cp$company_name <- as.character(cp$company_name)
# cp$product_uuid <- as.character(cp$product_uuid)
# cp$firm <- sapply(1:nrow(cp), function(i){
#   x <- cp$company_name[i]
#   if (i %% 1000 == 0) cat(sprintf(' %s (%.2f%s)\n',i,100*i/nrow(cp),'%'))
#   for (firm in firms){
#     # cat(sprintf('firm %s\n', firm))
#     for (pattern in firmsmap[[firm]]) {
#       if (grepl(pattern, x, ignore.case = T, perl = T)) {
#         return(firm)
#       }
#     }
#   }
#   return(NA)
# })
# 
prodsyn <- ddply(l$`Product Synopsis`, c('product_name'), summarize, 
                 # product_name = paste(product_name, collapse="|"),
                 other_names = paste(unique(other_names), collapse="|"),
                 active_ingredient = paste(unique(active_ingredient), collapse="|"),
                 therapeutic_category = paste(unique(therapeutic_category), collapse="|"),
                 condition_treated = paste(unique(condition_treated), collapse="|"),
                 therapeutic_class = paste(unique(therapeutic_class), collapse="|"),
                 product_firm_uuid = paste(unique(uuid), collapse="|"),
                 ##
                 drug_delivery_technology = paste(unique(drug_delivery_technology), collapse="|"),
                 highest_phase_of_development = paste(unique(highest_phase_of_development), collapse="|"),
                 product_type = paste(unique(product_type), collapse="|"),
                 molecule_type = paste(unique(molecule_type), collapse="|"),
                 company_name = paste(unique(company_name), collapse="|"),
                 cas_number = paste(unique(cas_number), collapse="|"),
                 pubchem_id = paste(unique(pubchem_id), collapse="|"),
                 substance_of_origin = paste(unique(substance_of_origin), collapse="|"),
                 enzyme_classification_number = paste(unique(enzyme_classification_number), collapse="|"),
                 target = paste(unique(target), collapse="|"),
                 moa = paste(unique(moa), collapse="|"),
                 mode_of_action = paste(unique(mode_of_action), collapse="|"),
                 marketing_status = paste(unique(marketing_status), collapse="|"),
                 route_of_administration = paste(unique(route_of_administration), collapse="|"),
                 substance_of_origin = paste(unique(substance_of_origin), collapse="|"),
                 dosage_form = paste(unique(dosage_form), collapse="|"),
                 ephmra_drug_class = paste(unique(ephmra_drug_class), collapse="|"),
                 chemical_biological_class = paste(unique(chemical_biological_class), collapse="|"),
                 product_description = paste(unique(product_description), collapse="|"),
                 strength = paste(unique(strength), collapse="|")
)
prodsyn$uuid <- sapply(1:nrow(prodsyn), UUIDgenerate)

write.csv(prodsyn, file= file.path(medtrack_dir, "product_synopsis_unique_product_concat.rds"))


## Add updated company_product to list
l$company_product <- cp


## join product attributes into sales 
## to filter period when product was actively in market (generating >0 revenue)
sa2 <- merge(sa, prodsyn, by.x='product_name', by.y='product_name', all.x=T, all.y=F)

## add firms
sa2$firms <- NA
for (i in 1:nrow(sa2)) {
  firms_i <- c()
  x <- sa2$companies[i]
  for (firm in firms){
    for (pattern in firmsmap[[firm]]) {
      if (grepl(pattern, x, ignore.case = T, perl = T)) {
        firms_i <- c(firms_i, firm)
      }
    }
  }
  sa2$firms[i] <- paste(unique(firms_i), collapse = "|")
}

l$Sales <- sa2

products_firm_file <- file.path(medtrack_dir, "products_firm_sales_list.rds")
saveRDS(l, file=products_firm_file)
l <- readRDS(products_firm_file)


##===========================================
## Get Firm-Market network 
##  (by sales>0 --> active in market)
##
## # friendship <- sienaDependent(friendshipData)
##
## # smoke1 <- coCovar( smoke[ , 1 ] )
## # alcohol <- varCovar( drink )
##
## # mydata <- sienaDataCreate( friendship, smoke1, alcohol )
## # print01Report( mydata, modelname="s50")
##
## # myeff <- getEffects( mydata )
## # effectsDocumentation(myeff)
## # myeff <- includeEffects( myeff, simX, interaction1 = "smoke1" )
##
## # myalgorithm <- sienaAlgorithmCreate( projname = 's50' )
## # ans <- siena07( myalgorithm, data = mydata, effects = myeff)
##===========================================
library(parallel)
ncores <- detectCores()

leisure1 <- as.matrix(read.table("GL-1-lsr.dat"))
leisure2 <- as.matrix(read.table("GL-2-lsr.dat"))
(nrpupils <- dim(leisure1)[1])
(nrleisureItems <- dim(leisure1)[2])
nwaves <- 2
friendshipData <- array(c(leisure1, leisure2), dim=c(nrpupils, nrleisureItems, nwaves))
leisure <- sienaDependent(friendshipD,
                          "bipartite", nodeSet=c("pupils", "leisureItems"))

smoke1 <- coCovar( smoke[ , 1 ] )
alcohol <- varCovar( drink )

mydata <- sienaDataCreate( friendship, smoke1, alcohol )
print01Report( mydata, modelname="s50")

myeff <- getEffects( mydata )
effectsDocumentation(myeff)
myeff <- includeEffects( myeff, simX, interaction1 = "smoke1" )

myalgorithm <- sienaAlgorithmCreate( projname = 's50' )
ans <- siena07( myalgorithm, data = mydata, effects = myeff, 
                batch = T,   returnDeps = T, ## necessary for GOF
                useCluster = T, nbrNodes = ncores, clusterType = 'PSOCK')

gofo1f <- sienaGOF(ans, OutdegreeDistribution, join=TRUE,
                   varName="friendship", verbose = T)

##--------------------------------------------
## Make bipartite networks
##--------------------------------------------
dat <- l$Sales
dat$num_firms <- sapply(dat$firms, function(x) length(unlist(strsplit(x,'[|]'))) )
dat <- dat[which(dat$num_firms >= 1), ]

## subset to positive sales in years
yrs <- as.character(2006:2017)
nyrs <- length(yrs)
dimYrs <- 'Years'
idx <- apply(dat[,yrs], 1, function(x) !(all(is.na(x)) | all(x==0)) )
dat <- dat[idx,]
## filter older (new incomplete) years
dropyrs <- as.character(c(1970:2005,2018:2020))
dat <- dat[, !names(dat) %in% dropyrs]
## filter only using 'Total Global Sales' for now
dat <- dat[which(dat$region=='Total Global Sales'),]

## rows by columns
mkt_name <- 'therapeutic_category' 
sep <- ', '
mkts <- unique(unlist(strsplit(dat[,mkt_name], sep)))
#
dimnameRows <- 'Firms'
dimnameCols <- 'Markets'
nrows <- length(firms)
ncols <- length(mkts)
#
netWaves <- c()
for (k in 1:length(yrs)) {
  yr <- yrs[k]
  cat(sprintf(' yr %s (%.1f%s)\n',yr, 100*k/nyrs,'%'))
  yrnet <- matrix(0, nrow = nrows, ncol = ncols)
  for (j in 1:length(mkts)) {
    mk <- mkts[j]
    for (i in 1:length(firms)) {
      fm <- firms[i]
      dat[is.na(dat[,yr]), yr] <- 0
      idx.mk <- which(grepl(mk, dat[,mkt_name]) & grepl(fm, dat$firms) &  dat[,yr]>0 )
      if (length(idx.mk)>0) {
        yrnet[i,j] <- 1
      }
      x <- dat[.rows, .cols]

    }
  }
  netWaves <- c(netWaves, yrnet)
}
#
netWavesArray <- array(netWaves, dim=c(nrows, ncols, nyrs))
#
FIRMS <- sienaNodeSet(nrows, 'FIRMS', firms)
MARKETS <- sienaNodeSet(ncols, 'MARKETS', mkts)
#
depMMC <- sienaDependent(netWavesArray, type="bipartite", nodeSet=c('FIRMS','MARKETS'), 
                         sparse = FALSE, allowOnly = FALSE)
#
pharmaDat <- sienaDataCreate(depMMC, nodeSets=list(FIRMS, MARKETS) ) #,smoke1, alcohol
#
pharmaEffects <- getEffects(pharmaDat)
#
print01Report(pharmaDat, modelname="pharma-test-1")
#
# pharmaEffects <- includeEffects(pharmaEffects, transTrip, name="depMMC")
pharmaEffects <- includeEffects(pharmaEffects, cycle4, name="depMMC")
#
pharmaModel <- sienaAlgorithmCreate(projname='pharma-mmc-test-proj-1')

pharmaResults <- siena07(pharmaModel, data=pharmaDat, effects=pharmaEffects, 
                         batch = T,   returnDeps = T, ## necessary for GOF
                         useCluster = T, nbrNodes = ncores, clusterType = 'PSOCK')
pharmaResults

gofo1f <- sienaGOF(pharmaResults, OutdegreeDistribution, join=TRUE,
                   varName="depMMC", verbose = T)

##============================================
## Load news/events data for competition
##--------------------------------------------
con <- mongo(collection = 'news_firm', db = 'medtrack', url = 'mongodb://127.0.0.1')
ne <- con$find('{}') ## string type for news_subcategory
ne <- ne[which(ne$news_subcategory!='NaN'), ]
ne$news_body <- NULL

print(dim(ne))
print(names(ne))

##============================================
## Load products data list
##--------------------------------------------



cp <- l$company_product








