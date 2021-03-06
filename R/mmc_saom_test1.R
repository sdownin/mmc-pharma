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
library(parallel)
library(RSiena)
library(texreg)
library(lubridate)
ncores <- detectCores()

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
# prodsyn <- ddply(l$`Product Synopsis`, c('product_name'), summarize, 
#                  # product_name = paste(product_name, collapse="|"),
#                  other_names = paste(unique(other_names), collapse="|"),
#                  active_ingredient = paste(unique(active_ingredient), collapse="|"),
#                  therapeutic_category = paste(unique(therapeutic_category), collapse="|"),
#                  condition_treated = paste(unique(condition_treated), collapse="|"),
#                  therapeutic_class = paste(unique(therapeutic_class), collapse="|"),
#                  product_firm_uuid = paste(unique(uuid), collapse="|"),
#                  ##
#                  drug_delivery_technology = paste(unique(drug_delivery_technology), collapse="|"),
#                  highest_phase_of_development = paste(unique(highest_phase_of_development), collapse="|"),
#                  product_type = paste(unique(product_type), collapse="|"),
#                  molecule_type = paste(unique(molecule_type), collapse="|"),
#                  company_name = paste(unique(company_name), collapse="|"),
#                  cas_number = paste(unique(cas_number), collapse="|"),
#                  pubchem_id = paste(unique(pubchem_id), collapse="|"),
#                  substance_of_origin = paste(unique(substance_of_origin), collapse="|"),
#                  enzyme_classification_number = paste(unique(enzyme_classification_number), collapse="|"),
#                  target = paste(unique(target), collapse="|"),
#                  moa = paste(unique(moa), collapse="|"),
#                  mode_of_action = paste(unique(mode_of_action), collapse="|"),
#                  marketing_status = paste(unique(marketing_status), collapse="|"),
#                  route_of_administration = paste(unique(route_of_administration), collapse="|"),
#                  substance_of_origin = paste(unique(substance_of_origin), collapse="|"),
#                  dosage_form = paste(unique(dosage_form), collapse="|"),
#                  ephmra_drug_class = paste(unique(ephmra_drug_class), collapse="|"),
#                  chemical_biological_class = paste(unique(chemical_biological_class), collapse="|"),
#                  product_description = paste(unique(product_description), collapse="|"),
#                  strength = paste(unique(strength), collapse="|")
# )
# prodsyn$uuid <- sapply(1:nrow(prodsyn), UUIDgenerate)
# 
# write.csv(prodsyn, file= file.path(medtrack_dir, "product_synopsis_unique_product_concat.rds"))

# prodsyn <- read.csv(file.path(medtrack_dir, "product_synopsis_unique_product_concat.rds"))

# ## Add updated company_product to list
# l$company_product <- cp


# ## join product attributes into sales 
# ## to filter period when product was actively in market (generating >0 revenue)
# sa2 <- merge(sa, prodsyn, by.x='product_name', by.y='product_name', all.x=T, all.y=F)
# 
# ## add firms
# sa2$firms <- NA
# for (i in 1:nrow(sa2)) {
#   firms_i <- c()
#   x <- sa2$companies[i]
#   for (firm in firms){
#     for (pattern in firmsmap[[firm]]) {
#       if (grepl(pattern, x, ignore.case = T, perl = T)) {
#         firms_i <- c(firms_i, firm)
#       }
#     }
#   }
#   sa2$firms[i] <- paste(unique(firms_i), collapse = "|")
# }
# 
# l$Sales <- sa2

products_firm_file <- file.path(medtrack_dir, "products_firm_sales_list.rds")
# saveRDS(l, file=products_firm_file)
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
# #leisure1 <- as.matrix(read.table("GL-1-lsr.dat"))
# #leisure2 <- as.matrix(read.table("GL-2-lsr.dat"))
# (nrpupils <- dim(leisure1)[1])
# (nrleisureItems <- dim(leisure1)[2])
# nwaves <- 2
# friendshipData <- array(c(leisure1, leisure2), dim=c(nrpupils, nrleisureItems, nwaves))
# leisure <- sienaDependent(friendshipD,
#                           "bipartite", nodeSet=c("pupils", "leisureItems"))
# 
# smoke1 <- coCovar( smoke[ , 1 ] )
# alcohol <- varCovar( drink )
# 
# mydata <- sienaDataCreate( friendship, smoke1, alcohol )
# print01Report( mydata, modelname="s50")
# 
# myeff <- getEffects( mydata )
# effectsDocumentation(myeff)
# myeff <- includeEffects( myeff, simX, interaction1 = "smoke1" )
# 
# myalgorithm <- sienaAlgorithmCreate( projname = 's50' )
# ans <- siena07( myalgorithm, data = mydata, effects = myeff, 
#                 batch = T,   returnDeps = T, ## necessary for GOF
#                 useCluster = T, nbrNodes = ncores, clusterType = 'PSOCK')
# 
# gofo1f <- sienaGOF(ans, OutdegreeDistribution, join=TRUE,
#                    varName="friendship", verbose = T)

##--------------------------------------------
## Make bipartite networks
##--------------------------------------------
dat <- l$Sales
dat$num_firms <- sapply(dat$firms, function(x) length(unlist(strsplit(x,'[|]'))) )
dat <- dat[which(dat$num_firms >= 1), ]

## subset to positive sales in years
trtyr <- 2014
yr1 <- 2010  # 2006
yrs <- as.character(yr1:2017)
# yrs <- as.character(c(2010, 2013, 2016))
nyrs <- length(yrs)
dimYrs <- 'Years'
idx <- apply(dat[,yrs], 1, function(x) !(all(is.na(x)) | all(x==0)) )
dat <- dat[idx,]
## filter older (new incomplete) years
dropyrs <- as.character(1970:2020)[ ! as.character(1970:2020) %in% yrs]
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
netWaves <- c() ##trtWaves <- c()
moTrtWaves <- c()
dyTrtWaves <- c()
moTrtLinearWaves <- c()
dyTrtLinearWaves <- c()
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
    }
  }
  netWaves <- c(netWaves, yrnet)
  yrint <- as.numeric(yr)
  trt <- ifelse(yrint >= trtyr, 1, 0)
  trtlinear <- ifelse(yrint >= trtyr, 1+yrint-trtyr, 0)
  cat(sprintf(' trt %s, trtlinear %s\n',trt, trtlinear))
  moTrtWaves <- c(moTrtWaves, rep(trt, nrows))
  dyTrtWaves <- c(dyTrtWaves, matrix(trt, nrow = nrows, ncol=ncols))
  moTrtLinearWaves <- c(moTrtLinearWaves, rep(trtlinear, nrows))
  dyTrtLinearWaves <- c(dyTrtLinearWaves, matrix(trtlinear, nrow = nrows, ncol=ncols))
}
#
netWavesArray <- array(netWaves, dim=c(nrows, ncols, nyrs))
#
FIRMS <- sienaNodeSet(nrows, 'FIRMS', firms)
MARKETS <- sienaNodeSet(ncols, 'MARKETS', mkts)
#
depMMC <- sienaDependent(netWavesArray, type="bipartite", nodeSet=c('FIRMS','MARKETS'), 
                         sparse = FALSE, allowOnly = FALSE)
# TREATMENT
moTrtArray <- array(moTrtWaves, dim=c(nrows, nyrs))
moCovTrt <- varCovar(moTrtArray, nodeSet="FIRMS")
#
# dyTrtArray <- array(dyTrtWaves, dim=c(nrows, ncols, nyrs))
# dyCovTrt <- varDyadCovar(dyTrtArray, nodeSet=c("FIRMS",'MARKETS'))
#
moTrtLinearArray <- array(moTrtLinearWaves, dim=c(nrows, nyrs))
moCovTrtLinear <- varCovar(moTrtLinearArray, nodeSet="FIRMS")
#
# dyTrtLinearArray <- array(dyTrtLinearWaves, dim=c(nrows, ncols, nyrs))
# dyCovTrtLinear <- varDyadCovar(dyTrtLinearArray, nodeSet=c("FIRMS",'MARKETS'))
#
pharmaDat <- sienaDataCreate(depMMC, moCovTrt,    ##dyCovTrt, dyCovTrtLinear, 
                             nodeSets=list(FIRMS, MARKETS) ) #,smoke1, alcohol
#
pharmaEffects <- getEffects(pharmaDat)
#
# effectsDocumentation(pharmaEffects)
# print01Report(pharmaDat, modelname="pharma-test-1")
#
pharmaEffects <- includeEffects(pharmaEffects, cycle4, name="depMMC")
# pharmaEffects <- includeEffects(pharmaEffects, outAct, name="depMMC")
# pharmaEffects <- includeEffects(pharmaEffects, outActSqrt, name="depMMC")
pharmaEffects <- includeEffects(pharmaEffects, outAct, name="depMMC")
# pharmaEffects <- includeEffects(pharmaEffects, outInAss, name="depMMC")
pharmaEffects <- includeTimeDummy(pharmaEffects, cycle4)
# pharmaEffects <- includeEffects(pharmaEffects, egoSqX, name="depMMC", interaction1 = 'moCovTrtLinear', type = 'eval')
# pharmaEffects <- includeEffects(pharmaEffects, egoRThresholdX, name="depMMC", interaction1 = 'moCovTrtLinear',  type = 'eval')
# pharmaEffects <- includeEffects(pharmaEffects, egoLThresholdX, name="depMMC", interaction1 = 'moCovTrtLinear')
# pharmaEffects <- includeEffects(pharmaEffects, altInDist2, name="depMMC", interaction1 = 'moCovTrt')
# pharmaEffects <- includeEffects(pharmaEffects, totInDist2, name="depMMC", interaction1 = 'moCovTrt')
#
# pharmaEffects <- includeEffects(pharmaEffects, sameXCycle4, name="depMMC", interaction1 = 'moCovTrt')
# pharmaEffects <- includeEffects(pharmaEffects, sameWXClosure, name="depMMC", interaction1 = 'moCovTrt')
# pharmaEffects <- includeEffects(pharmaEffects, from, name="depMMC", interaction1 = 'moCovTrt')
# pharmaEffects <- includeEffects(pharmaEffects, to, name="depMMC", interaction1 = 'moCovTrt')
#
pharmaModel <- sienaAlgorithmCreate(projname='pharma-mmc-test-proj-1')

pharmaResults <- siena07(pharmaModel, data=pharmaDat, effects=pharmaEffects, 
                         batch = T,   returnDeps = T, ## necessary for GOF
                         useCluster = T, nbrNodes = ncores, clusterType = 'PSOCK')
pharmaResults
# summary(pharmaResults)
screenreg(pharmaResults, digits = 4)

#
gofo1f <- sienaGOF(pharmaResults, OutdegreeDistribution, join=TRUE,
                   varName="depMMC", verbose = T)
plot(gofo1f)

##============================================
## Load news/events data for competition
##--------------------------------------------
con <- mongo(collection = 'news_firm', db = 'medtrack', url = 'mongodb://127.0.0.1')
ne <- con$find('{}') ## string type for news_subcategory
ne <- ne[which(ne$news_subcategory!='NaN'), ]
ne <- ne[which( !is.na(ne$news_subcategory) & ne$news_subcategory!='--' & ne$news_subcategory!='' ), ]
ne$news_body <- NULL
ne$year <- year(mdy(ne$release_date))

print(dim(ne))
print(names(ne))

##
# Complexity measure of actions 
#  @see Yu, Subramaniam, & Cannella, 2009
##
complexity <- function(x, scale=FALSE) {
  sumx <- sum(x)
  sq.prop <- sapply(x, function(k) (k/sumx)^2 )
  y <- 1/sum(sq.prop)
  if (scale)
    return(y / length(x))
  return(y)
}

## behavior (competitive aggressiveness)
be <- data.frame()
for (yr in yrs) {
  cat(sprintf('yr %s\n', yr))
  year <- as.numeric(yr)
  ne.yr <- ne[which(ne$year==year), ]
  for (firm in firms) {
    ne.yr.f <- ne.yr[which(ne.yr$firm==firm), ]
    if(nrow(ne.yr.f)>0) {
      ne.yr.f.c <- plyr::count(ne.yr.f$news_subcategory)
      activ <- nrow(ne.yr.f)
      compl <- complexity(ne.yr.f.c$freq)
      compl.s <- 100 * complexity(ne.yr.f.c$freq, scale=TRUE)
    } else {
      activ <- 0
      compl <- 0
      compl.s <- 0
    }
    .tmp <- data.frame(yr=yr, firm=firm, activity=activ, complexity=compl, complexity.scale=compl.s, stringsAsFactors = F)
    be <- rbind(be, .tmp)
  }
}
be$activity_ln <- log(be$activity)

## subset to only observations with at least four actions
be.na <- be[which(be$activity>0), ]
## primary relationship
plot(x=be.na$activity, y=be.na$complexity)
## scaled relationship
# be.na$y.hat <- exp(predict(lm(log(complexity.scale) ~ activity, data=be.na), be.na))
be.na$y.hat <- predict(loess(complexity.scale ~ activity, data=be.na), be.na)
# be.na$y.hat <- exp(predict(glm(round(complexity.scale) ~ activity, data=be.na, family = poisson(link = "log")), be.na))
# be.na$y.hat <- exp(predict(glm.nb(round(complexity.scale) ~ activity, data=be.na), be.na))
be.na <- be.na[order(be.na$activity),]
plot(x=be.na$activity, y=be.na$complexity.scale)
lines(be.na$activity, be.na$y.hat, lwd='2', col='steelblue')
## activity_ln, complexity.scale)
cor(be.na[,3:6])
plot(x=be.na$activity_ln, y=be.na$complexity.scale)
abline(lm(complexity.scale ~ activity_ln, data=be.na))


##
# return integer values for any inputs vectors
##
behaviorDV <- function(x, limit=7) {
  r <- seq(0, limit)
  p <- r / max(r)
  q <- quantile(x, probs = p)
  b <- sapply(x, function(x_i){
    for (i in 1:length(q)) {
      q_i <- q[i]
      if (x_i <= q_i) {
        return(i-1)
      }
    }
    return(NA)
  })
  return(b)
}

##==================================
## Pharma Regression 2 DVs
##----------------------------------

activWaves <- c()
complWaves <- c()
for (yr in yrs) {
  be.yr <- be[which(be$yr==yr), ]
  activWaves <- c(activWaves, behaviorDV(be.yr$activity) )
  complWaves <- c(complWaves, behaviorDV(be.yr$complexity.scale) )
}

activWavesArray <- array(activWaves, dim=c(nrows, nyrs))
complWavesArray <- array(complWaves, dim=c(nrows, nyrs))
  
depActiv <- sienaDependent(activWavesArray, type="behavior", nodeSet=c('FIRMS'), 
                         sparse = FALSE, allowOnly = FALSE)
depCompl <- sienaDependent(complWavesArray, type="behavior", nodeSet=c('FIRMS'), 
                             sparse = FALSE, allowOnly = FALSE)

##
pharmaDat <- sienaDataCreate(depMMC, depActiv,    ##dyCovTrt, dyCovTrtLinear, 
                             nodeSets=list(FIRMS, MARKETS) ) #,smoke1, alcohol
#
pharmaEffects <- getEffects(pharmaDat)
#
effectsDocumentation(pharmaEffects)
# print01Report(pharmaDat, modelname="pharma-test-1")

# pharmaEffects <- includeEffects(pharmaEffects, linear, name="depActiv")
pharmaEffects <- includeEffects(pharmaEffects, outdeg, name="depActiv", interaction1 = 'depMMC')
pharmaEffects <- includeEffects(pharmaEffects, outRate, name="depActiv", interaction1 = 'depMMC', type='rate')
# pharmaEffects <- includeEffects(pharmaEffects, isolate, name="depActiv", interaction1 = 'depMMC')
# pharmaEffects <- includeEffects(pharmaEffects, popAlt, name="depActiv", interaction1 = 'depMMC')
pharmaEffects <- includeEffects(pharmaEffects, density, name="depMMC")
# pharmaEffects <- includeEffects(pharmaEffects, inPop, name="depMMC")
# pharmaEffects <- includeEffects(pharmaEffects, outAct, name="depMMC")
pharmaEffects <- includeEffects(pharmaEffects, cycle4, name="depMMC")
pharmaEffects <- includeEffects(pharmaEffects, sameXCycle4, name="depMMC", interaction1 = 'depActiv')
#
pharmaEffects <- includeTimeDummy(pharmaEffects, outdeg, name="depActiv", interaction1 = 'depMMC', timeDummy = '5,6,7,8')
pharmaEffects <- includeTimeDummy(pharmaEffects, sameXCycle4, name="depMMC", interaction1 = 'depActiv', timeDummy = '5,6,7,8')

#
pharmaModel <- sienaAlgorithmCreate(projname='pharma-mmc-test-proj-1')

pharmaResults <- siena07(pharmaModel, data=pharmaDat, effects=pharmaEffects, 
                         batch = T,   returnDeps = T, ## necessary for GOF
                         useCluster = T, nbrNodes = ncores, clusterType = 'PSOCK')

screenreg(pharmaResults, digits = 4)


##
##
pharmaDat <- sienaDataCreate(depMMC, depCompl,    ##dyCovTrt, dyCovTrtLinear, 
                             nodeSets=list(FIRMS, MARKETS) ) #,smoke1, alcohol
pharmaEffects <- getEffects(pharmaDat)

# pharmaEffects <- includeEffects(pharmaEffects, linear, name="depActiv")
pharmaEffects <- includeEffects(pharmaEffects, outdeg, name="depCompl", interaction1 = 'depMMC')
pharmaEffects <- includeEffects(pharmaEffects, outRate, name="depCompl", interaction1 = 'depMMC', type='rate')
pharmaEffects <- includeEffects(pharmaEffects, density, name="depMMC")
pharmaEffects <- includeEffects(pharmaEffects, cycle4, name="depMMC")
pharmaEffects <- includeEffects(pharmaEffects, sameXCycle4, name="depMMC", interaction1 = 'depCompl')
#
pharmaEffects <- includeTimeDummy(pharmaEffects, outdeg, name="depCompl", interaction1 = 'depMMC', timeDummy = '5,6,7')
pharmaEffects <- includeTimeDummy(pharmaEffects, sameXCycle4, name="depMMC", interaction1 = 'depCompl', timeDummy = '5,6,7')

#
pharmaModel <- sienaAlgorithmCreate(projname='pharma-mmc-test-proj-1')

pharmaResults2 <- siena07(pharmaModel, data=pharmaDat, effects=pharmaEffects, 
                         batch = T,   returnDeps = T, ## necessary for GOF
                         useCluster = T, nbrNodes = ncores, clusterType = 'PSOCK')

screenreg(list(m1, pharmaResults2), digits = 4)












