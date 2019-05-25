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
pr_dir <- file.path(medtrack_dir,'COMBINED','20190521','press_releases')


## set working dir
setwd(proj_dir)

notNA <- function(df, var, digits=2) {
  x <- round(100*sum(!is.na(df[,var]))/nrow(df), digits)
  cat(sprintf('\nNot NA %s: %.2f%s\n',var, x ,'%'))
}

printSummary <- function(df, var, digits=2) {
  cat(sprintf('\nSummary %s:\n',var))
  print(summary(df[,var]))
  cat(sprintf('    Std.Dev: %.2f',sd(df[,var], na.rm = T)))
  cat('\n')
}

##============================================
## LOAD PRODUCT LIST
##--------------------------------------------

pr_file <- file.path(pr_dir, "medtrack_press_release")
df <- read.csv(sprintf('%s.csv',pr_file), na.strings = c('','--','NaN','NA'), stringsAsFactors = F)
# df <- read_excel(sprintf('%s.xlsx',pr_file), na = c('','--','NaN'), trim_ws = T)

## fix ISFIRMSRC NAs --> 0
df$ISFIRMSRC[is.na(df$ISFIRMSRC)] <- 0

n <- nrow(df)
m <- ncol(df)

##============================================
## SUMMARIZE 
##--------------------------------------------

printReport <- function()
{
  ## Date range
  print(range(df$DATE))
  
  ## Body
  notNA(df, 'BODYTEXT')
  
  ## ABOUTFIRM1
  notNA(df, 'ABOUTFIRM1')
  ## ABOUTFIRM2
  notNA(df, 'ABOUTFIRM2')
  ## ABOUTFIRM3
  notNA(df, 'ABOUTFIRM3')
  ## ABOUTFIRM4
  notNA(df, 'ABOUTFIRM4')
  ## ABOUTFIRM5
  notNA(df, 'ABOUTFIRM5')
  
  ## SRC
  notNA(df, 'SRC')
  ## SRC2
  notNA(df, 'SRC2')
  
  ## ISFIRMSRC
  printSummary(df, 'ISFIRMSRC')
  
  ## FIRMID
  tmp <- df %>% count(FIRMID, sort = T)
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
  
  ## COUNTRY
  tmp <- df %>% count(COUNTRY, sort = T)
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$COUNTRY))   ## Missing
  midx <- which(grepl(',',tmp$COUNTRY) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(COUNTRY='Other Single', n=nsingle),
               data.frame(COUNTRY='Multiple', n=nmulti),
               data.frame(COUNTRY='None', n=nmissing),
               data.frame(COUNTRY='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  
  ## INDICATION
  tmp <- df %>% count(INDICATION, sort = T)
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$INDICATION))   ## Missing
  midx <- which(grepl(',',tmp$INDICATION) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(INDICATION='Other Single', n=nsingle),
               data.frame(INDICATION='Multiple', n=nmulti),
               data.frame(INDICATION='None', n=nmissing),
               data.frame(INDICATION='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  
  ## CATEGORY
  tmp <- df %>% count(CATEGORY, sort = T)
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$CATEGORY))   ## Missing
  midx <- which(grepl(',',tmp$CATEGORY) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(CATEGORY='Other Single', n=nsingle),
               data.frame(CATEGORY='Multiple', n=nmulti),
               data.frame(CATEGORY='None', n=nmissing),
               data.frame(CATEGORY='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)

  ## SUBCAT
  tmp <- df %>% count(SUBCAT, sort = T)
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$SUBCAT))   ## Missing
  midx <- which(grepl(',',tmp$SUBCAT) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(SUBCAT='Other Single', n=nsingle),
               data.frame(SUBCAT='Multiple', n=nmulti),
               data.frame(SUBCAT='None', n=nmissing),
               data.frame(SUBCAT='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  
  
  ## PRODNAME
  tmp <- df %>% count(PRODNAME, sort = T)
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$PRODNAME))   ## Missing
  midx <- which(grepl(',',tmp$PRODNAME) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 10 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(PRODNAME='Other Single', n=nsingle),
               data.frame(PRODNAME='Multiple', n=nmulti),
               data.frame(PRODNAME='None', n=nmissing),
               data.frame(PRODNAME='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  
  ## DEVPHASE
  tmp <- df %>% count(DEVPHASE, sort = T)
  nall <- nrow(tmp)
  idxall <- 1:nall
  naidx <- which(is.na(tmp$DEVPHASE))   ## Missing
  midx <- which(grepl(',',tmp$DEVPHASE) & !idxall %in% c(naidx))  ##Multiple
  sidx <- which(idxall > 15 & !idxall %in% c(naidx,midx))  ##Multiple
  idx <- which( !idxall %in% c(naidx,midx,sidx))
  ntop <- sum(tmp$n[idx])
  nmissing <- sum(tmp$n[naidx])
  nsingle <- sum(tmp$n[sidx])
  nmulti <- sum(tmp$n[midx])
  tmp <- rbind(tmp[idx,], 
               data.frame(DEVPHASE='Other Single', n=nsingle),
               data.frame(DEVPHASE='Multiple', n=nmulti),
               data.frame(DEVPHASE='None', n=nmissing),
               data.frame(DEVPHASE='TOTAL', n=ntop+nsingle+nmulti+nmissing)
  )
  print(tmp)
  
  
  ## ABOUTOTR1
  notNA(df, 'ABOUTOTR1')
  ## ABOUTOTR2
  notNA(df, 'ABOUTOTR2')
  ## ABOUTOTR3
  notNA(df, 'ABOUTOTR3')
  ## ABOUTOTR4
  notNA(df, 'ABOUTOTR4')
  ## ABOUTOTR5
  notNA(df, 'ABOUTOTR5')
  ## ABOUTOTR6
  notNA(df, 'ABOUTOTR6')
  ## ABOUTOTR7
  notNA(df, 'ABOUTOTR7')
  ## ABOUTOTR
  notNA(df, 'ABOUTOTR8')
  ## ABOUTOTR9
  notNA(df, 'ABOUTOTR9')
  
  
  ## RIGFOG
  printSummary(df, 'RIGFOG')
  ## RICONCENSUS
  printSummary(df, 'RICONSENSUS')

}
printReport()





