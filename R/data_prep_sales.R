####################################################
##
##  Pharma MMC
##
##   - data prep: sales data pre-analysis, 
##                Bayesian structural timeseries
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
library(dplyr)
library(reshape2)
library(tibble)

## dir names
proj_dir <- "C:/Users/T430/Google Drive/PhD/Research/MMC/pharma_encounters/mmc-pharma"
medtrack_dir <- file.path(proj_dir,"medtrack_data")
medtrack_sales_dir <- file.path(medtrack_dir,"sales")

## set working dir
setwd(proj_dir)

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

##
exclude.regions <- c("Total Global Sales", "Worldwide","Rest Of World")


## Check regions frequency
region.summary <- dfl %>%
  filter(!is.na(Region) & (! Region %in% exclude.regions )) %>%
  group_by(Region, Currency) %>%
  tally(sort = T) %>%
  ungroup() %>%
  arrange(desc(n))
print(region.summary)
# ## same result using count() instead of group_by(),tally(),ungroup()
# region.summary2 <- dfl %>%
#   filter(!is.na(Region)) %>%
#   count(Region, Currency) %>%
#   arrange(desc(n))

## Firms by regional sphere_group of influence (coarse separation by continent)
firms.us <- unique(
  c("Johnson & Johnson",'Pfizer','Amgen','Biogen','Eli Lilly','AbbVie',
    'Merck','Abbott','Gilead Sciences','Bristol-Myers Squibb')
  )
firms.eu <- unique(
  c('Novartis','GlaxoSmithKline','AstraZeneca','Roche',
    'Sanofi','Bayer','Novo Nordisk')
  )

## flag is single company product sales (not multiple) 
dfl$is_single <- as.integer(sapply(dfl$Companies, function(x) length(strsplit(x,"[|]")[[1]])==1 ))

## region subnames
r.us <- "USA"
r.eu <- "Europe"
r.jp <- "Japan"
## add region column
dfl$is_us <- as.integer(grepl(r.us, dfl$Region))
dfl$is_eu <- as.integer(grepl(r.eu, dfl$Region))
dfl$is_jp <- as.integer(grepl(r.jp, dfl$Region))
## regional subset
df.us <- dfl[dfl$is_us==1 & dfl$is_eu==0 & dfl$is_jp==0, ]
df.eu <- dfl[dfl$is_us==0 & dfl$is_eu==1 & dfl$is_jp==0, ]
df.jp <- dfl[dfl$is_us==0 & dfl$is_eu==0 & dfl$is_jp==1, ]
df.bo <- dfl[ (dfl$is_us==1 & dfl$is_eu==1) | (dfl$is_us==1 & dfl$is_jp==1) | (dfl$is_eu==1 & dfl$is_jp==1) , ] ## both
df.ot <- dfl[dfl$is_us==0 & dfl$is_eu==0 & dfl$is_jp==0, ] ## other (neither)

## split product sales by party
## use SIMPLE assumption of equal revenue share

##------------------------------
## USA
##------------------------------
## single firm
dfx <- as_tibble(df.us)
dfx2 <- dfx[dfx$is_single==0, ]
dfxsep <- dfx[dfx$is_single==1, ]
## check if need to split companies
if (nrow(dfx2)>0) print("need to split some products among multiple companies")
## SPLIT IF NECESSARY
## rename
dfsep.us <- dfxsep
## firm group index
dfsep.us$sphere_group <- apply(dfsep.us[,"Companies"], 1, function(product_comanies){
    x.eu <- sapply(firms.eu, function(firm)grepl(firm, product_comanies))
    if (any(x.eu)) return("European")
    x.us <- sapply(firms.us, function(firm)grepl(firm, product_comanies))
    if (any(x.us)) return("American")
    return("Other")
  })

##------------------------------
## Europe
##------------------------------
## single firm
dfx <- as_tibble(df.eu)
dfx2 <- dfx[dfx$is_single==0, ]
dfxsep <- dfx[dfx$is_single==1, ]
## check if need to split companies
if (nrow(dfx2)>0) print("need to split some products among multiple companies")
## SPLIT IF NECESSARY
## rename
dfsep.eu <- dfxsep
## firm group index
dfsep.eu$sphere_group <- apply(dfsep.eu[,"Companies"], 1, function(product_comanies){
    x.eu <- sapply(firms.eu, function(firm)grepl(firm, product_comanies))
    if (any(x.eu)) return("European")
    x.us <- sapply(firms.us, function(firm)grepl(firm, product_comanies))
    if (any(x.us)) return("American")
    return("Other")
  })

##------------------------------
## Japan
##------------------------------
## single firm
dfx <- as_tibble(df.jp)
dfx2 <- dfx[dfx$is_single==0, ]
dfxsep <- dfx[dfx$is_single==1, ]
## check if need to split companies
if (nrow(dfx2)>0) print("need to split some products among multiple companies")
## SPLIT IF NECESSARY
## rename
dfsep.jp <- dfxsep
## firm group index
dfsep.jp$sphere_group <- apply(dfsep.jp[,"Companies"], 1, function(product_comanies){
  x.eu <- sapply(firms.eu, function(firm)grepl(firm, product_comanies))
  if (any(x.eu)) return("European")
  x.us <- sapply(firms.us, function(firm)grepl(firm, product_comanies))
  if (any(x.us)) return("American")
  return("Other")
})


##------------------------------
## Both
##------------------------------
## single firm
dfx <- as_tibble(df.bo)
dfx2 <- dfx[dfx$is_single==0, ]
dfxsep <- dfx[dfx$is_single==1, ]
## check if need to split companies
if (nrow(dfx2)>0) print("need to split some products among multiple companies")
## SPLIT IF NECESSARY
## rename
dfsep.bo <- dfxsep
## firm group index
dfsep.bo$sphere_group <- apply(dfsep.bo[,"Companies"], 1, function(product_comanies){
  x.eu <- sapply(firms.eu, function(firm)grepl(firm, product_comanies))
  if (any(x.eu)) return("European")
  x.us <- sapply(firms.us, function(firm)grepl(firm, product_comanies))
  if (any(x.us)) return("American")
  return("Other")
})

##------------------------------
## Other
##------------------------------
## single firm
dfx <- as_tibble(df.ot)
dfx2 <- dfx[dfx$is_single==0, ]
dfxsep <- dfx[dfx$is_single==1, ]
## check if need to split companies
if (nrow(dfx2)>0) print("need to split some products among multiple companies")
## SPLIT IF NECESSARY
## rename
dfsep.ot <- dfxsep
## firm group index
dfsep.ot$sphere_group <- apply(dfsep.ot[,"Companies"], 1, function(product_comanies){
  x.eu <- sapply(firms.eu, function(firm)grepl(firm, product_comanies))
  if (any(x.eu)) return("European")
  x.us <- sapply(firms.us, function(firm)grepl(firm, product_comanies))
  if (any(x.us)) return("American")
  return("Other")
})


##--------------------------
## Combined
##--------------------------
dfsep.us$region_sales <- "USA"
dfsep.eu$region_sales <- "Europe"
dfsep.jp$region_sales <- "Japan"
dfsep.bo$region_sales <- "Multiple"
dfsep.ot$region_sales <- "Other"
dfsep <- rbind(dfsep.us, dfsep.eu, dfsep.jp, dfsep.ot, dfsep.bo)

## Exclude companies outside of two main sphere_group of influence groups (USA,Europe)
# dfsep2 <- dfsep[dfsep$sphere_group != "Other", ]
dfsep2 <- dfsep

## sum products by sphere_group of influence group
grp.sales <- dfsep2 %>% # filter(!is.na(Region)) %>%
  group_by(sphere_group, region_sales, year) %>%
  # ungroup() %>%
  summarise(sales_total = sum(sales, na.rm=T))
## fix data types
grp.sales$year <- as.integer(as.character(grp.sales$year))
grp.sales$sphere_group <- as.factor(grp.sales$sphere_group)
grp.sales$region_sales <- as.factor(grp.sales$region_sales)
grp.sales$date <- as.Date(sapply(grp.sales$year, function(t)sprintf('%s-01-01',t)))
# View(grp.sales)

## plotting
library(ggplot2)

## data subset for plot
plt.idx <- which(
  grp.sales$year > 2002 & grp.sales$year < 2018 & 
  grp.sales$region_sales %in% c('USA','Europe') &   #Japan
  grp.sales$sphere_group %in% c('American','European')
)
df.plot <- grp.sales[plt.idx, ]

# ## annotations
# a.y <- max(df.plot$sales_total) * .87
# ##plot figure
# ggplot(aes(x=year, y=sales_total, colour=sphere_group), data=df.plot) + 
#   geom_line() + geom_point() + facet_grid(region_sales ~ .) +
#   geom_vline(xintercept = 2013, lty=1, lwd=1.05) + 
#   geom_vline(xintercept = 2010.33, lty=2, lwd=1.05) + 
#   ylab("Total Sales (US$ MM)") + labs(colour = "Firm Group") +
#   theme_bw() + 
#   annotate("text", x=2013.8, y=a.y, label='bold("ACA\nImplemented")', size=3.5, parse=T) +
#   annotate("text", x=2011, y=a.y, label='"ACA\nAnnounced"', size=3.5, parse=T)

## annotations
a.y <- max(df.plot$sales_total/1000) * .9
##plot figure
rev.plot1 <- ggplot(aes(x=date, y=sales_total/1000, colour=sphere_group, pch=sphere_group), data=df.plot[df.plot$region_sales!='Japan',]) + 
  geom_line() + geom_point() + 
  facet_grid(region_sales ~ .) +
  geom_vline(xintercept = as.Date("2013-01-01"), lty=2, lwd=1.05, col='gray') + 
  geom_vline(xintercept = as.Date("2010-03-01"), lty=3, lwd=1.05, col='gray') + 
  ylab("Total Sales (US$ Bn.)") + xlab("Year") + 
  labs(colour = "Competitor\nConstellation", pch="Competitor\nConstellation") +
  ggtitle("Healthcare Legislation Natural Experiment:\nImpact of the PPACA on Pharmaceutical Sales in USA vs Europe") + 
  annotate("text", x=as.Date("2011-11-20"), y=a.y, label="Individual\nMandate", size=4, col='darkgray') +
  annotate("text", x=as.Date("2009-05-20"), y=a.y, label="PPACA\nSigned", size=4, col='darkgray') +
  scale_color_manual(values=c('black', gray.colors(1,.5,.5),'steelblue')) + 
  theme_bw() + theme(legend.position = 'right')
rev.plot1
## save plot
rev.plot1.filename <- file.path("img","sphere_group_EuropeUSA_revenue_total_pharma_sales.png")
ggsave(filename = rev.plot1.filename, plot = rev.plot1, 
       width=7.5, height=5, units='in', dpi=200)

## classic theme -- consteallations title
rev.plot1 <- ggplot(aes(x=date, y=sales_total/1000, colour=sphere_group, pch=sphere_group), data=df.plot) + 
  geom_line(lwd=1.1) + #geom_point() + 
  facet_grid(region_sales ~ .) +
  geom_vline(xintercept = as.Date("2013-01-01"), lty=2, lwd=1.05, col='gray') + 
  geom_vline(xintercept = as.Date("2010-03-01"), lty=3, lwd=1.05, col='gray') + 
  ylab("Total Sales (US$ Bn.)") + xlab("Year") + 
  labs(colour = "Competitor\nConstellation", pch="Competitor\nConstellation") +
  ggtitle("Pharmaceutical Competitor Constellation Sales in USA vs Europe") + 
  annotate("text", x=as.Date("2011-11-20"), y=a.y, label="Coverage\nBegan", size=4, col='darkgray') +
  annotate("text", x=as.Date("2009-05-20"), y=a.y, label="PPACA\nSigned", size=4, col='darkgray') +
  scale_color_manual(values=c('black', 'darkgrey')) + 
  theme_classic() + theme(legend.position = 'right')
rev.plot1
## save plot
rev.plot1.filename <- file.path("img","comp_constellations_EuropeUSA_revenue_total_pharma_sales_theme_classic.png")
ggsave(filename = rev.plot1.filename, plot = rev.plot1, 
       width=7.5, height=5, units='in', dpi=200)


##========================================================
## Bayesian Structural Time Series Check of Causal Impact
##  of USA healthcare legislation on total pharma revenue 
##  in USA
##--------------------------------------------------------
library(CausalImpact)

##
# Print p-val function
##
pvalstr <- function(p, alpha=0.01, digits=3) {
  if (p < 0.001) return("Inferred Causation (p<0.001)")
  if (p < alpha) return(sprintf("Inferred Causation (p=%s)",round(p,digits)))
  return(sprintf("Insignificant (p=%s)",round(p,digits)))
}


## [sphere_group].[regional_sales]
us.us <- df.plot$sales_total[df.plot$sphere_group=="American" & df.plot$region_sales=="USA"]
us.eu <- df.plot$sales_total[df.plot$sphere_group=="American" & df.plot$region_sales=="Europe"]
us.jp <- df.plot$sales_total[df.plot$sphere_group=="American" & df.plot$region_sales=="Japan"]
eu.us <- df.plot$sales_total[df.plot$sphere_group=="European" & df.plot$region_sales=="USA"]
eu.eu <- df.plot$sales_total[df.plot$sphere_group=="European" & df.plot$region_sales=="Europe"]
eu.jp <- df.plot$sales_total[df.plot$sphere_group=="European" & df.plot$region_sales=="Japan"]

## time points
len <- length(unique(df.plot$year))
time.points <- as.Date(sapply((2017-len+1):2017, function(t)sprintf('%s-01-01',t)))
range.pre <- c(time.points[1], time.points[len-4])
range.post <- c(time.points[len-3], time.points[len])

##----------------------------------------
## USA Firms in USA Market
##----------------------------------------
## design matrix
X <-  zoo(cbind(us.us, eu.us, us.eu, eu.eu), time.points) ## rescale by millions

## run CausalImpact Bayesian Structual Time Series Inference of causal impact
set.seed(1111)
imp <- CausalImpact(X/1000, range.pre, range.post, alpha = .01, 
                    model.args = list(niter=10000))

## create plot
ciplot <- plot(imp) + 
  ggtitle(sprintf("American Firms in USA:  %s",pvalstr(imp$summary$p[2]))) + 
  ylab("US$ Bn.") +
  theme_bw()
## echo plot
ciplot
## save plot
ciplot.filename <- file.path("img","causal_inference_American_pharma_rev_inUSA_99ci.png")
ggsave(filename = ciplot.filename, plot = ciplot, 
       width=5.5, height=5.5, units='in', dpi=200)

## print summary and report
summary(imp)
summary(imp, "report")


##----------------------------------------
## (US - EU) Firms in USA Market
##    Controls:  Europe
##----------------------------------------
## design matrix
X3.us <-  zoo(cbind(us.us, us.eu), time.points) ## rescale by millions
X3.eu <-  zoo(cbind(eu.us, eu.eu), time.points) ## rescale by millions

# X3s <- X3.us / (X3.us + X3.eu) 
X3d <- X3.us - X3.eu

## run CausalImpact Bayesian Structual Time Series Inference of causal impact
set.seed(1111)
imp3 <- CausalImpact(X3d, range.pre, range.post, alpha = .01, 
                     model.args = list(niter=10000, prior.level.sd=.5))

## create plot
ciplot3 <- plot(imp3) +   
  ggtitle(sprintf("PPACA Effect in USA:  %s",pvalstr(imp3$summary$p[2]))) + 
  ylab("Competitor Constellation\nRevenue Difference in USA (US$ Bn.)") +
  xlab("Year") +
  theme_bw() #+
#annotate("text", x=as.Date("2011-11-20"), y=a.y, label="Individual\nMandate", size=4, col='darkgray') 
## echo plot
ciplot3
## save plot
ciplot3.filename <- file.path("img","causal_inference_American_pharma_rev_DIFF_inUSA_99ci.png")
ggsave(filename = ciplot3.filename, plot = ciplot3, 
       width=5.1, height=7, units='in', dpi=200)

## print summary and report
summary(imp3)
summary(imp3, "report")



##----------------------------------------
## (US - EU) Firms in USA Market
##   Controls:  Europe, Japan
##----------------------------------------
## design matrix
X3.us <-  zoo(cbind(us.us, us.eu, us.jp), time.points) ## rescale by millions
X3.eu <-  zoo(cbind(eu.us, eu.eu, eu.jp), time.points) ## rescale by millions

# X3s <- X3.us / (X3.us + X3.eu) 
X3d <- X3.us - X3.eu

## run CausalImpact Bayesian Structual Time Series Inference of causal impact
set.seed(1111)
imp3 <- CausalImpact(X3d/1000, range.pre, range.post, alpha = .01, 
                     model.args = list(niter=10000, prior.level.sd=1/3))

## create plot
ciplot3 <- plot(imp3) +   
  ggtitle(sprintf("PPACA Effect in USA:  %s",pvalstr(imp3$summary$p[2]))) + 
  ylab("Competitor Constellation\nRevenue Difference in USA (US$ Bn.)") +
  xlab("Year") +
  theme_bw() #+
  #annotate("text", x=as.Date("2011-11-20"), y=a.y, label="Individual\nMandate", size=4, col='darkgray') 
## echo plot
ciplot3
## save plot
ciplot3.filename <- file.path("img","causal_inference_American_pharma_rev_DIFF_inUSA_2ctrlEuroJap_99ci.png")
ggsave(filename = ciplot3.filename, plot = ciplot3, 
       width=5.1, height=7, units='in', dpi=200)

## print summary and report
summary(imp3)
summary(imp3, "report")

