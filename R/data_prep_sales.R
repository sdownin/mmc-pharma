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

## Check regions frequency
region.summary <- dfl %>%
  filter(!is.na(Region) & Region != "Total Global Sales") %>%
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
firms.us <- c("Johnson & Johnson",'Pfizer','Amgen','Biogen','Eli Lilly','AbbVie',
              'Merck','Abbott','Gilead Sciences','Biogen','Bristol-Myers Squibb')
firms.eu <- c('Novartis','GlaxoSmithKline','AstraZeneca','Roche',
              'Sanofi','Bayer','Novo Nordisk')

## flag is single company product sales (not multiple) 
dfl$is_single <- as.integer(sapply(dfl$Companies, function(x) length(strsplit(x,"[|]")[[1]])==1 ))

## region subnames
r.us <- "USA"
r.eu <- "Europe"
## add region column
dfl$is_us <- as.integer(grepl(r.us, dfl$Region) & !grepl(r.eu, dfl$Region))
dfl$is_eu <- as.integer(grepl(r.eu, dfl$Region) & !grepl(r.us, dfl$Region))
## regional subset
df.us <- dfl[dfl$is_us==1, ]
df.eu <- dfl[dfl$is_eu==1, ]

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

##--------------------------
## Combined
##--------------------------
dfsep.us$region_sales <- "USA"
dfsep.eu$region_sales <- "Europe"
dfsep <- rbind(dfsep.us, dfsep.eu)

## Exclude companies outside of two main sphere_group of influence groups (USA,Europe)
dfsep2 <- dfsep[dfsep$sphere_group != "Other", ]

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
View(grp.sales)

## plotting
library(ggplot2)

## data subset for plot
df.plot <- grp.sales[grp.sales$year > 2002 & grp.sales$year < 2018, ]

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
rev.plot1 <- ggplot(aes(x=date, y=sales_total/1000, colour=sphere_group, pch=sphere_group), data=df.plot) + 
  geom_line() + geom_point() + 
  facet_grid(region_sales ~ .) +
  geom_vline(xintercept = as.Date("2013-01-01"), lty=2, lwd=1.05, col='gray') + 
  geom_vline(xintercept = as.Date("2010-03-01"), lty=3, lwd=1.05, col='gray') + 
  ylab("Total Sales (US$ Bn.)") + labs(colour = "Firm Group", pch="Firm Group") +
  ggtitle("Healthcare Legislation Natural Experiment:\nImpact of the PPACA on Pharmaceutical Sales in USA vs Europe") + 
  annotate("text", x=as.Date("2011-11-20"), y=a.y, label="Individual\nMandate", size=4, col='darkgray') +
  annotate("text", x=as.Date("2009-05-20"), y=a.y, label="PPACA\nSigned", size=4, col='darkgray') +
  scale_color_manual(values=c('black', gray.colors(1,.5,.5))) + 
  theme_bw() + theme(legend.position = 'right')
rev.plot1
## save plot
rev.plot1.filename <- file.path("img","sphere_group_EuropeUSA_revenue_total_pharma_sales.png")
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
eu.us <- df.plot$sales_total[df.plot$sphere_group=="European" & df.plot$region_sales=="USA"]
eu.eu <- df.plot$sales_total[df.plot$sphere_group=="European" & df.plot$region_sales=="Europe"]

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
## European Firms in USA Market
##----------------------------------------
## design matrix
X2 <-  zoo(cbind(eu.us, us.us, eu.eu, us.eu), time.points) ## rescale by millions

## run CausalImpact Bayesian Structual Time Series Inference of causal impact
set.seed(1111)
imp2 <- CausalImpact(X2/1000, range.pre, range.post, alpha = .01, 
                     model.args = list(niter=10000))

## create plot
ciplot2 <- plot(imp2) + 
  ggtitle(sprintf("European Firms in USA:  %s",pvalstr(imp2$summary$p[2]))) + 
  ylab("US$ Bn.") +
  theme_bw()
## echo plot
ciplot2
## save plot
ciplot2.filename <- file.path("img","causal_inference_European_pharma_rev_inUSA_99ci.png")
ggsave(filename = ciplot2.filename, plot = ciplot2, 
       width=5.5, height=5.5, units='in', dpi=200)

## print summary and report
summary(imp2)
summary(imp2, "report")




##----------------------------------------
## (US - EU) Firms in USA Market
##----------------------------------------
## design matrix
X3.us <-  zoo(cbind(us.us, us.eu), time.points) ## rescale by millions
X3.eu <-  zoo(cbind(eu.us, eu.eu), time.points) ## rescale by millions
# d.us <- us.us-eu.us
# X3 <-  zoo(cbind(d.us, eu.us, us.eu, eu.eu), time.points) ## rescale by millions

# X3s <- X3.us / (X3.us + X3.eu) 
X3d <- X3.us - X3.eu

## run CausalImpact Bayesian Structual Time Series Inference of causal impact
set.seed(1111)
imp3 <- CausalImpact(X3d, range.pre, range.post, alpha = .01, 
                     model.args = list(niter=10000, prior.level.sd=.5))

## create plot
ciplot3 <- plot(imp3) + 
  ggtitle(sprintf("PPACA Effect in USA:  %s",pvalstr(imp3$summary$p[2]))) + 
  ylab("Revenue Difference in USA (US$ Bn.)") +
  theme_bw()
## echo plot
ciplot3
## save plot
ciplot3.filename <- file.path("img","causal_inference_American_pharma_rev_DIFF_inUSA_99ci.png")
ggsave(filename = ciplot3.filename, plot = ciplot3, 
       width=5.5, height=5.5, units='in', dpi=200)

## print summary and report
summary(imp3)
summary(imp3, "report")

