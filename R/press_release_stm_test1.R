
library(mongolite)
library(stm)
library(lubridate)
library(plyr)
library(dplyr)


##
# LOAD
##
con <- mongo(collection = 'press_release', db = 'medtrack', url = 'mongodb://127.0.0.1')
pr <- con$find('{}') ## string type for news_subcategory
pr <- pr[which(pr$SUBCAT!='NaN'), ]
pr <- pr[which( !is.na(pr$SUBCAT) & pr$SUBCAT!='--' & pr$SUBCAT!='' ), ]
pr <- pr[order(pr$DATE, decreasing = F),]
pr$year <- year(pr$DATE)
pr$timestamp <-  (as.numeric(pr$DATE)) / 1e6


print(dim(pr))
print(names(pr))

## cache all
if ( !'pr.all' %in% ls())
  pr.all <- pr

## filter
yr.max <- 2017
yr.min <- 2017
pr.idx <- which(
  pr.all$year <= yr.max & 
    pr.all$year >= yr.min & 
    pr.all$RIGFOG > 0 & 
    pr.all$RIGFOG < 50
)
pr <- pr.all[pr.idx, ]
pr <- pr[1:min(nrow(pr),1000),]

## filter limit

isEmpty <- function(x=NA) {
  if (is.null(x))
    return(TRUE)
  if (is.na(x) | is.nan(x) | trimws(x)=='') 
    return(TRUE)
  return(FALSE)
}

pr$firm_factor <- as.factor(pr$FIRMID)

## Fix isfirmsource NAs
pr$ISFIRMSRC <- sapply(pr$ISFIRMSRC, function(x) ifelse(isEmpty(x)|x==0, 0, 1))

## add binary vars for about firm sections
pr$has_about_firm_2 <- sapply(pr$ABOUTFIRM2, function(x) ifelse(isEmpty(x), 0, 1))
pr$has_about_firm_3 <- sapply(pr$ABOUTFIRM3, function(x) ifelse(isEmpty(x), 0, 1))
# pr$has_about_firm_4 <- sapply(pr$ABOUTFIRM4, function(x) ifelse(isEmpty(x), 0, 1))
# pr$has_about_firm_5 <- sapply(pr$ABOUTFIRM5, function(x) ifelse(isEmpty(x), 0, 1))
#### add binary vars for about other sections
pr$has_about_otr_1 <- sapply(pr$ABOUTOTR1, function(x) ifelse(isEmpty(x), 0, 1))
# pr$has_about_otr_2 <- sapply(pr$ABOUTOTR2, function(x) ifelse(isEmpty(x), 0, 1))
# pr$has_about_otr_3 <- sapply(pr$ABOUTOTR3, function(x) ifelse(isEmpty(x), 0, 1))
# pr$has_about_otr_4 <- sapply(pr$ABOUTOTR4, function(x) ifelse(isEmpty(x), 0, 1))
# pr$has_about_otr_5 <- sapply(pr$ABOUTOTR5, function(x) ifelse(isEmpty(x), 0, 1))
# pr$has_about_otr_6 <- sapply(pr$ABOUTOTR6, function(x) ifelse(isEmpty(x), 0, 1))
# pr$has_about_otr_7 <- sapply(pr$ABOUTOTR7, function(x) ifelse(isEmpty(x), 0, 1))
# pr$has_about_otr_8 <- sapply(pr$ABOUTOTR8, function(x) ifelse(isEmpty(x), 0, 1))

# cov.names <- c('timestamp','RIGFOG','ISFIRMSRC',
#                'has_about_firm_2','has_about_firm_3',
#                'has_about_firm_4','has_about_firm_5',
#                'has_about_otr_1','has_about_otr_2','has_about_otr_3',
#                'has_about_otr_4','has_about_otr_5','has_about_otr_6',
#                'has_about_otr_7','has_about_otr_8')
cov.names <- c('timestamp','RIGFOG','ISFIRMSRC',
               'has_about_firm_2','has_about_firm_3',
               'has_about_otr_1')

## Process Text (build corpus, remove punctuation, stopwords, numbers; stemming)
processed <- textProcessor(pr$BODYTEXT, metadata = pr[,cov.names])
## Prepare documents  (remove some terms due to frequency)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
## cache document data objects
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

## Structural Topic Model
prFit1 <- stm(documents = out$documents, vocab = out$vocab, K = 10, 
              prevalence =~ RIGFOG + ISFIRMSRC + s(timestamp) + has_about_firm_2 ,
               content =~ RIGFOG,
               max.em.its = 75, data = out$meta,
               init.type = "Spectral")


##
prSelect1 <- selectModel(out$documents, out$vocab, K = 10,
                         prevalence =~ RIGFOG + ISFIRMSRC + s(timestamp) + has_about_firm_2 ,
                         content =~ RIGFOG,
                        max.em.its = 15,
                        data = out$meta, runs = 20, seed = 8458159)

##
plotModels(prSelect1, pch=c(1,2,3,4), legend.position="bottomright")

##
storage <- searchK(out$documents, out$vocab, K = c(7, 10),
                   prevalence =~ RIGFOG + ISFIRMSRC + s(timestamp) + has_about_firm_2 ,
                   content =~ RIGFOG,
                   data = meta)


##
labelTopics(prFit1, c(3, 7, 20))


# ##
# thoughts3 <- findThoughts(prFit1, texts = shortdoc,
#                           n = 2, topics = 3)$docs[[1]]


# ##
# plotQuote(thoughts20, width = 30, main = "Topic 20")



##
out$meta$rating <- as.factor(out$meta$rating)
prep1 <- estimateEffect(1:20 ~ RIGFOG + ISFIRMSRC + s(timestamp) + has_about_firm_2 , 
                       prFit1,
                       meta = out$meta, uncertainty = "Global")
summary(prep1, topics=1)


## plot expected topic proportions
plot(prFit1, type = "summary", xlim = c(0, .3))


## plot coefficient effect of (Liberal vs Conservative)
plot(prep1, covariate = "RIGFOG", topics = c(3, 7, 20),
     model = prFit1, method = "difference",
     cov.value1 = "Complex", cov.value2 = "Simple",
     xlab = "More Complex ... More Simple",
     main = "Effect of Complex vs. Simple",
     xlim = c(-.1, .1), labeltype = "custom",
     custom.labels = c('??', '??__??','??__??__??'))

## Plot expected topic propotion timeseries
plot(prep1, "day", method = "continuous", topics = 7,
     model = z, printlegend = FALSE, xaxt = "n", xlab = "Time (2008)")
monthseq <- seq(from = as.Date("2008-01-01"),
                   to = as.Date("2008-12-01"), by = "month")
monthnames <- months(monthseq)
axis(1,at = as.numeric(monthseq) - min(as.numeric(monthseq)),
        labels = monthnames)



## plot perspectives
plot(poliblogContent, type = "perspectives", topics = 11)







## COVARIATE INTERACTIONS
poliblogInteraction <- stm(out$documents, out$vocab, K = 20,
                           prevalence =~ rating * day, 
                           max.em.its = 75,
                           data = out$meta, init.type = "Spectral")

prep <- estimateEffect(c(20) ~ rating * day, poliblogInteraction,
                       metadata = out$meta, uncertainty = "None")
plot(prep, covariate = "day", model = poliblogInteraction,
        method = "continuous", xlab = "Days", moderator = "rating",
        moderator.value = "Liberal", linecol = "blue", ylim = c(0, .12),
        printlegend = F)
plot(prep, covariate = "day", model = poliblogInteraction,
        method = "continuous", xlab = "Days", moderator = "rating",
        moderator.value = "Conservative", linecol = "red", add = T,
        printlegend = F)
legend(0, .08, c("Liberal", "Conservative"),
          lwd = 2, col = c("blue", "red"))



### word cloud
cloud(poliblogPrevFit, topic = 7, scale = c(2,.25))




## topic corelations
mod.out.corr <- topicCorr(poliblogPrevFit)
plot(mod.out.corr)










