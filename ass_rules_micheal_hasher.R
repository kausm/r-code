#' ---
#' title: "arules: Association Rule Mining with R --- A Tutorial"
#' author: "Michael Hahsler"
#' output:
#'  html_document:
#'    theme: united
#'    highlight: tango
#'    toc: true
#' ---

#' # Slides, code and about the author
#' * This R code contains a demo to accompany the following [slides.](http://michael.hahsler.net/research/arules_RUG_2015/talk/arules_RUG2015.pdf)
#' * The R code for this demo can be downloaded [here.](http://michael.hahsler.net/research/arules_RUG_2015/demo/R_arules_demo_SQF2012.R)
#' * Michael Hahsler is one of the creators of the R package arules. He is a
#'    professor at SMU in Dallas, TX and director
#'    of [IDA\@SMU](http://lyle.smu.edu/IDA/). His contact information
#'    can be found [here.](http://michael.hahsler.net)
#'
#' # Obtaining the data

#' This demo uses data from the Stop-Question-and-Frisk program in New York City.
#' "Stop-Question-and-Frisk is a practice of the New York City Police Department
#' by which police officers stop and question hundreds of thousands of
#' pedestrians annually, and frisk them for weapons and other
#' contraband." (Wikipedia)
#'
#' The data can be obtained from http://www.nyclu.org/content/stop-and-frisk-data
#'
#' Download files
if(!file.exists("SQF_Codebook.pdf")) {
  download.file("http://www.nyclu.org/files/SQF_Codebook.pdf", "SQF_Codebook.pdf")
}

#' [Click here](./SQF_Codebook.pdf) for a description of the data.

if(!file.exists("SQF 2012.csv")) {
  download.file("http://www.nyclu.org/files/stopandfrisk/Stop-and-Frisk-2012.zip",
                "Stop-and-Frisk-2012.zip")
  unzip("Stop-and-Frisk-2012.zip")
}

dat <- read.csv("SQF 2012.csv")
dim(dat)
summary(dat)

#' # Cleaning the data
#' ## Fix date and time
dat$datestop <- as.Date(sprintf("%08d", dat$datestop), format ="%m%d%Y")
dat$timestop <- as.integer(substr(sprintf("%04d", dat$timestop), 1, 2))

#' ## Clean continuous variables
#' Fix observation period
dat$perobs[ dat$perobs<1 | dat$perobs>120 ] <- NA

#' Fix stop period
head(dat$perstop)
dat$perstop[dat$perstop =="**"] <- NA
dat$perstop <- as.numeric(dat$perstop)

#' DOB is a really bad variable and we have age!
dat$dob <- NULL

#' Clean age
hist(dat$age)
table(dat$age)
dat$age[dat$age < 10 | dat$age > 90] <- NA
hist(dat$age, breaks=40)

#' Clean height
table(dat$height)
barplot(table(dat$height))
dat$height[dat$height < 40 | dat$height > 90] <- NA
hist(dat$height, breaks=40)

#' Clean weight
table(dat$weight)
barplot(table(dat$weight))
dat$weight[dat$weight < 50 | dat$weight > 400] <- NA
hist(dat$weight, breaks=40)

#' ## Change nominal variables into factors
dat$city <- factor(dat$city, labels=c("Manhattan", "Brooklyn", "Bronx",
                                      "Queens", "Staten Island"))

dat$race <- factor(dat$race, labels=c("Black", "Black Hispanic",
                                      "White Hispanic", "White", "Asian/Pacific Islander",
                                      "Am. Indian/ Native Alaskan"))

dat$sex <- factor(dat$sex+1L, label=c("female", "male"))

dat$build <- factor(dat$build, labels=c("heavy", "muscular",
                                        "medium", "thin"))

dat$forceuse <- factor(dat$forceuse, labels =c("defense of other",
                                               "defense of self", "overcome resistence", "other",
                                               "suspected flight", "suspected weapon"))

dat$inout <- factor(dat$inout+1L, labels=c("outside", "inside"))

dat$trhsloc <- factor(dat$trhsloc+1L, labels=c("neither",
                                               "housing authority", "transit authority"))

#' I copied the crimecodes from the variable description PDF into Excel
#' and saved them in crimecodes.csv
crimecodes <- read.csv("crimecodes.csv", header = FALSE)
dat$detailcm <- factor(dat$detailcm, levels= crimecodes[,1],
                       labels=crimecodes[,2])

dat$pct <- as.factor(dat$pct) # use names instead?
dat$addrpct <- as.factor(dat$addrpct)
dat$sector <- as.factor(dat$sector)

#' there are 4 types of ID in the data!!! So I leave the 4th as unknown
dat$typeofid <- factor(dat$typeofid,
                       labels=c("photo id", "verbal id", "refused to provide id", "unknown"))
dat$repcmd <- as.factor(dat$repcmd)
dat$revcmd <- as.factor(dat$revcmd)



#' ## Convert binary variables into logical
binary <- strsplit("frisked searched contrabn pistol riflshot asltweap knifcuti machgun othrweap arstmade sumissue sumoffen",
                   " ")[[1]]
for(b in binary) dat[[b]] <- as.logical(dat[[b]])
#' cs = reason for stop
for(b in grep("cs_", colnames(dat), value=TRUE)) dat[[b]] <- as.logical(dat[[b]])
#' rf = reason for frisk
for(b in grep("rf_", colnames(dat), value=TRUE)) dat[[b]] <- as.logical(dat[[b]])
#' sb = basis of search
for(b in grep("sb_", colnames(dat), value=TRUE)) dat[[b]] <- as.logical(dat[[b]])
#' ac = additional circumstance
for(b in grep("ac_", colnames(dat), value=TRUE)) dat[[b]] <- as.logical(dat[[b]])
#' pf = force used
for(b in grep("pf_", colnames(dat), value=TRUE)) dat[[b]] <- as.logical(dat[[b]])

dat$othpers <- as.logical(dat$othpers)
dat$explnstp <- as.logical(dat$explnstp)


#' If the officer is in uniform then the officer does not need to inform the
#' person that he/she is an officer and show his/her shield.
dat$offunif <- as.logical(dat$offunif)
dat$offverb <- as.logical(dat$offverb)
dat$offverb[dat$offunif] <- NA
dat$officrid <- as.logical(dat$officrid)
dat$officrid[dat$offunif] <- NA
dat$offshld <- as.logical(dat$offshld)
dat$offshld[dat$offunif] <- NA

dat$radio <- as.logical(dat$radio)

#' Remove unused variables
dat$year <- NULL # we only have 2012
dat$haircolr <- NULL
dat$eyecolor <- NULL
dat$ser_num <- NULL
dat$othfeatr <- NULL
dat$arstoffn <- NULL
dat$crimsusp <- NULL
dat$premname <- NULL
dat$addrnum <- NULL
dat$stname <- NULL
dat$stinter <- NULL
dat$ crossst <- NULL
dat$beat <- NULL
dat$post <- NULL
dat$recstat <- NULL
dat$linecm <- NULL

#' Check summary of cleaned data
summary(dat)

#' Save cleaned data
save(dat, file="SFQ_clean.rda")

#load("SFQ_clean.rda")
#' ## Look at number of stops
oldpar <- par("mar" = c(6,4,4,2)+.1)
barplot(table(dat$city), ylab="Number of Stops", las=3)
par(oldpar)

#' Compare to population (from Wikipedia)
pop <- c(Manhattan =1626159, Brooklyn = 2592149, Bronx = 1418733,
         Queens =2296175, 'Staten Island'=472621)
oldpar <- par("mar" = c(6,4,4,2)+.1)
barplot(table(dat$city)/pop*100, ylab="Stops in % of Population",
        las=3, ylim=c(0,10))
par(oldpar)

tbl <- table(dat$race)
names(tbl) <- abbreviate(names(tbl), 8)
barplot(tbl, ylab="Number of Stops", las=3)

#' Compare to NYC population (from Wikipedia)
pop <- c(White=44.6, Black=25.1, Hispanic=27.5, Other=11.8)
sum(pop)
#' does not add up to 100
pop <- pop/sum(pop)

#' aggregate data so the groups match the population data
tbl <- table(dat$race)
tbl <- c(tbl["White"], tbl["Black"],
         tbl["White Hispanic"]+tbl["Black Hispanic"],
         tbl["Asian/Pacific Islander"]+tbl["Am. Indian/ Native Alaskan"])
names(tbl) <- c("White", "Black", "Hispanic", "Other")
tbl <- tbl/sum(tbl)

barplot((rbind(pop, tbl)*100), beside = TRUE, ylab="Proportion [%]",
        col=gray.colors(2), ylim=c(0,70), main = "Stops in NYC")
legend(x = "topright", legend = c("Population", "Stops"),
       fill=gray.colors(2), bty="n")


#' ## Association plots (between nominal/binary variables)
if(! "vcd" %in% installed.packages()) install.packages("vcd", depend = TRUE)
library(vcd)
assoc(~ sex + race, data=dat, shade=TRUE, abbreviate_labs=6)
assoc(~ sex + forceuse, data=dat, shade=TRUE,
      labeling_args = list(rot_labels = c(25, 90, 0, 90), varnames=FALSE))
assoc(~ arstmade + sex, data=dat, shade=TRUE)
assoc(~ arstmade + race, data=dat, shade=TRUE, abbreviate_labs=6)

#' ## Google Map
#' Installing package proj4 is somewhat more complicated. You can skip this part.
library(ggmap)

NYC <- get_map("New York City", zoom=11)
map <- ggmap(NYC)

coords <- dat[, c("xcoord", "ycoord")]

# EPSG Projection 2263 - NAD83 / New York Long Island (ftUS)
# WGS84 Bounds: -74.2700, 40.4700, -71.7500, 41.3100
# Projected Bounds: 909126.0155, 110626.2880, 1610215.3590, 424498.0529
library(proj4) ### needs PROJ.4 installed (http://trac.osgeo.org/proj/)
c2 <- project(coords, inverse=TRUE, proj="+proj=lcc +lat_1=41.03333333333333 +lat_2=40.66666666666666 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000.0000000001 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs")
coords <- data.frame(lon=c2[[1]], lat=c2[[2]])

d2 <- coords
d2$detailcm <- dat$detailcm
d2 <- d2[d2$detailcm == "ROBBERY",]
#d2 <- d2[d2$detailcm == "CPW",]
#d2 <- d2[d2$detailcm =="MAKING GRAFFITI",]
d2 <- na.omit(d2)

# uses ggplot style plots
#map + geom_point(aes(x = lon, y = lat,
#  colour = detailcm), data= d2)

map +
  stat_density2d(
    aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
    size = 1, bins = 10, data = d2,
    geom = "polygon"
  )

#' # Association Rules
if(! "arules" %in% installed.packages()) install.packages("arules", depend = TRUE)
if(! "arulesViz" %in% installed.packages()) install.packages("arulesViz", depend = TRUE)
library(arules)
library(arulesViz)
#load("SFQ_clean.rda")


#' ## Choose only a subset of variables
d <- dat[, c(
  grep("rf_", colnames(dat), value = TRUE),
  grep("cs_", colnames(dat), value = TRUE),
  grep("ac_", colnames(dat), value = TRUE),
  grep("pf_", colnames(dat), value = TRUE),
  "arstmade", "sumissue", "detailcm", "race",
  "pct",
  #"city", ### city and precinct are related
  "typeofid", "othpers"
)]

d$female <- dat$sex == "female"
#d$detailcm[!(d$arstmade | d$sumissue)] <- NA
d$weapon <- dat$pistol | dat$riflshot | dat$asltweap |
  dat$knifcuti | dat$machgun | dat$othrweap
d$no_uniform <- !dat$offunif

d$inside <- dat$inout == "inside"
d$trhsloc <- dat$trhsloc
d$trhsloc[dat$trhsloc == "neither"] <- NA

#' Continuous variables need to be discretized!
d$minor <- dat$age<18
d$height <- discretize(dat$height, method = "frequency", 3)

#' ## Convert to transactions
trans <- as(d, "transactions")
trans
summary(trans)

#' The conversion to transactions created items (binary dummy variables for
#' nominal values)
dim(d)
dim(trans)
itemLabels(trans)
as(trans[1:2, 1:10], "matrix")


itemFrequencyPlot(trans, topN=50,  cex.names=.5)

#' ## Look at similarity between items
#' We use positively correlated  (Phi coefficient) items. dissimilarity()
#' converts the correlation into distances.
#' Note: we use sample to speed up and plot as large PDF
d <- dissimilarity(sample(trans, 50000), method = "phi", which = "items")
d[is.na(d)] <- 1 # get rid of missing values

pdf(file="similarity.pdf", width=25)
plot(hclust(d), cex=.5)
dev.off()
#' [Click here](./similarity.pdf)  to see the resulting dendrogram.
#'
#' ## Look at subsets of the population
trans_female <- subset(trans, items %in% "female")
itemFrequencyPlot(trans_female, topN = 25, population = trans, cex.names=.5)
itemFrequencyPlot(trans_female, topN = 25, population = trans, lift=TRUE, cex.names=.5)

trans_minor <- subset(trans, items %in% "minor")
itemFrequencyPlot(trans_minor, topN = 25, population = trans, lift=TRUE, cex.names=.5)

trans_white <- subset(trans, items %in% "race=White")
itemFrequencyPlot(trans_white, topN = 25, population = trans, lift=TRUE, cex.names=.5)

trans_black <- subset(trans, items %in% "race=Black")
itemFrequencyPlot(trans_black, topN = 25, population = trans, lift=TRUE, cex.names=.5)
#' Note: most of the data is stops for this race, therefore the lift is not
#' very high!
#'
#' ## Mine Frequent Itemsets
#' Find an interesting support (have at least 500 observations)
nrow(trans)
500/nrow(trans)

itemsets <- apriori(trans, parameter = list(target = "frequent",
                                            supp=0.001, minlen = 2, maxlen=4))
inspect(head(sort(itemsets), n=10))

#' Add an additional quality measure
quality(itemsets)$lift <- interestMeasure(itemsets, method="lift", trans = trans)
inspect(head(sort(itemsets, by = "lift"), n=10))

#' Default plot is a graph. Different subgroups with items that are related to
#' each othercan be identified.
plot(head(sort(itemsets, by = "lift"), n=50), control=list(cex=.8))
#plot(head(sort(itemsets, by = "lift"), n=50), interactive = TRUE)

#' remove pf_hcuff=TRUE (always with arrest)
trans <- trans[,-pmatch("pf_hcuff", colnames(trans))]
itemsets <- apriori(trans, parameter = list(target = "frequent",
                                            supp=0.001, minlen = 2, maxlen=4))
quality(itemsets)$lift <- interestMeasure(itemsets, method="lift", trans = trans)
inspect(head(sort(itemsets, by = "lift"), n=10))
plot(head(sort(itemsets, by = "lift"), n=50), control=list(cex=.8))

#' remove rf_bulg (always with cs_bulge)
trans <- trans[,-pmatch("rf_bulg", colnames(trans))]
itemsets <- apriori(trans, parameter = list(target = "frequent",
                                            supp=0.001, minlen = 2, maxlen=4))
quality(itemsets)$lift <- interestMeasure(itemsets, method="lift", trans = trans)
inspect(head(sort(itemsets, by = "lift"), n=10))
plot(head(sort(itemsets, by = "lift"), n=50), control=list(cex=.8))


#' remove detailcm=CPW (always with weapon)
trans <- trans[,-pmatch("detailcm=CPW", colnames(trans))]
itemsets <- apriori(trans, parameter = list(target = "frequent",
                                            supp=0.001, minlen = 2, maxlen=4))
quality(itemsets)$lift <- interestMeasure(itemsets, method="lift", trans = trans)
inspect(head(sort(itemsets, by = "lift"), n=10))
plot(head(sort(itemsets, by = "lift"), n=50), control=list(cex=.8))

#' remove rf_vcrim and rf_vcact (always with cs_vcrim)
trans <- trans[,-pmatch("rf_vcrim", colnames(trans))]
trans <- trans[,-pmatch("rf_vcact", colnames(trans))]
itemsets <- apriori(trans, parameter = list(target = "frequent",
                                            supp=0.001, minlen = 2, maxlen=4))
quality(itemsets)$lift <- interestMeasure(itemsets, method="lift", trans = trans)
inspect(head(sort(itemsets, by = "lift"), n=10))
plot(head(sort(itemsets, by = "lift"), n=50), control=list(cex=.8))

#' remove cs_drgtr=TRUE (always with controlled substance)
trans <- trans[,-pmatch("cs_drgtr", colnames(trans))]
itemsets <- apriori(trans, parameter = list(target = "frequent",
                                            supp=0.001, minlen = 2, maxlen=4))
quality(itemsets)$lift <- interestMeasure(itemsets, method="lift", trans = trans)
inspect(head(sort(itemsets, by = "lift"), n=10))
plot(head(sort(itemsets, by = "lift"), n=50), control=list(cex=.7))


#' ## Mine Association Rules
r <- apriori(trans, parameter = list(supp=0.001, maxlen=4))
inspect(head(sort(r, by="lift"), n=10))

#' Default is a scatter plot. Dark dots are interesting since they represent rules
#' with high lift.
plot(r)
#' Try interactive plot
#plot(r, interactive=TRUE)


#' ## Lower support to 100 cases
100/nrow(trans)
r <- apriori(trans, parameter = list(supp=0.00019, maxlen=4))
inspect(head(sort(r, by="lift"), n=10))

plot(r)
#' Try interactive plot
#plot(r, interactive=TRUE)

#' ### Females
r_female <- subset(r, subset = items %in% "female")
r_female
inspect(head(sort(r_female, by="lift"), 10))
itemFrequencyPlot(items(r_female), topN=30, cex.names=.6)

plot(head(sort(r_female, by="lift"), 50),
     method="graph", control=list(cex=.7))

plot(head(sort(r_female, by="support"), 50),
     method="graph", control=list(cex=.7))

#' ### Summons
r_summon <- subset(r, subset = items %pin% "sumissue")
r_summon
inspect(head(sort(r_summon, by="lift"), n=10))
itemFrequencyPlot(items(r_summon), topN=20)

plot(head(sort(r_summon, by="lift"), 50),
     method="graph", control=list(cex=.7))

plot(head(sort(r_summon, by="support"), 50),
     method="graph", control=list(cex=.7))

#' ### Terrorism
r_terrorism <- subset(r, subset = items %in% "detailcm=TERRORISM")
r_terrorism
inspect(sort(r_terrorism, by="lift"))
itemFrequencyPlot(items(r_terrorism), topN=20, cex.names=.6)

plot(head(sort(r_terrorism, by="lift"), 25),
     method="graph", control=list(cex=.7))

#' ### Minors
r_minor <- subset(r, subset = items %pin% "minor")
r_minor
inspect(head(sort(r_minor, by="lift"), 20))
itemFrequencyPlot(items(r_minor), topN=30, cex.names=.6)

plot(head(sort(r_minor, by="lift"), 30),
     method="graph", control=list(cex=.7))
