##load datasets
library(plyr)
library(psych)
library(ggplot2)
library(reshape)
options(stringsAsFactors = FALSE)
setwd("~/GitHub/EPrebels")
#source http://term7.votewatch.eu/en/voting-statistics.html
data <- read.csv("rebels_affil_EP.csv")

##clean data
#drop redundant columns
clean_data <- data[c(-7,-8,-9)]
#rename variables
names(clean_data) <- c("name","fraction","country","loyal_group","loyal_national","participation")
#change datatypes
clean_data$loyal_group <- as.numeric(clean_data$loyal_group)
clean_data$loyal_national <- as.numeric(clean_data$loyal_national)
clean_data$participation <- as.numeric(clean_data$participation)
#drop nils
clean_data <- clean_data[ which(clean_data$loyal_group > 0 & clean_data$loyal_national > 0), ]

##make dummies for fractions/countries/regions
fractions <- unique(clean_data$fraction)
countries <- unique(clean_data$country)
fractions_group_means <- c()
fractions_country_means <- c()
fractions_part_means <- c()
#fractions
for( f in 1:length(fractions) ) {
  clean_data[fractions[f]] <- clean_data$fraction == fractions[f]
  #get means per fraction
  fractions_group_means <- c( fractions_group_means, mean( clean_data[ which(clean_data$fraction == fractions[f]), ]$loyal_group ) )
  fractions_country_means <- c( fractions_country_means, mean( clean_data[ which(clean_data$fraction == fractions[f]), ]$loyal_national))
  fractions_part_means <- c( fractions_part_means, mean( clean_data[ which(clean_data$fraction == fractions[f]), ]$participation))
}
#compile means into a table
fraction_means <- data.frame( fractions, fractions_group_means, fractions_country_means, fractions_part_means )
#countries
c_group_means <- c()
c_country_means <- c()
c_part_means <- c()
for( c in 1:length(countries) ) {
  clean_data[countries[c]] <- clean_data$country == countries[c]
  #get means per country
  c_group_means <- c( c_group_means, mean( clean_data[ which(clean_data$country == countries[c]), ]$loyal_group ) )
  c_country_means <- c( c_country_means, mean( clean_data[ which(clean_data$country == countries[c]), ]$loyal_national))
  c_part_means <- c( c_part_means, mean( clean_data[ which(clean_data$country == countries[c]), ]$participation))
}

c_means <- data.frame( countries, c_group_means, c_country_means, c_part_means )
#regions by accession to EU
#source http://en.wikipedia.org/wiki/Statistics_relating_to_enlargement_of_the_European_Union#Foundation
founding <- c("Belgium", "France", "Germany", "Italy", "Luxembourg", "Netherlands")
seventies <- c("Denmark", "United Kingdom", "Ireland")
eighties <- c("Greece", "Portugal", "Spain")
nineties <- c("Austria", "Finland", "Sweden")
millennial <- c("Croatia", "Cyprus", "Czech Republic", "Estonia", "Hungary", "Latvia", "Lithanuia", "Malta", "Poland", "Slovakia", "Slovenia", "Bulgaria", "Romania")

clean_data$founding <- clean_data$country %in% founding
clean_data$millennial <- clean_data$country %in% millennial
clean_data$older <- clean_data$country %in% seventies | clean_data$country %in% eighties | clean_data$country %in% nineties 


#keep plain data without dummies
plain_data <- clean_data[ c(1:6) ]

##summary statistics
summary(plain_data)
describe.by(plain_data, plain_data$fraction)
describe.by(plain_data, plain_data$country)
describe.by(clean_data, clean_data$founding)

#plots by fractions ggplot
byfraction <- melt(fraction_means, id.vars='fractions')
fractions_plot <- ggplot(byfraction, aes(variable, value,fill=as.factor(variable)))+geom_bar(position="dodge", stat="identity")+facet_wrap(~fractions, nrow=2)
#plots by country ggplot
bycountry <- melt(c_means, id.vars='countries')
c_plot <- ggplot(bycountry, aes(variable, value,fill=as.factor(variable)))+geom_bar(position="dodge", stat="identity")+facet_wrap(~countries, nrow=2)
##ordered fraction plot (only group loyalty)
#order the original dataset
ord_f <- fraction_means[ order(fraction_means[,2]),  ]
#take only fraction loyalty means
ord_f <- ord_f[ c(1:2) ]
#make the fraction variable ordered
ord_f$fractions <- factor(ord_f$fractions, levels=ord_f$fractions, ordered=TRUE)
ord_f_plot <- ggplot(data=ord_f, aes(x=fractions, y=fractions_group_means)) + geom_bar(stat="identity", position="dodge")
##ordered country plot
ord_c <- c_means[ order(c_means[,2]),  ]
ord_c <- ord_c[ c(1:2) ]
ord_c$countries <- factor(ord_c$countries, levels=ord_c$countries, ordered=TRUE)
ord_c_plot <- ggplot(data=ord_c, aes(x=countries, y=c_group_means)) + geom_bar(stat="identity", position="dodge")

##regressions
#ALDE is the baseline for fraction regressions
#fraction loyalty on fractions
f_on_f <- lm(data=clean_data, loyal_group ~ EPP + EFD + NI + `S&D` + ECR + `Greens/EFA` + `GUE-NGL`)
#participation on fractions
p_on_f <- lm(data=clean_data, participation ~ EPP + EFD + NI + `S&D` + ECR + `Greens/EFA` + `GUE-NGL`)
#national party loyalty on fractions
n_on_f <- lm(data=clean_data, loyal_national ~ EPP + EFD + NI + `S&D` + ECR + `Greens/EFA` + `GUE-NGL`)

#on types of countries, founding being the baseline
#fraction loyalty on country group
f_on_g <- lm(data=clean_data, loyal_group ~ millennial + older)
#participation on country group
p_on_g <- lm(data=clean_data, participation ~ millennial + older)
#national party loyalty on country group
n_on_g <- lm(data=clean_data, loyal_national ~ millennial + older)

#on countries, Germany the baseline
#fraction loyalty on country
f_on_c <- lm(data=clean_data, loyal_group ~ Austria + Belgium + Bulgaria + Croatia + Cyprus + `Czech Republic` + Denmark + Estonia + Finland + France + Greece + Hungary + Ireland + Italy + Latvia + Lithuania + Luxembourg + Malta + Netherlands + Poland + Portugal + Romania + Slovakia + Slovenia + Spain + Sweden + `United Kingdom`)
#national party loyalty on country
n_on_c <- lm(data=clean_data, loyal_national ~ Austria + Belgium + Bulgaria + Croatia + Cyprus + `Czech Republic` + Denmark + Estonia + Finland + France + Greece + Hungary + Ireland + Italy + Latvia + Lithuania + Luxembourg + Malta + Netherlands + Poland + Portugal + Romania + Slovakia + Slovenia + Spain + Sweden + `United Kingdom`)
#participation on country
p_on_c <- lm(data=clean_data, participation ~ Austria + Belgium + Bulgaria + Croatia + Cyprus + `Czech Republic` + Denmark + Estonia + Finland + France + Greece + Hungary + Ireland + Italy + Latvia + Lithuania + Luxembourg + Malta + Netherlands + Poland + Portugal + Romania + Slovakia + Slovenia + Spain + Sweden + `United Kingdom`)

#interactions
efd_uk_f <- lm(data=clean_data, loyal_group ~ EFD + `United Kingdom` + EFD*`United Kingdom`)
efd_uk_n <- lm(data=clean_data, loyal_national ~ EFD + `United Kingdom` + EFD*`United Kingdom`)
efd_uk_p <- lm(data=clean_data, participation ~ EFD + `United Kingdom` + EFD*`United Kingdom`)