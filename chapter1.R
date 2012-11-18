# Date : 14-Nov-2012

# installing all the required packages and quick overview of the data

# loading the packages
library("HSAUR")

# this loads Forbes2000 dataset.
str(Forbes2000)

help("Forbes2000")

# grouping the factors 
table(Forbes2000$country)


# saving the data in R , so that this can be loaded directly to R
save(Forbes2000,file="Forbes2000.rda") # the extension should be .rda

# loading the ojects from R file directly
load(file="Forbes2000.rda")

#logical vector
table(Forbes2000$profits > median(Forbes2000$profits,na.rm=T))

#find out missing values
table(is.na(Forbes2000$profits))

#find out what are the companies missing profit
na.profit <- is.na(Forbes2000$profits)
Forbes2000[na.profit,]


# check the dataframe for missing value for any column
na.Forbes2000 <- !complete.cases(Forbes2000)
Forbes2000[na.Forbes2000,]
table(na.Forbes2000)

#subset of a dataframe
india.forbes <- subset(Forbes2000,Forbes2000$country == "India")
india.forbes

#structur of the dataset
str(Forbes2000)

#summary of the dataset
summary(Forbes2000)

# summary is a generic function and can be applied to list also
lapply(Forbes2000,summary) # this assumes variables are list elements


####### finding the distribution with some graphs ############


# formula:
# formula can be formulated without the variables defined
fm <- marketvalue ~ sales
class(fm)

#layout
# layout takes a matrix and devides the graph region accordingly
layout(matrix(1:2,nrow=2)) #alternative to layout is par() function
hist(Forbes2000$marketvalue) # the distribution is skewed. Consider log values for all the finance related values
hist(log(Forbes2000$marketvalue))
layout(matrix(1:1,nrow=1)) #make a practice to set the layout 1:1 after the job done

plot(log(marketvalue) ~ log(sales),data=Forbes2000,pch=".")

#If the independent variable is a factor, a boxplot representation is a natural choice
boxplot(log(marketvalue) ~ country,data = subset(Forbes2000, country %in% c("United Kingdom", "Germany","India", "Turkey")),ylab="log(marketvalue)",varwidth=T)

###########################################################
############ MORE PLOTS USING GGPLOT2 #####################

library(ggplot2)
#histograms
qplot(log(marketvalue),data=Forbes2000,geom="histogram")
#getting country wise
qplot(log(marketvalue),data=Forbes2000,geom="histogram",fill=country,alpha=.3)

# scatter plot
qplot(log(sales),log(marketvalue),data=Forbes2000,color=country,shape=category,alpha=.3)

#as warnings mentioned, lets simplify the plot
qplot(log(sales),log(marketvalue),data=Forbes2000,main="log(market value) vs log(sales)",size=.7,alpha=0.1)

#boxplot
qplot(country,log(marketvalue),data = subset(Forbes2000, country %in% c("United Kingdom", "Germany","India", "Turkey")),geom="boxplot")

qplot(country,log(marketvalue),data = subset(Forbes2000, country %in% c("United Kingdom", "Germany","India", "Turkey")),geom="boxplot")+geom_jitter()
qplot(country,log(marketvalue),data = subset(Forbes2000, country %in% c("United Kingdom", "Germany","India", "Turkey")))+geom_boxplot(outlier.colour="red")

#############################################################################

#################### EXERCISES #####################################
#Ex. 1.1 Calculate the median profit for the companies in the United States
#and the median profit for the companies in the UK, France and Germany.
sub.forbes <- subset(Forbes2000,country %in% c("United States","UK","France","Germany"))
library("plyr")
ddply(sub.forbes,c("country"),function(df){median(df$profits,na.rm=T)})

#alternatively this can be tested
median(Forbes2000[Forbes2000$country=="France",]$profits,na.rm=T)



#Ex. 1.2 Find all German companies with negative profit.
Forbes2000[Forbes2000$country=="Germany" & Forbes2000$profits < 0,"name"]
#test with
Forbes2000[Forbes2000$country=="Germany" & Forbes2000$profits < 0,]


#Ex. 1.3 Which business category are most of the companies situated at the
#Bermuda island working in?
sort(table(Forbes2000[Forbes2000$country=="Bermuda","category"]),decreasing=T)