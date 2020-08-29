
load("Data301Project.RData")

require(dplyr)
require(tidyr)
require(knitr)

##******************************************************************************************************##
#This bit just sets up the data - there's no ouput, except a table of the area descriptions
#that I used to create the 'Urban' variable. I've also noted the number of "C"s in the census variables I used
#(I changed them to NA), and a peculiarity in the total dwellings vs occupied dwellings (cases where the 
#latter was larger than the former - also changed to NA)

date <- strsplit(TandP_Police_full$Month.Year," ")
Year <- unlist(date)[2*(1:length(TandP_Police_full$Month.Year))]
TandP_Police_full$Year <- Year

rm(date,Year,TandP_Police)

TandP_Police_full$Division <- TandP_Police_full[,1]
TandP_Police_full <- TandP_Police_full[,-1]
TandP_Police_full <- select(TandP_Police_full,Group="ANZSOC.Group",Location.Type,Meshblock,Year,Division)

TandP_Police_full <- left_join(TandP_Police_full,area_conversions[,c("MB2013_code","SA12018_code")],
                               by=c("Meshblock"="MB2013_code"))

TandP <- TandP_Police_full[!is.na(TandP_Police_full$SA12018_code),]

TandP$SA12018_code <- as.character(TandP$SA12018_code)

SA12018 <- NULL

SA12018$SA12018_code <- unique(TandP$SA12018_code)
SA12018 <- as.data.frame(SA12018)
SA12018$SA12018_code <- as.character(SA12018$SA12018_code)

SA12018 <- left_join(SA12018,census2018[,c("Census_2018_usually_resident_population_count",
                                       "Census_2018_median_age_CURP",
                                       "Area_code")],
                   by=c("SA12018_code"="Area_code"))

SA12018 <- left_join(SA12018,
                    census2018_dwellings[,c("Area_code","Census_2018_Dwelling_occupancy_status_Total_all_dwellings",
                                            "Census_2018_Dwelling_occupancy_status_11_Occupied_Dwelling_all_dwellings")],
                    by=c("SA12018_code"="Area_code"))

area_conversions$SA12018_code <- as.character(area_conversions$SA12018_code)

temp <- area_conversions[,c("SA12018_code","IUR2018_name")]
temp2 <- unique(temp)

SA12018 <- left_join(SA12018,temp2,by=c("SA12018_code"))

# table(SA12018$IUR2018_name)
# Inland water             Inlet  Large urban area  Major urban area Medium urban area           Oceanic 
#           28                72              4164             14714              2529                12 
# Rural other  Rural settlement  Small urban area 
#        4018              1030              3033 

##Creating an 'urban' variable. The water locations will become NA, but there are only a small number.
SA12018$urban <- factor(SA12018$IUR2018_name,levels=c("Large urban area","Major urban area","Medium urban area",
                                                    "Small urban area","Rural settlement","Rural other"))

censusvars <- grep("Census_2018",names(SA12018),value=TRUE)

# lapply(censusvars,function(x) sum(SA12018[,x] %in% "C"))
##271 "C" values in median age
##336 "C" values in occupied dwellings

for(v in censusvars){
  SA12018[,v] <- as.numeric(as.character(SA12018[,v]))
}

# lapply(censusvars,function(x) sum(is.na(SA12018[,x])))

SA12018 <- select(SA12018,SA12018_code,
                Pop="Census_2018_usually_resident_population_count",
                Med_age="Census_2018_median_age_CURP",
                Dwellings="Census_2018_Dwelling_occupancy_status_Total_all_dwellings",
                Occ_dwellings="Census_2018_Dwelling_occupancy_status_11_Occupied_Dwelling_all_dwellings",
                Urban="urban")

SA12018 <- mutate(SA12018,Prop_occ=Occ_dwellings/Dwellings)
SA12018[!is.na(SA12018$Prop_occ) & SA12018$Prop_occ>1.001,"Prop_occ"] <- NA 
#Some of the numbers for occupied dwellings are greater than total dwellings, which makes no sense, so these 
#have been excluded (459 obs).
##***********************************************************************************************************##

##***********************************************************************************************************##
#This section sets up different data splits, based on using combinations of Year/Group/Division
#There is a summary of statistics table for each one. It is of note that the splits that don't include year
#use the full set of available data, which spans 2014 to 2020. The splits that do include year only use full 
#years, and therefore only span 2015-2019.

w <- group_by(TandP,SA12018_code,Group,Year)
groupw <- summarise(w,n=n())
groupw <- groupw[groupw$Year %in% c("2015","2016","2017","2018","2019"),]
groupw <- as.data.frame(groupw)
groupw <- left_join(groupw,SA12018,by="SA12018_code")

gb <- group_by(groupw,Group,Year)
summ <- summarise(gb,Mean=mean(n),Min=min(n),LQ=quantile(n)[2],Median=median(n),
                  UQ=quantile(n)[3],Max=max(n),Total=sum(n))
summ <- summ[order(summ$Group),]

############################################################################
##Table of crime data by group and year - Only full years: 2015-2019
kable(summ,caption="Time and Place crime records by Group and Year")

#At a glance, a bit of variation in years but not a huge amount. The general trend seems to be upward over 
#the entire time period, but it's not consistent. Since these are counts, you'd need to take population growth 
#into consideration.
############################################################################

x <- group_by(TandP,SA12018_code,Group)
groupx <- summarise(x,n=n())
groupx <- as.data.frame(groupx)
groupx <- left_join(groupx,SA12018,by="SA12018_code")

gb <- group_by(groupx,Group)
summ <- summarise(gb,Mean=mean(n),Min=min(n),LQ=quantile(n)[2],Median=median(n),
                  UQ=quantile(n)[3],Max=max(n),Total=sum(n))
summ <- summ[order(summ$Group),]

##########################################################################
##Table of crime records by group - All available data: 2014-2020
kable(summ,caption="Time and Place crime records by Group")

#There is a huge variation in the magnitude of the counts across the different groups, obviously.
##########################################################################

y <- group_by(TandP,SA12018_code,Division)
groupy <- summarise(y,n=n())
groupy <- as.data.frame(groupy)
groupy <- left_join(groupy,SA12018,by="SA12018_code")

gb <- group_by(groupy,Division)
summ <- summarise(gb,Mean=mean(n),Min=min(n),LQ=quantile(n)[2],Median=median(n),
                  UQ=quantile(n)[3],Max=max(n),Total=sum(n))
summ <- summ[order(summ$Division),]

#################################################################################
##Table of crime records by division - All available data: 2014-2020
kable(summ,caption="Time and Place crime records by Division")

#This is the same as the previous table, but at a slightly hgher level (Division vs group)
#################################################################################

z <- group_by(TandP,SA12018_code,Division,Year)
groupz <- summarise(z,n=n())
groupz <- groupz[groupz$Year %in% c("2015","2016","2017","2018","2019"),]
groupz <- as.data.frame(groupz)
groupz <- left_join(groupz,SA12018,by="SA12018_code")

gb <- group_by(groupz,Division,Year)
summ <- summarise(gb,Mean=mean(n),Min=min(n),LQ=quantile(n)[2],Median=median(n),
                  UQ=quantile(n)[3],Max=max(n),Total=sum(n))
summ <- summ[order(summ$Division),]

##################################################################################
##Table of crime records by division and year - Only full years: 2015-2019
kable(summ,caption="Time and Place crime records by Division and Year")

#Again, just a repeat of the group*year table, but with division instead of group
##################################################################################

ty <- group_by(TandP,SA12018_code,Year)
totyear <- summarise(ty,n=n())
totyear <- totyear[totyear$Year %in% c("2015","2016","2017","2018","2019"),]
totyear <- as.data.frame(totyear)
totyear <- left_join(totyear,SA12018,by="SA12018_code")

#########################################################################################
##Table of crime records by year - Only full years: 2015-2019
kable(summarise(group_by(totyear,Year),Mean=mean(n),Min=min(n),
                       LQ=quantile(n)[2],Median=median(n),UQ=quantile(n)[3],Max=max(n),Total=sum(n)),
      caption="Time and Place crime records by Year")

#This shows an overall trend upwards between 2015 and 2019 in total crime records, but it doesn't 
#move much in the middle. The big(ish) jumps are 2015-2016, and 2018-2019.
#########################################################################################

t <- group_by(TandP,SA12018_code)
totcr <- summarise(t,n=n())
totcr <- as.data.frame(totcr)
totcr <- left_join(totcr,SA12018,by="SA12018_code")

##########################################################################################
##Table of crime records - All available data: 2014-2020
kable(summarise(totcr,Mean=mean(n),Min=min(n),
          LQ=quantile(n)[2],Median=median(n),UQ=quantile(n)[3],Max=max(n),Total=sum(n)),
      caption="All Time and Place crime records")

#There is a clear indication of the skewness in the median vs max. 
##########################################################################################

##**********************************************************************************************************##

##**********************************************************************************************************##
#This section has plots. I used 2D histograms instead of scatter plots because of the skewness of the
#data. It's still quite hard to get a sense of the spread, because the concentration is in such a tiny area, 
#for the most part.

library(ggplot2)

###########################################################################################
#Histograms of recorded crimes - all, and then split into Areas with <100, 100-1000 and >1000 
#to get a clearer picture of the distribution
ggplot(totcr, aes(x=n)) + 
  geom_histogram(binwidth=10) +
  labs(title="Total recorded crimes by SA1\n(2014-2020)",
       x="Number of recorded crimes (binwidth=10)", y = "Count of SA1s")+
  theme_classic()

ggplot(totcr[totcr$n<100,], aes(x=n)) + 
  geom_histogram(binwidth=1) +
  labs(title="SA1s with fewer than 100 total \nrecorded crimes (2014-2020)",
       x="Number of recorded crimes (binwidth=1)", y = "Count of SA1s")+
  theme_classic()

ggplot(totcr[totcr$n>=100 & totcr$n<1000,], aes(x=n)) + 
  geom_histogram(binwidth=10) +
  labs(title="SA1s with between 100 and 999 total \nrecorded crimes (2014-2020)",
       x="Number of recorded crimes (binwidth=10)", y = "Count of SA1s")+
  theme_classic()

ggplot(totcr[totcr$n>=1000,], aes(x=n)) + 
  geom_histogram(binwidth=100) +
  labs(title="SA1s with between 1000 or more total \nrecorded crimes (2014-2020)",
       x="Number of recorded crimes (binwidth=100)", y = "Count of SA1s")+
  theme_classic()
#######################################################################################

##The distribution of recorded crimes is heavily skewed. the relationship between that and other variables
##is unlikely to be linear, and the residuals from a linear model are unikely to be normally
##distributed. Parametric methods are therefore probably not appropriate.

##**********************************************************************************************************##
##Following are 2D histograms to look at relationships between variables, first at top level 
##(total crime over full time period), and then by year and division.

#################################################################################
# Summary stats for population (by SA1, taken from the 2018 census data)
summary(totcr$Pop)

#You can see skewness here too.
#################################################################################

###################################################################################
# 2d histogram of population versus number of recorded crimes by SA1
ggplot(totcr, aes(x=Pop, y=n) ) +
  geom_bin2d() +
  scale_fill_gradient(low = "gray66", high = "gray1") +
  labs(title="2018 population versus number of \nrecorded crimes (2014-2020) by SA1",
       x="Population", y = "Number of recorded crimes")+
  theme_classic()

#Note the concentration of observations at very low numbers of crime records, and population around 150-200.
#The compounded skewness makes it hard to tell whether there is any relationship. 
####################################################################################

####################################################################################
#non-parametric correlation test (Kendall's tau, which I think is more robust in the presence of 
#multiple ties than Spearman's rho?) for Popuation*recorded crime

cor.test(totcr$Pop,totcr$n,method="kendal")

#The result suggests a weak positive correlation.
#To better understand the relationship (or lack of), you would probably need to refine the focus and then 
#structure analysis around that. For instance, you could perhaps take a subset of just the high crime areas, 
#and then compare the distribution of population within that subset to the underlying distribution (or the 
#distribution among all the areas included in the crime data at least).
######################################################################################

####################################################################################
# Summary stats for total number of dwellings (by SA1, taken from 2018 census)
summary(totcr$Dwellings)

#Again, this distribution is skewed. But you would expect that based on the population data
####################################################################################

####################################################################################
# 2d histogram of number of dwellings versus number of recorded crimes by SA1
ggplot(totcr, aes(x=Dwellings, y=n) ) +
  geom_bin2d() +
  scale_fill_gradient(low = "gray66", high = "gray1") +
  labs(title="2018 dwellings versus number of \nrecorded crimes (2014-2020) by SA1",
       x="Number of dwellings", y = "Number of recorded crimes")+
  theme_classic()

#This provides a picture similar to population, which is logical.
#####################################################################################

######################################################################################
#non-parametric correlation test for number of dwellings*recorded crime

cor.test(totcr$Dwellings,totcr$n,method="kendal")

#Another weak positive correlation, very similar to population*crime.
######################################################################################

######################################################################################
# Summary stats for proportion of dwellings occupied (by SA1, created from 2018 census data)
summary(totcr$Prop_occ)
hist(totcr$Prop_occ)

#Skewed in the other direction this time. Most areas have close to 100% of dwellings occupied.
######################################################################################

######################################################################################
# 2d histogram of proportion of dwellings occupied versus number of recorded crimes by SA1
ggplot(totcr, aes(x=Prop_occ, y=n) ) +
  geom_bin2d() +
  scale_fill_gradient(low = "gray66", high = "gray1") +
  labs(title="2018 dwelling occupancy versus\nnumber of recorded crimes\n(2014-2020) by SA1",
       x="Proportion of dwellings occupied", y = "Number of recorded crimes")+
  theme_classic()

#There is possibly some indication of a negative relationship, just looking at the outliers.
######################################################################################

######################################################################################
#non-parametric correlation test for proportion of dwellings occupied*recorded crime

cor.test(totcr$Prop_occ,totcr$n,method="kendal")

#There is a very weak negative correlation.
######################################################################################

######################################################################################
# Summary stats for median age (by SA1, from 2018 census)
summary(totcr$Med_age)
hist(totcr$Med_age)

#This is relatively normally distributed (with just a bit of a right tail).
######################################################################################

######################################################################################
# 2d histogram of median age versus number of recorded crimes by SA1
ggplot(totcr, aes(x=Med_age, y=n) ) +
  geom_bin2d() +
  scale_fill_gradient(low = "gray66", high = "gray1") +
  labs(title="2018 median age versus\nnumber of recorded crimes\n(2014-2020) by SA1",
       x="Median age of SA1", y = "Number of recorded crimes")+
  theme_classic()

#The combination of the skewness of recorded crime and relative normality of median age creates a funnel shape,
#with a high point around median median age. this is what you would expect to see in the absence of 
#relationship, I think.
######################################################################################

######################################################################################
#non-parametric correlation test for median age*recorded crime

cor.test(totcr$Med_age,totcr$n,method="kendal")

#There is a weak negative correlation.
######################################################################################

######################################################################################
#A set of grids showing Year and Division splits follow.

divlabs <- c("Abduct / harrass","Intent to injure","Robbery","Sexual assault","Theft","Unlawful entry")
names(divlabs) <- names(table(groupy$Division))

# 2d histogram of Population versus number of crimes by SA1
ggplot(groupz, aes(x=Pop, y=n) ) +
  geom_bin2d() +
  scale_fill_gradient(low = "gray66", high = "gray1") +
  labs(title="2018 Population versus\nnumber of recorded crimes\n(2015-2019) by SA1",
       x="Population of SA1", y = "Number of recorded crimes") +
  facet_grid(rows = vars(Year), cols = vars(Division),
             labeller=labeller(Division=divlabs)) +
  theme_classic()

# 2d histogram of Number of dwellings versus number of crimes by SA1
ggplot(groupz, aes(x=Dwellings, y=n) ) +
  geom_bin2d() +
  scale_fill_gradient(low = "gray66", high = "gray1") +
  labs(title="2018 number of dwellings\nversus number of recorded crimes\n(2015-2019) by SA1",
       x="Number of dwellings", y = "Number of recorded crimes") +
  facet_grid(rows = vars(Year), cols = vars(Division),
             labeller=labeller(Division=divlabs)) +
  theme_classic()

# 2d histogram of dwellings occupancy versus number of crimes by SA1
ggplot(groupz, aes(x=Prop_occ, y=n) ) +
  geom_bin2d() +
  scale_fill_gradient(low = "gray66", high = "gray1") +
  labs(title="2018 dwelling occupancy\nversus number of recorded crimes\n(2015-2019) by SA1",
       x="Proportion of dwellings occupied", y = "Number of recorded crimes") +
  facet_grid(rows = vars(Year), cols = vars(Division),
             labeller=labeller(Division=divlabs)) +
  theme_classic()

# 2d histogram of median age versus number of crimes by SA1
ggplot(groupz, aes(x=Med_age, y=n) ) +
  geom_bin2d() +
  scale_fill_gradient(low = "gray66", high = "gray1") +
  labs(title="2018 Median age versus\nnumber of recorded crimes\n(2015-2019) by SA1",
       x="Median age", y = "Number of recorded crimes") +
  facet_grid(rows = vars(Year), cols = vars(Division),
             labeller=labeller(Division=divlabs)) +
  theme_classic()

#Overall, it seems that patterns are similar across the years. Theft is the most common crime division, and has 
#the largest range and the largest numbers of recorded crimes by area. With the very different numbers of crimes 
#across the different divisions, it once again makes it difficult to spot differences in the patterns of 
#the data across the divisions (as they would be swamped by differences in scale). To compare across the 
#divisions, you could potentially rescale, so the magnitudes were more similar.
##############################################################################################################

##**********************************************************************************************************##
#This section looks at the ordinal variable 'Urban'

######################################################
#Table of SA1s by Urban/Rural
tab <- table(totcr$Urban)
tab <- as.data.frame(tab)
colnames(tab) <- c("Urban/Rural","Count of SA1s")
kable(tab)

#Roughly half of SA1s are classified as major urban.
#######################################################

#######################################################
#Violin plot of Urban/rural for total crime
ggplot(totcr[!is.na(totcr$Urban),], aes(x=Urban, y=n)) + 
  geom_violin() +
  labs(title="Total recorded crimes by SA1\n(2014-2020)",
       x="Number of recorded crimes (binwidth=10)", y = "Count of SA1s") +
  theme_classic()

#It's not easy to see the shape of the distributions with so many values at the bottom of the y axis, but you 
#get an indication from the plot that recorded crime numbers are positively associated with increased 
#urban density.
########################################################

########################################################
#Summary of total crime for each category of urban

##Large urban summary of recorded crime
summary(totcr[totcr$Urban %in% "Large urban area","n"])
##Major urban summary of recorded crime
summary(totcr[totcr$Urban %in% "Major urban area","n"])
##Medium urban summary of recorded crime
summary(totcr[totcr$Urban %in% "Medium urban area","n"])
##Small urban summary of recorded crime
summary(totcr[totcr$Urban %in% "Small urban area","n"])
##Rural settlement summary of recorded crime
summary(totcr[totcr$Urban %in% "Rural settlement","n"])
##Other rural summary of recorded crime
summary(totcr[totcr$Urban %in% "Rural other","n"])

#A general trend downward through the categories when looking at the centre of the data,
#but also some differences in the spread.
###########################################################

###########################################################
#A non-parametric test for differences among medians in reported crimes by urban (Kruskall-Wallis)

kruskal.test(n ~ Urban, data = totcr)

##Significant result suggesting there are differences among categories of Urban. 
#Post-hoc testing would be needed to determine which pairs are different. that's probabaly not
#within scope right now - looking at the summary statistics, I woud hypothesise that the levels are not 
#all significantly different from one another.
############################################################

##*********************************************************************************************************##

#Overall comment: The skewness of the distribution of recorded crime by area makes it really difficult to 
#identify and interpret any other patterns. There are other features of the data that also could contribute 
#to obscuring of patterns, such as issues of scale. 

#It would possibly be of interest to take a subset of only the high-crime areas and then compare the 
#distributions of variables within that small subset to their underlying distributions (across the full 
#set of areas included in the recorded crime data set at least). It might also be useful to standardise across
#some variables to make them easier to compare.


