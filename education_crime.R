#LOAD DATA AND PACKAGES

require(dplyr)
require(knitr)
require(ggplot2)
require(stringr)
require(tidyverse)
require(cowplot)

load(file = "Data301Project.RData")

# ---- CLEANING AND MERGING DATASETS ---- #

# Select area codes of interest
areas <- area_conversions %>% select(c("SA12018_code", "MB2013_code","REGC2020_code", "REGC2020_name" ))

TandP_Police_full$Year.Month <- gsub(".* ", "\\2", TandP_Police_full$Year.Month)

# Filter by 2018 and merge with area codes
TandP_2018 <- TandP_Police_full %>% filter(Year.Month == 2018)  %>% 
  left_join(areas, by= c("Meshblock" = "MB2013_code"))

#Removed meshblocks not matched
TandP_2018 <- TandP_2018  %>% filter(!is.na(SA12018_code))

# Merge with census 2018
TandP_2018$SA12018_code <- as.character(TandP_2018$SA12018_code)
TandP_2018 <- TandP_2018 %>% 
  left_join(census2018[ ,c("Area_code", "Census_2018_usually_resident_population_count")], by= c("SA12018_code" = "Area_code"))


# Filter qualifications by Territory
census2018_edu <- census2018_edu %>% 
  filter( Area_code!="Total", Maori_ethnic_group_indicator_summary_code=="Total", !Highest_qualification_code %in% c( "Total" ,"Total stated")) 

require(plyr)
# Aggregate qualifications for readability
census2018_edu$Highest_qualification_description <- 
  mapvalues(census2018_edu$Highest_qualification_description, 
            from=c("Level 1 certificate","Level 2 certificate","Level 3 certificate","Level 4 certificate",
                   "Level 5 diploma","Level 6 diploma","Master's degree", "Doctorate degree","Overseas secondary school qualification"), 
            to=c("Level 1-4 certificate or Overseas secondary school","Level 1-4 certificate or Overseas secondary school"
                 ,"Level 1-4 certificate or Overseas secondary school", "Level 1-4 certificate or Overseas secondary school",
                 "Level 5-6 diploma","Level 5-6 diploma","Master's and Doctorate degree", "Master's and Doctorate degree","Level 1-4 certificate or Overseas secondary school"))
detach(package:plyr)

# ---- Summarize  by Territory ---- #

# Calculate population by Territory Authority
TA_pop <- distinct(TandP_2018, by=SA12018_code,.keep_all = TRUE) %>% 
  group_by(Territorial.Authority) %>% 
  summarise(pop = sum(Census_2018_usually_resident_population_count)) 

# Calculate crimes by Territory Authority
TA_crimes <- TandP_2018 %>% 
  group_by(Territorial.Authority) %>% 
  summarise(crimes = sum(Number.of.Records)) 

# Calculate crimes rate in 2018 by Territory Authority
TA_summ <- left_join(TA_crimes, TA_pop) %>% 
  mutate(crime_rate = (crimes/pop)*10000)

#Pivot table with qualification by territory
temp <- census2018_edu %>%  filter(Area_type == 'Territorial Authority Local Board')
temp$Highest_qualification_percent <- as.numeric(as.character(temp$Highest_qualification_percent))

TA_summ$Territorial.Authority <- str_replace(TA_summ$Territorial.Authority,"\\.","")
temp <- pivot_wider(temp[ ,c("Area_description", "Highest_qualification_description","Highest_qualification_percent")], names_from = Highest_qualification_description, values_from = Highest_qualification_percent, values_fn = sum)
TA_summ2 <- left_join(TA_summ, temp, by=c("Territorial.Authority"="Area_description"))


# ---- Summarize  by Region ---- #

# Summ qualification by Region
census2018_edu$Census_usually_resident_population_count_aged_15_years_and_over <- as.numeric(census2018_edu$Census_usually_resident_population_count_aged_15_years_and_over)

qualRG <- census2018_edu %>% filter(Area_type == 'Regional Council') %>%
  group_by(Area_description, Highest_qualification_description) %>%
  summarise(Pop = sum(Census_usually_resident_population_count_aged_15_years_and_over)) %>%
  rename(Region = Area_description)

qualRG$Region <- str_replace(qualRG$Region,"Region","")

# Calculate population by Region
RG_pop <- distinct(TandP_2018, by=SA12018_code,.keep_all = TRUE) %>% 
  group_by(REGC2020_name) %>% 
  summarise(pop = sum(Census_2018_usually_resident_population_count)) 

# Calculate crimes by Region
RG_crimes <- TandP_2018 %>% 
  group_by(REGC2020_name) %>% 
  summarise(crimes = sum(Number.of.Records)) 

# Calculate crimes rate in 2018 by Region
RG_summ <- left_join(RG_crimes, RG_pop) %>% 
  mutate(crime_rate = (crimes/pop)*10000) %>% 
  rename(Region = REGC2020_name) 

# Clean region name
RG_summ$Region <- str_replace(RG_summ$Region,"\\?","u") %>% 
  str_replace("Manawatu-Whanganui Region","Manawatu-Wanganui Region")

#Pivot table with qualification by region 
temp <- census2018_edu[ ,c("Area_description", "Highest_qualification_description","Highest_qualification_percent")]
temp$Highest_qualification_percent <- as.numeric(as.character(temp$Highest_qualification_percent))
temp <- pivot_wider(temp, names_from = Highest_qualification_description, values_from = Highest_qualification_percent, values_fn = sum)
RG_summ2 <- left_join(RG_summ, temp, by=c("Region"="Area_description"))


# ---- Graphs ---- #

# Stacked plot graph with highest qualification % by Region 
p1 <- ggplot(data=qualRG, aes(x=Region, y=Pop, fill=Highest_qualification_description)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="2018 Highest qualification % by Region", x="Region", y = "Population %")+
  theme(legend.position="bottom")
p1+coord_flip()+scale_fill_grey()

# 2018 Crime Rate by Region (by 10k population)
p2 <- ggplot(data=RG_summ, aes(x=Region, y=crime_rate)) +
  geom_bar(stat="identity") +
  labs(title="2018 Crime Rate by Region (by 10k population)", x="Region", y = "Crime Rate")
p2+coord_flip()+scale_fill_grey()

#plot_grid(p1+coord_flip()+scale_fill_grey(), p2+coord_flip()+scale_fill_grey(), ncol=2, align="h",axis = "bt")

# ---- Summary tables ---- #

# 2018 Crime Rate per Territory Authority x Highest Qualification %
kable(arrange(TA_summ2[,c(1,4:10)],desc(crime_rate))[1:10,],"simple",caption="2018 Crime Rate per Territory Authority x Highest  Qualification %")

# 2018 Crime Rate per Region x Highest Qualification %
kable(arrange(RG_summ2[,c(1,4:10)],desc(crime_rate)),"simple", caption="2018 Crime Rate per Region x Highest Qualification %")

