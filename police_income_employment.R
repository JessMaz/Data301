# Packages
require(ggplot2)
require(dplyr)
require(tidyr)

#---------------EXPLORING AND MERGING DATASETS---------------
# Load data
load(file = "Data301Project.RData")

# Replace 'C' with NA
census2018[census2018 == "C"] <- NA

# Count NAs
NAs_Census <- as.data.frame(colSums(is.na(census2018)))
NA_Police <- as.data.frame(colSums(is.na(TandP_Police_full)))

# Summarize errors
table(unlist(TandP_Police_full$Area.Unit))
sum(TandP_Police_full$Meshblock < 100)
sum(TandP_Police_full$Location.Type == ".")
sum(TandP_Police_full$Location.Type == "Unspecified Location")
sum(TandP_Police_full$Occurrence.Day.Of.Week == "UNKNOWN")
sum(TandP_Police_full$Occurrence.Day.Of.Week == ".")
sum(TandP_Police_full$Area.Unit== "-29")
sum(TandP_Police_full$Area.Unit== "999999")
TandP_Police_full <- na.omit(TandP_Police_full)
sum(TandP_Police_full$Occurrence.Hour.Of.Day < 23)

# Clean for merging
linking_codes <- area_conversions %>% select(c("SA12018_code", "MB2013_code", ))
TandP_Police_full$Year.Month <- gsub(".* ", "\\2", TandP_Police_full$Year.Month)

# Filter police by 2018
TandP_Police2018 <- TandP_Police_full %>% filter(Year.Month == 2018)

# Merge police
area_police <- merge(x = linking_codes, y = TandP_Police2018, by.x = "MB2013_code", 
                   by.y = "Meshblock")

# Merged census
merged_full <- merge(x = area_police, y = census2018, by.x = "SA12018_code", 
                    by.y = "Area_code")



#---------------EXPLORATORY ANALYSIS STARTS HERE---------------
# Get attributes of interest
income_df <- merged_full %>% select(c("SA12018_code",
                             "Territorial.Authority", 
                             "Victimisations",
                             "Census_2018_Work_and_labour_force_status_3_Unemployed_CURP_15years_and_over",
                             "Census_2018_Total_personal_income_Median_CURP_15years_and_over",
                             "Census_2018_usually_resident_population_count"))

# Cleaning
income_df <- na.omit(income_df)
income_df$Census_2018_Work_and_labour_force_status_3_Unemployed_CURP_15years_and_over <- as.numeric(income_df$Census_2018_Work_and_labour_force_status_3_Unemployed_CURP_15years_and_over)
income_df$Census_2018_Total_personal_income_Median_CURP_15years_and_over <- as.numeric(income_df$Census_2018_Total_personal_income_Median_CURP_15years_and_over)

# Get population per territory
pop_by_SA <- income_df[!duplicated(income_df$SA12018_code), ]
pop_by_SA <- pop_by_SA %>% group_by(Territorial.Authority) %>% 
                             summarise(pop = sum(Census_2018_usually_resident_population_count)) 

# Sum by territory
income_df <- income_df %>% group_by(Territorial.Authority) %>% 
                            summarise(victims = sum(Victimisations), 
                             mean_UR = mean(Census_2018_Work_and_labour_force_status_3_Unemployed_CURP_15years_and_over),
                             mean_income = mean(Census_2018_Total_personal_income_Median_CURP_15years_and_over))                         

# Merge together
income_df <- merge(x = income_df, y = pop_by_SA, by = "Territorial.Authority")

# Get percent of territory victimized 
income_df$victims_by_pop <- income_df$victims/income_df$pop

# Plot victimization
victim_plot <- ggplot(income_df, aes(Territorial.Authority, victims_by_pop)) +
                      geom_bar(stat="identity") +
                      theme(axis.text.x = element_text(angle = 90, hjust=1),
                            text = element_text(size = 10)) +
                      labs(x="", y="Victimisation Rate (% of Territory Pop.)")

victim_plot

# Plot income
income_plot <- ggplot(income_df, aes(Territorial.Authority, mean_income)) +
                      geom_bar(stat="identity") +
                      theme(axis.text.x = element_text(angle = 90, hjust=1),
                            text = element_text(size = 10)) +
                      labs(x="", y="Mean Personal Income ($NZD)")

income_plot

# Plot unemployment
unemployment_plot <- ggplot(income_df, aes(Territorial.Authority, mean_UR)) +
                            geom_bar(stat="identity") +
                            theme(axis.text.x = element_text(angle = 90, hjust=1),
                                  text = element_text(size = 10)) +
                            labs(x="", y="Unemployment Rate (%)")

unemployment_plot
