library(dplyr)
library(stringr)

homeless_df <- read.csv("2022_Homeless_Population_by_State.csv")
drug_df <- read.csv("2022_VSRR_Provisional_Drug_Overdose_Death_Counts.csv")

drug_df[drug_df == ""] <- NA
drug_df <- na.omit(drug_df)

combined_df <- aggregate(. ~ State.Name, data = drug_df, paste, collapse = ", ")
combined_df$Year <- sapply(strsplit(combined_df$Year, ","), function(x) x[1])
combined_df$State <- sapply(strsplit(combined_df$State, ","), function(x) x[1])
combined_df$Period <- sapply(strsplit(combined_df$Period, ","), function(x) x[1])

combined_df$Percent.Complete <- as.character(combined_df$Percent.Complete)
combined_df$Percent.Complete <- sapply(strsplit(combined_df$Percent.Complete, ","), function(x) round(mean(as.numeric(x)), 1))

combined_df$Percent.Pending.Investigation <- as.character(combined_df$Percent.Pending.Investigation)
combined_df$Percent.Pending.Investigation <- sapply(strsplit(combined_df$Percent.Pending.Investigation, ","), function(x) round(mean(as.numeric(x)), 1))

combined_df$Predicted.Value <- as.character(combined_df$Predicted.Value)
combined_df$Predicted.Value <- sapply(strsplit(combined_df$Predicted.Value, ","), function(x) round(mean(as.numeric(x)), 1))                                                    
                                                    
split_month <- strsplit(combined_df$Month, ",")
unique_month <- lapply(split_month, unique)
combined_df$Month <- sapply(unique_month, paste, collapse = ",")

split_indicator <- strsplit(combined_df$Indicator, ",")
unique_indicator <- lapply(split_indicator, unique)
combined_df$Indicator <- sapply(unique_indicator, paste, collapse = ",")
                                                    
combined_df$region <- ifelse(combined_df$State == "CT" | combined_df$State == "ME" | combined_df$State == "MA" | combined_df$State == "NH" | combined_df$State == "RI" | combined_df$State == "VT", "New England",
                              ifelse(combined_df$State == "DE" | combined_df$State == "DC" | combined_df$State == "MD" | combined_df$State == "NJ" | combined_df$State == "NY" | combined_df$State == "PA", "Mideast",
                                      ifelse(combined_df$State == "IL" | combined_df$State == "IN" | combined_df$State == "MI" | combined_df$State == "OH" | combined_df$State == "WI", "Great Lakes",
                                              ifelse(combined_df$State == "IA" | combined_df$State == "KS" | combined_df$State == "MN" | combined_df$State == "MO" | combined_df$State == "NE" | combined_df$State == "ND" | combined_df$State == "SD", "Plains",
                                                      ifelse(combined_df$State == "AL" | combined_df$State == "AR" | combined_df$State == "FL" | combined_df$State == "GA" | combined_df$State == "KY" | combined_df$State == "LA" | combined_df$State == "MS" | combined_df$State == "NC" | combined_df$State == "SC" | combined_df$State == "TN" | combined_df$State == "VA" | combined_df$State == "WV", "Southeast",
                                                              ifelse(combined_df$State == "AZ" | combined_df$State == "NM" | combined_df$State == "OK" | combined_df$State == "TX", "Southwest",
                                                                      ifelse(combined_df$State == "CO" | combined_df$State == "ID" | combined_df$State == "MT" | combined_df$State == "UT" | combined_df$State == "WY", "Rocky Mountain",
                                                                              ifelse(combined_df$State == "AK" | combined_df$State == "CA" | combined_df$State == "HI" | combined_df$State == "NV" | combined_df$State == "OR" | combined_df$State == "WA", "Far West", "Other"))))))))
  
