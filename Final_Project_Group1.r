library(dplyr)
library(stringr)

homeless_df <- read.csv("2022_Homeless_Population_by_State.csv")
drug_df <- read.csv("2022_VSRR_Provisional_Drug_Overdose_Death_Counts.csv")

combined_df <- aggregate(. ~ State.Name, data = drug_df, paste, collapse = ", ")
combined_df$Year <- sapply(strsplit(combined_df$Year, ","), function(x) x[1])
combined_df$State <- sapply(strsplit(combined_df$State, ","), function(x) x[1])
combined_df$Period <- sapply(strsplit(combined_df$Period, ","), function(x) x[1])

combined_df$Percent.Complete <- as.character(combined_df$Percent.Complete)
combined_df$Percent.Complete <- sapply(strsplit(combined_df$Percent.Complete, ","), function(x) round(mean(as.numeric(x)), 1))

combined_df$Percent.Pending.Investigation <- as.character(combined_df$Percent.Pending.Investigation)
combined_df$Percent.Pending.Investigation <- sapply(strsplit(combined_df$Percent.Pending.Investigation, ","), function(x) round(mean(as.numeric(x)), 1))

split_month <- strsplit(combined_df$Month, ",")
unique_month <- lapply(split_month, unique)
combined_df$Month <- sapply(unique_month, paste, collapse = ",")

split_indicator <- strsplit(combined_df$Indicator, ",")
unique_indicator <- lapply(split_indicator, unique)
combined_df$Indicator <- sapply(unique_indicator, paste, collapse = ",")