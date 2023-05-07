library(dplyr)
library(stringr)

homeless_df <- read.csv("2022_Homeless_Population_by_State.csv")
drug_df <- read.csv("2022_VSRR_Provisional_Drug_Overdose_Death_Counts.csv")

drug_df[drug_df == ""] <- NA
drug_df <- na.omit(drug_df)

drug_df2 <- aggregate(. ~ State.Name, data = drug_df, paste, collapse = ", ")
drug_df2$Year <- sapply(strsplit(drug_df2$Year, ","), function(x) x[1])
drug_df2$State <- sapply(strsplit(drug_df2$State, ","), function(x) x[1])
drug_df2$Period <- sapply(strsplit(drug_df2$Period, ","), function(x) x[1])

drug_df2$Percent.Complete <- as.character(drug_df2$Percent.Complete)
drug_df2$Percent.Complete <- sapply(strsplit(drug_df2$Percent.Complete, ","), function(x) round(mean(as.numeric(x)), 1))

drug_df2$Percent.Pending.Investigation <- as.character(drug_df2$Percent.Pending.Investigation)
drug_df2$Percent.Pending.Investigation <- sapply(strsplit(drug_df2$Percent.Pending.Investigation, ","), function(x) round(mean(as.numeric(x)), 1))

drug_df2$Predicted.Value <- as.character(drug_df2$Predicted.Value)
drug_df2$Predicted.Value <- sapply(strsplit(drug_df2$Predicted.Value, ","), function(x) round(mean(as.numeric(x)), 1))

split_month <- strsplit(drug_df2$Month, ",")
unique_month <- lapply(split_month, unique)
drug_df2$Month <- sapply(unique_month, paste, collapse = ",")

split_indicator <- strsplit(drug_df2$Indicator, ",")
unique_indicator <- lapply(split_indicator, unique)
drug_df2$Indicator <- sapply(unique_indicator, paste, collapse = ",")

df <- left_join(drug_df2, homeless_df, by = c("State.Name" = "State"))

df$region <- ifelse(df$State == "CT" | df$State == "ME" | df$State == "MA" | df$State == "NH" | df$State == "RI" | df$State == "VT", "New England",
                              ifelse(df$State == "DE" | df$State == "DC" | df$State == "MD" | df$State == "NJ" | df$State == "NY" | df$State == "PA", "Mideast",
                                      ifelse(df$State == "IL" | df$State == "IN" | df$State == "MI" | df$State == "OH" | df$State == "WI", "Great Lakes",
                                              ifelse(df$State == "IA" | df$State == "KS" | df$State == "MN" | df$State == "MO" | df$State == "NE" | df$State == "ND" | df$State == "SD", "Plains",
                                                      ifelse(df$State == "AL" | df$State == "AR" | df$State == "FL" | df$State == "GA" | df$State == "KY" | df$State == "LA" | df$State == "MS" | df$State == "NC" | df$State == "SC" | df$State == "TN" | df$State == "VA" | df$State == "WV", "Southeast",
                                                              ifelse(df$State == "AZ" | df$State == "NM" | df$State == "OK" | df$State == "TX", "Southwest",
                                                                      ifelse(df$State == "CO" | df$State == "ID" | df$State == "MT" | df$State == "UT" | df$State == "WY", "Rocky Mountain",
                                                                              ifelse(df$State == "AK" | df$State == "CA" | df$State == "HI" | df$State == "NV" | df$State == "OR" | df$State == "WA", "Far West", "Other"))))))))

veterans_homeless <- sum(df$Veterans.Experiencing.Homelessness)
df$Veteran_Homelessness_Ratio <- (df$Veterans.Experiencing.Homelessness/veterans_homeless) * 100

state_grouped <- group_by(df, State.Name)
state_total_homeless <- summarize(state_grouped, total_homeless = sum(Total.Homeless))
state_total_overdose_death <- summarize(state_grouped, total_overdose_death = sum(Predicted.Value))
state_total <- merge(state_total_homeless, state_total_overdose_death, by = "State.Name", all = TRUE)
