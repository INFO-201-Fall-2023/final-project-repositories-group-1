library(dplyr)
library(stringr)
library(ggplot2)

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

#regions df starts here
aggregated_data <- aggregate(df[, c("Total.Homeless", "Predicted.Value", "Total.Family.Households.Experiencing.Homelessness", "Veterans.Experiencing.Homelessness", "Persons.Experiencing.Chronic.Homelessness", "Unaccompanied.Young.Adults..Aged.18.24..Experiencing.Homelessness")], 
                             by = list(df$region), FUN = sum)

regions_df <- data.frame(region = aggregated_data$Group.1,
                         total_homeless = aggregated_data$Total.Homeless,
                         total_overdose = aggregated_data$Predicted.Value,
                         total_family_households_experiencing_homelessness = aggregated_data$Total.Family.Households.Experiencing.Homelessness,
                         veterans_experiencing_homelessness = aggregated_data$Veterans.Experiencing.Homelessness,
                         persons_experiencing_chronic_homelessness = aggregated_data$Persons.Experiencing.Chronic.Homelessness,
                         unaccompanied_adults_aged_18_to_24_experiencing_homelessness = aggregated_data$Unaccompanied.Young.Adults..Aged.18.24..Experiencing.Homelessness)


barplot_homelessness <- ggplot(data = df, aes(x = region, y = Total.Homeless, fill = region)) +
  geom_col() +
  labs(x = "Region In US", y = "Number Of Homelessness", title = "Region vs Number Of People Experiencing Homelessness", 
       caption = "The total number of people expreincing homelessness in each state and is added to the corresponding region")
barplot_homelessness

scatter_homeless <- ggplot(data = df) + 
  geom_point(mapping = aes(x = State.Name, y = Total.Homeless, color = region)) +
  labs(x = "State In US", y = "Number Of People Experiencing Homelessness", title = "State vs Number Of People Experiencing Homelessness",
       caption = "The total number of people experiencing homelessness in each state") +
  theme(axis.text.x = element_text(angle = 90))
scatter_homeless

high_inc_barplot <- ggplot(data = df, aes(x = region, y = Predicted.Value, fill = region)) +
  geom_col() +
  labs(x = "Region In US", y = "Number Of People Who Overdosed", title = "Region vs Number Of People Who Overdosed",
       caption = "The total number of people who overdosed in each state is added to the corresponding region") 
high_inc_barplot

inc_scatter <-  ggplot(data = df) + 
  geom_point(mapping = aes(x = State.Name, y = Predicted.Value, color = region)) +
  labs(x = "State In US", y = "Number Of People Who Overdosed", title = "State vs Number Of People Who Overdosed", 
       caption = "The total number of people who overdosed in each state") + 
  theme(axis.text.x = element_text(angle = 90))
inc_scatter

bubble_plot_homeless <- ggplot(data = df, aes(x=Predicted.Value, y=Total.Homeless, color = region)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="Total Homeless") +
  theme(legend.position="none")
bubble_plot_homeless
