library(shiny)
library(ggplot2)
library(plotly)
library(treemap)

source("Final_Project_Group1.R")

# UI Stuff goes here
ui <- fluidPage(
  titlePanel("Examining Drug Overdoses and Homelessness in the United States"),
  tabsetPanel(

    tabPanel("Introduction",
             h2("About the Project"),
             h3("Project Team Members: Diana Thiamtisak, Selena Yu, and James Parrott"),
             h5("Growing up in downtown Seattle, we’ve experienced firsthand how our community has changed over the years. 
                One of the most significant shifts we’ve witnessed has been the drug epidemic. Highly potent and dangerous drugs 
                have not only caused our city devastation but other major metropolitan cities and small towns as well. It has 
                led to a national crisis and a substantial increase in overdoses and deaths. At the same time, homelessness is 
                an issue many people around the country are experiencing. According to the CDC, “people are considered to be 
                experiencing homelessness if they stay in a shelter, live in transitional housing, or sleep in a place not meant 
                for human habitation, such as a car or outdoors. Sometimes people are considered to be experiencing homelessness 
                if they are living in a motel or are doubled up with family or friends because they do not have anywhere else to 
                stay.” There are many different groups of people who are differently affected by homelessness and every individual’s
                experience is unique. However, people experiencing homelessness tend to be at higher risk of contracting bloodborne pathogens, 
                COVID-19, TB, mental illness, and worse health outcomes."),
             h5("Through this project, we intend to highlight the impact of drug overdoses in different cities in the United States. 
                Therefore, we hope that our examinations can spotlight how overdosage has impacted the cities we thrive in. Overall,
                we believe that this story is important and compelling because it highlights a crisis that is affecting communities across 
                the country. With overdoses and homelessness continuing to rise, it is evident that it is affecting individuals and communities
                from all walks of life. Because of this, we hope to dig deeper into this issue and analyze any trends within the data."),
             h5("We found two independent datasets that come from two independent sources. The first dataset is from the CDC National Center 
             for Health Statistics (https://www.cdc.gov/nchs/products/databriefs/db428.html) and the second dataset is from the U.S. Department 
             of Housing and Urban Development (https://millennialcities.com/homeless-population-by-state-homelessness-maps/)."),
             
             #ADD BELOW IN LAST TAB
             
             h2("Summary of the Project Represented Through Tables"),
             h4("Population by State"),
             tableOutput("summary_table1"),
             
             
             h4("Top Five States With the Most Drug Overdose Deaths"),
             tableOutput("summary_table3"),
    ),
    
    tabPanel("Homelessness in the U.S.",
             h4("Homelessness crisis in the US has escalated due to housing shortages, food insecurity, 
                poor access to health care, lack of support for veterans and unemployment. Lets take a closer look."),
             controls <- sidebarPanel(
               selectInput(
                 inputId = "region_name_h",
                 label = "Choose a Region",
                 choices = df$region
               ),
               htmlOutput(outputId = "homeless_region"),
             ),
             
             mainPanel(
               htmlOutput(outputId = "homeless_region_bar")
             ),
             
             h4("It appears Far West and Mideast have have a high concentration of homelessness population! Lets dig even deeper 
                and examine the homeless total by state."),
             
             mainPanel(
               htmlOutput(outputId = "homeless_state_plot"),
               h4("To sum it up..."),
               h5("California’s homelessness population is the highest followed by New York. 
             When analyzing regions that Far West and Mideast total homelessness population, 
             we can conclude California and New York occupies majority of the bar. When we 
             continue looking at the statistics for each state, North Dakota has the least amount 
             of homelessness population (541) followed by Wyoming (612). California has 161,548 which 
             is almost a third of the entire homelessness population in the US!"),
             ),
             
    ),
    
    tabPanel("Provisional Drug Overdoses in the U.S.",
             h4("At the same time, drug abuse and overdose has also increased for a 
                multitude of reasons including the pandemic, peer pressure, and difficult family situations. Lets take a closer look."),
             controls <- sidebarPanel(
               selectInput(
                 inputId = "region_name_o",
                 label = "Choose a Region",
                 choices = df$region
               ),
               htmlOutput(outputId = "overdose_region"),
             ),
             
             mainPanel(
               htmlOutput(outputId = "overdose_region_bar")
             ),
             
             h4("It appears the Southeast and Mideast regions have have the highest concentration of overdose cases! Lets dig even deeper 
                and examine the cases by state."),
             
             mainPanel(
               htmlOutput(outputId = "overdose_state_plot"),
               h4("To sum it up..."),
               h5("Georgia’s number of overdosage cases is the highest followed by Oregon. 
             When analyzing the number of overdosage cases in the Southeast and Mideast regions,
             we can conclude Georgia and Oregon occupies majority of the bar. However, it is only by a small margin. When we 
             continue looking at the statistics for each state, South Dakota has the least amount 
             of overdose cases (40.2) followed by Wyoming (46.7). Georgia has 398.3 which 
             is only 3.36% of all overdose cases in the US! Overall, there seems to be no trend in the scatterplot."),
             ),
             
    ),
    
    tabPanel("Comparing the Two Variables",
             h4("Lets look at the homeless and overdose numbers side by side on a state map!"),
             controls <- sidebarPanel(
               selectInput(
                 inputId = "selected_state",
                 label = "Choose a State",
                 choices = df$State.Name
               ),
               htmlOutput(outputId = "state_info"),
             ),
             
             mainPanel(
               htmlOutput(outputId = "bubble_plot"),
               h4("Overall, it looks like there is not much correlation between drug overdose and homelessness numbers.")
             )
    )
  ),
)

# server Stuff goes here
server <- function(input, output) {
  output$summary_table1 <- renderTable({
    summary_data <- data.frame(
      States = c("1. California", "2. New York", "3. Florida", "4. Washington", "5. Texas"),
      Population = c(161548, 91271, 27487, 22923, 27229)
    )
    
    summary_data$Population <- format(summary_data$Population, big.mark = ",", trim = TRUE)
    
    return(summary_data)
  })
  
  output$summary_table2 <- renderTable({
    summary_data <- data.frame(
      States = c("1. North Dakota", "2. Wyoming", "3. South Dakota", "4. Rhode Island", "5. Missouri"),
      Population = c(541, 612, 1058, 1104, 1107)
    )
    
    summary_data$Population <- format(summary_data$Population, big.mark = ",", trim = TRUE)
    
    return(summary_data)
  })
  
  output$summary_table3 <- renderTable({
    summary_data <- data.frame(
      States = c("1. Georgia", "2. Oregon", "3. Illinois", "4. New Mexico", "5. Minnesota"),
      Population = c(398, 341, 337, 334, 332)
    )
    
    summary_data$Population <- format(summary_data$Population, big.mark = ",", trim = TRUE)
    
    return(summary_data)
  })
  
  output$homeless_region <- renderUI ({
    h4(paste("The total homeless population in the", input$region_name_h, " in 2022 is", regions_df$total_homeless[regions_df$region == input$region_name_h]))
  })
  
  output$homeless_region_bar <- renderUI ({
    barplot_homelessness <- ggplot(data = regions_df, aes(x = region, y = total_homeless, fill = region)) +
      geom_col() +
      labs(x = "Region In US", y = "Number Of Homelessness", title = "Region vs Number Of People Experiencing Homelessness", 
           caption = "The total number of people expreincing homelessness in each state and is added to the corresponding region") +
      geom_text(data = subset(regions_df, region == input$region_name_h),
                aes(label = input$region_name_h),
                vjust = -0.5,
                color = "black")
    
    barplot_homelessness <- ggplotly(barplot_homelessness, tooltip = "text")
    return(barplot_homelessness)
  })
  
  output$homeless_state_plot <-  renderUI ({
    scatter_homeless <- ggplot(data = df) + 
      geom_point(mapping = aes(x = State.Name, y = Total.Homeless, color = region)) +
      labs(x = "State In US", y = "Number Of People Experiencing Homelessness", title = "State vs Number Of People Experiencing Homelessness",
           caption = "The total number of people experiencing homelessness in each state") +
      theme(axis.text.x = element_text(angle = 90))
    
    scatter_homeless <- ggplotly(scatter_homeless, tooltip = "text")
    return(scatter_homeless)
  })
  
  output$overdose_region <- renderUI ({
    h4(paste("The total number of overdose cases in the", input$region_name_o, " in 2022 is", regions_df$total_overdose[regions_df$region == input$region_name_o]))
  })
  
  output$overdose_region_bar <- renderUI ({
    barplot_overdose <- ggplot(data = regions_df, aes(x = region, y = total_overdose, fill = region)) +
      geom_col() +
      labs(x = "Region In US", y = "Number Of People Who Overdosed", title = "Region vs Number Of People Who Overdosed",
           caption = "The total number of people who overdosed in each state is added to the corresponding region") +
      geom_text(data = subset(regions_df, region == input$region_name_o),
                aes(label = input$region_name_o),
                vjust = -0.5,
                color = "black")
    
    barplot_overdose <- ggplotly(barplot_overdose, tooltip = "text")
    return(barplot_overdose)
  })
  
  output$overdose_state_plot <-  renderUI ({
    scatter_homeless <-  ggplot(data = df) + 
      geom_point(mapping = aes(x = State.Name, y = Predicted.Value, color = region)) +
      labs(x = "State In US", y = "Number Of People Who Overdosed", title = "State vs Number Of People Who Overdosed", 
           caption = "The total number of people who overdosed in each state") + 
      theme(axis.text.x = element_text(angle = 90))
    
    scatter_homeless <- ggplotly(scatter_homeless, tooltip = "text")
    return(scatter_homeless)
  })
  
  output$state_info <- renderUI ({
    h4(paste("The total homeless population in the", input$selected_state, " in 2022 is ", 
             df$Total.Homeless[df$State.Name == input$selected_state], " and the total number of overdose cases is ",
             df$Predicted.Value[df$State.Name == input$selected_state]))
  })
  
  output$bubble_plot <- renderUI({
    bubble_plot <- ggplot(data = df, aes(x=Predicted.Value, y=Total.Homeless, color = region)) +
      geom_point(alpha=0.7) +
      scale_size(range = c(1.4, 19), name="Total Homeless") +
      scale_color_viridis(discrete=TRUE, guide=FALSE) +
      theme_ipsum() +
      geom_text(data = subset(df, State.Name == input$selected_state),
                aes(label = input$selected_state),
                vjust = -0.5,
                color = "black") +
      labs(x = "Total Number Of People Who Overdosed", y = "Total Number Of People Experiencing Homelessness", title = "Total Number Of People Experiencing Homelessness vs Total Number Of People Who Overdosed", 
           caption = "The total number of people who overdosed in each state compared to the total number of people experiencing homelessness in each state.") 
    
    bubble_plot <- ggplotly(bubble_plot, tooltip = "text")
    return(bubble_plot)
  })

}

# Make the app
shinyApp(ui = ui, server = server)
