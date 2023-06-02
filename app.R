library(shiny)

# UI Stuff goes here
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Summary",
             h2("Summary of the Project Represented Through Tables and Pictures"),
             h4("Population by State"),
             tableOutput("summary_table1"),
             
             h5("State(s) shaded in blue represents a top five state with homelessness"),
             h5("State(s) shaded in red represent a top five state in population"),
             h5("State(s) shaded in purple represent a top five state with homelessness and top five state in population"),
             
             tags$img(src = 'top_5_states.png', height = 250, width = 350),
             
             h4("Bottom Five States With the Highest Homeless Population"),
             tableOutput("summary_table2"),
             
             h5("States shaded in purple represents a bottom five state with homelessness"),
             tags$img(src = 'bottom_5_states.png', height = 300, width = 400),
             
             h4("Top Five States With the Most Drug Overdose Deaths"),
             tableOutput("summary_table3"),
             
             h5("States shaded in green represents a top five state with the most drug overdose deaths"),
             tags$img(src = 'overdose.png', height = 250, width = 350),
             p(" ")
    ),
    tabPanel("About",
             h2("About the Project"),
             h4("Main Takeaway: states with a higher homeless population does not have a correlation with drug overdose deaths"),
             h4("Project Data Timeline: January, 2022 - December, 2022"),
             h4("Project Data References: CDC National Center for Health Statistics & U.S. Department of Housing and Urban Development"),
             h4("Project Team Members: Diana Thiamtisak, Selena Yu, and James Parrott")
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
}

# Make the app
shinyApp(ui = ui, server = server)
