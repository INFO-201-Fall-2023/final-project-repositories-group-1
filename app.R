library(shiny)

#UI Stuff goes here
ui <- fluidPage(
  h1("Let's do a quick survey!"),
  p("Answer the following questions"),
  textInput(
    inputId = "name",
    label = "What is your name?",
  ),
  radioButtons(
    inputId = "soup",
    label = "Is Cereal Soup?",
    choices = list("yes" = 1, "no" = 2)
  ),
  h1("Your answers:"),
  textOutput(outputId = "greetings"),
  textOutput(outputId = "soupScore")
)

#server Stuff goes here
server <- function(input, output) {
  output$greetings <- renderText({
    #what the person is typing will be stored in input$name
    return(paste("HELLO!!!", input$name))
  })
}

#Make the app
shinyApp(ui = ui, server = server)