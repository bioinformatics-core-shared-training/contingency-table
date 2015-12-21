####shiny::runGitHub("OneSidedT","markdunning")

library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Contingency tables"),
  
  sidebarPanel(
    h2("Data Import Parameters"),

    textInput("table", "Enter your data as a table",value="44|40--24|16"),
    helpText("Type the values in your table from left to right. Separate columns with a | and rows with --. You do not need to calculate the column or row totals - the app will do that for you"),
    radioButtons("test","Type of Test",choices = c("Chi squared"="chi-squared","Fishers"= "fishers"))

    ),
  
  mainPanel(
    tabsetPanel(
#      tabPanel("Plot",plotOutput("plot")),
      tabPanel("The data", verbatimTextOutput("mytable"),verbatimTextOutput("summary")),
      tabPanel("Test Result", 
      h3("Expected Frequencies for the table"),
      verbatimTextOutput("frequencies"),
      h3("Result of currently-selected test"),
      verbatimTextOutput("result"),
      plotOutput("distribution")
      )
    )
  )
  
  )
)

