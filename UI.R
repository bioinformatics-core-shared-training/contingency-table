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
      tabPanel("About",helpText("This app was developed by the Bioinformatics Core of Cancer Research Uk Cambridge Institute to accompany a training course. On the course webpage you will find lecture notes from the course and practical exercises that use this app"),
               a("Introduction to Statistical Analysis",href="http://bioinformatics-core-shared-training.github.io/IntroductionToStats/"),
               br(),
               helpText(),
               br(),
               br(),
               img(src="cruk-cambridge-institute.jpg",width=350,height=77), br(),a("cruk.cam.ac.uk",href="www.cruk.cam.ac.uk"),
               br(),
               br(),
               a("View source Code for app", href="https://github.com/bioinformatics-core-shared-training/contingency-table.git")),
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

