fluidPage(

  useShinyjs(),

  titlePanel(
    title = tags$div(
      "Contingency table tests",
      tags$a(
        href="http://www.cruk.cam.ac.uk", target = "_blank",
        tags$div(style = "float:right",
                 tags$img(style = "width:250px", src="cruk-cambridge-institute.jpg"),
                 tags$img(style = "width:150px; padding:1px; margin-left:5px; margin-bottom:15px; background-color:#666666", src="university-of-cambridge.png")
        )
      )
    ),
    windowTitle = "Contingency table tests"
  ),

  tags$div(style="line-height:25%;", br()),
  helpText(
    "This app was developed by the",
    a("CRUK-CI Bioinformatics Core", href = "http://www.cruk.cam.ac.uk/core-facilities/bioinformatics-core", target = "_blank"),
    "to accompany a training course.",
    br(),
    "Lecture notes and practical exercises that use this app can be found on the course",
    a("website.", href = "http://bioinformatics-core-shared-training.github.io/IntroductionToStats", target = "_blank"),
    br(),
    "The source code for the app is available",
    a("here.", href = "https://github.com/bioinformatics-core-shared-training/contingency-table", target = "_blank")
  ),
  tags$div(style="line-height:100%;", br()),

  sidebarLayout(

    sidebarPanel(
      fluidRow(
        column(4, selectInput("numberOfRows", "Rows", choices = 2:6, selected = 2)),
        column(4, selectInput("numberOfColumns", "Columns", choices = 2:6, selected = 2))
      ),
      helpText("Enter contingency table data in the table below. Note that missing values are treated as zeros."),
      tags$div(style="line-height:50%;", br()),
      rHandsontableOutput("contingencyTable"),
      tags$div(style="line-height:150%;", br()),
      actionButton("clearButton", "Clear contents"),
      tags$div(style="line-height:150%;", br()),
      radioButtons("testType", h4("Statistical test"), choices = c("Chi-squared" = "chi-squared","Fisher's exact" = "fisher")),
      actionButton("performTestButton", "Perform test")
    ),

    mainPanel(
      # tags$div(style="line-height:100%;", br()),
      fluidRow(
        column(6,
               h4("Observed counts"),
               tags$div(style="line-height:25%;", br()),
               rHandsontableOutput("observedTable")
        ),
        column(6,
               h4("Expected counts"),
               tags$div(style="line-height:25%;", br()),
               rHandsontableOutput("expectedTable")
        )
      ),
      tags$div(style="line-height:100%;", br()),
      h4("Test result"),
      verbatimTextOutput("testResult"),
      tags$div(style="line-height:150%;", br()),
      plotOutput("chiSquaredDistributionPlot")
    )
  ),

  tags$div(style="line-height:200%;", br())
)
