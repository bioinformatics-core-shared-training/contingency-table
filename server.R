
function(input, output, session) {

  values <- reactiveValues(
    tableData = emptyTable,
    observed = emptyTable,
    expected = emptyTable,
    result = NULL
  )

  output$contingencyTable <- renderRHandsontable({
    tableData <- values$tableData
    rhandsontable(
      tableData,
      colHeaders = LETTERS[1:ncol(tableData)],
      rowHeaders = 1:nrow(tableData)
    ) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })

  observe({
    if (!is.null(input$contingencyTable))
    {
      newTableData <- hot_to_r(input$contingencyTable)
      if (!is.null(newTableData))
      {
        isolate(values$tableData <- newTableData)
      }
    }
  })

  observe({
    tableData <- values$tableData
    if (is.null(tableData) || tableData %>% unlist %>% sum(na.rm = TRUE) == 0)
      shinyjs::disable("performTestButton")
    else
      shinyjs::enable("performTestButton")
  })

  observe({
    numberOfRows = as.integer(input$numberOfRows)
    numberOfColumns = as.integer(input$numberOfColumns)

    newTableData <- matrix(data = integer(0), nrow = numberOfRows, ncol = numberOfColumns) %>%
      as_data_frame

    currentTableData <- isolate(values$tableData)
    if (!is.null(currentTableData))
    {
      nr <- min(numberOfRows, nrow(currentTableData))
      nc <- min(numberOfColumns, ncol(currentTableData))
      newTableData[1:nr, 1:nc] <- currentTableData[1:nr, 1:nc]
    }

    values$tableData <- newTableData
  })

  observeEvent(input$clearButton, {
    tableData <- values$tableData
    tableData[1:nrow(tableData),1:ncol(tableData)] <- NA
    values$tableData <- tableData
  })

  observeEvent(input$performTestButton, {

    contingency_table <- values$tableData %>%
      mutate_all(funs(ifelse(is.na(.), 0, .)))

    chisqTestResult <- chisq.test(contingency_table, correct = FALSE)

    values$observed <- as.data.frame(chisqTestResult$observed)
    values$expected <- as.data.frame(chisqTestResult$expected)

    if (input$testType == "chi-squared")
      values$result <- chisqTestResult
    else
      values$result <- fisher.test(contingency_table)
  })

  output$observedTable <- renderRHandsontable({

    observed <- values$observed

    observedWithTotals <- observed %>%
      bind_rows(observed %>% summarize_all(funs(sum(., na.rm = TRUE)))) %>%
      mutate(Total = as.integer(rowSums(., na.rm = TRUE)))

    rhandsontable(
      observedWithTotals,
      colHeaders = c(LETTERS[1:ncol(observed)], "Total"),
      rowHeaders = c(1:nrow(observed), "Total"),
      readOnly = TRUE
    ) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })

  output$expectedTable <- renderRHandsontable({

    expected <- values$expected

    rhandsontable(
      expected,
      colHeaders = LETTERS[1:ncol(expected)],
      rowHeaders = 1:nrow(expected),
      readOnly = TRUE
    ) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })

  output$testResult <- renderPrint({
    result <- values$result
    if (is.null(result))
      invisible()
    else
      result
  })

  output$chiSquaredDistributionPlot <- renderPlot({

    result <- values$result

    statistic <- result$statistic
    if (is.null(statistic) || is.nan(statistic)) return(NULL)

    degreesOfFreedom <- result$parameter

    xmax <- max(statistic, 4) * 1.1
    x <- seq(0, xmax, length.out = 10000)
    distribution <- data_frame(X = x, Y = dchisq(x, degreesOfFreedom))

    title <- paste(" with ", degreesOfFreedom, " degree", ifelse(degreesOfFreedom == 1, "", "s"), " of freedom", sep = "")
    title <- substitute(paste(chi^2, title))

    ggplot(distribution, aes(x = X, y = Y)) +
      geom_line() +
      geom_vline(xintercept = statistic, col = "red") +
      xlim(0, xmax) +
      ggtitle(title) +
      xlab(expression(x)) +
      ylab(expression(f[k](x))) +
      theme_bw() +
      theme(
        title = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12)
      )
  })
}

