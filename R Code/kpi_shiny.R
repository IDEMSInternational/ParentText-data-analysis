kpi_shiny <- function(date_from = "2020-01-01", date_to = NULL){
  ui <- fluidPage(
    
    dashboardPage(
      header = dashboardHeader(title = "Download KPI Data"),
      skin = "blue",
      sidebar = dashboardSidebar(),
      
      # Main panel for displaying outputs ----
      dashboardBody(
            shinydashboard::valueBoxOutput("myvaluebox1"),
            shinydashboard::valueBoxOutput("myvaluebox2"),
            shinydashboard::valueBoxOutput("myvaluebox3"),
            
            column(6, align = "center",
                   box( width=NULL,
                        collapsible = FALSE,
                        solidHeader = TRUE,
                        splitLayout(dateRangeInput(
                                      inputId = "date_selector",
                                      label   = "Select dates",
                                      start   = Sys.Date() - 365,
                                      end     = Sys.Date() - 1
                                    ),
                                    actionButton("goButton", "Submit", class = "btn-success"),
                                    cellArgs = list(style = "vertical-align: top"),
                                    cellWidths = c("80%", "20%")))),
          
        fluidRow(
          box(width = 6, 
              # Input: Choose dataset ----
              selectInput("dataset", "Choose a dataset:",
                          choices = c("Ticket data",  "Counsellor data", "Overall summaries",
                                      "Parish summaries", "Gender summaries")),
              # Button
              downloadButton("downloadData", "Download"))),
        fluidRow(box(width = 12,
                     dataTableOutput("table")))
      )
  )
  )
  
  server <- function(input, output) {

    updated_data <- update_data()
    ticket_data <- updated_data[[1]]
    user_data <- updated_data[[2]]
    
    dates <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      input$date_selector
    })
    
    ticket_data_date_from <- reactive({
      ticket_data_from <- ticket_data %>%
        filter(opened_on >= as.Date(dates()[1]))
      return(ticket_data_from)
    })
    ticket_data_date_to <- reactive({
      ticket_data_to <- ticket_data_date_from() %>%
        filter(closed_on <= as.Date(dates()[2]))
      return(ticket_data_to)
    })
    assignee_data_date_from <- reactive({
      status_by_assignee <- summary_table(data = ticket_data_date_to(), factors = counsellor, columns_to_summarise = c("status"))
      time_by_assignee <- summary_table(data = ticket_data_date_to(),
                                        factors = counsellor,
                                        columns_to_summarise = "time_to_close",
                                        summaries = "mean",
                                        include_margins = FALSE)
      assignee_data <- full_join(status_by_assignee, time_by_assignee)
      assignee_data <- assignee_data %>%
        mutate(`Time to close mean (hrs)` = as.numeric(`Time to close mean`)) %>%
        mutate(`Time to close min (hrs)` = ifelse(`Time to close min` == "Inf", NA, `Time to close min`)) %>%
        mutate(`Time to close max (hrs)` = ifelse(`Time to close max` == "-Inf", NA, `Time to close max`)) %>%
        dplyr::select(-c(`Time to close mean`, `Time to close min`, `Time to close max`))
      return(assignee_data)
    })
    
    ticket_user_data_date <- reactive({
      valid_uuids <- ticket_data_date_to()$uuid
      ticket_user_data <- user_data %>% dplyr::filter(uuid %in% valid_uuids)
      return(ticket_user_data)
    })
    
    summaries_overall <- reactive({
      # Summaries overall --------
      open_overall <- ticket_data_date_to() %>% filter(status == "open") %>% summarise(`Opened` = n())
      closed_overall <- ticket_data_date_to() %>% filter(status == "closed") %>% summarise(`Closed` = n())
      time_overall <- ticket_data_date_to() %>% summarise(`Time to close (mean)` = mean(time_to_close, na.rm = TRUE),
                                                            `Time to close (min)` = min(time_to_close, na.rm = TRUE),
                                                            `Time to close (max)` = max(time_to_close, na.rm = TRUE))
      summaries_overall <- cbind(open_overall, closed_overall, time_overall)
      return(summaries_overall)
    })
    
    t_u_gender <- reactive({
      print(ticket_user_data_date())
      print(ticket_user_data_date()$gender)
      print(summary_table(ticket_user_data_date(),
                          factors = gender))
       return(summary_table(ticket_user_data_date(),
                            factors = gender))
    })
    
    t_u_parish <- reactive({
      summary_table(ticket_user_data_date(),
                    factors = parish)
    })
  
    # Reactive value for selected dataset ----
    datasetInput <- reactive({
      switch(input$dataset,
             "Ticket data" = ticket_data_date_to(),
             "Counsellor data" = assignee_data_date_from(),
             "Overall summaries" = summaries_overall(),
             "Gender summaries" = t_u_gender(),
             "Parish summaries" = t_u_parish())
    })
    
    # Table of selected dataset ----
    output$table <- renderDataTable({
      datasetInput()
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$dataset, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(datasetInput(), file, row.names = FALSE)
      }
    )
  
  output$myvaluebox1 <- shinydashboard::renderValueBox({
    n_open <- nrow(ticket_data_date_from() %>% filter(status == "open"))
    shinydashboard::valueBox(n_open, subtitle = "open", icon = icon("sun"),
                             color = "aqua"
    )
  })
  output$myvaluebox2 <- shinydashboard::renderValueBox({
    n_closed <- nrow(ticket_data_date_from() %>% filter(status == "closed"))
    shinydashboard::valueBox(n_closed, subtitle = "closed",icon = icon("check"),
                             color = "green"
    )
  })
  output$myvaluebox3 <- shinydashboard::renderValueBox({
    mean_ttc <- round(as.numeric(mean(ticket_data_date_from()$time_to_close, na.rm = TRUE)), 1)
    shinydashboard::valueBox(mean_ttc, subtitle = "average hours to close", icon = icon("clock"),
                             color = "yellow"
    )
  })
}
  
  shiny::shinyApp(ui = ui, server = server)
}
