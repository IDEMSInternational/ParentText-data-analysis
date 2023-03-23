# 3. Define UI -----------------------------------------------------------------------------
srh_shiny <- function(country = "Jamaica", study = "SRH"){
  # Define UI
  ui <- dashboardPage(
    header = dashboardHeader(title = paste(country, study, "ParentApp Dashboard")),
    sidebar = dashboardSidebar(
      sidebarMenu(
        menuItem("Demographics", tabName = "demographics", icon = icon("users")),
        menuItem("Engagement", tabName = "engagement", icon = icon("lightbulb")),
        menuItem("Data Library", tabName = "library", icon = icon("book-reader"))
      )), #closes sidebarMenu and dashboardSidebar
    
    dashboardBody(
      fluidRow(
        shinydashboard::valueBoxOutput("myvaluebox1", width=4),
        shinydashboard::valueBoxOutput("myvaluebox5", width=4),
        shinydashboard::valueBoxOutput("myvaluebox2", width=4)),
      #top_boxes(country = country), #closes fluidRow
      #fluidRow(checkbox_input(inputId = "Dem", country = country, study = study)), #closes fluidRow
      tabItems(
        # First tab content layout
        tabItem(tabName = "demographics",
                fluidRow(
                  column(12, align = "centre",
                         box(splitLayout(h2("Overall Demographics"), icon("users", "fa-6x"),
                                         cellArgs = list(style = "vertical-align: top"),
                                         cellWidths = c("80%", "20%")),
                             width = 15,
                             title = NULL,
                             collapsible = FALSE,
                             solidHeader = TRUE,
                             background = "light-blue",
                             height = "95px")
                  ) #closes box
                ), #closes fluid row
                
                # fluidRow(
                #   box(width = 6,
                #       collapsible = TRUE,
                #       solidHeader = TRUE,
                #       title = "Consent",
                #       status = "primary", # primary, success, info, warning, danger
                #       style='width:100%;overflow-x: scroll;',
                #       plotlyOutput(outputId = "plot_consent", height = "240"),
                #       shiny::tableOutput("table_consent")
                #   ), #closes box
                #   box(width = 6,
                #       collapsible = TRUE,
                #       solidHeader = TRUE,
                #       title = "Program",
                #       status = "primary",  
                #       style='width:100%;overflow-x: scroll;',
                #       plotlyOutput(outputId = "plot_program", height = "240"), #generates graph
                #       shiny::tableOutput("table_program")  #generates table
                #   ) #closes box
                # ), #closes fluidRow
                
                fluidRow(
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "Age",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_age", height = "240"),
                      shiny::tableOutput("table_age")
                  ), #closes box
                  
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "Gender",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_gender", height = "240"), #generates graph
                      shiny::tableOutput("table_gender")  #generates table
                  ) #closes box
                ), #closes fluidRow
                
                fluidRow(
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "Avatar",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_avatar", height = "240"),
                      shiny::tableOutput("table_avatar")
                  ), #closes box
                  
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "URN",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_urn", height = "240"), #generates graph
                      shiny::tableOutput("table_urn")  #generates table
                  ) #closes box
                ) #closes fluidRow
        ), #closes first tabItem
        # Engagement
        tabItem(tabName = "engagement",
                fluidRow(
                  column(12, align = "centre",
                         box(splitLayout(h2("Engagement"), icon("lightbulb", "fa-6x"),
                                         cellArgs = list(style = "vertical-align: top"),
                                         cellWidths = c("80%", "20%")),
                             width = 15,
                             title = NULL,
                             collapsible = FALSE,
                             solidHeader = TRUE,
                             background = "light-blue",
                             height = "95px")
                  ) #closes box
                ), #closes fluid row
                
                tabsetPanel(type = "tabs",
                            tabPanel("Overview",
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Menstruation",
                                           status = "primary", # info, success, info, warning, danger
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_Menstruation", height = "240"),
                                           shiny::tableOutput("table_Menstruation")
                                       ), #closes box
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Pregnancy",
                                           status = "primary",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_Pregnancy", height = "240"), #generates graph
                                           shiny::tableOutput("table_Pregnancy")  #generates table
                                       ) #closes box
                                     ), #closes fluidRow
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Parent age",
                                           status = "primary",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_Puberty", height = "240"),
                                           shiny::tableOutput("table_Puberty")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Parent gender",
                                           status = "primary",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_STIs", height = "240"), #generates graph
                                           shiny::tableOutput("table_STIs")  #generates table
                                       ) #closes box
                                     ), #closes fluidRow
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Gender",
                                           status = "primary",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_Gender", height = "240"),
                                           shiny::tableOutput("table_Gender")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Sexuality",
                                           status = "primary",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_Sexuality", height = "240"), #generates graph
                                           shiny::tableOutput("table_Sexuality")  #generates table
                                       ) #closes box
                                     ), #closes fluidRow
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Abstinence",
                                           status = "primary",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_Abstinence", height = "240"),
                                           shiny::tableOutput("table_Abstinence")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Mental Health",
                                           status = "primary",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_MH", height = "240"), #generates graph
                                           shiny::tableOutput("table_MH")  #generates table
                                       ) #closes box
                                     ), #closes fluidRow
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Violence & Abuse",
                                           status = "primary",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_Violence", height = "240"),
                                           shiny::tableOutput("table_Violence")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Healthy Relationships",
                                           status = "primary",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_Healthy", height = "240"), #generates graph
                                           shiny::tableOutput("table_Healthy")  #generates table
                                       ) #closes box
                                     ), #closes fluidRow
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Parenting",
                                           status = "primary",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_Parenting", height = "240"),
                                           shiny::tableOutput("table_Parenting")
                                       ) #closes box
                                     ) #closes fluidRow
                            ),
                            tabPanel("By UUID",
                              fluidRow(
                                box(width = 12,
                                    collapsible = TRUE,
                                    solidHeader = TRUE,
                                    title = "Accessed by UUID",
                                    status = "primary",  
                                    style='width:100%;overflow-x: scroll;',
                                    plotlyOutput(outputId = "plot_by_uuid", height = "240"),
                                    shiny::tableOutput("table_by_uuid")
                                ) #closes box
                              ) #closes fluidRow
                            )) # closes tabPanel, tabsetPanel
        ), #closes second tabItem
        tabItem(tabName = "library",
                fluidRow(
                  column(12, align = "centre",
                         box(splitLayout(h2("Data Library"), icon("book-reader", "fa-6x"),
                                         cellArgs = list(style = "vertical-align: top"),
                                         cellWidths = c("80%", "20%")),
                             width = 15,
                             title = NULL,
                             collapsible = FALSE,
                             solidHeader = TRUE,
                             background = "light-blue",
                             height = "95px")
                  ) #closes box
                ),
                fluidRow(
                  box(width = 6, 
                      # Input: Choose dataset ----
                      selectInput("dataset", "Choose a dataset:",
                                  choices = c("Demographics Data",  "Menstruation", "Pregnancy", "Puberty", "STIs",
                                              "Gender", "Sexuality", "Abstinence", "Mental Health", "Violence & Abuse",
                                              "Healthy Relationships", "Parenting")),
                      # Button
                      downloadButton("downloadData", "Download")),
                  fluidRow(box(width = 12,
                               dataTableOutput("table")))
                )
        ) # closes third tabitem
      ) # closes tabItems
    ), # closes dashboardBody
  )# closes dashboardPage
  
  # 4. Define Server -----------------------------------------------------------------------------
  server <- function(input, output) {
    # General Set Up ---------------------------------------------------
    #autoRefresh <- reactiveTimer(6 * 60 * 60 * 1000)
    srh_bank <- update_data(consent_only = FALSE)
    srh_df <- data.frame(srh_bank[[1]])
    srh_flow_df <- srh_bank[[2]]
    srh_flow_freq_df <- srh_bank[[3]]
    srh_by_uuid <- srh_bank[[4]]
    
    #SUMMARY STATS HEADER displays (same for all tabs)
    output$myvaluebox1 <- shinydashboard::renderValueBox({
      df_active_24 <- nrow(srh_df %>% filter(active_users_24_hrs == "Yes"))
      shinydashboard::valueBox(df_active_24, subtitle = "Active in last 24 hours",icon = icon("clock"),
                               color = "green"
      )
    })
    output$myvaluebox5 <- shinydashboard::renderValueBox({
      df_active_7d <- nrow(srh_df %>% filter(active_users_7_days == "Yes"))
      shinydashboard::valueBox(df_active_7d, subtitle = "Active in last 7 days", icon = icon("signal"),
                               color = "yellow"
      )
    })
    output$myvaluebox2 <- shinydashboard::renderValueBox({
      df_count <- nrow(srh_df)
      shinydashboard::valueBox(df_count, subtitle = "users", icon = icon("signal"),
                               color = "green"
      )
    })
    # Demographics Tab ---------------------------------------------------
    # summary_table_baseline <- reactive({
    #   summary_table_demographics <- summary_table_base_build(data = srh_df, )
    #   #summary_table_baseline_build <- summary_table_base_build(opt_factors = opt_factors(), data = selected_data_dem(), columns_to_summarise = data_baseline_survey)
    #   #mult_summary_table_filter(summary_table_baseline_build)
    # })
    
    # #Consent
    # table_consent <- reactive({
    #   srh_df %>%
    #     group_by(true_consent, .drop = FALSE) %>%
    #     summarise(n = n())
    # }) 
    # plot_consent  <- reactive({
    #   summary_plot(data = srh_df, columns_to_summarise = "true_consent", plot_type = "histogram")
    # }) 
    # output$table_consent <- shiny::renderTable({(table_consent())}, striped = TRUE)
    # output$plot_consent <- renderPlotly({plot_consent()})
    # 
    # #Programme
    # table_program <- reactive({
    #   srh_df %>%
    #     group_by(program, .drop = FALSE) %>%
    #     summarise(n = n())
    # }) 
    # plot_program  <- reactive({
    #   summary_plot(data = srh_df, columns_to_summarise = "program", plot_type = "histogram")
    # }) 
    # output$table_program <- shiny::renderTable({(table_program())}, striped = TRUE)
    # output$plot_program <- renderPlotly({plot_program()})
    
    # gender plot and tabled
    table_gender <- reactive({ summary_table(data = srh_df, factors = gender) }) 
    plot_gender  <- reactive({
      summary_plot(data = srh_df, columns_to_summarise = "gender", plot_type = "histogram")
    }) 
    output$table_gender <- shiny::renderTable({(table_gender())}, striped = TRUE)
    output$plot_gender <- renderPlotly({plot_gender()})
    
    # age plot and table
    table_age <- reactive({ summary_table(data = srh_df, factors = age) }) 
    plot_age  <- reactive({
      summary_plot(data = srh_df, columns_to_summarise = "age", plot_type = "histogram")
    }) 
    output$table_age <- shiny::renderTable({(table_age())}, striped = TRUE)
    output$plot_age <- renderPlotly({plot_age()})
    
    # gender plot and tabled
    table_avatar <- reactive({ summary_table(data = srh_df, factors = avatar) }) 
    plot_avatar  <- reactive({
      summary_plot(data = srh_df, columns_to_summarise = "avatar", plot_type = "histogram")
    }) 
    output$table_avatar <- shiny::renderTable({(table_avatar())}, striped = TRUE)
    output$plot_avatar <- renderPlotly({plot_avatar()})
    
    # gender plot and tabled
    table_urn <- reactive({ summary_table(data = srh_df, factors = urn) }) 
    plot_urn  <- reactive({
      summary_plot(data = srh_df, columns_to_summarise = "urn", plot_type = "histogram")
    }) 
    output$table_urn <- shiny::renderTable({(table_urn())}, striped = TRUE)
    output$plot_urn <- renderPlotly({plot_urn()})
    
    # Engagement Tab ---------------------------------------------------
    table_Menstruation <- reactive({ srh_flow_freq_df$`SRH - Answer - Menstruation` }) 
    plot_Menstruation  <- reactive({
      summary_plot(data = srh_flow_df$`SRH - Answer - Menstruation`, columns_to_summarise = ".id",
                   replace = "SRH - Answer - ", plot_type = "histogram") + labs(x = "Answer")
    }) 
    output$table_Menstruation <- shiny::renderTable({(table_Menstruation())}, striped = TRUE)
    output$plot_Menstruation <- renderPlotly({plot_Menstruation()})
    
    table_Menstruation <- reactive({ srh_flow_freq_df$`SRH - Answer - Menstruation` }) 
    plot_Menstruation  <- reactive({
      summary_plot(data = srh_flow_df$`SRH - Answer - Menstruation`, columns_to_summarise = ".id",
                   replace = "SRH - Answer - ", plot_type = "histogram") + labs(x = "Answer")
    }) 
    output$table_Menstruation <- shiny::renderTable({(table_Menstruation())}, striped = TRUE)
    output$plot_Menstruation <- renderPlotly({plot_Menstruation()})
    
    table_Pregnancy <- reactive({ srh_flow_freq_df$`SRH - Answer - Pregnancy` }) 
    plot_Pregnancy  <- reactive({
      summary_plot(data = srh_flow_df$`SRH - Answer - Pregnancy`, columns_to_summarise = ".id",
                   replace = "SRH - Answer - ", plot_type = "histogram") + labs(x = "Answer")
    }) 
    output$table_Pregnancy <- shiny::renderTable({(table_Pregnancy())}, striped = TRUE)
    output$plot_Pregnancy <- renderPlotly({plot_Pregnancy()})
    
    table_Puberty <- reactive({ srh_flow_freq_df$`SRH - Answer - Puberty` }) 
    plot_Puberty  <- reactive({
      summary_plot(data = srh_flow_df$`SRH - Answer - Puberty`, columns_to_summarise = ".id",
                   replace = "SRH - Answer - ", plot_type = "histogram") + labs(x = "Answer")
    }) 
    output$table_Puberty <- shiny::renderTable({(table_Puberty())}, striped = TRUE)
    output$plot_Puberty <- renderPlotly({plot_Puberty()})
    
    table_STIs <- reactive({ srh_flow_freq_df$`SRH - Answer - STIs` }) 
    plot_STIs  <- reactive({
      summary_plot(data = srh_flow_df$`SRH - Answer - STIs`, columns_to_summarise = ".id",
                   replace = "SRH - Answer - ", plot_type = "histogram") + labs(x = "Answer")
    }) 
    output$table_STIs <- shiny::renderTable({(table_STIs())}, striped = TRUE)
    output$plot_STIs <- renderPlotly({plot_STIs()})
    
    table_Gender <- reactive({ srh_flow_freq_df$`SRH - Answer - Gender` }) 
    plot_Gender  <- reactive({
      summary_plot(data = srh_flow_df$`SRH - Answer - Gender`, columns_to_summarise = ".id",
                   replace = "SRH - Answer - ", plot_type = "histogram") + labs(x = "Answer")
    }) 
    output$table_Gender <- shiny::renderTable({(table_Gender())}, striped = TRUE)
    output$plot_Gender <- renderPlotly({plot_Gender()})
    
    table_Sexuality <- reactive({ srh_flow_freq_df$`SRH - Answer - Sexuality` }) 
    plot_Sexuality  <- reactive({
      summary_plot(data = srh_flow_df$`SRH - Answer - Sexuality`, columns_to_summarise = ".id",
                   replace = "SRH - Answer - ", plot_type = "histogram") + labs(x = "Answer")
    }) 
    output$table_Sexuality <- shiny::renderTable({(table_Sexuality())}, striped = TRUE)
    output$plot_Sexuality <- renderPlotly({plot_Sexuality()})
    
    table_Abstinence <- reactive({ srh_flow_freq_df$`SRH - Answer - Abstinence` }) 
    plot_Abstinence  <- reactive({
      summary_plot(data = srh_flow_df$`SRH - Answer - Abstinence`, columns_to_summarise = ".id",
                   replace = "SRH - Answer - ", plot_type = "histogram") + labs(x = "Answer")
    }) 
    output$table_Abstinence <- shiny::renderTable({(table_Abstinence())}, striped = TRUE)
    output$plot_Abstinence <- renderPlotly({plot_Abstinence()})
    
    table_MH <- reactive({ srh_flow_freq_df$`SRH - Answer - Mental Health` }) 
    plot_MH  <- reactive({
      summary_plot(data = srh_flow_df$`SRH - Answer - Mental Health`, columns_to_summarise = ".id",
                   replace = "SRH - Answer - ", plot_type = "histogram") + labs(x = "Answer")
    }) 
    output$table_MH <- shiny::renderTable({(table_MH())}, striped = TRUE)
    output$plot_MH <- renderPlotly({plot_MH()})
    
    table_Violence <- reactive({ srh_flow_freq_df$`SRH - Answer - Violence & Abuse` }) 
    plot_Violence  <- reactive({
      summary_plot(data = srh_flow_df$`SRH - Answer - Violence & Abuse`, columns_to_summarise = ".id",
                   replace = "SRH - Answer - ", plot_type = "histogram") + labs(x = "Answer")
    }) 
    output$table_Violence <- shiny::renderTable({(table_Violence())}, striped = TRUE)
    output$plot_Violence <- renderPlotly({plot_Violence()})
    
    table_Healthy <- reactive({ srh_flow_freq_df$`SRH - Answer - Healthy Relationships` }) 
    plot_Healthy  <- reactive({
      summary_plot(data = srh_flow_df$`SRH - Answer - Healthy Relationships`, columns_to_summarise = ".id",
                   replace = "SRH - Answer - ", plot_type = "histogram") + labs(x = "Answer")
    }) 
    output$table_Healthy <- shiny::renderTable({(table_Healthy())}, striped = TRUE)
    output$plot_Healthy <- renderPlotly({plot_Healthy()})
    
    table_Parenting <- reactive({ srh_flow_freq_df$`SRH - Answer - Parenting` }) 
    plot_Parenting  <- reactive({
      summary_plot(data = srh_flow_df$`SRH - Answer - Parenting`, columns_to_summarise = ".id",
                   replace = "SRH - Answer - ", plot_type = "histogram") + labs(x = "Answer")
    }) 
    output$table_Parenting <- shiny::renderTable({(table_Parenting())}, striped = TRUE)
    output$plot_Parenting <- renderPlotly({plot_Parenting()})
    
    table_by_uuid <- reactive({ srh_by_uuid }) 
    plot_by_uuid  <- reactive({
      srh_by_uuid_long <- pivot_longer(srh_by_uuid, cols = !uuid)
      srh_by_uuid_long <- srh_by_uuid_long %>%
        filter(name != "Total") %>%
        filter(uuid != "Total")
      ggplot(srh_by_uuid_long, aes(x = name, y = value, colour = factor(uuid), group = factor(uuid))) +
        geom_point() +
        geom_line() +
        viridis::scale_colour_viridis(discrete = TRUE) +
        theme(legend.position = "none") +
        labs(x = "Category", y = "Count")
    }) 
    output$table_by_uuid <- shiny::renderTable({(table_by_uuid())}, striped = TRUE)
    output$plot_by_uuid <- renderPlotly({plot_by_uuid()})
    
    # Data library
    # Reactive value for selected dataset ----
    datasetInput <- reactive({
      switch(input$dataset,
             "Demographics Data" = srh_df,
             "Menstruation" = srh_flow_df$`SRH - Answer - Menstruation`,
             "Pregnancy" = srh_flow_df$`SRH - Answer - Pregnancy`,
             "Puberty" = srh_flow_df$`SRH - Answer - Puberty`,
             "STIs" = srh_flow_df$`SRH - Answer - STIs`,
             "Gender" = srh_flow_df$`SRH - Answer - Gender`,
             "Sexuality" = srh_flow_df$`SRH - Answer - Sexuality`,
             "Abstinence" = srh_flow_df$`SRH - Answer - Abstinence`,
             "Mental Health" = srh_flow_df$`SRH - Answer - Mental Health`,
             "Violence & Abuse" = srh_flow_df$`SRH - Answer - Violence & Abuse`,
             "Healthy Relationships" = srh_flow_df$`SRH - Answer - Healthy Relationships`,
             "Parenting" = srh_flow_df$`SRH - Answer - Parenting`)
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
  } #close server
  shinyApp(ui = ui, server = server)
} # close function
