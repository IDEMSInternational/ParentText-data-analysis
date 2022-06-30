library(shiny)
library(shinythemes)
library(shinyjs)
#library(rpivotTable)
library(plotly)
library(shinydashboard)

demographics_top_box <- function(country){
  consent_box <- box(width=NULL,
                     collapsible = FALSE,
                     title = "Consent Frequency",
                     status = status, # primary, success, info, warning, danger
                     solidHeader = TRUE,
                     plotlyOutput(outputId = "plot_consent", height = "240"),
                     shiny::tableOutput("consent_table"))
  language_box <- box(width=NULL,
                      title = "Language",
                      status = status,
                      solidHeader = TRUE,
                      collapsible = FALSE,
                      plotlyOutput(outputId = "plot_language", height = "240"),
                      shiny::tableOutput("language_table"))
  state_box <- box(width=NULL,
                   title = paste(state_title, "the parent is from"),
                   status = status,
                   solidHeader = TRUE,
                   collapsible = FALSE,
                   plotlyOutput(outputId = "plot_state", height = "240"),
                   shiny::tableOutput("state_table"))
  if (country == "Jamaica"){
    splitLayout(
      consent_box,
      state_box,
      cellWidths = c("50%", "50%"),
      cellArgs = list(style = "vertical-align: top")
    )
  } else {
    splitLayout(
      consent_box,
      language_box,
      state_box,
      cellWidths = c("33.3%", "33.3%", "33.3%"),
      cellArgs = list(style = "vertical-align: top")
    )
  }
}

parenttext_shiny <- function(country, date_from = NULL, date_to = NULL, include_archived_data = FALSE){
  # Define UI
  ui <- dashboardPage(
    header = dashboardHeader(title = paste(country, pt_name, "Dashboard")),
    
    skin = skin,
    
    sidebar = dashboardSidebar(
      if (country == "Jamaica"){
        sidebarMenu(
          menuItem("Demographics", tabName = "demographics", icon = icon("users")),
          menuItem("Women's Centre", tabName = "womenscentre", icon = icon("venus")),
          menuItem("Engagement", tabName = "engagement", icon = icon("clipboard")),
          menuItem("Behaviours", tabName = "behaviours", icon = icon("brain"))
        )
      } else {
        sidebarMenu(
          menuItem("Demographics", tabName = "demographics", icon = icon("users")),
          menuItem("Engagement", tabName = "engagement", icon = icon("clipboard")),
          menuItem("Behaviours", tabName = "behaviours", icon = icon("brain"))
        )
      }
    ),
    
    dashboardBody(
      fluidRow(
        shinydashboard::valueBoxOutput("myvaluebox1", width=2),
        shinydashboard::valueBoxOutput("myvaluebox2", width=2),
        shinydashboard::valueBoxOutput("myvaluebox3", width=2),
        shinydashboard::valueBoxOutput("myvaluebox4", width=2),
        shinydashboard::valueBoxOutput("myvaluebox5", width=2),
        shinydashboard::valueBoxOutput("myvaluebox6", width=2)),
    
      column(6, align = "center",
             box( width=NULL,
                  collapsible = FALSE,
                  solidHeader = TRUE,
                  splitLayout(textInput(inputId = "datefrom_text", label = "Date from:", value = date_from),
                              cellArgs = list(style = "vertical-align: top"),
                              cellWidths = c("80%", "20%")))),
      #,
      #            splitLayout(textInput(inputId = "dateto_text", label = "Date to:", value = ""),
      #                        actionButton("btn_dates", "Change Dates", class="btn-success"),
      #                        cellArgs = list(style = "vertical-align: top"),
      #                        cellWidths = c("80%", "20%")))),
      
      tabItems(
        # First tab content
        tabItem(tabName = "demographics",
                fluidRow(
                  column(12, align = "center",
                         box(splitLayout(h2("Demographics"), icon("users", "fa-6x"),
                                         cellArgs = list(style = "vertical-align: top"),
                                         cellWidths = c("80%", "20%")),
                             status = status,
                             background = background,
                             width = 10,
                             title = NULL,
                             collapsible = FALSE,
                             solidHeader = TRUE,
                             height = "95px"))),
                tabsetPanel(type = "tabs",
                            tabPanel("Overall",
                                     fluidRow(column(12, align = "center",
                                                     demographics_top_box(country = country)
                                                     )),
                                     fluidRow(column(12, align = "center",
                                                     splitLayout(
                                                       box(width=NULL,
                                                           collapsible = FALSE,
                                                           title = "Parent Demographics",
                                                           status = status, # primary, success, info, warning, danger
                                                           solidHeader = TRUE,
                                                           shiny::tableOutput("parent_gender_table"),
                                                           shiny::tableOutput("parent_age_table"),
                                                           shiny::tableOutput("parent_child_relationship_table"),
                                                           shiny::tableOutput("parent_relationship_table")),
                                                       box(width=NULL,
                                                           collapsible = FALSE,
                                                           title = "Child Demographics",
                                                           status = status, # primary, success, info, warning, danger
                                                           solidHeader = TRUE,
                                                           shiny::tableOutput("child_gender_table"),
                                                           shiny::tableOutput("child_age_table"),
                                                           shiny::tableOutput("child_disabilities_table")), # close child box
                                                       cellWidths = c("50%", "50%"),
                                                       cellArgs = list(style = "vertical-align: top"))), width = 10), # fluid row close
                                     fluidRow(column(12, align = "center",
                                                     box(height="300px",
                                                         width=12,
                                                         collapsible = FALSE,
                                                         title = "Parenting Goals",
                                                         status = status, # primary, success, info, warning, danger
                                                         solidHeader = TRUE,
                                                         plotlyOutput(outputId = "parenting_goals_plot", height = "240", width = "100%")),
                                                     box(height="300px",  width=12,
                                                         collapsible = FALSE,
                                                         title = "Recruitment Channel",
                                                         status = status, # primary, success, info, warning, danger
                                                         solidHeader = TRUE,
                                                         plotlyOutput(outputId = "recruitment_channel_plot", height = "240", width = "100%")))) # close fluid row
                            ), # close "Overall" tab 
                            tabPanel("By group",
                                     fluidRow(
                                       column(12, align = "center",
                                              box(width = NULL,
                                                  collapsible = FALSE,
                                                  solidHeader = TRUE,
                                                  fluidRow(column(6, uiOutput("groups"))),
                                              splitLayout(
                                                box(width=NULL,
                                                    collapsible = FALSE,
                                                    title = "Consent Frequency",
                                                    status = status, # primary, success, info, warning, danger
                                                    solidHeader = TRUE,
                                                    plotlyOutput(outputId = "plot_consent_group", height = "240"),
                                                    shiny::tableOutput("consent_table_group")),
                                                box(width=NULL,
                                                    title = "Language",
                                                    status = status,
                                                    solidHeader = TRUE,
                                                    collapsible = FALSE,
                                                    plotlyOutput(outputId = "plot_language_group", height = "240", width = "100%"),
                                                    shiny::tableOutput("language_table_group")),
                                                box(width=NULL,
                                                    title = paste(state_title, "the parent is from"),
                                                    status = status,
                                                    solidHeader = TRUE,
                                                    collapsible = FALSE,
                                                    plotlyOutput(outputId = "plot_state_group", height = "240", width = "100%"),
                                                    shiny::tableOutput("state_table_group")),
                                                cellWidths = c("33.3%", "33.3%", "33.3%"),
                                                cellArgs = list(style = "vertical-align: top"))))),
                                     fluidRow(column(12, align = "center",
                                                     splitLayout(
                                                       box(width=NULL,
                                                           collapsible = FALSE,
                                                           title = "Parent Demographics",
                                                           status = status, # primary, success, info, warning, danger
                                                           solidHeader = TRUE,
                                                           shiny::tableOutput("parent_gender_group_table"),
                                                           shiny::tableOutput("parent_age_group_table"),
                                                           shiny::tableOutput("parent_child_relationship_group_table"),
                                                           shiny::tableOutput("parent_relationship_group_table"),
                                                       ),
                                                       box(width=NULL,
                                                           collapsible = FALSE,
                                                           title = "Child Demographics",
                                                           status = status, # primary, success, info, warning, danger
                                                           solidHeader = TRUE,
                                                           shiny::tableOutput("child_gender_group_table"),
                                                           shiny::tableOutput("child_age_group_table"),
                                                           shiny::tableOutput("child_disabilities_group_table"),
                                                       ), # close child box
                                                       cellWidths = c("50%", "50%"),
                                                       cellArgs = list(style = "vertical-align: top")), # split layout for parent to child demographics close
                                                     box(height="300px",  width=12,
                                                         collapsible = FALSE,
                                                         title = "Parenting Goals",
                                                         status = status, # primary, success, info, warning, danger
                                                         solidHeader = TRUE,
                                                         plotlyOutput(outputId = "parenting_goals_group_plot", height = "240", width = "100%")),
                                                     box(height="300px",  width=12,
                                                         collapsible = FALSE,
                                                         title = "Recruitment Channel",
                                                         status = status, # primary, success, info, warning, danger
                                                         solidHeader = TRUE,
                                                         plotlyOutput(outputId = "recruitment_channel_group_plot", height = "240", width = "100%"))
                                     )) # close col, fr
                            ) # close by group panel
                ) # close tab panel       
        ),
        # Second tab content
        tabItem(tabName = "engagement",
                fluidRow(
                  column(12, align = "center",
                         box(splitLayout(h2("Engagement"), icon("clipboard", "fa-6x"),
                                         cellArgs = list(style = "vertical-align: top"),
                                         cellWidths = c("80%", "20%")),
                             width = 10,
                             status = status,
                             title = NULL,
                             collapsible = FALSE,
                             solidHeader = TRUE,
                             background = background,
                             height = "95px"))),
                tabsetPanel(type = "tabs",
                            tabPanel("Overall",
                                     fluidRow(column(12, align = "center",
                                                     box(width = NULL,
                                                         collapsible = FALSE,
                                                         solidHeader = TRUE,
                                                         splitLayout(
                                                           shiny::tableOutput("active_users_table"),
                                                           shiny::tableOutput("active_users_7_days_table"),
                                                           shiny::tableOutput("not_active_users_7_days_table"),
                                                           cellArgs = list(style = "vertical-align: top")),
                                                         br(),
                                                         plotlyOutput(outputId = "last_online_plot"),
                                                         br(),
                                                         shiny::tableOutput("comp_prog_table"),
                                                         shiny::tableOutput("completed_welcome_table"),
                                                         shiny::tableOutput("completed_survey_table"),
                                                         shiny::tableOutput("consented_survey_table"),
                                                         shiny::tableOutput("all_flows_response")), # close box
                                                     
                                                     box(height="300px",
                                                         width=12,
                                                         collapsible = FALSE,
                                                         title = "Plot of Flows",
                                                         status = status, # primary, success, info, warning, danger
                                                         solidHeader = TRUE,
                                                         plotlyOutput(outputId = "plot_flow", height = "240", width = "100%"))
                                     )) # close col, fr
                            ), # close Overall tab
                            tabPanel("By group",
                                     fluidRow(
                                       column(12, align = "center",
                                              box(width = NULL,
                                                  collapsible = FALSE,
                                                  solidHeader = TRUE,
                                                  fluidRow(column(6, uiOutput("groups_engagement"))),
                                              box(width = NULL,
                                                  collapsible = FALSE,
                                                  solidHeader = TRUE,
                                                  splitLayout(
                                                    shiny::tableOutput("active_users_group_table"),
                                                    shiny::tableOutput("active_users_7_days_group_table"),
                                                    shiny::tableOutput("not_active_users_7_days_group_table"),
                                                    cellArgs = list(style = "vertical-align: top")), # split layout close
                                                  br(),
                                                  plotlyOutput(outputId = "last_online_group_plot"),
                                                  br(),
                                                  shiny::tableOutput("completed_welcome_group_table"),
                                                  shiny::tableOutput("completed_survey_group_table"),
                                                  shiny::tableOutput("consented_survey_group_table")),
                                       shinydashboard::valueBoxOutput("comp_prog_group_table", width = 12),
                                       cellArgs = list(style = "vertical-align: top")
                                     )))) # close col, fr, by group tab
                ) # close tab type
        ), # close engagement tab
        
        # tabItem(tabName = "parentpals",
        #         fluidRow(
        #           column(10, align = "center",
        #                  box(splitLayout(h2("Parent Pals"), icon("user", "fa-6x"),
        #                                  cellArgs = list(style = "vertical-align:top"),
        #                                  cellWidths = c("80%", "20%")),
        #                      width = 10,
        #                      status = status,
        #                      title = NULL,
        #                      collapsible = FALSE,
        #                      solidHeader = TRUE,
        #                      background = background,
        #                      height = "95px"))),
        #         fluidRow(
        #           box(dataTableOutput('pp_table'),
        #               width = 10,
        #               title = NULL,
        #               collapsible = FALSE,
        #               solidHeader = TRUE
        #           )
        #         )
        # ),
        
        tabItem(tabName = "womenscentre",
                fluidRow(
                  column(10, align = "center",
                         box(splitLayout(h2("Women's Centre"), icon("venus", "fa-6x"),
                                         cellArgs = list(style = "vertical-align:top"),
                                         cellWidths = c("80%", "20%")),
                             width = 10,
                             status = status,
                             title = NULL,
                             collapsible = FALSE,
                             solidHeader = TRUE,
                             background = background,
                             height = "95px"))),
                
                fluidRow(box(width=NULL,
                             title = "Centre Location",
                             status = status,
                             solidHeader = TRUE,
                             collapsible = FALSE,
                             plotlyOutput(outputId = "womens_centre_plot", height = "240", width = "100%"),
                             shiny::tableOutput("womens_centre_table")))
                
                #fluidRow(
                #  box(datatableoutput('pp_table'),
                #      width = 10,
                #      title = null,
                #      collapsible = false,
                #      solidheader = true
                #  )
                #)
        ),
        tabItem(tabName = "behaviours",
                fluidRow(
                  column(10, align = "center",
                         box(splitLayout(h2("Behaviours"), icon("brain", "fa-6x"),
                                         cellArgs = list(style = "vertical-align: top"),
                                         cellWidths = c("80%", "20%")),
                             width = 10,
                             title = NULL,
                             collapsible = FALSE,
                             solidHeader = TRUE,
                             background = background,
                             height = "95px"))),
                tabsetPanel(type = "tabs",
                            tabPanel("Overall",
                                     fluidRow(column(12, align = "center",
                                                     box(width = NULL,
                                                         collapsible = FALSE,
                                                         solidHeader = TRUE,
                                                         fluidRow(column(6, uiOutput("groups_survey"))),
                                                         shiny::tableOutput("parenting_survey_table"),
                                                         plotlyOutput(outputId = "parenting_survey_plot"),#, height = "580px")
                                                         plotlyOutput(outputId = "behaviour_plots")
                                                     ), # close box
                                                     br(),
                                                     box(width=12,
                                                         collapsible = FALSE,
                                                         title = "Behaviour Problems",
                                                         status = status, # primary, success, info, warning, danger
                                                         solidHeader = TRUE,
                                                         splitLayout(
                                                           plotlyOutput("behaviour_baby_plot"),
                                                           plotlyOutput("behaviour_child_plot"),
                                                           cellWidths = c("50%", "50%"),
                                                           cellArgs = list(style = "vertical-align: top")),
                                                         splitLayout(
                                                           plotlyOutput("behaviour_teen_plot"),
                                                           plotlyOutput("behaviour_default_plot"),
                                                           cellWidths = c("50%", "50%"),
                                                           cellArgs = list(style = "vertical-align: top"))
                                                     ) # close box
                                     )) # close col, fluid row
                            ), # close overall tab
                            tabPanel("By group",
                                     fluidRow(
                                       column(12, align = "center",
                                              box(width = NULL,
                                                  collapsible = FALSE,
                                                  solidHeader = TRUE,
                                                  fluidRow(column(6, uiOutput("groups_behaviour"))),
                                                  plotlyOutput(outputId = "behaviour_group_plots")),
                                              box(width=12,
                                                  collapsible = FALSE,
                                                  title = "Behaviour Problems",
                                                  status = status, # primary, success, info, warning, danger
                                                  solidHeader = TRUE,
                                                  splitLayout(
                                                    plotlyOutput("behaviour_baby_group_plot"),
                                                    plotlyOutput("behaviour_child_group_plot"),
                                                    cellWidths = c("50%", "50%"),
                                                    cellArgs = list(style = "vertical-align: top")),
                                                  footer = splitLayout(
                                                    plotlyOutput(outputId = "behaviour_teen_group_plot"),
                                                    plotlyOutput(outputId = "behaviour_default_group_plot"),
                                                    cellWidths = c("50%", "50%"),
                                                    cellArgs = list(style = "vertical-align: top")) # close footer
                                              ) # close box
                                       ))) # col, fr, tabpanel
                ) # close tabs
        ) # close behaviour tab
      ) # close items
    ) # close body
  ) # close function
  
  # Define server function
  server <- function(input, output) {
    autoRefresh <- reactiveTimer(6 * 60 * 60 * 1000)
    
    observe({
      autoRefresh()
      updated_data <- update_data(country = country, date_to = date_to, include_archived_data = include_archived_data)
      df <- updated_data[[1]]
      df_consent <- updated_data[[2]]
      all_flows <- updated_data[[3]]
      parenting_survey <- updated_data[[4]]
      womens_centre_data <- updated_data[[5]]
    })
    
    updated_data <- update_data(country = country, date_to = date_to, include_archived_data = include_archived_data)
    df <- updated_data[[1]]
    df_consent <- updated_data[[2]]
    all_flows <- updated_data[[3]]
    parenting_survey <- updated_data[[4]]
    womens_centre_data <- updated_data[[5]]
    #pp_data_frame <- updated_data[[5]]
    
    # Subset data
    selected_data <- reactive({
      df
    })
    
    selected_data_date_from <- reactive({
      df <- df %>%
        filter(created_on >= as.Date(input$datefrom_text))
      return(df)
    })
    
    selected_consented_data_date_from <- reactive({
      df_consent <- df_consent %>%
        filter(created_on >= as.Date(input$datefrom_text))
      return(df_consent)
    })
    
    selected_survey_data_date_from <- reactive({
      parenting_survey <- parenting_survey %>%
        filter(created_on >= as.Date(input$datefrom_text))
      return(parenting_survey)
    })
    
    selected_womens_centre_date_from <- reactive({
      womens_centre_data <- womens_centre_data %>%
        filter(created_on >= as.Date(input$datefrom_text))
      return(womens_centre_data)
    })

    output$groups <- renderUI({
      df <- selected_data_date_from()
      selectInput(
        inputId = "grouper",
        label = "Group variable",
        choices = c("Parent Gender" = "parent_gender",
                    "Child Gender" = "child_gender",
                    "Child Age Group" = "child_age_group"),
        selected = "parent_gender"
      )
    })
    
    output$groups_engagement <- renderUI({
      df <- selected_data_date_from()
      selectInput(
        inputId = "grouper_engagement",
        label = "Group variable",
        choices = c("Parent Gender" = "parent_gender",
                    "Child Gender" = "child_gender",
                    "Child Age Group" = "child_age_group"),
        selected = "parent_gender"
      )
    })
    
    output$groups_behaviour <- renderUI({
      df <- selected_data_date_from()
      selectInput(
        inputId = "grouper_behaviour",
        label = "Group variable",
        choices = c("Parent Gender" = "parent_gender",
                    "Child Gender" = "child_gender",
                    "Child Age Group" = "child_age_group"),
        selected = "parent_gender"
      )
    })
    
    output$groups_survey <- renderUI({
      df <- selected_data_date_from()
      selectInput(
        inputId = "grouper_survey",
        label = "Survey to View",
        choices = c("Baseline" = "Baseline",
                    "Survey 2" = "2",
                    "Survey 3" = "3",
                    "Survey 4" = "4",
                    "Survey 5" = "5",
                    "Survey 6" = "6",
                    "Survey 7" = "7",
                    "Survey 8" = "8",
                    "Survey 9" = "9"),
        selected = "Baseline"
      )
    })
    
    observeEvent(input$groupby, {
      if(input$groupby){
        shinyjs::enable("grouper")
      }else{
        shinyjs::disable("grouper")
      }
    })
    
    language_table <- reactive({
      summary_table(data = selected_data_date_from(), factors = language, include_margins = TRUE, replace = NULL)
    })
    
    language_table_group <- reactive({
      summary_table(data = selected_data_date_from(), factors = c(language), columns_to_summarise = (!!!rlang::syms(input$grouper)), include_margins = TRUE, wider_table = TRUE, replace = NULL, together = FALSE, naming_convention = TRUE)
    })
    
    state_table <- reactive({
      summary_table(data = selected_data_date_from(), factors = state_of_origin, include_margins = TRUE, replace = NULL)
    })
    
    state_table_group <- reactive({
      summary_table(data = selected_data_date_from(), factors = c(state_of_origin), columns_to_summarise = (!!!rlang::syms(input$grouper)), include_margins = TRUE, wider_table = TRUE, replace = NULL, together = FALSE, naming_convention = TRUE)
    })
    
    output$plot_language <- renderPlotly({
      ggplot(selected_data_date_from(), aes(x = language)) +
        geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        labs(x = "language", y = "Count") +
        theme_classic()
    })
    
    output$plot_state <- renderPlotly({
      ggplot(selected_data_date_from(), aes(x = state_of_origin)) +
        geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        labs(x = paste(state_title), y = "Count") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    })
    
    output$plot_language_group <- renderPlotly({
      req(input$grouper)
      ggplot(selected_data_date_from(), aes(x = language, fill = (!!!rlang::syms(input$grouper)))) +
        geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        labs(x = "language", y = "Count") +
        theme_classic()
    })
    
    output$plot_state_group <- renderPlotly({
      req(input$grouper)
      ggplot(selected_data_date_from(), aes(x = state_of_origin, fill = (!!!rlang::syms(input$grouper)))) +
        geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        labs(x = paste(state_title), y = "Count") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    })
    
    output$last_online_plot <- renderPlotly({
      ggplot(selected_data_date_from(), aes(x = last_online)) +
        geom_point(stat="count") +
        geom_line(stat = "count") +
        labs(x = "Date Last Online", y = "Frequency") +
        viridis::scale_colour_viridis(discrete = TRUE, na.value = "navy") +
        theme_classic()
    })
    
    output$last_online_group_plot <- renderPlotly({
      req(input$grouper_engagement)
      ggplot(selected_data_date_from(), aes(x = last_online, colour = (!!!rlang::syms(input$grouper_engagement)))) +
        geom_point(stat="count") +
        geom_line(stat = "count") +
        labs(x = "Date Last Online", y = "Frequency") +
        viridis::scale_colour_viridis(discrete = TRUE, na.value = "navy") +
        theme_classic()
    })
    
    output$plot_consent <- renderPlotly({
      ggplot(selected_consented_data_date_from(), aes(x = consent)) +
        geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        labs(x = "Consent", y = "Count") +
        theme_classic()
    })
    
    output$plot_consent_group <- renderPlotly({
      req(input$grouper)
      ggplot(selected_consented_data_date_from(), aes(x = consent, fill = (!!!rlang::syms(input$grouper)))) +
        geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        labs(x = "Consented", y = "Count") +
        theme_classic()
    })
    
    output$parenting_goals_plot <- renderPlotly({
      df_goals <- selected_data_date_from() %>% group_by(parenting_goal_wrap) %>% summarise(n = n())
      
      fig <- plot_ly(df_goals, labels = ~parenting_goal_wrap, values = ~n, type = 'pie')
      fig %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    })
    
    output$recruitment_channel_plot <- renderPlotly({
      df_recruitment <- selected_data_date_from() %>% group_by(recruitment_channel) %>% summarise(n = n())
      
      fig <- plot_ly(df_recruitment, labels = ~recruitment_channel, values = ~n, type = 'pie')
      fig %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    })
    
    output$parenting_goals_group_plot <- renderPlotly({
      req(input$grouper)
      ggplot(selected_data_date_from(), aes(x = parenting_goal_wrap, fill = (!!!rlang::syms(input$grouper)))) +
        geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        labs(x = "Parenting goals", y = "Count") +
        theme_classic()
    })
    
    output$recruitment_channel_group_plot <- renderPlotly({
      req(input$grouper)
      ggplot(selected_data_date_from(), aes(x = recruitment_channel, fill = (!!!rlang::syms(input$grouper)))) +
        geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        labs(x = "Recruitment channel", y = "Count") +
        theme_classic()
    })
    
    consent_table <- reactive({
      summary_table(data = selected_consented_data_date_from(), factors = consent, include_margins = TRUE, replace = NULL)
    })
    
    consent_table_group <- reactive({
      req(input$grouper)
      summary_table(selected_consented_data_date_from(), consent, (!!!rlang::syms(input$grouper)), include_margins = TRUE, wider_table = TRUE, replace = NULL, together = FALSE, naming_convention = TRUE)
    })
    
    parent_gender_table <- reactive({
      summary_table(data = selected_data_date_from(), factors = parent_gender, include_margins = TRUE, replace = NULL)
    })
    
    parent_gender_group_table <- reactive({
      summary_table(selected_data_date_from(), parent_gender, (!!!rlang::syms(input$grouper)), include_margins = FALSE, wider_table = FALSE, replace = NULL, together = FALSE, naming_convention = TRUE)
    })
    
    child_gender_table <- reactive({
      summary_table(data = selected_data_date_from(), factors = child_gender, include_margins = TRUE, replace = NULL)
      })
    
    child_gender_group_table <- reactive({
      req(input$grouper)
      summary_table(selected_data_date_from(), child_gender, (!!!rlang::syms(input$grouper)), include_margins = FALSE, wider_table = FALSE, replace = NULL, together = FALSE, naming_convention = TRUE)
    })
    child_age_table <- reactive({
      summary_table(data = selected_data_date_from(), factors = child_age_group, include_margins = TRUE, replace = NULL)
    })
    child_age_group_table <- reactive({
      req(input$grouper)
      summary_table(selected_data_date_from(), child_age_group, (!!!rlang::syms(input$grouper)), include_margins = TRUE, wider_table = TRUE, replace = NULL, together = FALSE, naming_convention = TRUE)
    })
    parent_child_relationship_table <- reactive({
      summary_table(data = selected_data_date_from(), factors = parent_child_relationship, include_margins = TRUE, replace = NULL)
    })
    parent_child_relationship_group_table <- reactive({
      req(input$grouper)
      summary_table(selected_data_date_from(), parent_child_relationship, (!!!rlang::syms(input$grouper)), include_margins = TRUE, wider_table = TRUE, replace = NULL, together = FALSE, naming_convention = TRUE)
    })
    child_disabilities_table <- reactive({
      summary_table(data = selected_data_date_from(), factors = child_disabilities, include_margins = TRUE, replace = NULL)
    })
    child_disabilities_group_table <- reactive({
      req(input$grouper)
      summary_table(selected_data_date_from(), child_disabilities, (!!!rlang::syms(input$grouper)), include_margins = TRUE, wider_table = TRUE, replace = NULL, together = FALSE, naming_convention = TRUE)
    })
    parent_relationship_table <-  reactive({
      summary_table(data = selected_data_date_from(), factors = parent_relationship, include_margins = TRUE, replace = NULL)
    })
    parent_relationship_group_table <-  reactive({
      summary_table(selected_data_date_from(), parent_relationship, (!!!rlang::syms(input$grouper)), include_margins = TRUE, wider_table = TRUE, replace = NULL, together = FALSE, naming_convention = TRUE)
    })
    
    ####### Womens Centre ###########
    womens_centre_table <- reactive({
      summary_table(data = selected_womens_centre_date_from(), factors = womens_centre_location, include_margins = TRUE, replace = NULL)
    })
    output$womens_centre_plot <- renderPlotly({
      ggplot(selected_womens_centre_date_from(), aes(x = womens_centre_location)) +
        geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        labs(x = "Location", y = "Count") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
        scale_fill_discrete(drop=FALSE) +
        scale_x_discrete(drop=FALSE)
    })
    
    ###### Engagement ########
    active_users_group_table <- reactive({
      req(input$grouper_engagement)
      summary_table(selected_data_date_from(), active_users, (!!!rlang::syms(input$grouper_engagement)), include_margins = TRUE, wider_table = TRUE, replace = NULL, together = FALSE, naming_convention = TRUE)
    })
    active_users_table <- reactive({
      summary_table(data = selected_data_date_from(), factors = active_users, include_margins = TRUE, replace = NULL)
    })
    active_users_7_days_table <- reactive({
      summary_table(data = selected_data_date_from(), factors = active_users_7_days, include_margins = TRUE, replace = NULL)
      })
    active_users_7_days_group_table <- reactive({
      req(input$grouper_engagement)
      summary_table(selected_data_date_from(), active_users_7_days, (!!!rlang::syms(input$grouper_engagement)), include_margins = TRUE, wider_table = TRUE, replace = NULL, together = FALSE, naming_convention = TRUE)
    })
    not_active_users_7_days_table <- reactive({
      summary_table(data = selected_data_date_from(), factors = not_active_7_days, include_margins = TRUE, replace = NULL)
    })
    not_active_users_7_days_group_table <- reactive({
      req(input$grouper_engagement)
      summary_table(selected_data_date_from(), not_active_7_days, (!!!rlang::syms(input$grouper_engagement)), include_margins = TRUE, wider_table = TRUE, replace = NULL, together = FALSE, naming_convention = TRUE)
    })
    
    comp_prog_table <- reactive({
      comp_prog_df <- selected_data_date_from() %>% 
        summarise(program_completion_mean = round(mean(comp_prog_overall, na.rm = TRUE), 2),
                  program_completion_sd = round(sd(comp_prog_overall, na.rm = TRUE), 2))
      colnames(comp_prog_df) <- naming_conventions(colnames(comp_prog_df))
      comp_prog_df
    })
    
    comp_prog_group_table <- reactive({
      req(input$grouper_engagement)
      comp_prog_df <- selected_data_date_from() %>% group_by(!!!rlang::syms(input$grouper_engagement)) %>%
        summarise(program_completion_mean = round(mean(comp_prog_overall, na.rm = TRUE), 2),
                  program_completion_sd = round(sd(comp_prog_overall, na.rm = TRUE), 2))
      colnames(comp_prog_df) <- naming_conventions(colnames(comp_prog_df))
      comp_prog_df
    })
    
    parent_age_table <- reactive({
      req(input$grouper)
      parent_age_df <- selected_data_date_from() %>% 
        summarise(parent_age_mean = round(mean(parent_age, na.rm = TRUE), 2),
                  parent_age_sd = round(sd(parent_age, na.rm = TRUE), 2))
      colnames(parent_age_df) <- naming_conventions(colnames(parent_age_df))
      parent_age_df
    })
    parent_age_group_table <- reactive({
      req(input$grouper)
      parent_age_df <- selected_data_date_from() %>% group_by(!!!rlang::syms(input$grouper)) %>%
        summarise(parent_age_mean = round(mean(parent_age, na.rm = TRUE), 2),
                  parent_age_sd = round(sd(parent_age, na.rm = TRUE), 2))
      colnames(parent_age_df) <- naming_conventions(colnames(parent_age_df))
      parent_age_df
    })
    
    completed_welcome_table <- reactive({
      summary_table(data = selected_data_date_from(), factors = completed_welcome, include_margins = TRUE, replace = NULL)
      })
    
    completed_welcome_group_table <- reactive({
      req(input$grouper_engagement)
      summary_table(selected_data_date_from(), completed_welcome, (!!!rlang::syms(input$grouper_engagement)), include_margins = TRUE, wider_table = TRUE, replace = NULL, together = FALSE, naming_convention = TRUE)
    })
    
    
    # Note: These are the *number* of people that have completed the survey
    completed_survey_table <- reactive({
      df <- selected_data_date_from()
      survey_completed <- NULL
      survey_completed[[1]] <- df %>% summarise(n = sum(comp_survey_w1 == 1, na.rm = TRUE))
      survey_completed[[1]]$perc <- survey_completed[[1]]$n/nrow(df) * 100
      survey_completed[[1]]$Week <- "Week 1"
      for (i in 2:9){
        survey_completed[[i]] <- df %>% summarise(n = sum(comp_survey_w2 == i, na.rm = TRUE))
        survey_completed[[i]]$perc <- survey_completed[[i]]$n/nrow(df) * 100
        survey_completed[[i]]$Week <- paste("Week ", i, sep = "")
      }
      survey_completed <- plyr::ldply(survey_completed)
      
      survey_completed <- survey_completed %>%
        mutate("Completed survey (%)" := str_c(`n`, ' (', round(`perc`, 2), ")")) %>%
        dplyr::select(-c(n, perc))
      
      pivot_wider(survey_completed, names_from = Week, values_from = `Completed survey (%)`)
      
    })
    
    # Note: These are the *number* of people that have completed the survey
    consented_survey_table <- reactive({
      df <- selected_data_date_from()
      survey_completed <- NULL
      
      wek <- NULL
      for (i in 1:9){
        c_wek <- paste0("consent_survey_w", i)
        if (c_wek %in% names(df)){
          wek[[i]] <- summary_table(data = df, factors = consent_survey_w1, include_margins = TRUE, replace = NULL)
          names(wek[[i]]) <- c("Consented", paste0("Week ", i))
          
        } else {
          wek[[i]] <- NULL
        }
      }
      
      if (length(wek) == 1){
        data.frame(wek)
      } else {
        Reduce(full_join, wek)
      }
    })
    
    consented_survey_group_table <- reactive({
      req(input$grouper)
      df <- selected_data_date_from()
      survey_completed <- NULL

      wek1 <- summary_table(data = df, factors = consent_survey_w1, (!!!rlang::syms(input$grouper)), include_margins = TRUE, wider_table = TRUE, replace = NULL, together = FALSE, naming_convention = TRUE)
      wek2 <- summary_table(data = df, factors = consent_survey_w2, (!!!rlang::syms(input$grouper)), include_margins = TRUE, wider_table = TRUE, replace = NULL, together = FALSE, naming_convention = TRUE)
      wek3 <- summary_table(data = df, factors = consent_survey_w3, (!!!rlang::syms(input$grouper)), include_margins = TRUE, wider_table = TRUE, replace = NULL, together = FALSE, naming_convention = TRUE)
      wek4 <- summary_table(data = df, factors = consent_survey_w4, (!!!rlang::syms(input$grouper)), include_margins = TRUE, wider_table = TRUE, replace = NULL, together = FALSE, naming_convention = TRUE)
      
      names(wek1)[c(1, 3)] <- c("Consented", "Baseline")
      names(wek2)[c(1, 3)] <- c("Consented", "Week 2")
      names(wek3)[c(1, 3)] <- c("Consented", "Week 3")
      names(wek4)[c(1, 3)] <- c("Consented", "Week 4")
      
      merge(merge(merge(wek1, wek2), wek3), wek4)
    })
    
    completed_survey_group_table <- reactive({
      req(input$grouper)
      df <- selected_data_date_from()
      survey_completed <- NULL
      survey_completed[[1]] <- df %>%
        group_by(!!!rlang::syms(input$grouper)) %>%
        summarise(n = sum(comp_survey_w1 == 1, na.rm = TRUE))
      survey_completed[[1]]$perc <- survey_completed[[1]]$n/nrow(df) * 100
      survey_completed[[1]]$Week <- "Week 1"
      for (i in 2:9){
        survey_completed[[i]] <- df %>% group_by(!!!rlang::syms(input$grouper)) %>% summarise(n = sum(comp_survey_w2 == i, na.rm = TRUE))
        survey_completed[[i]]$perc <- survey_completed[[i]]$n/nrow(df) * 100
        survey_completed[[i]]$Week <- paste("Week ", i, sep = "")
      }
      survey_completed <- plyr::ldply(survey_completed)
      
      survey_completed <- survey_completed %>%
        mutate("Completed survey (%)" := str_c(`n`, ' (', round(`perc`, 2), ")")) %>%
        dplyr::select(-c(n, perc))
      
      pivot_wider(survey_completed, names_from = Week, values_from = `Completed survey (%)`)
    })
    
    # Survey stuff -----------------------------------------------------------------------
    parenting_survey_table <- reactive({
      req(input$grouper_survey)
      
      selected_survey_data_date_from() %>%
        dplyr::filter(week == input$grouper_survey) %>%
        group_by(week, Group) %>%
        summarise(Mean = mean(vals, na.rm = TRUE), SD = sd(vals, na.rm = TRUE), `Number of responses` = sum(!is.na(vals))) %>%
        mutate(Scale = ifelse(Group %in% c("Food insecurity", "Sexual abuse prevention"), "0-30", "0-7")) %>%
        mutate(Mean = round(Mean, 2),
               SD = round(SD, 2),
               `Number of responses` = round(`Number of responses`, 0))
    })
    
    output$parenting_survey_plot <- renderPlotly({
      parenting_survey1 <- selected_survey_data_date_from() %>% mutate(week = as.numeric(week)) %>% filter(Group %in% c("Positive parenting", "Child maltreatment", "Stress", "Child Behaviour"))
      parenting_survey_plot <- summarySE(parenting_survey1, groups = c(week, Group), var = vals, na.rm = TRUE)
      ggplot(parenting_survey_plot, aes(x=week, y=mean, colour=Group, group = Group), width = 2) + 
        geom_line() +
        geom_point(data = parenting_survey_plot, aes(size = N)) +
        geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
        viridis::scale_color_viridis(discrete = TRUE) +
        labs(x = "Survey", y = "Frequency", title = "Survey Responses with (SE) Error Bars") +
        scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9),
                           labels=c("Baseline", "2", "3", "4", "5", "6", "7", "8", "9")) +
        theme_classic()
    })
    
    output$behaviour_plots <- renderPlotly({
      parenting_survey1 <- selected_survey_data_date_from() %>% filter(!is.na(vals))
      
      parenting_survey_plot <- summarySE(parenting_survey1, groups = c(week), var = vals, na.rm = TRUE)
      ggplot(parenting_survey_plot, aes(x=week, y=mean, group = week), width = 2) + 
        geom_line(colour = "black") +
        geom_point(data = parenting_survey_plot, aes(size = N)) +
        geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
        viridis::scale_color_viridis(discrete = TRUE) +
        labs(x = "Survey", y = "Frequency", title = "Survey Responses with (SE) Error Bars") +
        theme_classic()
    })
    
    output$behaviour_group_plots <- renderPlotly({
      #req(input$grouper)
      
      df_age_group <- selected_data_date_from() %>% dplyr::select(c(ID, child_age_group, parent_gender, child_gender))
      parenting_survey1 <- merge(selected_survey_data_date_from(), df_age_group)
      
      parenting_survey1 <- parenting_survey1 %>% filter(!is.na(vals))
      
      parenting_survey_plot <- summarySE(parenting_survey1, groups = c(week, child_age_group), var = vals, na.rm = TRUE)
      ggplot(parenting_survey_plot, aes(x=week, y=mean, colour = child_age_group, group = child_age_group), width = 2) + 
        geom_line() +
        geom_point(data = parenting_survey_plot, aes(size = N)) +
        geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
        viridis::scale_color_viridis(discrete = TRUE) +
        labs(x = "Survey", y = "Frequency", title = "Survey Responses with (SE) Error Bars") +
        theme_classic()
    })
    
    output$behaviour_baby_plot <- renderPlotly({
      df_baby <- selected_data_date_from() %>% filter(child_age_group == "Baby")
      
      behaviour_plot <- ggplot(df_baby, aes(x = challenge_behav_wrap)) +
        geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        labs(x = NULL, y = NULL) +
        theme_classic()
      
      if (nrow(df_baby) > 0){
        behaviour_plot <- behaviour_plot + facet_grid(cols = vars(child_age_group))
      }
      behaviour_plot
    })
    
    output$behaviour_child_plot <- renderPlotly({
      df_child <- selected_data_date_from() %>% filter(child_age_group == "Child")
      
      behaviour_plot <- ggplot(df_child, aes(x = challenge_behav_wrap)) +
        geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        labs(x = NULL, y = NULL) +
        theme_classic()
      
      if (nrow(df_child) > 0){
        behaviour_plot <- behaviour_plot + facet_grid(cols = vars(child_age_group))
      }
      behaviour_plot
    })
    
    output$behaviour_teen_plot <- renderPlotly({
      df_teen <- selected_data_date_from() %>% filter(child_age_group == "Teen")
      
      behaviour_plot <- ggplot(df_teen, aes(x = challenge_behav_wrap)) +
        geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        labs(x = NULL, y = NULL) +
        theme_classic()
      
      if (nrow(df_teen) > 0){
        behaviour_plot <- behaviour_plot + facet_grid(cols = vars(child_age_group))
      }
      behaviour_plot
    })
    
    output$behaviour_default_plot <- renderPlotly({
      df_default <- selected_data_date_from() %>% filter(child_age_group == "Default")
      
      behaviour_plot <- ggplot(df_default, aes(x = challenge_behav_wrap)) +
        geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        labs(x = NULL, y = NULL) +
        theme_classic()
      
      if (nrow(df_default) > 0){
        behaviour_plot <- behaviour_plot + facet_grid(cols = vars(child_age_group))
      }
      behaviour_plot
    })
    
    output$behaviour_baby_group_plot <- renderPlotly({
      req(input$grouper_behaviour)
      df_baby <- selected_data_date_from() %>% filter(child_age_group == "Baby")
      
      behaviour_plot <- ggplot(df_baby, aes(x = challenge_behav_wrap, fill = (!!!rlang::syms(input$grouper_behaviour)))) +
        geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        labs(x = NULL, y = NULL) +
        theme_classic()
      
      if (nrow(df_baby) > 0){
        behaviour_plot <- behaviour_plot + facet_grid(cols = vars(child_age_group))
      }
      behaviour_plot
    })
    
    output$behaviour_child_group_plot <- renderPlotly({
      req(input$grouper_behaviour)
      df_child <- selected_data_date_from() %>% filter(child_age_group == "Child")
      
      behaviour_plot <- ggplot(df_child, aes(x = challenge_behav_wrap, fill = (!!!rlang::syms(input$grouper_behaviour)))) +
        geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        labs(x = NULL, y = NULL) +
        theme_classic()
      
      if (nrow(df_child) > 0){
        behaviour_plot <- behaviour_plot + facet_grid(cols = vars(child_age_group))
      }
      behaviour_plot
    })
    
    output$behaviour_teen_group_plot <- renderPlotly({
      req(input$grouper_behaviour)
      df_teen <- selected_data_date_from() %>% filter(child_age_group == "Teen")
      
      behaviour_plot <- ggplot(df_teen, aes(x = challenge_behav_wrap, fill = (!!!rlang::syms(input$grouper_behaviour)))) +
        geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        labs(x = NULL, y = NULL) +
        theme_classic()
      
      if (nrow(df_teen) > 0){
        behaviour_plot <- behaviour_plot + facet_grid(cols = vars(child_age_group))
      }
      behaviour_plot
    })
    
    output$behaviour_default_group_plot <- renderPlotly({
      req(input$grouper_behaviour)
      df_default <- selected_data_date_from() %>% filter(child_age_group == "Default")
      
      behaviour_plot <- ggplot(df_default, aes(x = challenge_behav_wrap, fill = (!!!rlang::syms(input$grouper_behaviour)))) +
        geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        labs(x = NULL, y = NULL) +
        theme_classic()
      
      if (nrow(df_default) > 0){
        behaviour_plot <- behaviour_plot + facet_grid(cols = vars(child_age_group))
      }
      behaviour_plot
    })
    
    # Flow Tab ------------------------------------------------------------
    selected_flow_data_date_from <- reactive({
      valid_IDs <- selected_data_date_from()$ID
      all_flows <- all_flows %>% filter(ID %in% c(valid_IDs))
      all_flows_df <- flow_data_table_function(all_flows, Flow) %>% 
        return(all_flows_df)
    })
    
    all_flows_response <- reactive({
      selected_flow_data_date_from() %>%
        #filter(!is.na(Flow)) %>%
        pivot_wider(id_cols = interacted, names_from = Flow, values_from = `Count (%)`) %>%
        dplyr::select(-c(`NA`))
    })
    
    output$plot_flow <- renderPlotly({
      all_flows_df <- selected_flow_data_date_from() %>%
        separate(`Count (%)`, into = "Count") %>%
        mutate(Count = as.numeric(as.character(Count)))
      
      ggplot(all_flows_df, aes(x = interacted, y = Count, fill = `Flow`)) +
        geom_bar(stat = "identity") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        theme_classic() +
        labs(x = "Interacted")
    })
    
    # Output render ------------------------------------------------------------
    output$myvaluebox1 <- shinydashboard::renderValueBox({
      df_enrolled <- summary_table(data = selected_consented_data_date_from(), factors = enrolled, include_margins = TRUE, wider_table = TRUE, replace = NULL, together = FALSE, naming_convention = FALSE)
      df_enrolled <- df_enrolled %>% mutate(group =  enrolled, count = n) %>% dplyr::select(c(group, count))
      shinydashboard::valueBox(df_enrolled$count[1], subtitle = "Enrolled", icon = icon("user"),
                               color = "aqua"
      )
    })
    output$myvaluebox2 <- shinydashboard::renderValueBox({
      df_consented <- summary_table(data = selected_data_date_from(), factors = consent, include_margins = TRUE, wider_table = TRUE, replace = NULL, together = FALSE, naming_convention = FALSE)
      df_consented <- df_consented %>% mutate(group = consent, count = n) %>% dplyr::select(c(group, count))
      shinydashboard::valueBox(df_consented$count[1],subtitle = "Consented",icon = icon("check"),
                               color = "green"
      )
    })
    output$myvaluebox3 <- shinydashboard::renderValueBox({
      df_program <- summary_table(data = selected_data_date_from(), factors = program, include_margins = TRUE, wider_table = TRUE, replace = NULL, together = FALSE, naming_convention = FALSE)
      df_program <- df_program %>% mutate(group =  program, count = n) %>% dplyr::select(c(group, count))
      shinydashboard::valueBox(df_program$count[1],subtitle = "In Program",icon = icon("clipboard"),
                               color = "yellow"
      )
    })
    output$myvaluebox4 <- shinydashboard::renderValueBox({
      df_active_24 <-  summary_table(data = selected_data_date_from(), factors = active_users, include_margins = TRUE, wider_table = TRUE, replace = NULL, together = FALSE, naming_convention = FALSE)
      df_active_24 <- (df_active_24 %>% filter(active_users == "Yes"))$n
      shinydashboard::valueBox(df_active_24, subtitle = "Active in last 24 hours",icon = icon("clock"),
                               color = "purple")
    })
    output$myvaluebox5 <- shinydashboard::renderValueBox({
      df_active_7d <- summary_table(data = selected_data_date_from(), factors = active_users_7_days, include_margins = TRUE, wider_table = TRUE, replace = NULL, together = FALSE, naming_convention = FALSE)
      df_active_7d <- (df_active_7d %>% filter(active_users_7_days == "Yes"))$n
      shinydashboard::valueBox(df_active_7d, subtitle = "Active in last 7 days", icon = icon("signal"),
                               color = "fuchsia"
      )
    })
    output$myvaluebox6 <- shinydashboard::renderValueBox({
      df_not_active_7d <- summary_table(data = selected_data_date_from(), factors = not_active_7_days, include_margins = TRUE, wider_table = TRUE, replace = NULL, together = FALSE, naming_convention = FALSE)
      df_not_active_7d <- (df_not_active_7d %>% filter(not_active_7_days == "Yes"))$n
      shinydashboard::valueBox(df_not_active_7d, subtitle = "Not active in last 7 days", icon = icon("signal"),
                               color = "red"
      )
    })
    #output$pp_table <- renderDataTable(pp_data_frame,
    #                                   options = list(
    #                                     pageLength = 5)
    #)
    output$language_table <- shiny::renderTable({(language_table())}, striped = TRUE)
    output$language_table_group <- shiny::renderTable({(language_table_group())}, striped = TRUE)
    output$state_table <- shiny::renderTable({(state_table())}, striped = TRUE)
    output$state_table_group <- shiny::renderTable({(state_table_group())}, striped = TRUE)
    output$consent_table <- shiny::renderTable({(consent_table())}, striped = TRUE)
    output$consent_table_group <- shiny::renderTable({(consent_table_group())}, striped = TRUE)
    output$parent_gender_table <- shiny::renderTable({(parent_gender_table())}, striped = TRUE)
    output$parent_gender_group_table <- shiny::renderTable({(parent_gender_group_table())}, striped = TRUE)
    output$parent_age_table <- shiny::renderTable({(parent_age_table())}, striped = TRUE)
    output$parent_age_group_table <- shiny::renderTable({(parent_age_group_table())}, striped = TRUE)
    output$child_gender_table <- shiny::renderTable({(child_gender_table())}, striped = TRUE)
    output$child_gender_group_table <- shiny::renderTable({(child_gender_group_table())}, striped = TRUE)
    output$child_age_table <- shiny::renderTable({(child_age_table())}, striped = TRUE)
    output$child_age_group_table <- shiny::renderTable({(child_age_group_table())}, striped = TRUE)
    output$parent_child_relationship_table <- shiny::renderTable({(parent_child_relationship_table())}, caption = "Relationship between the parent and child", striped = TRUE)
    output$parent_child_relationship_group_table <- shiny::renderTable({(parent_child_relationship_group_table())}, caption = "Relationship between the parent and child", striped = TRUE)
    output$parent_relationship_group_table <- shiny::renderTable({(parent_relationship_group_table())}, caption = "Relationship status of the parent", striped = TRUE)
    output$parent_relationship_table <- shiny::renderTable({(parent_relationship_table())}, caption = "Relationship status of the parent", striped = TRUE)
    output$child_disabilities_table <- shiny::renderTable({(child_disabilities_table())}, caption = "Does the child have a disability?", striped = TRUE)
    output$child_disabilities_group_table <- shiny::renderTable({(child_disabilities_group_table())}, caption = "Does the child have a disability?", striped = TRUE)
    output$womens_centre_table <- shiny::renderTable({(womens_centre_table())}, striped = TRUE)
    output$active_users_table <- shiny::renderTable({(active_users_table())}, striped = TRUE)
    output$active_users_group_table <- shiny::renderTable({(active_users_group_table())}, striped = TRUE)
    output$active_users_7_days_table <- shiny::renderTable({(active_users_7_days_table())}, striped = TRUE)
    output$active_users_7_days_group_table <- shiny::renderTable({(active_users_7_days_group_table())}, striped = TRUE)
    output$not_active_users_7_days_table <- shiny::renderTable({(not_active_users_7_days_table())}, striped = TRUE)
    output$not_active_users_7_days_group_table <- shiny::renderTable({(not_active_users_7_days_group_table())}, striped = TRUE)
    output$comp_prog_table <- shiny::renderTable({(comp_prog_table())}, caption = "Number of skills in toolkit", striped = TRUE)
    output$comp_prog_group_table <- shiny::renderTable({(comp_prog_group_table())}, caption = "Number of skills in toolkit", striped = TRUE)
    output$completed_welcome_table <- shiny::renderTable({completed_welcome_table()}, striped = TRUE, caption = "Number (and percentage) of individuals who have completed the welcome survey")
    output$completed_welcome_group_table <- shiny::renderTable({{completed_welcome_group_table()}}, striped = TRUE, caption = "Number (and percentage) of individuals who have completed the welcome survey")
    output$completed_survey_table <- shiny::renderTable({{completed_survey_table()}}, striped = TRUE, caption = "Number (and percentage) of individuals who have completed different surveys")
    output$completed_survey_group_table <- shiny::renderTable({{completed_survey_group_table()}}, striped = TRUE, caption = "Number (and percentage) of individuals who have completed different surveys")
    output$consented_survey_table <- shiny::renderTable({{consented_survey_table()}}, striped = TRUE, caption = "Number (and percentage) of individuals who have consented to different surveys")
    output$consented_survey_group_table <- shiny::renderTable({{consented_survey_group_table()}}, striped = TRUE, caption = "Number (and percentage) of individuals who have consented to different surveys")
    output$all_flows_response <- shiny::renderTable({(all_flows_response())}, caption = "Count (%) for each flow", striped = TRUE)
    output$parenting_survey_table <- shiny::renderTable({(parenting_survey_table())}, caption = "How many times in the past week ... \n For Sexual abuse prevention, the timeframe is how many days in the past month.", striped = TRUE)
  }
  
  # Create Shiny object
  shinyApp(ui = ui, server = server)
}
