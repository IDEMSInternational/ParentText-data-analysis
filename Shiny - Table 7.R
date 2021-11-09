library(shiny)
library(shinythemes)
library(shinyjs)
#library(rpivotTable)
library(plotly)
library(shinydashboard)
library(httr)
library(jsonlite)
library(tidyverse)

source("Functions.R")
# source("Code Book.R")

# RapidPro set up --------------------------------------------------------------
# for this to work you need to change the directory to where the token key is stored.
key <- read.table("./tokens/PT_malaysia_key.txt", quote="\"", comment.char="")
set_rapidpro_key(key = key)
set_rapidpro_site(site = "https://app.rapidpro.io/api/v2/")
set_rapidpro_uuid_names()

update_data <- function() {
  contacts_unflat <- get_user_data(flatten = FALSE)
  
  contacts_unflat <- contacts_unflat %>% filter(as.POSIXct("2021-10-14", format="%Y-%m-%d", tzone = "UTC") < as.POSIXct(contacts_unflat$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  
  # Variables Manipulation -------------------------------------------------------
  # get enrolled and consented data
  enrolled <- NULL
  consent <- NULL
  program <- NULL
  for (i in 1:length(contacts_unflat$groups)){
    contact_name <- contacts_unflat$groups[[i]]
    if (length(contact_name)==0) {
      enrolled[i] <- NA
      consent[i] <- NA
      program[i] <- NA
    } else{
      enrolled[i] <- ifelse(any(contact_name$name %in% "joined"), "Yes", "No")
      consent[i] <- ifelse(any(contact_name$name %in% "consent"), "Yes", "No")
      program[i] <- ifelse(any(contact_name$name %in% "in program"), "Yes", "No")
    }
  }
  
  parent_gender <- contacts_unflat$fields$gender
  parent_gender <- factor(ifelse(parent_gender %in% c("female", "f", "woman", "Woman"), "Woman",
                                 ifelse(parent_gender %in% c("male", "m", "man", "Man"), "Man",
                                        ifelse(parent_gender %in% "no", NA, parent_gender))))
  parent_gender <- forcats::fct_relevel(parent_gender, c("Woman", "Man"))
  
  child_age_group <- contacts_unflat$fields$age_group_for_tips
  know_age_group <- contacts_unflat$fields$know_age_group_for_tips
  child_age_group <- ifelse(child_age_group == "child" & know_age_group == "no", "Default", child_age_group)
  child_age_group <- factor(child_age_group)
  child_age_group <- forcats::fct_recode(child_age_group,
                                         Baby = "baby",
                                         Child = "child",
                                         Teen = "teen")
  child_age_group <- forcats::fct_relevel(child_age_group, c("Baby", "Child", "Teen", "Default"))
  
  child_gender <- factor(contacts_unflat$fields$survey_behave_sex)
  child_gender <-  forcats::fct_recode(child_gender,
                                       Boy = "male",
                                       Girl = "female", 
                                       `Prefer not to say` = "no")
  child_gender <- forcats::fct_relevel(child_gender, c("Girl", "Boy", "Prefer not to say"))
  
  parent_child_relationship <- factor(contacts_unflat$fields$survey_behave_relationship)
  parent_child_relationship <- forcats::fct_recode(parent_child_relationship,
                                                   Parent = "parent",
                                                   Grandparent = "grandparent",
                                                   `Aunt/Uncle`= "uncle",
                                                   `Foster Parent` = "foster",
                                                   Other = "other",
                                                   `Prefer not to say`  = "no")
  parent_child_relationship <- forcats::fct_relevel(parent_child_relationship,
                                                    c("Parent", "Grandparent", "Aunt/Uncle", "Foster Parent", "Other", "Prefer not to say"))
  
  parent_relationship_status <- factor(contacts_unflat$fields$marital_status)
  parent_relationship_status <- forcats::fct_recode(parent_relationship_status,
                                        `Prefer not to say`  = "no")
  parent_relationship_status <- forcats::fct_relevel(parent_relationship_status, c("Single", "Married", "Partnered", "Divorced", "Widowed", "Prefer not to say"))
  
  child_living_with_disabilities <- factor(contacts_unflat$fields$has_disability)
  child_living_with_disabilities <- forcats::fct_recode(child_living_with_disabilities,
                                        Yes = "yes",
                                        No = "no",
                                        `supp_disab`= "supp_disab")
  child_living_with_disabilities <- forcats::fct_relevel(child_living_with_disabilities,
                                         c("Yes", "No", "supp_disab"))
  
  parenting_goals <- factor(as.numeric(contacts_unflat$fields$parenting_goal))
  parenting_goals <- forcats::fct_recode(parenting_goals,
                                         `Relationship` = "1",
                                         `Behaviour` = "2",
                                         `School` = "3",
                                         `COVID-19` = "4",
                                         `Stress` = "5",
                                         `Finances` = "6",
                                         `Family conflict` = "7",
                                         `Safety`= "8",
                                         `Disabilities` = "9",
                                         `Other` = "0")
  parenting_goals <- str_wrap(parenting_goals, width = 15)
  parenting_goals <- forcats::fct_relevel(parenting_goals,
                                          c("Relationship","Behaviour",
                                            "School", "COVID-19",
                                            "Stress", "Finances",
                                            "Family conflict", "Safety",
                                            "Disabilities", "Other"))
  
  # Calculations -----------------------------------------------------------------
  # active users # N = contacts for which the time difference between the current time and the datetime variable "last_seen_on" is less than 24 h 
  active_users_24hr <- difftime(lubridate::now(tzone = "UTC"), as.POSIXct(contacts_unflat$last_seen_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"), units = "hours") <= 24
  active_users_24hr <- factor(active_users_24hr)
  if (length(levels(active_users_24hr)) == 1){
    if (levels(active_users_24hr) == "FALSE"){
      levels(active_users_24hr) <- c(levels(active_users_24hr),"TRUE")
    } else if (levels(active_users_24hr) == "TRUE"){
      levels(active_users_24hr) <- c(levels(active_users_24hr),"FALSE")
    }
  }
  active_users_24hr <- forcats::fct_recode(active_users_24hr,
                                           "No" = "FALSE",
                                           "Yes" = "TRUE")
  active_users_24hr <- forcats::fct_relevel(active_users_24hr, c("Yes", "No"))
  
  active_users_7d <- difftime(lubridate::now(tzone = "UTC"), as.POSIXct(contacts_unflat$last_seen_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"), units = "hours") <= 7*24
  active_users_7d <- factor(active_users_7d)
  if (length(levels(active_users_7d)) == 1){
    if (levels(active_users_7d) == "FALSE"){
      levels(active_users_7d) <- c(levels(active_users_7d),"TRUE")
    } else if (levels(active_users_7d) == "TRUE"){
      levels(active_users_7d) <- c(levels(active_users_7d),"FALSE")
    }
  }
  active_users_7d <- forcats::fct_recode(active_users_7d,
                                         "No" = "FALSE",
                                         "Yes" = "TRUE")
  active_users_7d <- forcats::fct_relevel(active_users_7d, c("Yes", "No"))
  
  # comp_prog_overall
  # TODO: only should be lookign at this for those who consented
  n_skills <- as.numeric(as.character(contacts_unflat$fields$n_skills))
  
  # Participant age etc
  # TODO: only should be lookign at this for those who consented
  parent_age <- as.numeric(as.character(contacts_unflat$fields$age))
  
  survey_completed_wk1 <- str_count(contacts_unflat$fields$surveyparenting_completion, fixed("|"))
  if (length(survey_completed_wk1) == 0){survey_completed_wk1 <- rep(NA, length(enrolled))}
  
  survey_completed_wk2 <- str_count(contacts_unflat$fields$fields.surveyparentingbehave_completion, fixed("|"))
  if (length(survey_completed_wk2) == 0){survey_completed_wk2 <- rep(NA, length(enrolled))}
  
  # sum of response to content, calm, check in, supportive, praise messages
  # supportive
  supportive_flow_names <- c("PLH - Content - Extra - CheckIn - COVID",
                             "PLH - Supportive - Family", "PLH - Supportive - Help reminder", "PLH - Supportive - Share", "PLH - Supportive - Share - Enrollment", "GG - PLH - Supportive - Share - Enrollment", "PLH - Supportive - Budget", "PLH - Supportive - Activities for babies", "PLH - Supportive - Activities",
                             "PLH - Supportive - Behave reminder", "PLH - Supportive - Children reminder", "PLH - Supportive - Covid", "PLH - Supportive - Development", "PLH - Supportive - Disabilities")
  
  supportive_calm <- "PLH - Supportive - Calm"
  
  supportive_praise <- "PLH - Supportive - Praise"
  
  check_in_flow_names <- c("PLH - Content - Extra - CheckIn - COVID", "PLH - Content - Positive - CheckIn - Book sharing", "PLH - Content - Positive - CheckIn - Budget adults", "PLH - Content - Positive - CheckIn - Budget with children", "PLH - Content - Positive - CheckIn - Community safety", "PLH - Content - Positive - CheckIn - Consequences", "PLH - Content - Positive - CheckIn - Crisis", "PLH - Content - Positive - CheckIn - Crying", "PLH - Content - Positive - CheckIn - Education", "PLH - Content - Positive - CheckIn - Emotion", "PLH - Content - Positive - CheckIn - Family", "PLH - Content - Positive - CheckIn - Ignore",
                           #"PLH - Content - Positive - CheckIn - Instructions",
                           "PLH - Content - Positive - CheckIn - IPV 1", "PLH - Content - Positive - CheckIn - IPV 2", "PLH - Content - Positive - CheckIn - IPV 3", "PLH - Content - Positive - CheckIn - IPV 4", "PLH - Content - Positive - CheckIn - IPV 5", "PLH - Content - Positive - CheckIn - Online adults", "PLH - Content - Positive - CheckIn - Online children", "PLH - Content - Positive - CheckIn - Praise", "PLH - Content - Positive - CheckIn - ProblemSolving", "PLH - Content - Positive - CheckIn - Redirect", "PLH - Content - Positive - CheckIn - Routines", "PLH - Content - Positive - CheckIn - Rules", "PLH - Content - Positive - CheckIn - Safe or unsafe touch", "PLH - Content - Relax - CheckIn - Anger management", "PLH - Content - Relax - CheckIn - Connect", "PLH - Content - Relax - CheckIn - List of things",
                           "PLH - Content - Relax - CheckIn - Loving Kindness", "PLH - Content - Relax - CheckIn - Notice how you feel", "PLH - Content - Relax - CheckIn - Three is a magical number", "PLH - Content - Time - CheckIn - One on one time")
  
  content_tip_flow_names <- c("PLH - Content - Positive - Behave - Consequences - Timed intro", "PLH - Content - Positive - Behave - Crisis - Timed intro", "PLH - Content - Positive - Behave - Crying - Timed intro", "PLH - Content - Positive - Behave - Emotion - Timed intro", "PLH - Content - Positive - Behave - Ignore - Timed intro", "PLH - Content - Positive - Behave - Praise - Timed intro", "PLH - Content - Positive - Behave - ProblemSolving - Timed intro", "PLH - Content - Positive - Behave - Redirect - Timed intro", "PLH - Content - Positive - Behave - Routines - Timed intro",
                              "PLH - Content - Positive - Book sharing - Timed intro", "PLH - Content - Positive - Budget adults - Timed intro", "PLH - Content - Positive - Budget with children - Timed intro","PLH - Content - Positive - Education - Timed intro",
                              "PLH - Content - Positive - Family - Timed intro", "PLH - Content - Positive - Online adults - Timed intro", "PLH - Content - Positive - Online children - Timed intro", "PLH - Content - Positive - Rules - Timed intro",
                              "PLH - Content - Positive - Safe or unsafe touch - Timed intro", "PLH - Content - Relax - Take a pause - Timed intro", "PLH - Content - Relax - Exercise", "PLH - Content - Time - One on one time baby - Timed intro",
                              "PLH - Content - Time - One on one time child - Timed intro", "PLH - Content - Time - One on one time teen - Timed intro", "PLH - Content - Positive - introduction", "PLH - Content - Positive - Positive instructions", "PLH - Content - Relax - Quick Pause", "PLH - Content - Relax - Anger management", "PLH - Content - Relax - Anger management 2", "PLH - Content - Positive - IPV", "PLH - Content - Positive - Community safety")
  
  df <- data.frame(enrolled, consent, program, parent_gender, child_gender, child_age_group, parent_child_relationship, 
                   parent_relationship_status, child_living_with_disabilities, parenting_goals,
                   active_users_24hr, active_users_7d, n_skills, parent_age, survey_completed_wk1, survey_completed_wk2)
  
  objects_to_return <- NULL
  objects_to_return[[1]] <- df
  objects_to_return[[2]] <- supportive_calm
  objects_to_return[[3]] <- supportive_praise
  objects_to_return[[4]] <- check_in_flow_names
  objects_to_return[[5]] <- content_tip_flow_names
  objects_to_return[[6]] <- supportive_flow_names
  objects_to_return[[7]] <- enrolled
  objects_to_return[[8]] <- consent
  objects_to_return[[9]] <- program
  return(objects_to_return)
}

update_data()

updated_data <- update_data()
df <- updated_data[[1]]
supportive_calm <- updated_data[[2]]
supportive_praise <- updated_data[[3]]
check_in_flow_names <- updated_data[[4]]
content_tip_flow_names <- updated_data[[5]]
supportive_flow_names <- updated_data[[6]]
enrolled <- updated_data[[7]]
consent <- updated_data[[8]]
program <- updated_data[[9]]

# retention_exit ---------------------------------------------------------------
# number of contacts for which the contact field "exit_message" is not empty &
# they are NOT in the group "in program"
#df %>% filter(program == "No") %>% nrow(.)
#contacts_unflat$fields$exit_message

# Define UI
ui <- dashboardPage(
  header = dashboardHeader(title = "ParentText Dashboard"),
  
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Engagement", tabName = "engagement", icon = icon("clipboard")),
      menuItem("Behaviours", tabName = "behaviours", icon = icon("brain"))
    )),
  
  dashboardBody(
    fluidRow(
      shinydashboard::valueBoxOutput("myvaluebox1", width=2),
      shinydashboard::valueBoxOutput("myvaluebox2", width=2),
      shinydashboard::valueBoxOutput("myvaluebox3", width=2),
      shinydashboard::valueBoxOutput("myvaluebox4", width=2),
      shinydashboard::valueBoxOutput("myvaluebox5", width=2)),
    fluidRow(
      column(
        width = 12,
        #align = "center",
        fluidRow(
          column(6,
                 box( height=NULL, width=NULL,
                      #background = "light-blue",
                      collapsible = FALSE,
                      title = NULL,
                      #status = "success",
                      solidHeader = TRUE,
                      checkboxInput(inputId = "groupby", label = strong("Group by variables"), value = FALSE),
                      uiOutput("groups"))),
        ) #fluid row closure
      ) #Outer column closure
    ),
    tabItems(
      # First tab content
      
      tabItem(tabName = "demographics",
              br(),
              fluidRow(h2(paste0("     Demographics"), align="centre")),
              #shiny::tableOutput("enrolled_summary"),
              shiny::tableOutput("consent_summary"),
              
              fluidRow(
                column(6,
                       box( height="300px",  width=NULL,
                            collapsible = FALSE,
                            title = "Consent Frequency",
                            status = "primary", # primary, success, info, warning, danger
                            solidHeader = TRUE,
                            plotlyOutput(outputId = "plot_consent", height = "240")
                       ),  style='width: 500px; height: 400px'),
                column(6,
                       box(height="300px", width="450px",
                           title = "Enrollment Frequency",
                           status = "primary",
                           solidHeader = TRUE,
                           collapsible = FALSE,
                           plotlyOutput(outputId = "plot_category", height = "240", width = "100%")
                       ),  style='width: 500px; height: 400px') 
              ),
              shiny::tableOutput("parent_gender_summary"),
              shiny::tableOutput("parent_age_summary"),
              shiny::tableOutput("child_gender_summary"),
              shiny::tableOutput("child_age_group_summary"),
              shiny::tableOutput("parent_child_relationship_summary"),
              shiny::tableOutput("parent_relationship_status_summary"),
              shiny::tableOutput("child_living_with_disabilities_summary"),
              fluidRow(
                column(9,
                       box( height="300px",  width=12,
                            collapsible = FALSE,
                            title = "Parenting Goals",
                            status = "primary", # primary, success, info, warning, danger
                            solidHeader = TRUE,
                            plotlyOutput(outputId = "parenting_goals_plot", height = "240", width = "100%")
                       )
                ),
                
              )),
      
      # Second tab content
      tabItem(tabName = "engagement",
              br(),
              fluidRow(h2(paste0("     Engagement"), align="centre")),
              shiny::tableOutput("all_flows_response"), #fluidRow closure
              #shiny::tableOutput("supportive_praise_response"), #fluidRow closure
              #shiny::tableOutput("supportive_response"),#fluidRow closure
              #shiny::tableOutput("check_in_response"),
              #shiny::tableOutput("content_response"),
              shiny::tableOutput("response_message_overall"),
              fluidRow(
                column(6,
                       box( height="300px",  width=NULL,
                            collapsible = FALSE,
                            title = "Plot of Flows",
                            status = "primary", # primary, success, info, warning, danger
                            solidHeader = TRUE,
                            plotlyOutput(outputId = "plot_flow", height = "240", width = "100%")
                       ))),
              shiny::tableOutput("active_users_24hr_summary"),
              shiny::tableOutput("active_users_7d_summary"),
              shiny::tableOutput("comp_prog_summary"),
              shiny::tableOutput("completed_survey_summary")),
      
      tabItem(tabName = "behaviours",
              br(),
              fluidRow(h2(paste0("     Behaviours"), align="centre")))
    )
  )#dashboard Body closure
)  #dashboard Page closure

# Define server function
server <- function(input, output) {
  
  autoRefresh <- reactiveTimer(6 * 60 * 60 * 1000)
  
  observe({
    autoRefresh()
    
    updated_data <- update_data()
    df <- updated_data[[1]]
    supportive_calm <- updated_data[[2]]
    supportive_praise <- updated_data[[3]]
    check_in_flow_names <- updated_data[[4]]
    content_tip_flow_names <- updated_data[[5]]
    supportive_flow_names <- updated_data[[6]]    
    enrolled <- updated_data[[7]]
  })
  
  updated_data <- update_data()
  df <- updated_data[[1]]
  supportive_calm <- updated_data[[2]]
  supportive_praise <- updated_data[[3]]
  check_in_flow_names <- updated_data[[4]]
  content_tip_flow_names <- updated_data[[5]]
  supportive_flow_names <- updated_data[[6]]
  enrolled <- updated_data[[7]]
  consent <- updated_data[[8]]
  program <- updated_data[[9]]
  
  # Subset data
  selected_data <- reactive({
    df
  })
  
  output$groups <- renderUI({
    df <- df
    selectInput(
      inputId = "grouper",
      label = "Group variable",
      choices = c("Parent Gender" = "parent_gender",
                  "Child Gender" = "child_gender",
                  "Child Age Group" = "child_age_group"),
      selected = "parent_gender"
    )
  })
  
  observeEvent(input$groupby, {
    if(input$groupby){
      shinyjs::enable("grouper")
    }else{
      shinyjs::disable("grouper")
    }
  })
  
  #enrolled_summary <- reactive({
  #  req(input$grouper)
  #  if(input$groupby == TRUE){
  #    summary_PT(df, c(enrolled, (!!!rlang::syms(input$grouper))))
  #  } else {
  #   summary_PT(df, enrolled)
  #  }
  #})
  
  output$plot_category <- renderPlotly({
    req(input$grouper)
    if(input$groupby == TRUE){
      ggplot(df, aes(x = enrolled, fill = (!!!rlang::syms(input$grouper)))) +
        geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        labs(x = "Enrolled", y = "Count")
      
      # Note: Plotly doesnt not render well with a pie chart made this way.
      #df_enrolled <- df %>% filter(enrolled = TRUE)
      #ggplot(df_enrolled, aes(x = "", fill =(!!!rlang::syms(input$grouper)))) +
      #  geom_bar(position = "stack") +
      #  coord_polar(theta = "y") +
      #  viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      #  labs(x = "Enrolled", y = "Count", title = "Enrollment frequency")
    } else {
      ggplot(df, aes(x = enrolled)) +
        geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        labs(x = "Enrolled", y = "Count")
    }
  })
  
  output$plot_consent <- renderPlotly({
    req(input$grouper)
    if(input$groupby == TRUE){
      ggplot(df, aes(x = consent, fill = (!!!rlang::syms(input$grouper)))) +
        geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        labs(x = "Consented", y = "Count")
      
      # Note: Plotly doesnt not render well with a pie chart made this way.
      #df_enrolled <- df %>% filter(enrolled = TRUE)
      #ggplot(df_enrolled, aes(x = "", fill =(!!!rlang::syms(input$grouper)))) +
      #  geom_bar(position = "stack") +
      #  coord_polar(theta = "y") +
      #  viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      #  labs(x = "Consent", y = "Count", title = "Consent frequency")
    } else {
      ggplot(df, aes(x = consent)) +
        geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        labs(x = "Consent", y = "Count")
    }
  })
  
  output$parenting_goals_plot <- renderPlotly({
    req(input$grouper)
    if(input$groupby == TRUE){
      ggplot(df, aes(x = parenting_goals, fill = (!!!rlang::syms(input$grouper)))) +
        geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        labs(x = "Parenting goals", y = "Count")
      
      # Note: Plotly doesnt not render well with a pie chart made this way.
      #df_enrolled <- df %>% filter(enrolled = TRUE)
      #ggplot(df_enrolled, aes(x = "", fill =(!!!rlang::syms(input$grouper)))) +
      #  geom_bar(position = "stack") +
      #  coord_polar(theta = "y") +
      #  viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      #  labs(x = "Enrolled", y = "Count", title = "Enrollment frequency")
    } else {
      ggplot(df, aes(x = parenting_goals)) +
        geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        labs(x = "Parenting goals", y = "Count")
    }
  })
  
  consent_summary <- reactive({
    req(input$grouper)
    if(input$groupby == TRUE){
      summary_PT(df, c(consent, !!!rlang::syms(input$grouper)), enrolled, "Yes", TRUE, naming_convention = TRUE) %>% map_df(rev)
    } else {
      summary_PT(df, "consent", enrolled, "Yes", TRUE, naming_convention = TRUE) %>% map_df(rev)
    }
  })
  
  parent_gender_summary <- reactive({
    req(input$grouper)
    if(input$groupby == TRUE){
      summary_PT(df, c(parent_gender, !!!rlang::syms(input$grouper)), consent, "Yes", TRUE, naming_convention = TRUE)
    } else {
      summary_PT(df, parent_gender, consent, "Yes", TRUE, naming_convention = TRUE)
    }
  })
  child_gender_summary <- reactive({
    req(input$grouper)
    if(input$groupby == TRUE){
      summary_PT(df, c(child_gender, (!!!rlang::syms(input$grouper))), consent, "Yes", TRUE, naming_convention = TRUE)
    } else {
      summary_PT(df, child_gender, enrolled, "Yes", TRUE, naming_convention = TRUE)
    }
  })
  child_age_group_summary <- reactive({
    req(input$grouper)
    if(input$groupby == TRUE){
      summary_PT(df, c(child_age_group, (!!!rlang::syms(input$grouper))), enrolled, "Yes", TRUE, naming_convention = TRUE)
    } else {
      summary_PT(df, child_age_group, consent, "Yes", TRUE, naming_convention = TRUE)
    }
  })
  parent_child_relationship_summary <- reactive({
    req(input$grouper)
    if(input$groupby == TRUE){
      summary_PT(df, c(parent_child_relationship, !!!rlang::syms(input$grouper)), consent, "Yes", TRUE, naming_convention = TRUE)
    } else {
      summary_PT(df, parent_child_relationship, consent, "Yes", TRUE, naming_convention = TRUE)
    }
  })
  child_living_with_disabilities_summary <- reactive({
    req(input$grouper)
    if(input$groupby == TRUE){
      summary_PT(df, c(child_living_with_disabilities, !!!rlang::syms(input$grouper)), consent, "Yes", TRUE, naming_convention = TRUE)
    } else {
      summary_PT(df, child_living_with_disabilities, consent, "Yes", TRUE, naming_convention = TRUE)
    }
  })
  parent_relationship_status_summary <-  reactive({
    req(input$grouper)
    if(input$groupby == TRUE){
      summary_PT(df, c(parent_relationship_status, !!!rlang::syms(input$grouper)), consent, "Yes", TRUE, naming_convention = TRUE)
    } else {
      summary_PT(df, parent_relationship_status, consent, "Yes", TRUE, naming_convention = TRUE)
    }
  })
  active_users_24hr_summary <- reactive({
    req(input$grouper)
    if(input$groupby == TRUE){
      summary_PT(df, c(active_users_24hr, !!!rlang::syms(input$grouper)), program, together = TRUE, naming_convention = TRUE)
    } else {
      summary_PT(df, active_users_24hr, program, together = TRUE, naming_convention = TRUE)
    }
  })
  active_users_7d_summary <- reactive({
    req(input$grouper)
    if(input$groupby == TRUE){
      summary_PT(df, c(active_users_7d, !!!rlang::syms(input$grouper)), program, together = TRUE, naming_convention = TRUE)
    } else {
      summary_PT(df, active_users_7d, program, together = TRUE, naming_convention = TRUE)
    }
  })
  
  comp_prog_summary <- reactive({
    req(input$grouper)
    if(input$groupby == TRUE){
      comp_prog_df <- df %>% group_by(!!!rlang::syms(input$grouper)) %>%
        filter(consent == "Yes") %>%
        summarise(program_completion_mean = round(mean(n_skills, na.rm = TRUE), 2),
                  program_completion_sd = round(sd(n_skills, na.rm = TRUE), 2))
    } else {
      comp_prog_df <- df %>% summarise(program_completion_mean = round(mean(n_skills, na.rm = TRUE), 2),
                                       program_completion_sd = round(sd(n_skills, na.rm = TRUE), 2))
    }
    colnames(comp_prog_df) <- naming_conventions(colnames(comp_prog_df))
    comp_prog_df
  })
  
  parent_age_summary <- reactive({
    req(input$grouper)
    if(input$groupby == TRUE){
      parent_age_df <- df %>% group_by(!!!rlang::syms(input$grouper)) %>%
        filter(consent == "Yes") %>%
        summarise(parent_age_mean = round(mean(parent_age, na.rm = TRUE), 2),
                  parent_age_sd = round(sd(parent_age, na.rm = TRUE), 2))
    } else {
      parent_age_df <- df %>% summarise(parent_age_mean = round(mean(parent_age, na.rm = TRUE), 2),
                                        parent_age_sd = round(sd(parent_age, na.rm = TRUE), 2))
    }
    colnames(parent_age_df) <- naming_conventions(colnames(parent_age_df))
    parent_age_df
  })
  # Note: These are the *number* of people that have completed the survey
  completed_survey_summary <- reactive({
    req(input$grouper)
    if(input$groupby == TRUE){
      completed_survey <- df %>% group_by(!!!rlang::syms(input$grouper)) %>%
        filter(consent == "Yes") %>%
        summarise(completed_survey_wk1 = sum(!is.na(survey_completed_wk1)),
                  completed_survey_wk2 = sum(!is.na(survey_completed_wk2)),
                  completed_survey_perc_wk1 = sum(!is.na(survey_completed_wk1))/nrow(.)*100,
                  completed_survey_perc_wk2 = sum(!is.na(survey_completed_wk2))/nrow(.)*100)
    } else {
      completed_survey <- df %>%
        filter(consent == "Yes") %>%
        summarise(completed_survey_wk1 = sum(!is.na(survey_completed_wk1)),
                  completed_survey_wk2 = sum(!is.na(survey_completed_wk2)),
                  completed_survey_perc_wk1 = sum(!is.na(survey_completed_wk1))/nrow(.)*100,
                  completed_survey_perc_wk2 = sum(!is.na(survey_completed_wk2))/nrow(.)*100)
    }
    completed_survey <- completed_survey %>%
      mutate("Completed survey week 1 (%)" := str_c(`completed_survey_wk1`, ' (', round(`completed_survey_perc_wk1`, 1), ")")) %>%
      mutate("Completed survey week 2 (%)" := str_c(`completed_survey_wk2`, ' (', round(`completed_survey_perc_wk2`, 1), ")")) %>%
      dplyr::select(-c(completed_survey_wk1, completed_survey_wk2, completed_survey_perc_wk1, completed_survey_perc_wk2))
    colnames(completed_survey) <- naming_conventions(colnames(completed_survey))
    completed_survey
  })
  
  supportive_calm_flow_df <- flow_data_function(supportive_calm)
  if (is.null(supportive_calm_flow_df)) { supportive_calm_flow_df <- data.frame(response = c("No", "Yes"), count = c(NA, NA)); colnames(supportive_calm_flow_df)[2] <- "Count (%)"}
  supportive_praise_flow_df <- flow_data_function(supportive_praise)
  if (is.null(supportive_praise_flow_df)) { supportive_praise_flow_df <- data.frame(response = c("No", "Yes"), count = c(NA, NA)); colnames(supportive_praise_flow_df)[2] <- "Count (%)"}
  supportive_flow_df <- flow_data_function(supportive_flow_names)
  check_in_flow_df <- flow_data_function(check_in_flow_names)
  content_flow_df <- flow_data_function(content_tip_flow_names)
  
  response_message_overall <- reactive({
    all_flows_df <- rbind(content_flow_df, check_in_flow_df, supportive_calm_flow_df, supportive_praise_flow_df, supportive_flow_df)
    all_flows_df <- separate(all_flows_df, `Count (%)`, into = "Value") %>% mutate(Value = as.numeric(as.character(Value)))
    all_flows_df_total <- sum(all_flows_df$Value, na.rm = TRUE) 
    all_flows_df_summary <- all_flows_df %>% group_by(response) %>% summarise(n = sum(Value, na.rm = TRUE),
                                                                              perc = n/all_flows_df_total*100)
    all_flows_df_summary <- all_flows_df_summary %>%
      mutate("Count (%)" := str_c(`n`, ' (', round(`perc`, 1), ")")) %>%
      dplyr::select(-c(n, perc))
    colnames(all_flows_df_summary)[1] <- c("Overall response")
    all_flows_df_summary %>% map_df(rev)
  })
  
  all_flows_response <- reactive({
    colnames(content_flow_df)[2] <- "Content flows"
    colnames(check_in_flow_df)[2] <- "Check-in flows"
    colnames(supportive_calm_flow_df)[2] <- "Calm flow"
    colnames(supportive_praise_flow_df)[2] <- "Praise flow"
    colnames(supportive_flow_df)[2] <- "Other supportive flows"
    
    
    
    table_flows_df <- left_join(left_join(left_join(left_join(content_flow_df, check_in_flow_df), supportive_calm_flow_df), supportive_praise_flow_df), supportive_flow_df)
    colnames(table_flows_df)[1] <- "Response"
    table_flows_df
  })
  
  output$plot_flow <- renderPlotly({
    colnames(supportive_calm_flow_df)[2] <- "Supportive - Calm flow"
    colnames(supportive_praise_flow_df)[2] <- "Supportive - Praise flow"
    colnames(supportive_flow_df)[2] <- "Supportive flow"
    colnames(check_in_flow_df)[2] <- "Check-in flows"
    colnames(content_flow_df)[2] <- "Content flows"
    table_flows_df <- left_join(left_join(left_join(left_join(supportive_calm_flow_df, supportive_praise_flow_df), supportive_flow_df), check_in_flow_df), content_flow_df)
    colnames(table_flows_df)[1] <- "Response"
    flow_pivot <- pivot_longer(table_flows_df,
                               cols = c(`Supportive - Calm flow`, `Supportive - Praise flow`, `Supportive flow`, `Check-in flows`, `Content flows`),
                               names_to = "Flow name",
                               values_to = "Value") %>%
      separate(Value, into = "Value") %>%
      mutate(Value = as.numeric(as.character(Value)))
    
    ggplot(flow_pivot, aes(x = Response, y = Value, fill = `Flow name`)) +
      geom_bar(stat = "identity") +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy")
  })
  
  df_enrolled <- summary_PT(df, enrolled, enrolled, "Yes")
  df_enrolled <- df_enrolled %>% mutate(group = enrolled, count = enrolled_n) %>% dplyr::select(c(group, count))
  
  df_consented <- summary_PT(df, consent, consent, "Yes")
  df_consented <- df_consented %>% mutate(group = consent, count = consent_n) %>% dplyr::select(c(group, count))
  
  df_program <- summary_PT(df, program, program, "Yes")
  df_program <- df_program %>% mutate(group = program, count = program_n) %>% dplyr::select(c(group, count))
  
  df_active_24 <- (summary_PT(df, active_users_24hr, program) %>% filter(active_users_24hr == "Yes"))$active_users_24hr_n
  df_active_7d <- (summary_PT(df, active_users_7d, program) %>% filter(active_users_7d == "Yes"))$active_users_7d_n

  output$myvaluebox1 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(df_enrolled$count,subtitle = "Enrolled",icon = icon("user"),
                             color = "aqua"
    )
  })
  output$myvaluebox2 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(df_consented$count,subtitle = "Consented",icon = icon("check"),
                             color = "green"
    )
  })
  output$myvaluebox3 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(df_program$count,subtitle = "In Program",icon = icon("clipboard"),
                             color = "yellow"
    )
  })
  output$myvaluebox4 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(df_active_24,subtitle = "Active in last 24 hours",icon = icon("clock"),
                             color = "purple"
    )
  })
  output$myvaluebox5 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(df_active_7d, subtitle = "Active in last 7 days",icon = icon("signal"),
                             color = "fuchsia"
    )
  })
  #output$enrolled_summary <- shiny::renderTable({(enrolled_summary())})
  output$consent_summary <- shiny::renderTable({(consent_summary())})
  output$parent_gender_summary <- shiny::renderTable({(parent_gender_summary())})
  output$parent_age_summary <- shiny::renderTable({(parent_age_summary())})
  output$child_gender_summary <- shiny::renderTable({(child_gender_summary())})
  output$child_age_group_summary <- shiny::renderTable({(child_age_group_summary())})
  output$parent_child_relationship_summary <- shiny::renderTable({(parent_child_relationship_summary())})
  output$parent_relationship_status_summary <- shiny::renderTable({(parent_relationship_status_summary())})
  output$child_living_with_disabilities_summary <- shiny::renderTable({(child_living_with_disabilities_summary())})
  output$active_users_24hr_summary <- shiny::renderTable({(active_users_24hr_summary())})
  output$active_users_7d_summary <- shiny::renderTable({(active_users_7d_summary())})
  output$comp_prog_summary <- shiny::renderTable({(comp_prog_summary())}, caption = "Number of skills in toolkit")
  output$completed_survey_summary <- shiny::renderTable({{completed_survey_summary()}})
  output$all_flows_response <- shiny::renderTable({(all_flows_response())}, caption = "Count (%) for each flow")
  output$response_message_overall <- shiny::renderTable({(response_message_overall())})
}

# Create Shiny object
shinyApp(ui = ui, server = server)
