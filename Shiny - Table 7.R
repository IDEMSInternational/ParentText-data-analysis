# Load packages
library(shiny)
library(shinythemes)
library(shinyjs)
#library(rpivotTable)
library(plotly)
library(shinydashboard)

# RapidPro set up --------------------------------------------------------------
# for this to work you need to change the directory to where the token key is stored.
key <- read.table("C:/Users/lzc1n17/OneDrive - University of Southampton/PhD/IDEMS/ParentText/PT_malaysia_key.txt", quote="\"", comment.char="")
set_rapidpro_key(key = key)
set_rapidpro_site(site = "https://app.rapidpro.io/api/v2/")
set_rapidpro_uuid_names()
contacts_unflat <- get_user_data(flatten = FALSE)

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
                                      parent_gender)))
child_age_group <- factor(contacts_unflat$fields$age_group_for_tips)
child_age_group <- forcats::fct_recode(child_age_group,
                                       Baby = "baby",
                                       Child = "child",
                                       Teen = "teen")
child_gender <- factor(contacts_unflat$fields$survey_behave_sex)
child_gender <-  forcats::fct_recode(child_gender,
                                     Boy = "male",
                                     Girl = "female", 
                                     `Prefer not to say` = "no")

parent_child_relationship <- factor(contacts_unflat$fields$survey_behave_relationship)
parent_child_relationship <- forcats::fct_recode(parent_child_relationship,
                                                 Parent = "parent",
                                                 Grandparent = "grandparent",
                                                 `Aunt/Uncle`= "uncle",
                                                 `Foster Parent` = "foster",
                                                 Other = "other",
                                                 `Prefer Not to Say`  = "no")

df <- data.frame(enrolled, consent, program, parent_gender, child_gender, child_age_group, parent_child_relationship)

# Calculations -----------------------------------------------------------------
# active users # N = contacts for which the time difference between the current time and the datetime variable "last_seen_on" is less than 24 h 
active_users_24hr <- Sys.time() - as.POSIXct(contacts_unflat$last_seen_on, format="%Y-%m-%dT%H:%M:%OS") <= 1
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


active_users_7d <- Sys.time() - as.POSIXct(contacts_unflat$last_seen_on, format="%Y-%m-%dT%H:%M:%OS") <= 7
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
df <- data.frame(df, active_users_24hr, active_users_7d)


# comp_prog_overall
# TODO: only should be lookign at this for those who consented
n_skills <- as.numeric(as.character(contacts_unflat$fields$n_skills))
df <- data.frame(df, n_skills)

# Participant age etc
# TODO: only should be lookign at this for those who consented
parent_age <- as.numeric(as.character(contacts_unflat$fields$age))
df <- data.frame(df, parent_age)

# sum of response to content, calm, check in, supportive, praise messages
# supportive
supportive_flow_names <- c("PLH - Supportive - Family", "PLH - Supportive - Help reminder", "PLH - Supportive - Share", "PLH - Supportive - Share - Enrollment", "GG - PLH - Supportive - Share - Enrollment", "PLH - Supportive - Budget", "PLH - Supportive - Activities for babies", "PLH - Supportive - Activities",
                           "PLH - Supportive - Behave reminder", "PLH - Supportive - Children reminder", "PLH - Supportive - Covid", "PLH - Supportive - Development", "PLH - Supportive - Calm", "PLH - Supportive - Disabilities")

check_in_flow_names <- c("PLH - Content - Extra - CheckIn - COVID", "PLH - Content - Positive - CheckIn - Book sharing", "PLH - Content - Positive - CheckIn - Budget adults", "PLH - Content - Positive - CheckIn - Budget with children", "PLH - Content - Positive - CheckIn - Community safety", "PLH - Content - Positive - CheckIn - Consequences", "PLH - Content - Positive - CheckIn - Crisis", "PLH - Content - Positive - CheckIn - Crying", "PLH - Content - Positive - CheckIn - Education", "PLH - Content - Positive - CheckIn - Emotion", "PLH - Content - Positive - CheckIn - Family", "PLH - Content - Positive - CheckIn - Ignore",
                         #"PLH - Content - Positive - CheckIn - Instructions",
                         "PLH - Content - Positive - CheckIn - IPV 1", "PLH - Content - Positive - CheckIn - IPV 2", "PLH - Content - Positive - CheckIn - IPV 3", "PLH - Content - Positive - CheckIn - IPV 4", "PLH - Content - Positive - CheckIn - IPV 5", "PLH - Content - Positive - CheckIn - Online adults", "PLH - Content - Positive - CheckIn - Online children", "PLH - Content - Positive - CheckIn - Praise", "PLH - Content - Positive - CheckIn - ProblemSolving", "PLH - Content - Positive - CheckIn - Redirect", "PLH - Content - Positive - CheckIn - Routines", "PLH - Content - Positive - CheckIn - Rules", "PLH - Content - Positive - CheckIn - Safe or unsafe touch", "PLH - Content - Relax - CheckIn - Anger management", "PLH - Content - Relax - CheckIn - Connect", "PLH - Content - Relax - CheckIn - List of things",
                         "PLH - Content - Relax - CheckIn - Loving Kindness", "PLH - Content - Relax - CheckIn - Notice how you feel", "PLH - Content - Relax - CheckIn - Three is a magical number", "PLH - Content - Time - CheckIn - One on one time")

# retention_exit ---------------------------------------------------------------
# number of contacts for which the contact field "exit_message" is not empty &
# they are NOT in the group "in program"
#df %>% filter(program == "No") %>% nrow(.)
#contacts_unflat$fields$exit_message

# Define UI
ui <- dashboardPage(
  header = dashboardHeader(title = "Table 7 Dashboard"),
  sidebar = dashboardSidebar(
    checkboxInput(inputId = "groupby", label = strong("Group by variables"), value = TRUE),
    uiOutput("groups")),
  body = dashboardBody(
    fluidRow(
      column(
        width = 12,
        align = "center",
        fluidRow(
          tags$head(tags$style(HTML(".small-box {height: 120px}"))),
          uiOutput("mstatvalbox"),
          
        ) #fluid row closure
      ) #Outer column closure
    ),
    #shiny::tableOutput("enrolled_summary"),
        shiny::tableOutput("consent_summary"),
plotlyOutput(outputId = "plot_category", height = "300px", width = "50%"),
    shiny::tableOutput("parent_gender_summary"),
    shiny::tableOutput("child_gender_summary"),
    shiny::tableOutput("child_age_group_summary"),
    shiny::tableOutput("parent_child_relationship_summary"),
    shiny::tableOutput("active_users_24hr_summary"),
    shiny::tableOutput("active_users_7d_summary"),
    shiny::tableOutput("comp_prog_summary"),
    shiny::tableOutput("parent_age_summary"),
    shiny::tableOutput("check_in_response"),
    shiny::tableOutput("supportive_response")#fluidRow closure
  )  #dashboard Body closure
)  #dashboard Page closure

# Define server function
server <- function(input, output) {
  
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
          labs(x = "Enrolled", y = "Count", title = "Enrollment frequency")
        
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
          labs(x = "Enrolled", y = "Count", title = "Enrollment frequency")
      }
  })
  
  consent_summary <- reactive({
    req(input$grouper)
    if(input$groupby == TRUE){
      summary_PT(df, c(consent, !!!rlang::syms(input$grouper)), enrolled, "Yes")
    } else {
      summary_PT(df, consent, enrolled, "Yes")
    }
  })

  parent_gender_summary <- reactive({
    req(input$grouper)
    if(input$groupby == TRUE){
      summary_PT(df, c(parent_gender, !!!rlang::syms(input$grouper)), consent, "Yes")
    } else {
      summary_PT(df, parent_gender, consent, "Yes")
    }
  })
  child_gender_summary <- reactive({
    req(input$grouper)
    if(input$groupby == TRUE){
      summary_PT(df, c(child_gender, (!!!rlang::syms(input$grouper))), consent, "Yes")
    } else {
      summary_PT(df, child_gender, enrolled, "Yes")
    }
  })
  child_age_group_summary <- reactive({
    req(input$grouper)
    if(input$groupby == TRUE){
      summary_PT(df, c(child_age_group, (!!!rlang::syms(input$grouper))), enrolled, "Yes")
    } else {
      summary_PT(df, child_age_group, consent, "Yes")
    }
  })
  parent_child_relationship_summary <- reactive({
    req(input$grouper)
    if(input$groupby == TRUE){
      summary_PT(df, c(parent_child_relationship, !!!rlang::syms(input$grouper)), consent, "Yes")
    } else {
      summary_PT(df, parent_child_relationship, consent, "Yes")
    }
  })
  active_users_24hr_summary <- reactive({
    req(input$grouper)
    if(input$groupby == TRUE){
      summary_PT(df, c(active_users_24hr, !!!rlang::syms(input$grouper)), program)
    } else {
      summary_PT(df, active_users_24hr, program)
    }
  })
  active_users_7d_summary <- reactive({
    req(input$grouper)
    if(input$groupby == TRUE){
      summary_PT(df, c(active_users_7d, !!!rlang::syms(input$grouper)), program)
    } else {
      summary_PT(df, active_users_7d, program)
    }
  })
  
  comp_prog_summary <- reactive({
    req(input$grouper)
    if(input$groupby == TRUE){
      df %>% group_by(!!!rlang::syms(input$grouper)) %>%
        filter(consent == "Yes") %>%
    summarise(comp_prog_mean = round(mean(n_skills, na.rm = TRUE), 2),
              comp_prog_sd = round(sd(n_skills, na.rm = TRUE), 2))
    } else {
      df %>% summarise(comp_prog_mean = round(mean(n_skills, na.rm = TRUE), 2),
                       comp_prog_sd = round(sd(n_skills, na.rm = TRUE), 2))
    }
  })
  parent_age_summary <- reactive({
    req(input$grouper)
    if(input$groupby == TRUE){
      df %>% group_by(!!!rlang::syms(input$grouper)) %>%
        filter(consent == "Yes") %>%
        summarise(parent_age_mean = round(mean(parent_age, na.rm = TRUE), 2),
                  parent_age_sd = round(sd(parent_age, na.rm = TRUE), 2))
    } else {
      df %>% summarise(parent_age_mean = round(mean(parent_age, na.rm = TRUE), 2),
                       parent_age_sd = round(sd(parent_age, na.rm = TRUE), 2))
    }
  })
  
  supportive_response <- reactive({
    flow_data_function(supportive_flow_names)
  })
  check_in_response <- reactive({
    flow_data_function(check_in_flow_names)
  })

  #output$enrolled_summary <- shiny::renderTable({(enrolled_summary())})
  output$consent_summary <- shiny::renderTable({(consent_summary())})
  output$parent_gender_summary <- shiny::renderTable({(parent_gender_summary())})
  output$child_gender_summary <- shiny::renderTable({(child_gender_summary())})
  output$child_age_group_summary <- shiny::renderTable({(child_age_group_summary())})
  output$parent_child_relationship_summary <- shiny::renderTable({(parent_child_relationship_summary())})
  output$active_users_24hr_summary <- shiny::renderTable({(active_users_24hr_summary())})
  output$active_users_7d_summary <- shiny::renderTable({(active_users_7d_summary())})
  output$comp_prog_summary <- shiny::renderTable({(comp_prog_summary())})
  output$parent_age_summary <- shiny::renderTable({(parent_age_summary())})
  output$check_in_response <- shiny::renderTable({(check_in_response())})
  output$supportive_response <- shiny::renderTable({(supportive_response())})
  
  output$mstatvalbox <- renderUI({
    req(input$grouper)
    mstatvalbox <- lapply(1:4, function(i) {
      
      #getting multicolor based on value of i
      if (i == 1){
        mcol ="aqua"
      }
      else if (i == 2){
        mcol ="green"
      }
      else if (i == 3){
        mcol ="yellow"
      }
      else if (i == 4){
        mcol ="purple"
      }
      #else if (i == 5){
      #  mcol ="orange"
      #}
      
      df_enrolled <- summary_PT(df, enrolled, enrolled, "Yes")
      df_enrolled <- df_enrolled %>% mutate(group = enrolled, count = enrolled_n) %>% dplyr::select(c(group, count))
      
      df_consented <- summary_PT(df, consent, consent, "Yes")
      df_consented <- df_consented %>% mutate(group = consent, count = consent_n) %>% dplyr::select(c(group, count))
      
      df_program <- summary_PT(df, program, program, "Yes")
      df_program <- df_program %>% mutate(group = program, count = program_n) %>% dplyr::select(c(group, count))

      df_active_24 <- (summary_PT(df, active_users_24hr, program) %>% filter(active_users_24hr == "Yes"))$active_users_24hr_n
      #df_active_7 <- (summary_PT(df, active_users_7d, program) %>% filter(active_users_7d == "Yes"))$active_users_7d_n
      
      df_all <- rbind(df_enrolled, df_consented, df_program, df_active_24)#, df_active_7)
      df_all[1] <- c("Enrolled", "Consented", "In Program", "Active in last 24 hours")#, "Active in last 7 days") 
      
      #if(input$grouper == "child_gender"){
      #  df2 <- summary_PT(df, child_gender, consent, "Yes")
      #  df2 <- df2 %>% mutate(group = child_gender, count = child_gender_n, perc = child_gender_perc) %>% dplyr::select(c(group, count, perc))
      #  df_all <- rbind(df3, df2)
      #  df_all[1] <- c("Total Enrolled and Consented", "Men", "Women", "Unknown gender") 
      #} else if(input$grouper == "child_age_group"){
      #  df2 <- summary_PT(df, child_age_group, consent, "Yes")
      #  df2 <- df2 %>% mutate(group = child_age_group, count = child_age_group_n, perc = child_age_group_perc) %>% dplyr::select(c(group, count, perc))
      #  df2 <- rbind(df2, c("NA", 0, 0))
      #  df_all <- rbind(df3, df2)
      #  df_all[1] <- c("Total Enrolled and Consented", "Men", "Women", "Unknown gender") 
      #} else {
      #  df2 <- summary_PT(df, parent_gender, consent, "Yes")
      #  df2 <- df2 %>% mutate(group = parent_gender, count = parent_gender_n, perc = parent_gender_perc) %>% dplyr::select(c(group, count, perc))
      #  df_all <- rbind(df3, df2)
      #  df_all[1] <- c("Total Enrolled and Consented", "Men", "Women", "Unknown gender") 
      #}
      
      inputName <- paste0("M",df_all[i,1])
      div(id=inputName,
          column(width = 3,
                 valueBox(
                   value = tags$p(df_all[i,2],style = "font-size: 150%;"),
                   subtitle = tags$p(df_all[i,1], style = "font-size: 85%;"),
                   color =mcol ,
                   width = "600px")),
          
          
          tags$style(".popover-title{
         background-color:#8A7B57;
         color:white;
         text-align:center;
         font-size:18px;
         overflow-wrap:break-word;
         }"),
          
          tags$style(".popover{
         background-color:#FFF2B3;
         text-align:left;
         font-size:15px;
         border:solid;  # see here for boarder design: https://www.w3schools.com/css/css_border.asp
         border-radius:unset;
         border-color: black;
         border-width: 2px;
         min-width:350px;
         #width:300px;
         # max-width:800px;
         height:325px;
         # max-height:600px;
         overflow-wrap:break-word;
         }"),
          
      )
      
    })
    
    # Create a tagList of sliders (this is important)
    do.call(tagList, mstatvalbox)
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)
