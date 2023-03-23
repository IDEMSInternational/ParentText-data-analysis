
setwd("~/GitHub/ParentText-data-analysis/R Code")

ui <- fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      img(src = "rstudio.png", height = 140, width = 400)
    )
  )
)

server <- function(input, output, session) {
  message("You can do it!")
}

shinyApp(ui, server)