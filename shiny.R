library(shiny)
library(knitr)
library(purrrlyr)
library(kableExtra)

source("utils.R")
source("ui_functions.R")
source("calculations.R")
source("mortgage.R")

ui <- shinyUI(
  
  fluidPage(
    titlePanel(""),
    sidebarLayout(
      sidebarPanel(
        get_input_sliders()
      ),
      mainPanel(
        uiOutput('markdown')
      )
    )
  
  )
  

)

server <- function(input, output, session) { 
  
  get_env <- callModule(environment_generator, "my_module", input)
  
  output$markdown <- renderUI({
    
    my_env <- get_env()
    
    rmdfiles <- c("my_markdown.Rmd")
    knitr::knit("my_markdown.Rmd", quiet = TRUE, envir = my_env)
    
    HTML(markdown::markdownToHTML("my_markdown.md", stylesheet = "", encoding="utf-8"))
  })
  
}

shinyApp(ui, server)