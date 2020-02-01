library(shiny)
library(tesseract)
library(shinyjs)
library(shinycssloaders)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "icon", type = "image/png", href = "Logo.png")
  ),
  useShinyjs(),
  navbarPage(title = "MyClinData", id = 'tabs',
             tabPanel('Upload Data', value = 'uploadData',
                      sidebarLayout(
                        sidebarPanel(h4("Upload Data"),
                                     p("This is where we will implement Tesseract OCR")), 
                        mainPanel(p("Here too!")))
                      ),
             tabPanel('Graphical Display', value = 'graphicalDisplay',
                      sidebarLayout(
                        sidebarPanel(h4("Graphical Display"),
                                     p("This will be a graphical display")), 
                        mainPanel(p("Here too!")))
                      )
  )
)

server <- function(input, output) {
  
}
  
shinyApp(ui = ui, server = server)
