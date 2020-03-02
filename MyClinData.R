library(shiny)
library(magick)
library(tesseract)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(shinythemes)


ui <- fluidPage (
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "icon", type = "image/png", href = "Logo.png")
  ),
  useShinyjs(),
  #shinythemes::themeSelector(),
  navbarPage(theme = shinytheme("flatly"), title = "MyClinData", id = 'tabs',
             tabPanel('Upload Data', value = 'uploadData',
                      sidebarLayout(
                        sidebarPanel(
                          tags$div(id = "headertitle",
                                   tags$h2("Shiny Tesseract"),
                                   tags$div(id="tesseract")),
                          tags$div(
                            id = "header",
                            fileInput(
                              "upload",
                              "Upload image",
                              accept = c('image/png', 'image/jpeg'),
                              buttonLabel = "BROWSE",
                              placeholder = "No Image"
                            ),
                            tags$div(style  = "font-size:18px;",
                                     textInput("size", "Size", value = "200x400")),
                            actionButton("rotateButton", "Rotate Clockwise 90\u00b0",
                                         icon("sync"))
                          ),
                          tags$h4("Selected Area"),
                          verbatimTextOutput("coordstext"),
                          tags$h4("Select Test"),
                          selectInput(inputId = "testType", label = "Tests:", choices = c("CBC (Complete Blood Count)", "CMP (Complete Metabolic Panel)"))
                        ), 
                        mainPanel(      
                          skin = "black",
                          tags$head(
                            tags$link(rel  = "stylesheet",
                                      type = "text/css",
                                      href = "https://fonts.googleapis.com/css?family=Muli|Work+Sans"),
                            tags$link(rel  = "stylesheet",
                                      type = "text/css",
                                      href = "theme.css")
                            ),
                          fluidRow(column(width = 12,
                                          box(width = 12,
                                              imageOutput(
                                                "image",
                                                click = "image_click",
                                                hover = hoverOpts(
                                                  id        = "image_hover",
                                                  delay     = 500,
                                                  delayType = "throttle"),
                                                brush = brushOpts(
                                                  id        = "image_brush",
                                                  fill      = "#F5A623",
                                                  stroke    = "#F5A623",
                                                  clip      = FALSE),
                                                height = "auto"
                                                ),
                                              title = "Click & Drag Over Image")
                                        )),
                                        fluidRow(column(width = 12,
                                                        box(width = 12,
                                                            textOutput("ocr_text"),
                                                            verbatimTextOutput("text_extract"),
                                                            title = "Text Output"
                                                        )
                                        )))
                      )
             ),
             tabPanel(
               title = "Verification",
               sidebarLayout(
                 sidebarPanel("Display the cropped image"),
                 mainPanel("Display an interactive table with the parsed data")
               )
             ),  tabPanel(
               title = "Graphical Display"
             ),  tabPanel(
               title = "Content"
             )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(data=NULL, rotate = NULL, rotatedImage = NULL)
  
  image <- image_read("DefaultImage.png")
  
  imageData <- NULL
    
  observeEvent(input$upload, {
    if (length(input$upload$datapath)) {
      image <<- image_read(input$upload$datapath)
      info   <- image_info(image)
      updateTextInput(session, "size", value = paste(info$width, info$height, sep = "x"))
    }
  })
  
  output$image_brushinfo <- renderPrint({
    cat("Selected:\n")
    str(input$image_brush$coords_css)
  })
  
  output$image <- renderImage({
    width   <- session$clientData$output_image_width
    height  <- session$clientData$output_image_height
    img <- image %>% image_resize(input$size) 
    if (!is.null(rv$rotate)){
      img <- image_rotate(img, 90 * input$rotateButton)
      rv$rotate = NULL
      info <- image_info(img)
      updateTextInput(session, "size", value = paste(info$width, info$height, sep = "x"))
    }
    image <<- img
    img <- image_write(img, tempfile(fileext = 'jpg'), format = 'jpg') 
    list(src = img, contentType = "image/jpeg")
  })
  
  
  coords <- reactive({
    w   <- round(input$image_brush$xmax - input$image_brush$xmin, digits = 2)
    h   <- round(input$image_brush$ymax - input$image_brush$ymin, digits = 2)
    dw  <- round(input$image_brush$xmin, digits = 2)
    dy  <- round(input$image_brush$ymin, digits = 2)
    coords <- paste0(w, "x", h, "+", dw, "+", dy)
    return(coords)
    # "500x300+10+20" – Crop image to 500 by 300 at position 10,20
  })
  
  output$coordstext <- renderText({
    if (is.null(input$image_brush$xmin)) {
      "No Area Selected!"
    } else {
      coords()}
  })
  
  output$ocr_text <- renderText({
    req(input$image_brush)
    image <- image
    text   <- image %>% #image_resize(paste(image$width, image$height, sep = "x")) %>%
      image_crop(coords(), repage = FALSE) %>%
      image_ocr()
    imageData <<- text
    output <- text
    return(output)
  })
  
  observeEvent(input$rotateButton, {
    rv$rotate = 90
  })
}

shinyApp(ui, server)