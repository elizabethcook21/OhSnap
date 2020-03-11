library(shiny)
library(magick)
library(tesseract)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(shinythemes)
library(rhandsontable)
library(stringr)
library(ggplot2)
library(plotly)
library(googleAuthR)
library(googlesheets4)
# library(xlsx)
library(zip)
library(readxl)
library(writexl)

# Global variables and functions ----------------
options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/userinfo.email",
                                        "https://www.googleapis.com/auth/userinfo.profile"))
options("googleAuthR.webapp.client_id" = "543814214955-9u26dmgeaoo8p03fna1gc11ond5md1ta.apps.googleusercontent.com")
options("googleAuthR.webapp.client_secret" = "4mbPAzE7UFZjTFYGcjPS1MYS")

#as adapted from 'image_ocr' in package:magick
data_selection_ocr <- function (image, whitelist = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789.^%[]/-", HOCR = FALSE, ...) 
{
  #assert_image(image)
  allowed_chars = tesseract(options = list(tessedit_char_whitelist = whitelist))
  ocr(image, engine = allowed_chars, HOCR = HOCR)
}

dataTypes = list(CMP = c("WBC", "RBC", "HGB", "HCT", "MCV", "MCH", "MCHC", "PLT", "RDW-SD", "RDW-CV", "MPV", "NEUT", "LYMPH", "MONO", "EO", "BASO"), 
                 CBC = c("Na", "K","Cl", "ECO2", "AGAP", "AHOL", "TBI", "TP", "GLOB", "ALPI","TGL", "CHOL", "AST", "ALTI", "ALB", "A/G", "GLUC", "BUN", "CA", "CRE2", "BN/CR"))


# ui ----------
ui <- fluidPage (
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "icon", type = "image/png", href = "Logo.png")
  ),
  useShinyjs(),
  #shinythemes::themeSelector(),
  navbarPage(theme = shinytheme("flatly"), title = "MyClinData", id = 'tabs',
             # * login tab ----------
             tabPanel(
               title = "Login",  value = "login",
               fluidRow(
                 align = "center",
                 br(), br(),
                 tags$img(src = 'TempLogo.jpg', align = "center", height = "300px"),
                 br(),
                 actionButton("personal", label = "Store on Personal Computer"),
                 br(), br(),
                 googleAuthUI("gauth_login"))   
             ),
             # * upload data tab ----------
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
                          actionButton("goToVerificationTab", "Next")
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
                          ))
                        )
                      )
             ),
             # * verifiction tab ----------
             tabPanel(
               title = "Verification", value = "verification",
               sidebarLayout(
                 sidebarPanel(imageOutput("croppedImage"),
                              actionButton("goToGraphsTab", "Next")),
                 mainPanel(rHandsontableOutput("verificationTable"))
               )
             ),  
             # * graphical display tab ----------
             tabPanel(
               title = "Graphical Display", value = "graphs",
               sidebarLayout(
                 sidebarPanel(
                   tags$h2("View Your Data"),
                   uiOutput("plotDataType")
                 ),
                 mainPanel(
                   #tags$div(class = "plot",
                  #          plotlyOutput("plot")
                  # )
                 )
               )
             ),  
             # * contact tab ----------
             tabPanel("Contact", value = "contact",
                      sidebarLayout(
                        sidebarPanel(HTML('<center><img src="BYULogo.png" width="170"></center>')
                        ),
                        mainPanel(
                          h4("Contact"),
                          HTML(paste('<div>This app was developed by Erica Suh, Ed Ringger, Tyler Heaton, and Elizabeth Anderson
                                      under the direction of Dr. Samuel Payne for the 2020 Winter BYU Capstone class.
                                     For questions and comments, please visit', 
                                     '<a target="_blank", href="https://biology.byu.edu/sam-payne-lab">https://biology.byu.edu/sam-payne-lab</a>.',
                                     '<p>The source code for MyClinData can be found at', 
                                     '<a target="_blank", href="https://github.com/elizabethcook21/MyClinData">https://github.com/elizabethcook21/MyClinData</a>.</p></div>'))
                        )
                        
                      )
             )
  )
)

# server ----------
server <- function(input, output, session) {
  # global variables ----------
  rv <- reactiveValues(data=NULL, rotate = NULL, rotatedImage = NULL, selectedTestType = NULL, login = FALSE, currDF = NULL)
  
  image <- image_read("DefaultImage.png")
  
  imageData <- NULL
  
  testData = data.frame(Date = c("2019-8-14", "2019-9-23", "2019-10-25", "2019-11-22", "2019-12-19", "2020-1-31", "2020-2-14", "2020-2-21"),
                        WBC = c(6.26, 6.7, 7.05, 6.33, 5.58, 6.13, 6.18, 6.14))
  
  # Google Login Code -------------------------------------------------
  accessToken <- callModule(googleAuth, "gauth_login",
                            login_class = "btn btn-primary",
                            logout_class = "btn btn-primary")
  userDetails <- reactive({
    validate(
      need(accessToken(), "not logged in")
    )
    rv$login <- TRUE
    with_shiny(get_user_info, shiny_access_token = accessToken())
  })
  
  userDetails <- reactive({
    validate(
      need(accessToken(), "not logged in")
    )
    rv$login <- TRUE
    with_shiny(get_user_info, shiny_access_token = accessToken())
  })
  
  observe({
    if (rv$login) {
      shinyjs::onclick("gauth_login-googleAuthUi",
                       shinyjs::runjs("window.location.href = 'https://yourdomain.shinyapps.io/appName';"))
      print(googledrive::drive_find(""))
    }
  })
  
  # Personal Data Login Code
  observeEvent(input$personal, {
    showModal(modalDialog(title = tags$b("Store Data on your Personal Computer"),
                          h4("First Time User:"),
                          p("If this is your first time using the app, please navigate to a location on your
                          computer that you wish to store your test results and make a folder called \"MyClinData\"."),
                          p("Once you're done, please click the following button and navigate to the folder you just made. 
                          Clicking the button will generate excel sheets that will be host the data you upload in this app."),
                          downloadButton("makeFiles", "Make Excel Files"),
                          p("Now that you've downloaded the zip fle, please extract it somewhere on your computer that you can remember, then continue
                            following the instructions for the continuing user below."),
                          h4("Continuing User:"),
                          p("Please select which type of test you are planning on uploading:"),
                          selectInput(inputId = "testType", label = "Tests:", choices = c("CBC (Complete Blood Count)", "CMP (Complete Metabolic Panel)")),
                          p("Now, please navigate to the file you wish to have your data added to and put the path in the following text box:"),
                          textInput("currFile", "File to append to:"),
                          size = "m", easyClose = FALSE))
  })
  
  observeEvent(input$currFile, {
    if(input$currFile != ""){
      print(input$currFile)
      rv$currDF <- readxl_example(input$currFile)
    }
   
  })
  

  # Make Zipped fie
  output$makeFiles <- downloadHandler(
    filename = "MyClinData.zip",
    content = function(file) {
      files <- NULL
      for (i in 1:length(dataTypes)){
        fileName <- paste(names(dataTypes[i]),".xlsx",sep = "")
        data <- c("Date", dataTypes[[i]])
        data <- rbind(data)
        write_xlsx(as.data.frame(data), paste0(fileName), col_names = FALSE, format_headers = FALSE)
        files <- c(fileName, files)
      }
      zip::zipr(file, files)
      if(file.exists(paste0(file, ".zip"))) {
        file.rename(paste0(file, ".zip"), file)
      }
    }
  )
  
  # upload data tab ------------
  observeEvent(input$upload, {
    if (length(input$upload$datapath)) {
      image <<- image_read(input$upload$datapath)
      info   <- image_info(image)
      updateTextInput(session, "size", value = paste(info$width, info$height, sep = "x"))
    }
  })
  
  observeEvent(input$testType, {
    rv$selectedTestType = str_split(input$testType, pattern = " ")[[1]][1]
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
    list(src = img, contentType = "image/jpeg")  # width = "100%" makes the image fit the size of the mainPanel, but it messes up rotation and tesseract
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
    print('parsing text')
    text   <- image %>% #image_resize(paste(image$width, image$height, sep = "x")) %>%
      image_crop(coords(), repage = FALSE) %>%
      data_selection_ocr()#image_ocr()
    imageData <<- text
    selected_text <- text
    rv$data <- str_match_all(selected_text, "([A-Za-z-]+)[\\s-]*([0-9.]+)[^\n]*")
    print(selected_text)
    return(selected_text)
  })
  
  observeEvent(input$rotateButton, {
    rv$rotate = 90
  })
  
  observeEvent(input$goToVerificationTab,{
    updateTabsetPanel(session, "tabs", selected = "verification")
  })
  
  # verification tab --------- 
  output$croppedImage = renderImage({
    croppedImg = image_crop(image, coords(), repage = FALSE)
    croppedImg = image_write(croppedImg, tempfile(fileext = 'jpg'), format = 'jpg') 
    list(src = croppedImg, width = "100%", height = "100%", contentType = "image/jpeg", alt = "This is the selected area of the original image")
  })
  
  output$verificationTable = renderRHandsontable({
    if (input$testType == "CBC (Complete Blood Count)") {
      cbc = (rv$data)[[1]][,2]
      values = (rv$data)[[1]][,3]
      clinDF = data.frame(CBC = cbc, Value = values)
    } else if (input$testType == "CMP (Complete Metabolic Panel)") {
      cmp = (rv$data)[[1]][,2]
      values = (rv$data)[[1]][,3]
      clinDF = data.frame(CMP = cmp, Value = values)
    }
    if(!is.null(clinDF))
      rhandsontable(clinDF, rowHeaders = NULL)
  })
  
  observeEvent(input$goToGraphsTab, {
    updateTabsetPanel(session, "tabs", selected = "graphs")
  }) 
  
  # graphical display tab ----------
  makePlot = function() {
    testData$Date = as.Date(testData$Date)
    fig = ggplot(testData, aes(x = Date, y = WBC, group = 1)) +
      geom_point() +
      geom_line() +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
      scale_y_discrete(limits = seq(from = floor(min(testData$WBC)), to = ceiling(max(testData$WBC)), by = 0.5)) +
      labs(title = "CMP - White Blood Cell (WBC)",
           x = "Date",
           y = "WBC (10^3/uL)") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 18),
            axis.title.x = element_text(size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12, margin = margin(b = 20)),
            axis.title.y = element_text(size = 16),
            axis.text.y = element_text(size = 12, margin = margin(l = 15)),
            axis.ticks.length = unit(.25, "cm"))
    fig = ggplotly(fig) %>% layout(height = 600)
    return(fig)
  }
  
  
  output$plot = renderPlotly(
    makePlot()
  )
  
  output$plotDataType = renderUI({
    selectInput(inputId = "dataType", label = "Data Type:", choices = dataTypes[[rv$selectedTestType]])
  })
}

shinyApp(ui, server)