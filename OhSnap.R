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
library(zip)
library(readxl)
library(writexl)
library(tidyverse)

# Global variables and functions ----------------
#options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/userinfo.email",
#                                        "https://www.googleapis.com/auth/userinfo.profile"))
#options("googleAuthR.webapp.client_id" = "543814214955-9u26dmgeaoo8p03fna1gc11ond5md1ta.apps.googleusercontent.com")
#options("googleAuthR.webapp.client_secret" = "4mbPAzE7UFZjTFYGcjPS1MYS")

#as adapted from 'image_ocr' in package:magick
data_selection_ocr <- function (image, whitelist = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789.-", HOCR = FALSE, ...) 
{
  #assert_image(image)
  allowed_chars = tesseract(options = list(tessedit_char_whitelist = whitelist))
  ocr(image, engine = allowed_chars, HOCR = HOCR)
}

dataTypes = list(CBC = c("WBC", "RBC", "HGB", "HCT", "MCV", "MCH", "MCHC", "PLT", "RDW-SD", "RDW-CV", "MPV", "NEUT", "LYMPH", "MONO", "EO", "BASO"), 
                 CMP = c("Na", "K","Cl", "ECO2", "AGAP", "AHOL", "TBI", "TP", "GLOB", "ALPI","TGL", "CHOL", "AST", "ALTI", "ALB", "A/G", "GLUC", "BUN", "CA", "CRE2", "BN/CR"))

dataInfo = list(# CBC types
                WBC = list(def = "White Blood Cell Count", units = "10^3/uL", adult = c(4.5, 10)),
                RBC = list(def = "Red Blood Cell Count",  units = "10^6/uL", male = c(4.5, 5.9), female = c(4.1, 5.1)),
                HGB = list(def = "Hemoglobin", units = "g/dL", male = c(14, 17.5), female = c(12.3, 15.3)),
                HCT = list(def = "Hematocrit", units = "%", male = c(41.5, 50.4), female = c(36.9, 44.6)),
                MCV = list(def = "Mean Corpuscular Volume", units = "pg", adult = c(80, 96)),
                MCH = list(def = "Mean Corpuscular Hemoglobin", units = "pg", adult = c(27, 33)),
                MCHC = list(def = "Mean Corpuscular Hemoglobin Concentration", units = "g/dL", adult = c(31, 37)),
                PLT = list(def = "Platelet Count", units = "10^3/uL", adult = c(150, 450)),
                `RDW-SD` = list(def = "Red Blood Cell Distribution Width", units = "fL", adult = c(40, 55)),
                `RDW-CV` = list(def = "Red Blood Cell Distribution Width", units = "%", adult = c(11, 15)),
                MPV = list(def = "Mean Platelet Volume", units = "fL", adult = c(9.4, 12.3)),
                NEUT = list(def = "Neutrophils", units = "%", adult = c(40, 60)),
                LYMPH = list(def = "Lymphocytes", units = "%", adult = c(20, 40)),
                MONO = list(def = "Monocytes", units = "%", adult = c(2, 8)),
                EO = list(def = "Esinophils", units = "%", adult = c(1, 4)),
                BASO = list(def = "Basophils", units = "%", adult = c(0.5, 1)),
                # CMP types
                Na = list(def = "Sodium", units = "mmol/L", adult = c(136, 145)),
                K = list(def = "Potassium", units = "mmol/L", adult = c(3.5, 5.1)),
                Cl = list(def = "Chloride", units = "mmol/L", adult = c(98, 107)),
                ECO2 = list(def = "Carbon Dioxide", units = "mmol/L", adult = c(21, 32)),
                AGAP = list(def = "Anion Gap", units = "mmol/L", adult = c(3, 10)),
                AHDL = list(def = "High-Density Lipoprotein", units = "mg/dL", adult = c(40, 60)),
                TBI = list(def = "Total Bilirubin", units = "mg/dL", adult = c(0.2, 1)),
                TP = list(def = "Total Protein", units = "g/dL", adult = c(6.4, 8.2)),
                GLOB = list(def = "Globulin", units = "g/dL", adult = c(2, 3.5)),
                ALPI = list(def = "Alkaline Phosphatase", units = "units/L", adult = c(46, 116)),
                TGL = list(def = "Triglycerides", units = "mg/dL", adult = c(30, 150)),
                CHOL = list(def = "Cholesterol", units = "mg/dL", adult = c(0, 200)),
                AST = list(def = "Aspartate Aminotransferase", units = "units/L", adult = c(15, 37)),
                ALTI = list(def = "Alanine Aminotransferase", units = "units/L", adult = c(12, 78)),
                ALB = list(def = "Albumin", units = "g/dL", adult = c(3.4, 5)),
                `A/G` = list(def = "Albumin/Globulin Ratio", units = "No units"),
                GLUC = list(def = "Glucose", units = "mg/dL", adult = c(74, 106)),
                BUN = list(def = "Blood Urea Nitrogen", units = "mg/dL", adult = c(7, 18)),
                CA = list(def = "Calcium", units = "mg/dL", adult = c(8.5, 10.1)),
                CRE2 = list(def = "Creatinine", units = "mg/dL", adult = c(0.6, 1.3)),
                `BN/CR` = list(def = "", units = "No units", adult = c(10, 20)))

# ui ----------
ui <- fluidPage (
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "icon", type = "image/png", href = "Logo.png")
  ),
  useShinyjs(),
  #shinythemes::themeSelector(),
  navbarPage(theme = shinytheme("flatly"), title = "OhSnap!", id = 'tabs',
             # * login tab ----------
             tabPanel(
               title = "Login",  value = "login",
               fluidRow(
                 align = "center",
                 br(), br(),
                 tags$img(src = 'TempLogo.png', align = "center", height = "300px"),
                 br(),
                 actionButton("personal", label = "Store on Personal Computer"),
                 br(), br(),
                 actionButton("gauth_login", label = "Login with Google", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))   
             ),
             # * upload image tab ----------
             tabPanel('Upload Image', value = 'uploadData',
                      sidebarLayout(
                        sidebarPanel(
                          tags$div(id = "headertitle",
                                   tags$h2("Upload Image"),
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
                            uiOutput("imageEditing")
                            
                          )
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
                          ))
                        )
                      )
             ),
             # * verifiction tab ----------
             tabPanel(
               title = "Verification", value = "verification",
               sidebarLayout(
                 sidebarPanel(h2("Verification"),
                              imageOutput("croppedImage"),
                              br(),
                              downloadButton("saveData", "Save to Spreadsheet"),
                              br(),
                              br(),
                              actionButton("goToGraphsTab", "Continue")),
                 mainPanel(p("Verify that the data in the table below exactly matches the data in the image. 
                             Right-click in a cell to enable a context menu that includes customizable table actions."),
                           fluidRow(column(3,rHandsontableOutput("referenceTable")),
                                    column(3,rHandsontableOutput("verificationTable"))))
               )
             ),  
             # * graphical display tab ----------
             tabPanel(
               title = "Graphical Display", value = "graphs",
               sidebarLayout(
                 sidebarPanel(
                   tags$h2("View Your Data"),
                   uiOutput("selectedDataType"),
                   uiOutput("dataInfo")
                 ),
                 mainPanel(
                  tags$div(class = "plot",
                           plotlyOutput("plot")
                  )
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
                                     '<p>The source code for OhSnap can be found at', 
                                     '<a target="_blank", href="https://github.com/elizabethcook21/OhSnap">https://github.com/elizabethcook21/OhSnap</a>.</p></div>'))
                        )
                        
                      )
             )
  )
)

# server ----------
server <- function(input, output, session) {
  # global variables ----------
  rv <- reactiveValues(data=NULL, rotate = NULL, rotatedImage = NULL, selectedTest = NULL, selectedSex = NULL, 
                       selectedDataType = NULL, login = FALSE, currDF = NULL, testDate = NULL,
                       readyToEditImages = FALSE, imageSize = NULL, originalImage = NULL)
  
  dataDescriptions = read_tsv("Data_Info.tsv")
  
  image <- image_read("DefaultImage.png")
  imageData <- NULL
  
  # Google Login Code -------------------------------------------------
  # accessToken <- callModule(googleAuth, "gauth_login",
  #                           login_class = "btn btn-primary",
  #                           logout_class = "btn btn-primary")
  # userDetails <- reactive({
  #   validate(
  #     need(accessToken(), "not logged in")
  #   )
  #   rv$login <- TRUE
  #   with_shiny(get_user_info, shiny_access_token = accessToken())
  # })
  
  userDetails <- reactive({
    validate(
      need(accessToken(), "not logged in")
    )
    rv$login <- TRUE
    with_shiny(get_user_info, shiny_access_token = accessToken())
  })
  
  observeEvent(input$gauth_login, {
    showModal(modalDialog(p("Future functionality! Please use personal data for now"),
                          title = "Google Currently Disabled", footer = modalButton("Dismiss"),
                          size = c("m", "s", "l"), easyClose = FALSE, fade = FALSE))
  })
  
  # observe({
  #   if (rv$login) {
  #     showModal(modalDialog(p("Future functionality! Please use personal data for now"),
  #                           title = "Google Currently Disabled", footer = modalButton("Dismiss"),
  #                            size = c("m", "s", "l"), easyClose = FALSE, fade = FALSE))
  #     #shinyjs::onclick("gauth_login-googleAuthUi",
  #     #                 shinyjs::runjs("window.location.href = 'https://yourdomain.shinyapps.io/appName';"))
  #     #print(googledrive::drive_find(""))
  #   }
  # })
  
  # Personal Data Login Code
  observeEvent(input$personal, {
    showModal(modalDialog(title = tags$b("Store Data on your Personal Computer"),
                          h4("First Time User:"),
                          p("If this is your first time using the app, please navigate to a location on your
                          computer that you wish to store your test results and make a folder called \"OhSnapData\"."),
                          p("Once you're done, please click the following button and navigate to the folder you just made. 
                          Clicking the button will generate excel sheets that will be host the data you upload in this app."),
                          downloadButton("makeFiles", "Make Excel Files"),
                          p("Now that you've downloaded the zip fle, please extract it somewhere on your computer that you can remember, then continue
                            following the instructions for the continuing user below."),
                          h4("Continuing User:"),
                          p("Please select which type of test you are planning on uploading:"),
                          selectInput(inputId = "testType", label = "Tests:", choices = c("CBC (Complete Blood Count)", "CMP (Comprehensive Metabolic Panel)")),
                          p("Now, please navigate to the file you wish to have your data added to and put the path in the following text box:"),
                          fileInput("infile", "Pleaseupload corresponding file", placeholder = "Browse for file", accept=c('application/vnd.ms-excel',
                                                                                                                           'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                                                                                                                           '.xls',
                                                                                                                           '.xlsx',
                                                                                                                           '.csv',
                                                                                                                           '.tsv')
                                    ),
                          footer = tagList(
                            actionButton("continueToUpload", "Continue")
                          ),
                          size = "m", easyClose = FALSE))
  })
  
  observeEvent(input$infile, {
    rv$currDF <- read_excel(input$infile$datapath)
  })
  
  observeEvent(input$continueToUpload, {
    removeModal()
    updateTabsetPanel(session, "tabs", selected = "uploadData")
  })
  

  # Make Zipped fie
  output$makeFiles <- downloadHandler(
    filename = "OhSnapData.zip",
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
      rv$originalImage <- image_read(input$upload$datapath)
      info   <- image_info(image)
      rv$imageSize =  paste(info$width, info$height, sep = "x")
      #updateTextInput(session, "size", value = paste(info$width, info$height, sep = "x"))
    }
    
    output$imageEditing <- renderUI({
      rv$readyToEditImages = TRUE
      tagList(
        #textInput("size", "Size", value = "200x400"),                                    
        sliderInput("imageSizeSlider", label = "Change Image Size", min = 25, max = 200, value = 100),
        br(),
        actionButton("rotateButton", "Rotate Clockwise 90\u00b0",
                     icon("sync")),
        br(),br(),
        tags$b("Text Output"),
        textOutput("ocr_text"),
        br(),
        selectInput(inputId = "sexType", label = "Select your sex", choices = c("Female", "Male")),
        dateInput(inputId = "testDate", label = "Input the date of the test", format = "yyyy-mm-dd"),
        actionButton("goToVerificationTab", "Next")
      )
    })
  })
  
  observeEvent(input$sexType, {
    rv$selectedSex = tolower(input$sexType)
    print(rv$selectedSex)
  })
  
  observeEvent(input$testDate,{
    rv$testDate = input$testDate
  })
  
  observeEvent(input$testType, {
    rv$selectedTest = str_split(input$testType, pattern = " ")[[1]][1]
  })
  
  output$image_brushinfo <- renderPrint({
    cat("Selected:\n")
    str(input$image_brush$coords_css)
  })
  
  output$image <- renderImage({
    height  <- session$clientData$output_image_height
    width <- input$size
    img <- image %>% image_resize(rv$imageSize) 
    
    if(rv$readyToEditImages == TRUE){
      if(!is.null(input$imageSizeSlider)){
        if(input$imageSizeSlider != "100"){
          width <- paste0(input$imageSizeSlider, "%") 
          img <- rv$originalImage %>% image_resize(width) 
        }
      }
    }
    if (!is.null(rv$rotate)){
      img <- image_rotate(img, 90 * input$rotateButton)
      rv$rotate = NULL
      rv$originalImage <- img
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
    # "500x300+10+20" Crop image to 500 by 300 at position 10,20
  })
  
  #output$coordstext <- renderText({
  #  if (is.null(input$image_brush$xmin)) {
  #    "No Area Selected!"
  #  } else {
  #    coords()}
  #})
  
  observeEvent(input$image_brush,{
    #req(input$image_brush)
    image <- image
    print('parsing text')
    text   <- image %>% 
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
    list(src = croppedImg, width = "auto", height = "auto", contentType = "image/jpeg", alt = "This is the selected area of the original image")
  })
  
  output$referenceTable = renderRHandsontable({
    expected_table = data.frame(Expected = (colnames(rv$currDF)[-1]))
    expected_table$Expected = as.character(expected_table$Expected)
    rhandsontable(expected_table,rowHeaders = NULL, width = 300)%>%
      hot_col(col = "Expected", readOnly = TRUE) #make the reference table/column read only
  })
  
  output$verificationTable = renderRHandsontable({
    if (input$testType == "CBC (Complete Blood Count)") {
      cbc = (rv$data)[[1]][,2]
      values = (rv$data)[[1]][,3]
      clinDF = data.frame(CBC = cbc, Value = as.numeric(values))
      clinDF$CBC = as.character(clinDF$CBC)
    } else if (input$testType == "CMP (Comprehensive Metabolic Panel)") {
      cmp = (rv$data)[[1]][,2]
      values = (rv$data)[[1]][,3]
      clinDF = data.frame(CMP = cmp, Value = as.numeric(values))
      clinDF$CMP = as.character(clinDF$CMP)
    }
    if(!is.null(clinDF)) {
      rhandsontable(clinDF, rowHeaders = NULL, width = 300)
    }
  })
  
  output$saveData <- downloadHandler(
    filename = paste0("OhSnap_", rv$selectedTest, ".xlsx"),
    content = function(filePath) {
      newData = as.vector(hot_to_r(input$verificationTable)[,2])
      newRow = c(0, newData)
      stored_col_names = colnames(rv$currDF) #store the colnames so they don't get overwritten
      rv$currDF$Date = as.character(rv$currDF$Date)
      rv$currDF = rbind(rv$currDF, newRow, stringsAsFactors=FALSE)
      rv$currDF$Date[length(rv$currDF$Date)] = as.character(rv$testDate)
      colnames(rv$currDF) = stored_col_names
      write_xlsx(rv$currDF, filePath)
      rv$currDF = read_excel(filePath)
    }
  )
  
  observeEvent(input$goToGraphsTab, {
    updateTabsetPanel(session, "tabs", selected = "graphs") #change from Verification tab to Graphical Display Tab
  }) 
  
  # graphical display tab ----------
  output$selectedDataType = renderUI({
    selectInput(inputId = "dataType", label = "Data Type:", choices = dataTypes[[rv$selectedTest]], selected = dataTypes[[rv$selectedTest]][1])
  })
  
  observeEvent(input$dataType, {
    rv$selectedDataType = input$dataType
  })
  
  output$dataInfo = renderUI({
    if (rv$selectedTest == "CBC") {
      HTML(paste0("<strong>Complete Blood Count</strong><br/>", 
                 dataDescriptions$Description[dataDescriptions$ID == "CBC"],
                 "<br/><a href='", dataDescriptions$Links[dataDescriptions$ID == "CBC"], "'>See more info</a>",
                 "<br/><br/><strong>", dataInfo[[rv$selectedDataType]]$def, 
                 "</strong><br/>",  dataDescriptions$Description[dataDescriptions$ID == rv$selectedDataType],
                 "<br/><a href='", dataDescriptions$Links[dataDescriptions$ID == rv$selectedDataType], "'>See more info</a>",
                 "<br/><br/><strong>Normal Range(s)</strong><br/>", 
                 dataDescriptions$Range[dataDescriptions$ID == rv$selectedDataType]))
    } else if (rv$selectedTest == "CMP") {
      HTML(paste0("<strong>Comprehensive Metabolic Panel</strong><br/>", 
                  dataDescriptions$Description[dataDescriptions$ID == "CMP"],
                  "<br/><a href='", dataDescriptions$Links[dataDescriptions$ID == "CMP"], "'>See more info</a>",
                  "<br/><br/><strong>", dataInfo[[rv$selectedDataType]]$def, 
                  "</strong><br/>",  dataDescriptions$Description[dataDescriptions$ID == rv$selectedDataType],
                  "<br/><a href='", dataDescriptions$Links[dataDescriptions$ID == rv$selectedDataType], "'>See more info</a>",
                  "<br/><br/><strong>Normal Range(s)</strong><br/>", 
                  dataDescriptions$Range[dataDescriptions$ID == rv$selectedDataType]))
    }
  })
  
  getIncrementSize = function(min, max) {
    diff = max - min
    print(diff)
    if (between(diff, 0, 3)) {
      increment = 0.25
    } else if (between(diff, 4, 8)) {
      increment = 0.5
    } else if (between(diff, 9, 20)) {
      increment = 1
    } else if (between(diff, 21, 40)) {
      increment = 5
    } else if (between(diff, 41, 60)){
      increment = 10
    } else {
      increment = 20
    }
    return(increment)
  }
  
  makePlot = function() {
    df = select(rv$currDF, Date, UQ(as.name(rv$selectedDataType)))
    df$Date = as.Date(df$Date)
    info = dataInfo[[rv$selectedDataType]]
    
    # base plot
    fig = ggplot(df, aes(x = Date, y = UQ(as.name(rv$selectedDataType)))) +
      geom_point() +
      geom_line() +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
      labs(title = paste0(rv$selectedTest, " - ", info$def, " (", rv$selectedDataType, ")"),
           x = "Date",
           y = paste0(rv$selectedDataType, " (", info$units, ")")) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 18),
            axis.title.x = element_text(size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12, margin = margin(b = 20)),
            axis.title.y = element_text(size = 16),
            axis.text.y = element_text(size = 12, margin = margin(l = 15)),
            axis.ticks.length = unit(.25, "cm"))
    
    # add normal ranges
    if (length(info) > 2) {  # if normal range exists
      if (length(info) > 3) {  # if separate normal ranges exist for male and female
        adult = as.numeric(info[[rv$selectedSex]])
        print("df")
        print(df)
        print("selectedDataType")
        print(rv$selectedDataType)
        print("adult")
        print(adult)
        print("pull df")
        print(pull(df, rv$selecteDataType))
        min = floor(min(c(adult, pull(df, rv$selectedDataType))))
        print("min")
        print(min)
        max = ceiling(max(c(adult, pull(df, rv$selectedDataType))))
        incrementSize = getIncrementSize(min, max)
        fig = fig +
          geom_hline(yintercept = adult[1], linetype = "dashed", color = "green") +
          geom_hline(yintercept = adult[2], linetype = "dashed", color = "green") +
          scale_y_continuous(limits = c(floor(min), ceiling(max)), 
                             breaks = seq(floor(min), ceiling(max), by = incrementSize))
      } else {  # else if only general normal range exists with no specificity to sex
        min = floor(min(c(as.numeric(info$adult[1]), pull(df, rv$selectedDataType))))
        max = ceiling(max(c(as.numeric(info$adult[2]), pull(df, rv$selectedDataType))))
        incrementSize = getIncrementSize(min, max)
        fig = fig +
          geom_hline(yintercept = min, linetype = "dashed", color = "green") +
          geom_hline(yintercept = max, linetype = "dashed", color = "green", ) +
          scale_y_continuous(limits = c(floor(min), ceiling(max)), 
                             breaks = seq(floor(min), ceiling(max), by = incrementSize))
      }
    } else {  # else if no normal range exists
      min = floor(min(df[rv$selectedDataType]))
      max = ceiling(max(df[rv$selectedDataType]))
      incrementSize = getIncrementSize(min, max)
      fig = fig +
        scale_y_continuous(limits = c(min, max), 
                           breaks = seq(min, max, by = incrementSize))
    }
    
    fig = ggplotly(fig, height = 600)
    return(fig)
  }
  
  
  output$plot = renderPlotly(
    if (!is.null(rv$selectedDataType)) {
      makePlot()
    }
  )
}

shinyApp(ui, server)