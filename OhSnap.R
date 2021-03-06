# Importing External Libraries ------------------------------
library(shiny)            # for the overall structure/framework of the app
library(shinydashboard)   # for the tab layout of the app
library(shinyjs)          # for integrating packages like rhandsontable
library(shinycssloaders)  
library(shinythemes)      # for styling the interface
library(magick)           # for processing the image that the user uploads (size, orientation)
library(tesseract)        # for automatic optical character recognition
library(stringr)          # for parsing the text into a table with regular expressions
library(rhandsontable)    # for the interactive table where the
library(ggplot2)          # for creating the base plots of the data
library(plotly)           # for making the plots interactive
library(googleAuthR)      # for interacting with google authentication servers
library(googlesheets4)    # for reading and writing google sheets
library(zip)              # for downloading templates for the user to store their data in
library(readxl)           # for reading from an existing excel file
library(writexl)          # for writing the parsed data to an existing excel file
library(tidyverse)        

# Google Set Up
#options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/userinfo.email",
#                                        "https://www.googleapis.com/auth/userinfo.profile"))
#options("googleAuthR.webapp.client_id" = "543814214955-9u26dmgeaoo8p03fna1gc11ond5md1ta.apps.googleusercontent.com")
#options("googleAuthR.webapp.client_secret" = "4mbPAzE7UFZjTFYGcjPS1MYS")

# UI ------------------------------
ui <- fluidPage (
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "icon", type = "image/png", href = "OhSnapLogo.png")
  ),
  useShinyjs(),
  navbarPage(theme = shinytheme("flatly"), title = "OhSnap!", id = 'tabs',
             # * login tab ----------
             tabPanel(
               title = "Login",  value = "login",
               fluidRow(
                 align = "center",
                 br(), br(),
                 tags$img(src = 'OhSnapLogo.png', align = "center", height = "300px"),
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
                              "uploadImageButton",
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
                              imageOutput("croppedImage", height = "auto"),
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

# Server ------------------------------
server <- function(input, output, session) {
  # * global variables ------------------------------
  rv <- reactiveValues(rawData=NULL, rotate = NULL, rotatedImage = NULL, selectedTest = NULL, selectedSex = NULL, 
                       selectedDataType = NULL, login = FALSE, userData = NULL, testDate = NULL,
                       readyToEditImages = FALSE, imageSize = NULL, originalImage = NULL)
  
  dataInfo = read_tsv("www/Data_Info.tsv")
  dataTypes = list(CBC = dataInfo$ID[dataInfo$Test == "CBC"], 
                   CMP = dataInfo$ID[dataInfo$Test == "CMP"])
  image <- image_read("www/DefaultImage.png")
  
  # Google Login Code
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
  
  # * login tab ------------------------------
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
                            actionButton("goToUploadTab", "Continue")
                          ),
                          size = "m", easyClose = FALSE))
  })
  
  observeEvent(input$infile, {
    rv$userData <- read_excel(input$infile$datapath)
  })
  
  observeEvent(input$goToUploadTab, {
    removeModal()
    updateTabsetPanel(session, "tabs", selected = "uploadData")
  })
  

  # Make Zipped file containing template spreadsheets for CBC and CMP data
  output$makeFiles <- downloadHandler(
    filename = "OhSnapData.zip",
    content = function(file) {
      files <- NULL
      for (i in 1:length(dataTypes)){
        fileName <- paste0("OhSnap_", names(dataTypes[i]),".xlsx")
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
  
  # * upload data tab ------------------------------
  observeEvent(input$uploadImageButton, {
    if (length(input$uploadImageButton$datapath)) {
      image <<- image_read(input$uploadImageButton$datapath)
      rv$originalImage <- image_read(input$uploadImageButton$datapath)
      info   <- image_info(image)
      rv$imageSize =  paste(info$width, info$height, sep = "x")
    }
    
    output$imageEditing <- renderUI({
      rv$readyToEditImages = TRUE
      tagList(                                   
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
  
  observeEvent(input$image_brush,{
    croppedImage = image_crop(image, coords(), repage = FALSE)
    whitelistChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789.- \n"
    restrictedTesseract = tesseract(options = list(tessedit_char_whitelist = whitelistChars))
    rawText = ocr(croppedImage, engine = restrictedTesseract, HOCR = FALSE)
    altDf = ocr_data(croppedImage, engine = restrictedTesseract)
    # we only care about the alphanumeric test label, and the numeric data, extracted in () ()
    rowPattern = "([A-Za-z-]+)[\\s-]*([0-9.]*).*[^\n]*"
    rv$rawData <- str_match_all(rawText, rowPattern)
  })
  
  observeEvent(input$rotateButton, {
    rv$rotate = 90
  })
  
  observeEvent(input$goToVerificationTab,{
    updateTabsetPanel(session, "tabs", selected = "verification")
  })
  
  # * verification tab ------------------------------
  output$croppedImage = renderImage({
    croppedImg = image_crop(image, coords(), repage = FALSE)
    croppedImg = image_write(croppedImg, tempfile(fileext = 'jpg'), format = 'jpg') 
    list(src = croppedImg, width = "auto", height = "auto", contentType = "image/jpeg", alt = "This is the selected area of the original image")
  })
  
  output$referenceTable = renderRHandsontable({
    expected_table = data.frame(Expected = (colnames(rv$userData)[-1]))
    expected_table$Expected = as.character(expected_table$Expected)
    displayTable = rhandsontable(expected_table,rowHeaders = NULL, width = 300)
    hot_col(displayTable, col = "Expected", readOnly = TRUE) #make the reference table/column read only
  })
  
  output$verificationTable = renderRHandsontable({
      testNames = (rv$rawData)[[1]][,2]
      values = (rv$rawData)[[1]][,3]
      formattedData = data.frame(TestName = testNames, Value = as.numeric(values))
      formattedData$TestName = as.character(formattedData$TestName)
    if(!is.null(formattedData)) {
      rhandsontable(formattedData, rowHeaders = NULL, width = 300)
    }
  })
  
  # when the user clicks the download button, append the new data to userData and write it to an excel file
  output$saveData <- downloadHandler(
    filename = paste0("OhSnap_", rv$selectedTest, ".xlsx"),
    content = function(filePath) {
      newData = as.vector(hot_to_r(input$verificationTable)[,2]) # extract the raw data from the handsontable
      newRow = c(0, newData) # the new row is type numeric, so append 0 as a placeholder for the date
      stored_col_names = colnames(rv$userData) # store the colnames so they don't get overwritten
      rv$userData$Date = as.character(rv$userData$Date) # make userDatas Date column chars instead of Date objects
      rv$userData = rbind(rv$userData, newRow, stringsAsFactors=FALSE)  # insert the row of data into userData
      colnames(rv$userData) = stored_col_names # reset the column names, if they were overwritten
      rv$userData$Date[length(rv$userData$Date)] = as.character(rv$testDate) # insert the date into the date column
      write_xlsx(rv$userData, filePath)
    }
  )
  
  observeEvent(input$goToGraphsTab, {
    updateTabsetPanel(session, "tabs", selected = "graphs") # change from Verification tab to Graphical Display Tab
  }) 
  
  # * graphical display tab ------------------------------
  output$selectedDataType = renderUI({
    selectInput(inputId = "dataType", label = "Data Type:", choices = dataTypes[[rv$selectedTest]], selected = dataTypes[[rv$selectedTest]][1])
  })
  
  observeEvent(input$dataType, {
    rv$selectedDataType = input$dataType
  })
  
  # fill in description and links in side panel
  output$dataInfo = renderUI({
    if (rv$selectedTest == "CBC") {
      HTML(paste0("<strong>Complete Blood Count</strong><br/>", 
                  dataInfo$Description[dataInfo$ID == "CBC"],
                 "<br/><a href='", dataInfo$Links[dataInfo$ID == "CBC"], "'>See more info</a>",
                 "<br/><br/><strong>", dataInfo$Title[dataInfo$ID == rv$selectedDataType], 
                 "</strong><br/>",  dataInfo$Description[dataInfo$ID == rv$selectedDataType],
                 "<br/><a href='", dataInfo$Links[dataInfo$ID == rv$selectedDataType], "'>See more info</a>",
                 "<br/><br/><strong>Normal Range(s)</strong><br/>", 
                 dataInfo$Range[dataInfo$ID == rv$selectedDataType]))
    } else if (rv$selectedTest == "CMP") {
      HTML(paste0("<strong>Comprehensive Metabolic Panel</strong><br/>", 
                  dataInfo$Description[dataInfo$ID == "CMP"],
                  "<br/><a href='", dataInfo$Links[dataInfo$ID == "CMP"], "'>See more info</a>",
                  "<br/><br/><strong>",  dataInfo$Title[dataInfo$ID == rv$selectedDataType], 
                  "</strong><br/>",  dataInfo$Description[dataInfo$ID == rv$selectedDataType],
                  "<br/><a href='", dataInfo$Links[dataInfo$ID == rv$selectedDataType], "'>See more info</a>",
                  "<br/><br/><strong>Normal Range(s)</strong><br/>", 
                  dataInfo$Range[dataInfo$ID == rv$selectedDataType]))
    }
  })
  
  # customize how much x-axis tick marks increment by depending on diff between min and max
  getIncrementSize = function(min, max) {
    diff = max - min
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
  
  # create plot
  makePlot = function() {
    df = select(rv$userData, Date, UQ(as.name(rv$selectedDataType)))
    df$Date = as.Date(df$Date)
    
    # base plot
    fig = ggplot(df, aes(x = Date, y = UQ(as.name(rv$selectedDataType)))) +
      geom_point() +
      geom_line() +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
      labs(title = paste0(rv$selectedTest, " - ", dataInfo$Title[dataInfo$ID == rv$selectedDataType], " (", rv$selectedDataType, ")"),
           x = "Date",
           y = paste0(rv$selectedDataType, " (", dataInfo$Units[dataInfo$ID == rv$selectedDataType], ")")) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 18),
            axis.title.x = element_text(size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12, margin = margin(b = 20)),
            axis.title.y = element_text(size = 16),
            axis.text.y = element_text(size = 12, margin = margin(l = 15)),
            axis.ticks.length = unit(.25, "cm"))
    
    # add health ranges
    maleMin = dataInfo$M_Min[dataInfo$ID == rv$selectedDataType]
    maleMax = dataInfo$M_Max[dataInfo$ID == rv$selectedDataType]
    femaleMin = dataInfo$F_Min[dataInfo$ID == rv$selectedDataType]
    femaleMax = dataInfo$F_Max[dataInfo$ID == rv$selectedDataType]
    if (!is.na(maleMin) && !is.na(maleMax)) {  # if range exists
      if (!is.na(femaleMin) && !is.na(femaleMax)) {  # if separate ranges exist for male and female
        if (rv$selectedSex == "female") {
          min = floor(min(c(femaleMin, pull(df, rv$selectedDataType))))
          max = ceiling(max(c(femaleMax, pull(df, rv$selectedDataType))))
          incrementSize = getIncrementSize(min, max)
          fig = fig +
            geom_hline(yintercept = femaleMin, linetype = "dashed", color = "green") +
            geom_hline(yintercept = femaleMax, linetype = "dashed", color = "green") +
            scale_y_continuous(limits = c(floor(min), ceiling(max)), 
                               breaks = seq(floor(min), ceiling(max), by = incrementSize))
        } else {
          min = floor(min(c(maleMin, pull(df, rv$selectedDataType))))
          max = ceiling(max(c(maleMax, pull(df, rv$selectedDataType))))
          incrementSize = getIncrementSize(min, max)
          fig = fig +
            geom_hline(yintercept = maleMin, linetype = "dashed", color = "green") +
            geom_hline(yintercept = maleMax, linetype = "dashed", color = "green", ) +
            scale_y_continuous(limits = c(floor(min), ceiling(max)), 
                               breaks = seq(floor(min), ceiling(max), by = incrementSize))
        }
      } else {  # else if only general range exists with no specificity to sex
        min = floor(min(c(maleMin, pull(df, rv$selectedDataType))))
        max = ceiling(max(c(maleMax, pull(df, rv$selectedDataType))))
        incrementSize = getIncrementSize(min, max)
        fig = fig +
          geom_hline(yintercept = maleMin, linetype = "dashed", color = "green") +
          geom_hline(yintercept = maleMax, linetype = "dashed", color = "green", ) +
          scale_y_continuous(limits = c(floor(min), ceiling(max)), 
                             breaks = seq(floor(min), ceiling(max), by = incrementSize))
      }
    } else {  # else if no range exists
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

# Running the App ------------------------------
shinyApp(ui, server)
