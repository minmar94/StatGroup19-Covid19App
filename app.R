# Load packages
require(tidyverse)
require(magrittr)
require(shiny)
require(shinythemes)
require(shinyWidgets)
require(lubridate)
require(DT)
require(stringi)
require(MASS)
require(rmutil)
require(nplr)
require(tscount)
require(foreach)
require(doSNOW)
require(sf)
require(sp)
require(rmarkdown)
require(shinydashboard)
require(plotly)
require(rgeos)
require(lme4)

rm(list=ls())


Sys.setlocale(locale = "C")

# Load functions and data
source("Script/FunctionsDefAttempt2.R")
source("Script/UsefulFunsApp2.R")

load("Data/PastICUPred.RData")


# Read aggregated Italian data up to today 
dati_Ita <- read_italian(path = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv")
today <- max(dati_Ita$data)

# Read regional data up to today
dati_reg <- read_regional(path = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")

# Read province data up o today
dati_prov <- read_province(path = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv")

# Read residents data
residents <- read.csv("Data/residenti2019.csv",header=TRUE,sep=",")
residents[,1] <- as.character(residents[,1])

# Read data for regional map
italy_sf_reg <- read_shape_italy(province = F)

# Read data for province map
italy_sf_prov <- read_shape_italy(province = T)

# Join data regional
joined_reg <- italy_sf_reg %>% sp::merge(dati_reg, by.y = "denominazione_regione", by.x = "NAME")

# Join data province
joined_prov <- italy_sf_prov %>% 
  sp::merge(dati_prov %>% dplyr::select(data, denominazione_provincia, denominazione_regione, `Cumulative positives`),
            by.y = c("denominazione_regione", "denominazione_provincia"),
            by.x = c("NAME", "NAME_2"), all.x = T) %>% 
  gather(Key, Value, `Cumulative positives`) %>% 
  dplyr::select(NAME_2, NAME, Key, data, Value, geometry)


# Data preparation for the model
data_formodel_prep <- prepdata_for_model(dftoprep = dati_reg %>% spread(Key, Value), resdata = residents)

TotPop <- data_formodel_prep %>% distinct(region, residents) %$% sum(residents) 

# Today summary
tod_summary <- return_current_situa(da = dati_Ita, TotPop = TotPop)

# Icu data preparation
datasuTabella <- max(outmod_terapie_tab$DataPred)
outmod_terapie_plot <- outmod_terapie_tab %>% filter(DataPred < datasuTabella)
outmod_terapie_table <- outmod_terapie_tab %>% filter(DataPred == max(DataPred))

# Count user logged
users = reactiveValues(count = 0)

# Table with today raw data
raw_today <- show_raw_data(dati_Ita, dati_reg)

# ShinyApp ----------------------------------------------------------------
ui <- navbarPage(theme = shinytheme("sandstone"), 
                 title = HTML(paste0('<b>Analysis of the italian COVID-19 pandemic </b><i class="fas fa-certificate"></i>')),
                 # First tab: Overview
                 tabPanel(title = "Overview", icon = icon("binoculars"),
                          
                          div(style="margin-top:-3.5em",
                              
                              fluidRow(tags$hr(),
                                       useShinydashboard(),
                                       column(width = 12,
                                              htmlOutput(outputId = "CurrentSitua"),
                                              valueBoxOutput("CumPos"), valueBoxOutput("CurrPos"), valueBoxOutput("CurrHosp"),
                                              valueBoxOutput("ICU"), valueBoxOutput("HospSym"),valueBoxOutput("HomeIs"),
                                              valueBoxOutput("NewPos"), valueBoxOutput("DishRec"), valueBoxOutput("Deces"),
                                              valueBoxOutput("Swabs"), valueBoxOutput("MortRate"), valueBoxOutput("LetRate"),
                                              actionBttn(inputId = "ImpDef", 
                                                         icon = icon("question"), style = "material-circle", color = "warning", size = "sm"),
                                              helpText("Any doubts? Click the button for a better understanding!"),
                                              style="text-align:justify;padding:20px;")
                              ),
                              fluidRow(
                                column(11, uiOutput(outputId = "COV19"), style="text-align:justify;padding:20px;")
                                
                              )
                          ), 
                          
                          tags$style(HTML("hr {border-top: 1px solid #000000;}")),
                          tags$hr(),
                          #themeSelector(),
                          fluidPage(
                            fluidRow(
                              sidebarLayout(
                                sidebarPanel(width = 3,
                                             prettyRadioButtons(
                                               inputId = "VarRow1", 
                                               label = "Pick a variable", 
                                               choices = list("Cumulative positives", "Current positives"),
                                               selected = "Cumulative positives", status = "danger", shape = "round",
                                               inline = F),
                                             prettySwitch(
                                               inputId = "DonBarPerReg", 
                                               label = "View by region",
                                               value = F, 
                                               status = "danger", slim = T, bigger = T
                                             ),
                                             uiOutput(outputId = "DisplayRegRow1")
                                ),
                                mainPanel(
                                  splitLayout(
                                    cellWidths = c("53%","55%"),
                                    addSpinner(plotlyOutput(outputId = "DonughtPlot"), spin = "fading-circle", color = "firebrick"),
                                    addSpinner(plotlyOutput(outputId = "BarStackPos"), spin = "fading-circle", color = "firebrick")
                                  )
                                  
                                )
                                
                              )
                            ),
                            tags$hr(),
                            
                            fluidRow(
                              sidebarLayout(
                                sidebarPanel(width = 3,
                                  pickerInput(inputId = "SelIdxforTSandMap", 
                                              label = "Pick a variable", 
                                              choices = list("Cumulative positives", "Current positives", 
                                                             "Current hospitalized", "Hospitalized with symptoms","Intensive care",
                                                             "Home isolation","Discharged recovered", "Deceased", "Swabs"),
                                              width = "fit",
                                              selected = "Cumulative positives"),
                                  prettySwitch(inputId = "TSRegioneSiNo", status = "danger", bigger = F, slim = T,
                                                 label = "View time series by region",
                                                 value = F),
                                  uiOutput(outputId = "SelRegTSandMap"),
                                  prettyCheckbox(inputId = "IncrementiSiNo", 
                                                label = "View time series of the daily variations",
                                                 status = "danger",icon = icon("check"),
                                                value = F),
                                  uiOutput(outputId = "IncrPercSiNo"),
                                  uiOutput(outputId = "SelDateTSandMap")
                                ),
                                mainPanel(
                                  splitLayout(
                                    cellWidths = c("53%", "55%"),
                                    addSpinner(plotlyOutput(outputId = "TSIncrPerc", height = "450px"), spin = "fading-circle", color = "firebrick"),
                                    addSpinner(plotlyOutput(outputId = "MapIta", height = "450px"), spin = "fading-circle", color = "firebrick")
                                  )
                                )
                              )),
                            
                            tags$hr(),
                            fluidRow(
                              #sidebarLayout(
                              #  sidebarPanel(width = 3,
                                  # dateInput(inputId = "DateTableOverview", label = "Select a date",
                                  #           min = "2020-02-25", max = today, value = today, width = "250px"),
                              tags$h3(tags$strong("Table with the raw data"), align = "center"),
                              DTOutput(outputId = "Rawdata"),
                              downloadButton(outputId = "DownloadRawData"),
                              helpText("Click the button to download the raw data up to today.")
                              )#,
                                #mainPanel(
                                  
                                #)
                              #)
                            ,
                            tags$hr(),
                            uiOutput("NUsers", inline = T))),
                 
                 tabPanel("Model", icon = icon("chart-line"),
                          fluidPage(
                            
                            
                            tags$h2(tags$strong("Generalized linear (Mirrored) Richards model*")),
                            tags$h5("*further details on the methodology can be found ",
                                    tags$a(href = "https://statgroup-19.blogspot.com/p/covid-19-epidemic-progress-medium-term.html" , "here")),
                            sidebarLayout(
                              sidebarPanel(
                                tags$h3(tags$strong("Model parameters")),
                                pickerInput(inputId = "VarForModel", 
                                            label = "Select the variable you want to model",
                                            choices = list("Cumulative positives", "Current positives", "Current hospitalized", 
                                                           "Hospitalized with symptoms","Intensive care",
                                                           "Home isolation","Discharged recovered", "Deceased"),
                                            selected = "Cumulative positives"), 
                                radioButtons(inputId = "ModelFamily",
                                             label = "Choose a distribution",
                                             choices = list("Poisson", "Negative Binomial"),
                                             selected = "Poisson"),
                                materialSwitch(inputId = "ModRegioneSiNo", 
                                               label = "Model by region", 
                                               value = F, status = "danger"),
                                uiOutput(outputId = "SelRegModel"),
                                actionBttn(inputId = "updatemodel", label = "Fit the model", style = "pill", color = "warning"),
                                helpText("Select the parameters, push the button and wait for the model to be estimated"),
                                tags$hr(),
                                uiOutput(outputId = "Doyouwantbands"),
                                uiOutput(outputId = "Showbands"),
                                uiOutput(outputId = "CannotPlotBands"),
                                sliderInput(
                                  inputId = "SelDateModel",
                                  label = "Forecast horizon (days):",
                                  width = 250,
                                  value = 1,
                                  animate = animationOptions(interval = 500, loop = F),
                                  min = 1,
                                  max = 15
                                ), 
                                helpText("Push the play button to see the time forecast"),
                                tags$br(),
                                htmlOutput("SummaryModel")
                              ),
                              
                              mainPanel(tags$style(type="text/css",
                                                   ".shiny-output-error { visibility: hidden; }",
                                                   ".shiny-output-error:before { visibility: hidden; }"
                              ), 
                              verticalLayout(
                                uiOutput("ModelHead"),
                                fluidRow(
                                  splitLayout(cellWidths = c("49%", "49%"), 
                                              plotlyOutput("PredCumCases"), 
                                              plotlyOutput("PredNewCases"))
                                ), 
                                tags$hr(),
                                fluidRow(
                                  DTOutput("DatPred")
                                )
                              )
                              )
                            )
                            
                          )
                 ),
                 tabPanel(title ="Prediction of Intensive care units", icon = icon("hospital"), 
                          fluidPage(
                            tags$h2(tags$strong(paste("Prediction of Intensive care units on the ", 
                                                      paste_eng_date(datasuTabella)))),
                            tags$h5("*further details on the methodology can be found ",
                                    tags$a(href = "https://statgroup-19.blogspot.com/p/short-term-predictions-of-daily.html" , "here")),
                            fluidRow(DTOutput("ICuTab"), 
                                     helpText("Click the button to download the predictions"),
                                     downloadButton(outputId = "DownICUTomorrow", label = "Download")),
                            tags$hr(),
                            fluidRow(
                              sidebarLayout(
                                sidebarPanel(
                                  
                                  helpText(tags$h4(tags$strong("Comparison of predicted vs observed values on the chosen date*")),
                                           tags$h5("*starting from 17th of March 2020")),
                                  dateInput(inputId = "ICubarDate", label = "Choose a date", value = datasuTabella-1,
                                            min = "2020-03-17", max = datasuTabella-1, width = "250px"),
                                  uiOutput("CovICUModel"),
                                  helpText("Click the button to download the predictions on the date you chose"),
                                  downloadButton(outputId = "DownICUWhatever", label = "Download")
                                  
                                ), 
                                mainPanel(plotlyOutput("BarplotIcu"))))
                          )),
                 tabPanel("Info and Credits", icon = icon("info-circle"), div(style="margin-top:-2.5em", includeMarkdown("InfoandCredits.md")))
                 
)

server <- function(input, output, session){
  
  # Welcome message
  sendSweetAlert(
    session,
    title = "Welcome to StatGroup-19 shinyapp!",
    text = "This app is built to give people an easy tool for accessing information about the COVID-19 pandemic in an interactive and transparent way.",
    type = "success",
    btn_labels = "Ok, let's start!",
    btn_colors = "darkorange",
    html = FALSE,
    closeOnClickOutside = TRUE,
    showCloseButton = FALSE,
    width = NULL
  )
  
  # Try to get timestamp when a user logs
  users_data <- data.frame(START = Sys.time())
  
  # This code will be run after the client has disconnected
  session$onSessionEnded(function() {
      users_data$END <- Sys.time()
      # Write a file in your working directory
      write.table(x = users_data, file = file.path(getwd(), "users_logs.txt"),
                  append = TRUE, row.names = FALSE, col.names = F, sep = ";")
  })
  
  # Count logged users
  onSessionStart <- isolate({
    users$count <- users$count + 1
  })
  
  
  onSessionEnded(function() {
    isolate({
      users$count <- users$count - 1
    })
    
  })
  
  
  output$NUsers <- renderUI({
    h5(paste0("There are ", users$count, " user(s) connected to this app"), align = "right")
  })
  
  
  # What is COVID19
  output$COV19 <- renderUI({HTML("<h2><b>What is COVID-19?</b></h2>","Coronavirus disease 2019 (COVID-19) is an infectious disease caused by Severe Acute Respiratory Syndrome Coronavirus 2 (SARS-CoV-2). The disease was first identified in December 2019 in Wuhan, the capital of China's Hubei province, and it has spread globally. The first confirmed case of what was then an unknown coronavirus was traced back to November 2019 in Hubei. Common symptoms include fever, cough, and shortness of breath. Other symptoms may include fatigue, muscle pain, diarrhoea, sore throat, loss of smell, and abdominal pain. The time from exposure to onset of symptoms is typically around five days but may range from two to fourteen days. While the majority of cases result in mild symptoms, some progress to viral pneumonia and multi-organ failure.")})
  
  
  # Pandemic situation today
  output$CurrentSitua <- renderUI({
    #HTML(print_current_situa(da = dati_Ita, dati_letalita = letalita_reg, dati_mortalita = mortalita_reg))
    HTML(paste0(tags$h2(tags$strong("Italian current situation about the COVID-19 pandemic*"), align = "center"),
                h5("*most recent update: ", paste_eng_date(today), align = "center")))

  })
  
  # Value boxes
  output$CumPos <- renderValueBox({
    
    valueBox(
      value = tod_summary$`Cumulative positives`[1],
      subtitle = HTML(paste0("<b>Cumulative positives</b> (", tod_summary$`Cumulative positives`[2], ")")),
      icon = icon("plus-square"),
      color = "red"
    )
    
  })
  
  output$CurrPos <- renderValueBox({
    
    valueBox(
      value = tod_summary$`Current positives`[1],
      subtitle = HTML(paste0("<b>Current positives</b> (", tod_summary$`Current positives`[2], ")")),
      icon = icon("user-plus"),
      color = "orange"
    )
  })
  
  output$CurrHosp <- renderValueBox({
    
    valueBox(
      value = tod_summary$`Current hospitalized`[1],
      subtitle = HTML(paste0("<b>Currently hospitalized</b> (", tod_summary$`Current hospitalized`[2], ")")),
      icon = icon("hospital"),
      color = "yellow"
    )
    
  })
  
  output$ICU <- renderValueBox({
    
    valueBox(
      value = tod_summary$`Intensive care`[1],
      subtitle = HTML(paste0("<b>Intensive care</b> (", tod_summary$`Intensive care`[2], ")")),
      icon = icon("procedures"),
      color = "yellow"
    )
  })
  
  output$HospSym <- renderValueBox({
    
    valueBox(
      value = tod_summary$`Hospitalized with symptoms`[1],
      subtitle = HTML(paste0("<b>Hospitalized with symptoms</b> (", tod_summary$`Hospitalized with symptoms`[2], ")")),
      icon = icon("ambulance"),
      color = "red"
    )
  })
  
  output$HomeIs <- renderValueBox({
   
    valueBox(
      value = tod_summary$`Home isolation`[1],
      subtitle = HTML(paste0("<b>Home isolation</b> (", tod_summary$`Home isolation`[2], ")")),
      icon = icon("home"),
      color = "orange"
    )
  })
  
  output$NewPos <- renderValueBox({
    
    valueBox(
      value = tod_summary$`New positives`[1],
      subtitle = HTML(paste0("<b>New positives</b> (", tod_summary$`New positives`[2], ")")),
      icon = icon("plus-square"),
      color = "orange"
    )
  })
  
  output$DishRec <- renderValueBox({
    
    valueBox(
      value = tod_summary$`Discharged recovered`[1],
      subtitle = HTML(paste0("<b>Discharged recovered</b> (", tod_summary$`Discharged recovered`[2], ")")),
      icon = icon("band-aid"),
      color = "yellow"
    )
  })
  
  output$Deces <- renderValueBox({
    
    valueBox(
      value = tod_summary$Deceased[1],
      subtitle = HTML(paste0("<b>Deceased</b> (", tod_summary$Deceased[2], ")")),
      icon = icon("ribbon"),
      color = "red"
    )
  })
  
  
  output$Swabs <- renderValueBox({
   
    valueBox(
      value = tod_summary$Swabs[1],
      subtitle = HTML(paste0("<b>Swabs</b> (", tod_summary$Swabs[2], ")")),
      icon = icon("syringe"),
      color = "red"
    )
  })
  
  
  output$MortRate <- renderValueBox({
  
    valueBox(
      value = paste0(tod_summary$Mortalita[1], "%"),
      subtitle = HTML("<b>Mortality rate</b>"),
      #subtitle = HTML("January <dialog open>This is an open dialog window</dialog>"),
      icon = icon("skull"),
      color = "orange"
    )
  })
  
  output$LetRate <- renderValueBox({
    
    valueBox(
      value = paste0(tod_summary$Letalita[1], "%"),
      subtitle = HTML("<b>Fatality rate</b>"),
      icon = icon("skull"),
      color = "yellow"
    )
  })
  
  # Help button for definitions
  observeEvent(input$ImpDef, {
    showModal(modalDialog(
      title = HTML("</br></br><h3><b>Important information:</b></h3>"),
      HTML("
  <ul><li><b>Cumulative positives =</b>  current positives + deceased + discharged recovered </li>
  <li><b>Current positives =</b> hospitalized with symptoms + intensive care + home isolation</li>
  <li><b>New positives =</b> current positives of today - current positives of yesterday</li>
  <li><b>Current hospitalized =</b> hospitalized with symptoms + intensive care</li>
  <li><b>Death rate =</b> deceased/population</li>
  <li><b>Fatality rate =</b> deceased/cumulative positives</li>
   <li><b>NB: </b> the data, as it is reported by Italian Protezione Civile, refers to the number of people that have been found positive to SARS-Cov-2. 
           This does not imply that the same number of people developed COVID-19 disease.</li></ul>"), 
      easyClose = TRUE
    ))
  })
  
  
  # Dynamic inputs
  output$SelDateTSandMap <- renderUI({
    sliderTextInput(
      inputId = "SelDateTSandMap",
      label = "Choose a day",
      grid = F,
      force_edges = TRUE,
      width = 250,
      selected = max(dati_reg$data),
      choices = seq(min(dati_reg$data), max(dati_reg$data),1)
    )
    
  })
  
  
  # If Variations, then select which
  output$IncrPercSiNo <- renderUI({
    
    if(input$IncrementiSiNo){
      prettyRadioButtons(inputId = "IncrPercSiNo", 
                   label = "", shape = "round", status = "danger",
                   choices = list("Absolute", "Relative (%)"), 
                   selected = "Absolute") 
    } else {
      return(NULL)
    }
    
  })
  
  # If region, then pick one
  output$DisplayRegRow1 <- renderUI({
    if(input$DonBarPerReg){
      pickerInput(inputId = "DisplayRegRow1",
                  label = "Pick a region",
                  choices = list("Northern Italy" = list("Lombardia", "Liguria", "Piemonte", "Valle d'Aosta",
                                               "Emilia-Romagna", "Friuli Venezia Giulia", "Veneto", "Trentino-Alto Adige"),
                                 "Central Italy" = list("Lazio", "Marche", "Toscana", "Umbria"),
                                 "Southern Italy" = list("Abruzzo", "Basilicata", "Calabria",
                                              "Campania", "Molise", "Puglia"),
                                 "Insular" = list("Sardegna", "Sicilia")),
                  selected = "Lombardia",
                  width = "fit", 
                  inline = F,
                  options = list(
                    `actions-box` = FALSE,
                    header = "Regions"
                  ))
    }else{
      return(NULL)
    }
    
  })
  
  
  # If region, then pick max. 5
  output$SelRegTSandMap <- renderUI({
    if(input$TSRegioneSiNo){
      pickerInput(inputId = "SelRegTSandMap",
                  label = "Pick one or more regions (max. 5)",
                  choices = list("Northern Italy" = list("Lombardia", "Liguria", "Piemonte", "Valle d'Aosta",
                                                         "Emilia-Romagna", "Friuli Venezia Giulia", "Veneto", "Trentino-Alto Adige"),
                                 "Central Italy" = list("Lazio", "Marche", "Toscana", "Umbria"),
                                 "Southern Italy" = list("Abruzzo", "Basilicata", "Calabria",
                                                         "Campania", "Molise", "Puglia"),
                                 "Insular" = list("Sardegna", "Sicilia")),
                  selected = c("Lombardia", "Piemonte"),
                  multiple = T,
                  width = "370px", 
                  options = list(
                    `actions-box` = FALSE,
                    header = "Regions",
                    `max-options` = 5
                  ))
    }else{
      return(NULL)
    }
    
  })
  
  
  # Pick region for model
  output$SelRegModel <- renderUI({
    if(input$ModRegioneSiNo){
      pickerInput(inputId = "SelRegModel",
                  label = "Select a region",
                  choices = list("Northern Italy" = list("Lombardia", "Liguria", "Piemonte", "Valle d'Aosta",
                                                         "Emilia Romagna", "Friuli V. G.", "Veneto", "TrentinoAltoAdige"),
                                 "Central Italy" = list("Lazio", "Marche", "Toscana", "Umbria"),
                                 "Southern Italy" = list("Abruzzo", "Basilicata", "Calabria",
                                                         "Campania", "Molise", "Puglia"),
                                 "Insular" = list("Sardegna", "Sicilia")),
                  selected = "Lombardia",
                  options = list(
                    `actions-box` = FALSE,
                    header = "Regions"
                  ))
    }else{
      return(NULL)
    }
    
  })
  
  
  # Donut plot
  output$DonughtPlot <- renderPlotly({
    
    donut_out <- do_ciambella(da = dati_reg, is.reg = input$DonBarPerReg, reg = input$DisplayRegRow1, variable = input$VarRow1)
    
    return(donut_out)
    
  })
  
  
  # Barplot
  output$BarStackPos <- renderPlotly({
    
    bar_out <- do_barplot_ts(da = dati_reg, is.reg = input$DonBarPerReg, reg = input$DisplayRegRow1, variable = input$VarRow1)
    
    return(bar_out)
    
  })
  
  
  # Map objects
  variable <- reactive({
    input$SelIdxforTSandMap
  })
  
  datetoselect <- reactive({
    input$SelDateTSandMap
  })
  
  Regselected <- reactive({
    if(input$TSRegioneSiNo){
      return(input$SelRegTSandMap)
    }else{
      return(NULL)
    }
  })
  
  # Map
  output$MapIta <- renderPlotly({
    
    mmmap <- draw_map(dajoined = joined_reg, reg = NULL, dajoinedprov = joined_prov,
                      varsel = as.character(variable()), datasel = as.character(datetoselect()))
    if(input$TSRegioneSiNo){
      mmmap <- draw_map(dajoined = joined_reg, reg = as.character(Regselected()), dajoinedprov = joined_prov,
                        varsel = as.character(variable()), datasel = as.character(datetoselect()))
    }
    
    return(mmmap)
    
  })
  
  # Time series
  output$TSIncrPerc <- renderPlotly({
    
      ts_plot <- do_ts(da = dati_reg, reg = as.character(Regselected()), is.reg = input$TSRegioneSiNo,
                       varsel = as.character(variable()), datasel = as.character(datetoselect()), 
                       is.incrementi = input$IncrementiSiNo, tipo.incremento = input$IncrPercSiNo)
    
    return(ts_plot)
  })
  
  ### Raw data
  output$Rawdata <- renderDT({
    
    # my_vals <- unique(Dt_out_raw$Country)
    # my_colors <- ifelse(my_vals=='Italy','grey1', "grey1")
    
    raw_today %>% 
      datatable(class = "cell-border stripe", rownames = F, filter = "top",
                options = list(initComplete = JS(
                  "function(settings, json) {",
                  "$('body').css({'font-family': 'Calibri'});",
                  "}"
                ),
                dom = 'tp', columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>% 
      formatStyle("Country", target = "row", #backgroundColor = styleEqual(my_vals,my_colors),
        fontWeight = "bold", fontSize = "120%")
    
    
  })
  
  # Downooad the raw data
  output$DownloadRawData <- downloadHandler(
    filename = function() {
      paste0("Covid19Data_upto_", today,".csv")
    },
    content = function(file) {
      write.csv(raw_today, file, row.names = FALSE)
    }
  )
  
  ###### Model
  
  # Objects
  VMSelected <- eventReactive(input$updatemodel, {
    input$VarForModel
  })
  
  RegMSelected <- eventReactive(input$updatemodel, {
    input$SelRegModel
  })
  
  FamMSelected <- eventReactive(input$updatemodel, {
    input$ModelFamily
  })
  
  # Run the model
  outputModello <- eventReactive( input$updatemodel, {
    
    updateSliderInput(session, inputId = "SelDateModel", value = 1)
    
    withProgress(message = "In progress...", value = 0.5, expr =  {             
      ## Your code
      if(input$ModRegioneSiNo){
        return(
          tryCatch(
            suppressWarnings(run_growth_model(da = data_formodel_prep, reg = RegMSelected(), 
                                              wh = VMSelected(), horizon = 30, fam = FamMSelected())),
            error = function(e){
              return("The model could not converge. Try again!")
              
            } 
          )
        )
      }else{
        return(
          tryCatch(
            suppressWarnings(run_growth_model(da = data_formodel_prep, reg = NULL, 
                                              wh = VMSelected(), horizon = 30, fam = FamMSelected())),
            error = function(e){
              return("The model could not converge. Try again!")
            } 
          )
        )
      }
    })
    
  })
  
  
  
  # Head
  ModelHeadReactive <- eventReactive(input$updatemodel, {
    HTML(paste0(tags$h3(tags$strong(paste0("Comparison between fitted and observed values for ", tolower(VMSelected()))), align = "center")))
    })
  output$ModelHead <- renderUI({ModelHeadReactive()})
  
  # Display bands and related messages
  output$Doyouwantbands <- renderUI({
    if(outputModello()$NoConv & !is.character(outputModello()$BandsError)){
      HTML(paste("<h5><b>Possible local optimum. Confidence interval could be inaccurate (or not available)!</b></h5>"))
    }else{
      return(NULL)
    }
  })
  # 
  output$Showbands <- renderUI({
    
    if(is.character(outputModello()$BandsError)){
      HTML(paste0("<b>Confidence interval is not available!</b><br/>"))
    } else{
      if(outputModello()$NoConv){
        checkboxInput(
          inputId = "Showbands",
          label = "Add 95% confidence interval",
          value = F
        )
      }else{
        return(NULL)
      }
    }
    
  })
  
  output$CannotPlotBands <- renderUI({
    if(input$Showbands & sum(is.na(outputModello()$stderrs[[1]]) | is.nan(outputModello()$stderrs[[1]]))>0){
      HTML(paste0("<b>Confidence interval is not available!</b><br/>"))
    }else{
      return(NULL)
    }
  })
  
  # Summary of the model
  output$SummaryModel <- reactive({
    
    if(!is.character(outputModello())){
      if(input$ModRegioneSiNo){
        return(
          summary_out_model(outputmod = outputModello(), VarModel = VMSelected(), resdata = residents, 
                            reg = RegMSelected(), is.reg = T)
        )
      }else{
        summary_out_model(outputmod = outputModello(), VarModel = VMSelected(), resdata = residents, is.reg = F, reg = NULL)
      }
      
    }else{
      return(HTML(outputModello()))
    }
    
    
  })
  
  # Plot 1
  output$PredCumCases <- renderPlotly({
    plot_out_model(outputmod = outputModello(), horizon = input$SelDateModel, what = "Cumulati",
                   VarModel = VMSelected(), showbands = input$Showbands)
  })
  
  # Plot 2
  output$PredNewCases <- renderPlotly({
    plot_out_model(outputmod = outputModello(), horizon = input$SelDateModel, 
                   what = "Nuovi", VarModel = VMSelected(), showbands = input$Showbands)
  })
  
  # Table model output
  output$DatPred <- renderDT({
    DT_out_model(outputmod = outputModello(), horizon = 15, VarModel = VMSelected(), showbands = input$Showbands)
  })
  
  
  
  #### ICU section
  output$ICuTab <- renderDT({
    
    outmod_terapie_table %>% dplyr::select(-DataPred) %>% 
      datatable(rownames = F, options = list(dom = 'tp', pageLength = 10, scrollX = T, 
                                             columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    
  })
  
  
  output$DownICUTomorrow <- downloadHandler(
    filename = function() {
      paste0("IntensiveCarePrediction_", datasuTabella,".csv")
    },
    content = function(file) {
      write.csv(outmod_terapie_table, file, row.names = FALSE)
    }
  )
  
  dataICUbar <- reactive({
    
    levels(outmod_terapie_plot$Region)[5] <- "Emilia-Romagna"
    levels(outmod_terapie_plot$Region)[6] <- "Friuli Venezia Giulia"
    levels(outmod_terapie_plot$Region)[17] <- "Trentino-Alto Adige"
    
    dplot <- outmod_terapie_plot %>% 
      rename(denominazione_regione = Region, data = DataPred) %>% 
      left_join(dati_reg %>% spread(Key, Value) %>% 
                  filter(data <= max(outmod_terapie_plot$DataPred)) %>% 
                  dplyr::select(data, starts_with("Int"), denominazione_regione), 
                by = c("denominazione_regione", "data")) 
    
    dplot
    
  })
  
  output$BarplotIcu <- renderPlotly({
    
    dataICUbar() %>% 
      filter(data == as.character(input$ICubarDate)) %>% 
      plot_ly(x = ~denominazione_regione, y = ~`Intensive care`, type = "bar", 
              name = "Observed", marker = list(color = "firebrick")) %>% 
      add_trace(y = ~Prediction, name = "Predicted", marker = list(color = "darkorange")) %>% 
      layout(title = paste0("Intensive care units on the ", paste_eng_date(input$ICubarDate)),
             xaxis = list(title = ""),
             yaxis = list(title = "ICUs"))
    
  })
  
  output$CovICUModel <- renderUI({
    
    dplot2 <- dataICUbar() %>% filter(data == as.character(input$ICubarDate))
    quanteIn <- sum(dplot2$`Intensive care` >= dplot2$`Lower bound` & dplot2$`Intensive care` <= dplot2$`Upper bound`)
    
    capts <- paste("the observed value fell into the 99% confidence bounds for", quanteIn, "regions out of 20.")
    HTML(paste("On the ", paste_eng_date(as.character(input$ICubarDate)), ", ",capts))
    
  })
  
  output$DownICUWhatever <- downloadHandler(
    filename = function() {
      paste0("IntensiveCarePrediction_", as.character(input$ICubarDate),".csv")
    },
    content = function(file) {
      write.csv(dataICUbar() %>% filter(data == as.character(input$ICubarDate)), file, row.names = FALSE)
    }
  )
  
}


shinyApp(ui = ui, server = server)