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
require(rdrop2)

rm(list=ls())


Sys.setlocale(locale = "C")

# Load functions and data
source("Script/FunctionsFinal2.R")
source("Script/UsefulFunsApp2.R")

# Read residents data
residents <- read_residents(path = "Data/residenti2019.csv")

# Read data for regional map
italy_sf_reg <- read_shape_italy(province = F)

# Read data for province map
italy_sf_prov <- read_shape_italy(province = T)

TotPop <- 60359546

# Count user logged
users = reactiveValues(count = 0)

# Get decrees
decrees <- get_decrees()
decreeplot <- plot_decree(decrees)

# ICU
token <- readRDS("droptoken.rds")
# Then pass the token to each drop_ function
drop_acc(dtoken = token)
tICU <- reactiveValues(tb = loadData(token = token))

reg_choices <- list("Northern Italy" = list("Lombardia", "Liguria", "Piemonte", "Valle d'Aosta", "Emilia-Romagna", "Friuli Venezia Giulia", "Veneto", "Trentino-Alto Adige"),
                    "Central Italy" = list("Lazio", "Marche", "Toscana", "Umbria"),
                    "Southern Italy" = list("Abruzzo", "Basilicata", "Calabria", "Campania", "Molise", "Puglia"),
                    "Insular" = list("Sardegna", "Sicilia"))

# token <- drop_auth()
# saveRDS(token, "droptoken.rds")


# ShinyApp ----------------------------------------------------------------
ui <- navbarPage(theme = shinytheme("sandstone"), 
                 title = 'Analysis of the italian COVID-19 pandemic',
                 # First tab: Overview
                 tabPanel(title = "Overview", icon = icon("binoculars"),
                          
                          div(style="margin-top:-3.5em",
                              
                              fluidRow(tags$hr(),
                                       useShinydashboard(),
                                       column(width = 12,
                                              htmlOutput(outputId = "CurrentSitua"),
                                              valueBoxOutput("CumPos"), valueBoxOutput("CurrPos"), valueBoxOutput("CurrHosp"),
                                              valueBoxOutput("ICU"), valueBoxOutput("HospSym"), valueBoxOutput("HomeIs"),
                                              valueBoxOutput("NewPos"), valueBoxOutput("DishRec"), valueBoxOutput("Deces"),
                                              valueBoxOutput("Swabs"), valueBoxOutput("TestedCases"), valueBoxOutput("LetRate"),
                                              
                                              actionBttn(inputId = "ImpDef", icon = icon("question"), 
                                                         style = "material-circle", color = "warning", size = "sm"),
                                              helpText("Any doubts? Click the help button or"), 
                                              downloadLink(outputId = "USGuideDown", label = "Download the complete user guide"),
                                              style="text-align:justify;padding:20px;")
                              ),
                              fluidRow(
                                column(11, uiOutput(outputId = "COV19"), style="text-align:justify;padding:20px;")
                                
                              )
                          ), 
                          
                          tags$style(HTML("hr {border-top: 1px solid #000000;}")),
                          tags$hr(),
                          fluidPage(
                            fluidRow(
                              column(width = 12,
                                     plotlyOutput(outputId = "decrees_timeline")
                              )
                            ),
                            tags$hr(),
                            fluidRow(
                              sidebarLayout(
                                sidebarPanel(width = 3,
                                             prettyRadioButtons(
                                               inputId = "VarRow1", label = "Pick a variable", 
                                               choices = list("Cumulative positives", "Current positives"),
                                               selected = "Cumulative positives", status = "danger", shape = "round",
                                               inline = F, icon = icon("check")),
                                             prettySwitch(
                                               inputId = "DonBarPerReg", label = "View by region",
                                               value = F, status = "danger", slim = T, bigger = F
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
                                                         choices = list("Cumulative positives", "Current positives", "New positives",
                                                                        "Current hospitalized", "Hospitalized with symptoms","Intensive care",
                                                                        "Home isolation","Discharged recovered", "Deceased", "Swabs",
                                                                        "Tested cases"),
                                                         width = "fit",
                                                         selected = "Cumulative positives"),
                                             helpText("Note that for tested cases data are only available from the 19th April 2020."),
                                             prettySwitch(inputId = "TSRegioneSiNo", status = "danger", bigger = F, slim = T,
                                                          label = "View time series by region",
                                                          value = F),
                                             uiOutput(outputId = "SelRegTSandMap"),
                                             prettyRadioButtons(inputId = "IncrementiSiNo", 
                                                                label = "Type of time series:",
                                                                choices = list("raw","daily variations", "weekly variations"),
                                                                status = "danger",icon = icon("check"), 
                                                                selected = "raw"),
                                             uiOutput(outputId = "IncrPercSiNo"),
                                             prettyCheckbox(inputId = "WantDensity", label = "Rescale map w.r.t. the population size", 
                                                            status = "danger",icon = icon("check"), value = F),
                                             uiOutput(outputId = "SelDateTSandMap")
                                             ),
                                mainPanel(
                                  splitLayout(
                                    cellWidths = c("53%", "55%"),
                                    addSpinner(plotlyOutput(outputId = "TSIncrPerc", height = "450px"), spin = "fading-circle", color = "firebrick"),
                                    addSpinner(plotlyOutput(outputId = "MapIta", height = "450px"), spin = "fading-circle", color = "firebrick")
                                    
                                  )
                                )
                              )
                              ),
                            
                            tags$hr(),
                            fluidRow(
                              sidebarLayout(
                                sidebarPanel(width = 3,
                                             pickerInput(inputId = "RatioIn", label = "Select a rate",
                                                         choices = list("Positivity", "Fatality",
                                                                        "Healing", "Severity"),
                                                         selected = "New positives/Swabs"),
                                             prettySwitch(inputId = "Ratio_by_region", value = F, status = "danger", label = "View by region",
                                                          bigger = F, slim = T),
                                             uiOutput(outputId = "RegionRatio"),
                                             helpText("The time series of each rate is obtained as the ratio between the indicators showed in the bar plot. In particular, orange color is for the indicator at the enumerator, red one is for the denominator.")
                                ),
                                mainPanel(
                                  splitLayout(
                                    cellWidths = c("53%", "55%"),
                                    plotlyOutput(outputId = "TS_ratio"),
                                    plotlyOutput(outputId = "Bar_ratio")
                                  )
                                )
                                )
                            ),
                            tags$hr(),
                            fluidRow(
                              tags$h3(tags$strong("Table with the raw data"), align = "center"),
                              uiOutput(outputId = "RawDatadate"),
                              addSpinner(DTOutput(outputId = "Rawdata"),spin = "fading-circle", color = "firebrick"),
                              tags$br(),
                              downloadButton(outputId = "DownloadRawData"),
                              helpText("Click the button to download all the raw data up to the most recent update")
                            ),
                            tags$hr(),
                            uiOutput("NUsers", inline = T)
                          
                 )),
                 # Second tab: Model
                 tabPanel("Short-term forecast", icon = icon("chart-line"),
                          fluidPage(
                            tags$head(
                              tags$style(
                                HTML(".shiny-notification {
              height: 100px;
              width: 300px;
              position:fixed;
              top: calc(10%);;
              left: calc(50% - 50px);;
            }
           "
                                )
                              )
                            ),
                            tags$h2(tags$strong("Generalized linear (Mirrored) Richards model*")),
                            # tags$h5("*further details on the methodology can be found ",
                            #         tags$a(href = "https://statgroup-19.blogspot.com/p/covid-19-epidemic-progress-medium-term.html" , "here")),
                            tags$h5("*for further details on the methodology we recommend to look at the user guide."),
                            sidebarLayout(
                              sidebarPanel(
                                tags$h3(tags$strong("Model parameters")),
                                pickerInput(inputId = "VarForModel", 
                                            label = "Select the variable you want to model",
                                            choices = list("New positives", "Cumulative positives", "Deceased",
                                                           "Current hospitalized", "Hospitalized with symptoms",
                                                           "Home isolation","Discharged recovered"),
                                            selected = "New positives", width = "300px"), 
                                uiOutput(outputId = "ExcludeDaysModel"),
                                radioButtons(inputId = "ModelFamily",
                                             label = "Choose a distribution",
                                             choices = list( "Negative Binomial", "Poisson"),
                                             selected = "Negative Binomial"),
                                helpText("If you are not satisfied by the Poisson coverage, try the Negative Binomial (we recommend it)!"),
                                materialSwitch(inputId = "ModRegioneSiNo", 
                                               label = HTML("<b>Model by region</b>"), 
                                               value = F, status = "danger"),
                                uiOutput(outputId = "SelRegModel"),
                                actionBttn(inputId = "updatemodel", label = "Fit the model", style = "pill", color = "warning"),
                                helpText("Select the parameters, push the button and wait for the model to be estimated"),
                                tags$hr(),
                                uiOutput(outputId = "Doyouwantbands"),
                                uiOutput(outputId = "Showbands"),
                                uiOutput(outputId = "CannotPlotBands"),
                                sliderInput(
                                  inputId = "SelDateModel", label = "Forecast horizon (days):", width = 250,
                                  value = 1,min = 1, max = 15,
                                  animate = animationOptions(interval = 500, loop = F)
                                ), 
                                helpText("Push the play button to see the time forecast"),
                                tags$br()#,
                              ),
                              
                              mainPanel(
                                tags$style(type="text/css",
                                           ".shiny-output-error { visibility: hidden; }",
                                           ".shiny-output-error:before { visibility: hidden; }"
                                ), 
                                
                                verticalLayout(
                                  fluidRow(
                                    splitLayout(cellWidths = c("60%", "39%"), 
                                                plotlyOutput("PredCumCases"), 
                                                htmlOutput("SummaryModel")
                                    )
                                  ), 
                                  tags$hr(),
                                  fluidRow(DTOutput("DatPred"))
                                )
                              )
                            )
                          )
                 ),
                 # Third tab: ICU predictions
                 tabPanel(title ="ICU nowcasting", icon = icon("hospital"), 
                          fluidPage(
                            htmlOutput(outputId = "ICUTabHead"),# 3
                            # tags$h5("*further details on the methodology can be found ",
                            #         tags$a(href = "https://statgroup-19.blogspot.com/p/short-term-predictions-of-daily.html" , "here")),
                            tags$h5("*for further details on the methodology we recommend to look at the user guide."),
                            fluidRow(addSpinner(DTOutput("ICuTab"), spin = "fading-circle", color = "firebrick"), 
                                     helpText("Click the button to download the predictions"),
                                     downloadButton(outputId = "DownICUTomorrow", label = "Download")),
                            tags$hr(),
                            fluidRow(
                              sidebarLayout(
                                sidebarPanel(
                                  
                                  helpText(tags$h4(tags$strong("Comparison of predicted vs observed values on the chosen date*")),
                                           tags$h5("*starting from 17th of March 2020")),
                                  uiOutput(outputId = "ICubarDate"),
                                  uiOutput("CovICUModel"),
                                  helpText("Click the button to download the predictions on the date you chose"),
                                  downloadButton(outputId = "DownICUWhatever", label = "Download")
                                  
                                ), 
                                mainPanel(addSpinner(plotlyOutput("BarplotIcu"), spin = "fading-circle", color = "firebrick"))))
                          )),
                 # Fourth tab: Info and credits
                 tabPanel("Info and Credits", icon = icon("info-circle"), div(style="margin-top:-2.5em", includeMarkdown("InfoandCredits.md")))
                 
)


server <- function(input, output, session){
  # token <- readRDS("droptoken.rds")
  # # Then pass the token to each drop_ function
  # drop_acc(dtoken = token)
  
  # Welcome message
  sendSweetAlert(
    session,
    title = "This is the StatGroup-19 app!",
    text = "The app is free and is built to provide the general public with a tool for accessing information about the Italian COVID-19 epidemic in an interactive and transparent way.",
    type = "success",
    btn_labels = "Ok, let's start!",
    btn_colors = "darkorange",
    html = FALSE,
    closeOnClickOutside = TRUE,
    showCloseButton = FALSE,
    width = NULL
  )
  
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
  
  # Read aggregated Italian data up to today 
  dati_Ita <- reactive({
    read_italian(path = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv")
  })
  tdy <- reactive({max(dati_Ita()$data)})
  
  # Read regional data up to today
  dati_reg <- reactive({
    read_regional(path = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
  })
  
  # Read province data up o today
  dati_prov <- reactive({
    read_province(path = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv")
  })
  
  # Join data regional
  joined_reg <- reactive({
    italy_sf_reg %>% 
      sp::merge(dati_reg(), by.y = "denominazione_regione", by.x = "NAME") %>% 
      add_residents_tomap(resdata = residents)
  })
  
  # Join data province
  joined_prov <- reactive({
    italy_sf_prov %>% 
      sp::merge(dati_prov() %>% dplyr::select(data, denominazione_provincia, denominazione_regione, `Cumulative positives`),
                by.y = c("denominazione_regione", "denominazione_provincia"),
                by.x = c("NAME", "NAME_2"), all.x = T) %>% 
      add_residents_tomap(resdata = residents) %>% 
      gather(Key, Value, `Cumulative positives`) %>% 
      dplyr::select(NAME_2, NAME, Key, data, Value, geometry, residenti)
  })
  
  
  # Data preparation for the model
  data_formodel_prep <- reactive({prepdata_for_model(dftoprep = isolate(dati_reg()) %>% spread(Key, Value), resdata = residents)})
  
  # Today summary
  tod_summary <- reactive({return_current_situa(da = dati_Ita(), TotPop = TotPop)})
  
  
  # Table with today raw data
  raw_today <- reactive({show_raw_data(dati_Ita(), dati_reg())})
  
  # Date reactive inputs
  output$SelDateTSandMap <- renderUI({
    sliderTextInput(
      inputId = "SelDateTSandMap", label = "Choose a day", grid = F, force_edges = TRUE, width = 250, selected = tdy(), choices = seq(min(dati_Ita()$data), tdy(),1)
    )
  })
  
  # Data for raw data
  output$RawDatadate <- renderUI({
    airDatepickerInput(inputId = "RawDatadate", label = "Select a date", clearButton = F, todayButton = T, multiple = F, minDate = min(dati_Ita()$data), maxDate = tdy(), value = tdy())
  })
  
  ### Raw data
  output$Rawdata <- renderDT({table_raw_data(tab = raw_today(), datesel = input$RawDatadate, resdata = residents)})
  
  # Downooad the raw data
  output$DownloadRawData <- downloadHandler(
    filename = function() {
      paste0("Covid19Data_upto_", tdy(),".csv")
    },
    content = function(file) {
      write.csv(raw_today(), file, row.names = FALSE)
    }
  )
  
  # Data for model
  output$ExcludeDaysModel <- renderUI({
    sliderInput(inputId = "ExcludeDaysModel", label = "Fitting interval (exclude up to the last 15 days):", 
                min = tdy()-14, max = tdy(), value = tdy(), width = "300px")
  })
  
  # What is COVID19
  output$COV19 <- renderUI({whatiscovid()})
  
  # Timeline of decrees
  output$decrees_timeline <- renderPlotly({decreeplot})
  
  # Pandemic situation today
  output$CurrentSitua <- renderUI({
    HTML(paste0(tags$h2(tags$strong("Italian current situation about the COVID-19 pandemic*"), align = "center"), h5("*most recent update: ", paste_eng_date(tdy()), align = "center")))
  })
  
  # Value boxes
  output$CumPos <- renderValueBox({
    valueBox(
      value = prettyNum(tod_summary()$`Cumulative positives`[1], big.mark = " "),
      subtitle = HTML(paste0("<b>Cumulative positives</b> (", tod_summary()$`Cumulative positives`[2], ")")),
      icon = icon("plus-square"),
      color = "red"
    )
    
  })
  
  output$CurrPos <- renderValueBox({
    valueBox(
      value = prettyNum(tod_summary()$`Current positives`[1], big.mark = " "),
      subtitle = HTML(paste0("<b>Current positives</b> (", tod_summary()$`Current positives`[2], ")")),
      icon = icon("user-plus"),
      color = "orange"
    )
  })
  
  output$CurrHosp <- renderValueBox({
    valueBox(
      value = prettyNum(tod_summary()$`Current hospitalized`[1], big.mark = " "),
      subtitle = HTML(paste0("<b>Currently hospitalized</b> (", tod_summary()$`Current hospitalized`[2], ")")),
      icon = icon("hospital"), 
      color = "yellow"
    )
  })
  
  output$ICU <- renderValueBox({
    valueBox(
      value = prettyNum(tod_summary()$`Intensive care`[1], big.mark = " "),
      subtitle = HTML(paste0("<b>Intensive care</b> (", tod_summary()$`Intensive care`[2], ")")),
      icon = icon("procedures"),
      color = "yellow"
    )
  })
  
  output$HospSym <- renderValueBox({
    valueBox(
      value = prettyNum(tod_summary()$`Hospitalized with symptoms`[1], big.mark = " "),
      subtitle = HTML(paste0("<b>Hospitalized with symptoms</b> (", tod_summary()$`Hospitalized with symptoms`[2], ")")),
      icon = icon("ambulance"),
      color = "red"
    )
  })
  
  output$HomeIs <- renderValueBox({
    valueBox(
      value = prettyNum(tod_summary()$`Home isolation`[1], big.mark = " "),
      subtitle = HTML(paste0("<b>Home isolation</b> (", tod_summary()$`Home isolation`[2], ")")),
      icon = icon("home"),
      color = "orange"
    )
  })
  
  output$NewPos <- renderValueBox({
    valueBox(
      value = prettyNum(tod_summary()$`New positives`[1], big.mark = " "),
      subtitle = HTML(paste0("<b>New positives</b> (", tod_summary()$`New positives`[2], ")")),
      icon = icon("plus-square"),
      color = "orange"
    )
  })
  
  output$DishRec <- renderValueBox({
    valueBox(
      value = prettyNum(tod_summary()$`Discharged recovered`[1], big.mark = " "),
      subtitle = HTML(paste0("<b>Discharged recovered</b> (", tod_summary()$`Discharged recovered`[2], ")")),
      icon = icon("band-aid"),
      color = "yellow"
    )
  })
  
  output$Deces <- renderValueBox({
    valueBox(
      value = prettyNum(tod_summary()$Deceased[1], big.mark = " "),
      subtitle = HTML(paste0("<b>Deceased</b> (", tod_summary()$Deceased[2], ")")),
      icon = icon("ribbon"),
      color = "red"
    )
  })
  
  
  output$Swabs <- renderValueBox({
    valueBox(
      value = prettyNum(tod_summary()$Swabs[1], big.mark = " "),
      subtitle = HTML(paste0("<b>Swabs</b> (", tod_summary()$Swabs[2], ")")),
      icon = icon("syringe"),
      color = "red"
    )
  })
  
  
  output$TestedCases <- renderValueBox({
    valueBox(
      value = prettyNum(tod_summary()$`Tested cases`[1], big.mark = " "),
      subtitle = HTML(paste0("<b>Tested cases</b> (", tod_summary()$`Tested cases`[2], ")")),
      #subtitle = HTML("January <dialog open>This is an open dialog window</dialog>"),
      icon = icon("band-aid"),
      color = "orange"
    )
  })
  
  output$LetRate <- renderValueBox({
    valueBox(
      value = paste0(tod_summary()$Letalita[1], "%"),
      subtitle = HTML(paste0("<b>Fatality rate</b> (mortality rate is ", tod_summary()$Mortalita,"%)")),
      icon = icon("skull"),
      color = "yellow"
    )
  })
  
  # Help button for definitions
  observeEvent(input$ImpDef, {helpbttn()})
  
  # Download the user guide
  output$USGuideDown <- downloadHandler(
    filename = function() paste0("COVIDApp_UserGuide_SG19", ".pdf"),
    content = function(file) {
      file.copy("User_Guide.pdf", file)
    }
  )
  
  
  # If Variations, then select which
  output$IncrPercSiNo <- renderUI({
    if(input$IncrementiSiNo %in% c("daily variations","weekly variations")){
      prettyRadioButtons(inputId = "IncrPercSiNo", label = "", shape = "round", status = "danger", choices = list("Absolute", "Relative (%)"), icon = icon("check"), selected = "Absolute", inline = T)
    } else {
      return(NULL)
    }
  })
  
  # If region, then pick one
  output$DisplayRegRow1 <- renderUI({
    if(input$DonBarPerReg){
      pickerInput(inputId = "DisplayRegRow1", label = "Pick a region",
                  choices = reg_choices,
                  selected = "Lombardia", width = "fit", inline = F, options = list(`actions-box` = FALSE,header = "Regions"))
    }else{
      return(NULL)
    }
    
  })
  
  # If region, then pick max. 5
  output$SelRegTSandMap <- renderUI({
    if(input$TSRegioneSiNo){
      pickerInput(inputId = "SelRegTSandMap", label = "Pick one or more regions (max. 5)",
                  choices = reg_choices,
                  selected = c("Lombardia", "Piemonte"), multiple = T, width = "370px", 
                  options = list( `actions-box` = FALSE,  header = "Regions",  `max-options` = 5))
    }else{
      return(NULL)
    }
    
  })
  
  
  # Pick region for model
  output$SelRegModel <- renderUI({
    if(input$ModRegioneSiNo){
      pickerInput(inputId = "SelRegModel",
                  label = "Select a region",
                  choices = reg_choices,
                  selected = "Lombardia",width = "300px",
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
    donut_out <- do_ciambella(da = dati_reg(), is.reg = input$DonBarPerReg, reg = input$DisplayRegRow1, variable = input$VarRow1)
    return(donut_out)
  })
  
  # Barplot
  output$BarStackPos <- renderPlotly({
    bar_out <- do_barplot_ts(da = dati_reg(), is.reg = input$DonBarPerReg, reg = input$DisplayRegRow1, variable = input$VarRow1)
    return(bar_out)
  })
  
  # objects
  variable <- reactive({input$SelIdxforTSandMap })
  datetoselect <- reactive({input$SelDateTSandMap})
  Regselected <- reactive({
    if(input$TSRegioneSiNo){
      return(input$SelRegTSandMap)
    }else{
      return(NULL)
    }
  })
  
  # Map
  output$MapIta <- renderPlotly({
    mmmap <- draw_map(dajoined = joined_reg(), reg = NULL, dajoinedprov = joined_prov(), is.density = input$WantDensity,
                      varsel = as.character(variable()), datasel = as.character(datetoselect()))
    if(input$TSRegioneSiNo){
      mmmap <- draw_map(dajoined = joined_reg(), reg = as.character(Regselected()), dajoinedprov = joined_prov(), is.density = input$WantDensity,
                        varsel = as.character(variable()), datasel = as.character(datetoselect()))
    }
    return(mmmap)
  })
  
  # Time series
  output$TSIncrPerc <- renderPlotly({
    if(input$IncrementiSiNo %in% c("daily variations","weekly variations")){
      ts_plot <- do_ts(da = dati_reg(), reg = as.character(Regselected()), is.reg = input$TSRegioneSiNo,lag_incr = as.character(input$IncrementiSiNo),
                       varsel = as.character(variable()), datasel = as.character(datetoselect()), is.incrementi = T, tipo.incremento = input$IncrPercSiNo)
    }else{
      ts_plot <- do_ts(da = dati_reg(), reg = as.character(Regselected()), is.reg = input$TSRegioneSiNo, lag_incr = as.character(input$IncrementiSiNo),
                       varsel = as.character(variable()), datasel = as.character(datetoselect()), is.incrementi = F, tipo.incremento = input$IncrPercSiNo)
    }
    return(ts_plot)
  })
  
  # RATIOS
  output$RegionRatio <- renderUI({
    if(input$Ratio_by_region){
      pickerInput(inputId = "RegionRatio", label = "Pick a region",
                  choices = reg_choices,
                  selected = "Lombardia", width = "300px")
    }else{
      return(NULL)
    }
  })
  
  # Barplot ratios
  output$Bar_ratio <- renderPlotly({
    bar_out <- plot_ratios(da = dati_reg(), is.reg = F, reg = NULL, type_of_ratio = input$RatioIn, plot_type = "barplot")
    if(input$Ratio_by_region){
      bar_out <- plot_ratios(da = dati_reg(), is.reg = T, reg = input$RegionRatio, type_of_ratio = input$RatioIn, plot_type = "barplot")
    }
    return(bar_out)
  })
  #Time series ratios
  output$TS_ratio <- renderPlotly({
    ts_rat_out <- plot_ratios(da = dati_reg(), is.reg = F, reg = NULL, type_of_ratio = input$RatioIn, plot_type = "ts")
    if(input$Ratio_by_region){
      ts_rat_out <- plot_ratios(da = dati_reg(), is.reg = T, reg = input$RegionRatio, type_of_ratio = input$RatioIn, plot_type = "ts")
    }
    return(ts_rat_out)
  })
  
  # Icu data preparation
  
  # Modello terapia intensiva
  tICU$tb <- loadData(token = token)
  
  icu_data <- reactive({
    if(isolate(tdy()) == max(isolate(tICU$tb)$DataPred)){dataprep_terapie(dftoprep = dati_reg() %>% spread(Key, Value), resdata = residents)}
  })
  
  if(isolate(tdy()) == max(isolate(tICU$tb)$DataPred)){
    showModal(modalDialog(
      HTML("<p align='center'>We are updating the app..please be patient and wait!</p>"),
      size = "m",
      title = HTML("<b>WARNING</b>"), easyClose = F))
  }
  
  outmod_terapie_tomor <- reactive({
    if(isolate(tdy()) == max(isolate(tICU$tb)$DataPred)){modello_terapia_intensiva(dat = icu_data()$da, dattopred = icu_data()$dapred)}
  })
  
  isolate(
    if(isolate(tdy()) == max(isolate(tICU$tb)$DataPred)){ 
      tICU$tb %<>% bind_rows(isolate(outmod_terapie_tomor()) %>% mutate(DataPred = max(isolate(tICU$tb)$DataPred)+1))
      saveData(tICU$tb, token = token)
    }
  )
  
  datasuTabella <- max(isolate(tICU$tb)$DataPred)
  outmod_terapie_plot <- isolate(tICU$tb) %>% filter(DataPred < datasuTabella)
  outmod_terapie_table <- isolate(tICU$tb) %>% filter(DataPred == datasuTabella)
  
  # Data head ICU
  output$ICUTabHead <- renderUI({
    tags$h2(tags$strong(paste("Prediction* of Intensive care units on the ", paste_eng_date(datasuTabella))))
  })
  
  # Data barplot ICU
  output$ICubarDate <- renderUI({
    airDatepickerInput(inputId = "ICubarDate", label = "Choose a date", value = datasuTabella-1,
                       minDate = "2020-03-17", maxDate = datasuTabella-1, width = "250px")
  })
  
  
  #### ICU section
  output$ICuTab <- renderDT({
    tab_tomor_ICU(outmod_terapie_table, outmod_terapie_plot)
  })
  # 
  # 
  output$DownICUTomorrow <- downloadHandler(
    filename = function() { paste0("IntensiveCarePrediction_", datasuTabella,".csv") },
    content = function(file) { write.csv(outmod_terapie_table, file, row.names = FALSE) }
  )
  # 
  dataICUbar <- reactive({prep_bar_ICU(outmod_terapie_plot, dati_reg())})
  # 
  output$BarplotIcu <- renderPlotly({
    barplot_ICU(dataICUbar(),as.character(input$ICubarDate))
  })
  
  output$CovICUModel <- renderUI({
    dplot2 <- dataICUbar() %>% filter(data == as.character(input$ICubarDate))
    quanteIn <- sum(dplot2$`Intensive care` >= dplot2$`Lower bound` & dplot2$`Intensive care` <= dplot2$`Upper bound`)
    
    capts <- paste("the observed value fell into the 99% confidence bounds for", as.character(quanteIn), "regions out of 20.")
    HTML(paste("On the ", paste_eng_date(as.character(input$ICubarDate)), ", ",capts))
  })
  
  output$DownICUWhatever <- downloadHandler(
    filename = function() { paste0("IntensiveCarePrediction_", as.character(input$ICubarDate),".csv") },
    content = function(file) { write.csv(dataICUbar() %>% filter(data == as.character(input$ICubarDate)), file, row.names = FALSE) }
  )
  
  ###### Model
  # Objects
  VMSelected <- eventReactive(input$updatemodel, {input$VarForModel})
  RegMSelected <- eventReactive(input$updatemodel, {input$SelRegModel})
  FamMSelected <- eventReactive(input$updatemodel, {input$ModelFamily})
  RegModSiNo <-  eventReactive(input$updatemodel, {input$ModRegioneSiNo})
  
  # Run the model
  outputModello <- eventReactive( input$updatemodel, {
    
    updateSliderInput(session, inputId = "SelDateModel", value = 1)
    
    withProgress(message = "In progress...", detail = "please be patient", value = 0.5, expr =  {             
      ## Your code
      if(RegModSiNo()){
        return(
          tryCatch(
            suppressWarnings(run_growth_model(da = data_formodel_prep(), reduce_obs = as.character(input$ExcludeDaysModel), reg = RegMSelected(), 
                                              wh = VMSelected(), horizon = 30, fam = FamMSelected())),
            error = function(e){ return("The model could not converge. Try again!") } 
          )
        )
      }else{
        return(
          tryCatch(
            suppressWarnings(run_growth_model(da = data_formodel_prep(), reg = NULL, reduce_obs = as.character(input$ExcludeDaysModel),
                                              wh = VMSelected(), horizon = 30, fam = FamMSelected())),
            error = function(e){ return("The model could not converge. Try again!") } 
          )
        )
      }
    })
    
  })
  
  
  # Display bands and related messages
  output$Doyouwantbands <- renderUI({
    if(outputModello()$NoConv & !is.character(outputModello()$BandsError)){
      HTML(paste("<h5><b>Possible local optimum. Confidence interval could be inaccurate (or not available)!</b></h5>"))
    }else{
      return(NULL)
    }
  })
   
  
  output$Showbands <- renderUI({
    if(is.character(outputModello()$BandsError)){
      HTML(paste0("<b>Confidence interval is not available!</b><br/>"))
    } else{
      if(outputModello()$NoConv){
        checkboxInput(inputId = "Showbands", label = "Add 99% confidence interval", value = F)
      }else{ return(NULL) }
    }
  })
  
  output$CannotPlotBands <- renderUI({
    if(input$Showbands & sum(is.na(outputModello()$stderrs) | is.nan(outputModello()$stderrs))>0){
      HTML(paste0("<b>Confidence interval is not available!</b><br/>"))
    }else{
      return(NULL)
    }
  })
  
  # Summary of the model
  output$SummaryModel <- reactive({
    if(!is.character(outputModello())){
      sum_out <-  summary_out_model(outputmod = outputModello(), VarModel = VMSelected(), resdata = residents, reg = RegMSelected(), is.reg = RegModSiNo())
    }else{
      sum_out <- HTML(outputModello())
    }
    return(sum_out)
  })
  
  # Plot 1
  output$PredCumCases <- renderPlotly({
    reg <- ifelse(is.null(RegMSelected()), "Italy", as.character(RegMSelected()))
    plot_out_model(outputmod = outputModello(), horizon = input$SelDateModel, VarModel = VMSelected(), showbands = input$Showbands, reg = reg)
  })
  
  # Table model output
  output$DatPred <- renderDT({
    reg <- ifelse(is.null(RegMSelected()), "Italy", as.character(RegMSelected()))
    DT_out_model(outputmod = outputModello(), horizon = 15, VarModel = VMSelected(), showbands = input$Showbands, reg = reg)
  })
  
  
}

shinyApp(ui = ui, server = server)