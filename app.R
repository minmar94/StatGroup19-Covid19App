# Load packages
require(tidyverse)
require(magrittr)
require(shiny)
require(shinythemes)
require(shinyWidgets)
require(lubridate)
require(DT)
require(stringi)
require(tscount)
require(foreach)
require(doSNOW)
require(sf)
require(sp)
require(rmarkdown)
require(shinydashboard)
require(plotly)
require(leaflet)
require(rgeos)
require(lme4)
require(rdrop2)
rm(list=ls())


Sys.setlocale(locale = "C")

# Load functions and data
source("Script/Fun_DRichFit_CovsOnLambda.R")
source("Script/UsefulFunsApp2.R")

# Read residents data
residents <- read_residents(path = "Data/residenti2019.csv")
# dati_Ita <- read_italian(path = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv")
# dati_reg <- read_regional(path = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
# data_formodel_prep <- prepdata_for_model(dati_reg %>% spread(Key, Value), residents)


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

capacitaICU <- read_csv("Data/CapacitaICU.csv")

reg_choices <- list("Northern Italy" = list("Lombardia", "Liguria", "Piemonte", "Valle d'Aosta", "Emilia-Romagna", "Friuli Venezia Giulia", "Veneto", "Trentino-Alto Adige"),
                    "Central Italy" = list("Lazio", "Marche", "Toscana", "Umbria"),
                    "Southern Italy" = list("Abruzzo", "Basilicata", "Calabria", "Campania", "Molise", "Puglia"),
                    "Insular" = list("Sardegna", "Sicilia"))

# ShinyApp ----------------------------------------------------------------
ui <- navbarPage(theme = shinytheme("sandstone"), 
                 title = 'Analysis of the italian COVID-19 pandemic',
                 # First tab: Overview
                 tabPanel(title = "Overview", icon = icon("binoculars"),
                          
                          div(style="margin-top:-3.5em",
                              
                              fluidRow(HTML('<meta name="viewport" content="width=1024">'),tags$hr(),
                                       useShinydashboard(),
                                       column(width = 12,
                                              htmlOutput(outputId = "CurrentSitua"),
                                              valueBoxOutput("CumPos"), valueBoxOutput("Deces"), valueBoxOutput("ICU"), 
                                              valueBoxOutput("DishRec"), valueBoxOutput("Swabs"), valueBoxOutput("TestedCases"), 
                                              valueBoxOutput("CurrPos"),valueBoxOutput("HospSym"), valueBoxOutput("HomeIs"),
                                              #valueBoxOutput("CurrHosp"),
                                              #valueBoxOutput("NewPos"), 
                                              valueBoxOutput("PosRate"), valueBoxOutput("LetRate"), valueBoxOutput("MortRate"),
                                              #valueBoxOutput("ICUStress"), 
                                              
                                              actionBttn(inputId = "ImpDef", icon = icon("question"), 
                                                         style = "material-circle", color = "warning", size = "sm"),
                                              helpText("Any doubts? Click the help button or"), 
                                              downloadLink(outputId = "USGuideDown", label = "Download the complete user guide"),
                                              style="text-align:justify;padding:20px;")
                              ),
                              fluidRow(HTML('<meta name="viewport" content="width=1024">'),
                                column(11, uiOutput(outputId = "COV19"), style="text-align:justify;padding:20px;")
                                
                              )
                          ), 
                          
                          tags$style(HTML("hr {border-top: 1px solid #000000;}")),
                          tags$hr(),
                          fluidPage(HTML('<meta name="viewport" content="width=1024">'),
                            fluidRow(
                              column(width = 12,
                                     plotlyOutput(outputId = "decrees_timeline")
                              )
                            ),
                            tags$hr(),
                            
                            fluidRow(
                              fluidPage(
                                tabsetPanel(
                                  tabPanel(title = "Ratios Time Series", 
                                           fluidRow(
                                             sidebarLayout(
                                               sidebarPanel(width = 3,
                                                            pickerInput(inputId = "RatioIn", label = "Select a rate",
                                                                        choices = list("Positivity", "Fatality",
                                                                                       "Healing", "Severity", "Seriousness", "ICU Stress"),
                                                                        selected = "New positives/Swabs"),
                                                            prettySwitch(inputId = "Ratio_by_region", value = F, status = "danger", label = "View by region",
                                                                         bigger = F, slim = T),
                                                            uiOutput(outputId = "RegionRatio"),
                                                            prettyCheckbox(inputId = "VarWeekratio", 
                                                                           label = "weekly average", value = F,
                                                                           status = "warning",icon = icon("check")),
                                                            helpText("The time series of each rate is obtained as the ratio between the indicators showed in the bar plot. In particular, orange color is for the indicator at the enumerator, red one is for the denominator."),
                                                            helpText("If the ratio is lower than 0 or higher than 100, it is a data entry error."),
                                                            helpText("NB: Tested cases are only available from April 19, 2020."),
                                                            helpText("NB: From January 15, 2021, swabs include also antigenic swabs.")
                                               ),
                                               mainPanel(
                                                 splitLayout(
                                                   cellWidths = c("53%", "55%"),
                                                   plotlyOutput(outputId = "TS_ratio"),
                                                   plotlyOutput(outputId = "Bar_ratio")
                                                 )
                                               )
                                             )
                                           )
                                  ),
                                  tabPanel(title = "Distribution of Positives", 
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
                                           )
                                  ),
                                  tabPanel(title = "Time series and Map",
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
                                                   addSpinner(leafletOutput(outputId = "MapIta", height = "450px"), spin = "fading-circle", color = "firebrick")
                                                   
                                                 )
                                               )
                                             )
                                           )
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
                          fluidPage(HTML('<meta name="viewport" content="width=1024">'),
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
                            tags$h2(tags$strong("Generalized linear Richards model*")),
                            tags$h5("*further details on the methodology can be found ",
                                    tags$a(href = "https://arxiv.org/pdf/2010.12679.pdf" , "here")),
                            #tags$h5("*for further details on the methodology we recommend to look at the user guide."),
                            sidebarLayout(
                              sidebarPanel(
                                tags$h3(tags$strong("Parameters of the model")),
                                prettyRadioButtons(
                                  inputId = "WhichWave", label = "Which part of the epidemic do you want to model?",
                                  choices = list("I part", "II part"),
                                  selected = "I part", status = "danger", shape = "round",
                                  inline = T),
                                helpText("If you are modeling the I part, then you will use all the data up to the selected date."),
                                helpText("If you are modeling the II part, then you will use all the data from the selected date."),
                                sliderInput(inputId = "DateWave", label = "End/Start date of the two parts of the epidemic",
                                            min = as.Date("2020-07-01"), max = as.Date("2020-07-31"),
                                            value = as.Date("2020-07-19"), width = "300px"),
                                pickerInput(inputId = "VarForModel", 
                                            label = "Select the incidence indicator you want to model",
                                            choices = list("New positives", "New deceased",
                                                           #"Cumulative positives", "Deceased",
                                                           #"Intensive care",
                                                           #"Current hospitalized", "Hospitalized with symptoms","Home isolation",
                                                           "New discharged recovered"),
                                            selected = "New positives", width = "300px"), 
                                uiOutput(outputId = "HelpCovs"),
                                #uiOutput(outputId = "AddCovariates"),
                                uiOutput(outputId = "ExcludeDaysModel"),
                                radioButtons(inputId = "ModelFamily",
                                             label = "Choose a distribution",
                                             choices = list( "Negative Binomial", "Poisson"),
                                             selected = "Negative Binomial"),
                                #helpText("If you are not satisfied by the Poisson coverage, try the Negative Binomial (we recommend it)!"),
                                helpText("If you are not satisfied by the output, change the parameters (e.g. use all data, change distribution, etc)"),
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
                          fluidPage(HTML('<meta name="viewport" content="width=1024">'),
                            htmlOutput(outputId = "ICUTabHead"),# 3
                            tags$h5("*further details on the methodology can be found ", 
                                    tags$a(href = "https://onlinelibrary.wiley.com/doi/10.1002/bimj.202000189" , "here")),
                            tags$h5("**ICU capacity source is ", 
                                    tags$a(href = "https://www.agenas.gov.it/covid19/web/index.php?r=site%2Ftab2" , "here")),
                            helpText("Click the button to download the predictions"),
                            fluidRow(addSpinner(DTOutput("ICuTab"), spin = "fading-circle", color = "firebrick"), 
                                     helpText("Click the button to download the predictions"),
                                     downloadButton(outputId = "DownICUTomorrow", label = "Download")),
                            tags$hr(),
                            fluidRow(
                              sidebarLayout(
                                sidebarPanel(
                                  
                                  helpText(tags$h4(tags$strong("Comparison of predicted vs observed values on the chosen date")),
                                           tags$h5("Starting from 17th of March 2020")),
                                  uiOutput(outputId = "ICubarDate"),
                                  uiOutput("CovICUModel"),
                                  helpText("Click the button to download the predictions on the date you chose"),
                                  downloadButton(outputId = "DownICUWhatever", label = "Download")
                                  
                                ), 
                                mainPanel(addSpinner(plotlyOutput("BarplotIcu"), spin = "fading-circle", color = "firebrick"))))
                          )),
                 # TAB 4 - VACCINI
                 tabPanel(
                   "Vaccines", icon = icon("syringe"),
                   sidebarLayout(
                     sidebarPanel(width = 3,
                                  prettyRadioButtons("VacMapType", label = "Vaccine doses map (select one of the followings)", 
                                                     choices = list("Administered", "Administered/Delivered", "Administered/Residents"),
                                                     selected = "Administered/Delivered", inline = F),
                                  materialSwitch(inputId = "VacIsReg", 
                                                 label = HTML("<b>Donut plot by region</b>"), 
                                                 value = F, status = "danger"), 
                                  uiOutput("VacReg"),
                                  tags$hr(),
                                  tableOutput("VacItaDT")
                     ),
                     mainPanel(width = 9,
                               fluidRow(column(dataTableOutput("VaccineRawData"), width = 7),
                                        column(tags$br(),tags$br(),
                                               addSpinner(leafletOutput("VaccineMap"), spin = "fading-circle", color = "firebrick"), width = 5)),
                               tags$hr(),
                               fluidRow(
                                 column(addSpinner(plotlyOutput("VacDonutSesso"), spin = "fading-circle", color = "firebrick"), width = 4),
                                 column(addSpinner(plotlyOutput("VacDonutCategoria"), spin = "fading-circle", color = "firebrick"), width = 4),
                                 column(addSpinner(plotlyOutput("VacDonutEta"), spin = "fading-circle", color = "firebrick"), width = 4))
                     )
                   )
                 ),
                 # Fifth tab: Info and credits
                 tabPanel("Info and Credits", icon = icon("info-circle"), div(style="margin-top:-2.5em", includeMarkdown("InfoandCredits.md")))
                 
)


server <- function(input, output, session){
  
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
    isolate({users$count <- users$count - 1})
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
  
  # Vaccini
  v_data_summ <- reactive({
    read_vaccines_latest_summ("https://raw.githubusercontent.com/italia/covid19-opendata-vaccini/master/dati/vaccini-summary-latest.csv")
  })
  v_data_ana <- reactive({
    read_vaccines_latest_ana("https://raw.githubusercontent.com/italia/covid19-opendata-vaccini/master/dati/anagrafica-vaccini-summary-latest.csv")
  })
  v_data_somm <- reactive({
    read_vaccines_2("https://raw.githubusercontent.com/italia/covid19-opendata-vaccini/master/dati/somministrazioni-vaccini-latest.csv")
  })
  v_data_reg <- reactive({
    read_vaccines_1("https://raw.githubusercontent.com/italia/covid19-opendata-vaccini/master/dati/somministrazioni-vaccini-latest.csv")
  })
  
  # Update ICU
  # CapICUUpdated <- reactive({
  #   if(isolate(tdy()) == max(isolate(dati_Ita())$data)){
  #     read_updated_ICUCapacity(path = "https://www.agenas.gov.it/covid19/web/index.php?r=site%2Ftab2", capICU = capacitaICU)
  #   }
  # })
  # 
  # CapICUNew <- reactive({
  #   capacitaICU$Capienza[capacitaICU$data>=max(isolate(dati_Ita())$data)] <- CapICUUpdated()$`PL in Terapia Intensiva`
  #   return(capacitaICU)
  # })
  # 
  
  # Join data regional
  joined_reg <- reactive({
    italy_sf_reg %>% 
      sp::merge(dati_reg() %>% spread(Key, Value), by.y = "denominazione_regione", by.x = "NAME") %>% 
      add_residents_tomap(resdata = residents, tomerge = "NAME") %>% gather(Key, Value, `Tested cases`:`Current positives`)
  })
  
  # Join data province
  joined_prov <- reactive({
    italy_sf_prov %>% 
      sp::merge(dati_prov() %>% dplyr::select(data, denominazione_provincia, denominazione_regione, `Cumulative positives`),
                by.y = c("denominazione_regione", "denominazione_provincia"),
                by.x = c("NAME", "NAME_2"), all.x = T) %>% 
      add_residents_tomap(resdata = residents, tomerge = "NAME_2") %>% 
      gather(Key, Value, `Cumulative positives`) %>% 
      dplyr::select(NAME_2, NAME, Key, data, Value, geometry, residenti)
  })
  
  
  # Data preparation for the model
  data_formodel_prep <- reactive({prepdata_for_model(dftoprep = isolate(dati_reg()) %>% spread(Key, Value), resdata = residents)})
  
  # Today summary
  tod_summary <- reactive({return_current_situa(da = dati_Ita(), TotPop = TotPop, CapICUTot = sum(capacitaICU$Capienza[capacitaICU$data == isolate(tdy())]))})
  
  
  # Table with today raw data
  raw_today <- reactive({show_raw_data(dati_Ita(), dati_reg())})
  
  # Date reactive inputs
  output$SelDateTSandMap <- renderUI({
    sliderTextInput(
      inputId = "SelDateTSandMap", label = "Choose a day", grid = F, force_edges = TRUE, width = 250, selected = tdy(), choices = seq(as.Date("2020-03-01"), tdy(),1)
    )
  })
  
  # Data for raw data
  output$RawDatadate <- renderUI({
    airDatepickerInput(inputId = "RawDatadate", label = "Select a date", clearButton = F, todayButton = T, multiple = F, minDate = min(dati_Ita()$data), maxDate = tdy(), value = tdy())
  })
  
  ### Raw data
  output$Rawdata <- renderDT({table_raw_data(tab = raw_today(), datesel = input$RawDatadate, resdata = residents, capICU = capacitaICU)})
  
  # Downooad the raw data
  output$DownloadRawData <- downloadHandler(
    filename = function() {
      paste0("Covid19Data_upto_", tdy(),".csv")
    },
    content = function(file) {
      write.csv(raw_today(), file, row.names = FALSE)
    }
  )
  
  
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
      value = HTML(paste0("New positives <br>", 
                          prettyNum(tod_summary()$`New positives`[1], big.mark = ".", decimal.mark = ","),
                          " (", tod_summary()$`New positives`[2], ")")),
      subtitle = HTML(
        paste0("<b>Molecular</b> ", prettyNum(tod_summary()$`New positives`[3], big.mark = ".", decimal.mark = ","),
               " <b>Antigenic</b> ", prettyNum(tod_summary()$`New positives`[4], big.mark = ".", decimal.mark = ","),
               " <b>Total</b> ", prettyNum(tod_summary()$`Cumulative positives`[1], big.mark = ".", decimal.mark = ",")
        )
      ),
      icon = icon("plus-square"),
      color = "red"
    )
    
  })
  
  output$Deces <- renderValueBox({
    
    valueBox(
      value = HTML(paste0("Deaths <br>", prettyNum(tod_summary()$Deceased[3], big.mark = ".", decimal.mark = ","),
                          " (", tod_summary()$Deceased[2], ")")),
      subtitle = HTML(paste0("<b>Total</b> ", prettyNum(tod_summary()$Deceased[1], big.mark = ".", decimal.mark = ","))),
      icon = icon("ribbon"),
      color = "orange"
    )
  })
  
  output$ICU <- renderValueBox({
    
    valueBox(
      value = HTML(paste0("Intensive care <br>", prettyNum(tod_summary()$`Intensive care`[3], big.mark = ".", decimal.mark = ","), 
                          " (", tod_summary()$`Intensive care`[2], ")")),
      subtitle = HTML(paste0("<b>Occupied beds</b> ", prettyNum(tod_summary()$`Intensive care`[1], big.mark = ".", decimal.mark = ","), 
                             "  <b>ICU stress</b> ", prettyNum(tod_summary()$ICUStress[1], big.mark = ".", decimal.mark = ","), "%")),
      icon = icon("procedures"),
      color = "yellow"
    )
  })
  
  output$CurrPos <- renderValueBox({
    
    valueBox(
      value = HTML(paste0("Current positives <br>", prettyNum(tod_summary()$`Current positives`[1], big.mark = ".", decimal.mark = ","), 
                          " (", tod_summary()$`Current positives`[2], ")")),
      subtitle = HTML(paste0("<b>Balance</b> ", prettyNum(tod_summary()$`Current positives`[3], big.mark = ".", decimal.mark = ","))),
      icon = icon("user-plus"),
      color = "yellow"
    )
  })
  
  # output$CurrHosp <- renderValueBox({
  #   
  #   valueBox(
  #     value = prettyNum(tod_summary()$`Attualmente ricoverati`[1], big.mark = ".", decimal.mark = ","),
  #     subtitle = HTML(paste0("<b>Attualmente ricoverati</b> (", tod_summary()$`Attualmente ricoverati`[2], ")")),
  #     icon = icon("hospital"),
  #     color = "yellow"
  #   )
  #   
  # })
  
  output$HospSym <- renderValueBox({
    
    valueBox(
      value = HTML(paste0("Current hospitalized <br>",  prettyNum(tod_summary()$`Current hospitalized`[3], big.mark = ".", decimal.mark = ","),
                          " (", tod_summary()$`Current hospitalized`[2], ")")),
      subtitle = HTML(paste0("<b>Totali</b> ", prettyNum(tod_summary()$`Current hospitalized`[1], big.mark = ".", decimal.mark = ","), 
                             "  <b>Hospitalized with symptoms</b> ", prettyNum(tod_summary()$`Hospitalized with symptoms`[1], big.mark = ".", decimal.mark = ","))),
      icon = icon("ambulance"),
      color = "red"
    )
  })
  
  output$HomeIs <- renderValueBox({
    
    valueBox(
      value = HTML(paste0("Home isolation <br>", prettyNum(tod_summary()$`Home isolation`[3], big.mark = ".", decimal.mark = ","), 
                          " (", tod_summary()$`Home isolation`[2], ")")),
      subtitle = HTML(paste0("<b>Total</b> ", prettyNum(tod_summary()$`Home isolation`[1], big.mark = ".", decimal.mark = ","))),
      icon = icon("home"),
      color = "orange"
    )
  })
  
  # output$NewPos <- renderValueBox({
  #   
  #   valueBox(
  #     value = prettyNum(tod_summary()$`Nuovi positivi`[1], big.mark = " "),
  #     subtitle = HTML(paste0("<b>Nuovi positivi</b> (", tod_summary()$`Nuovi positivi`[2], ")")),
  #     icon = icon("plus-square"),
  #     color = "orange"
  #   )
  # })
  
  output$DishRec <- renderValueBox({
    
    valueBox(
      value = HTML(paste0("Discharged/Recovered <br>", prettyNum(tod_summary()$`Discharged recovered`[3], big.mark = ".", decimal.mark = ","), 
                          " (", tod_summary()$`Discharged recovered`[2], ")")),
      subtitle = HTML(paste0("<b>Total</b> ", prettyNum(tod_summary()$`Discharged recovered`[1], big.mark = ".", decimal.mark = ","))),
      icon = icon("band-aid"),
      color = "orange"
    )
  })
  
  
  output$Swabs <- renderValueBox({
    
    valueBox(
      value = HTML(paste0("Swabs <br>", prettyNum(tod_summary()$Swabs[3], big.mark = ".", decimal.mark = ","), " (", tod_summary()$Swabs[2], ")")),
      subtitle = HTML(
        paste0("<b>Molecular</b> ", prettyNum(tod_summary()$Swabs[4], big.mark = ".", decimal.mark = ","),
               " <b>Antigenic</b> ", prettyNum(tod_summary()$Swabs[5], big.mark = ".", decimal.mark = ","),
               " <b>Total</b> ", prettyNum(tod_summary()$Swabs[1], big.mark = ".", decimal.mark = ",")
        )
      ),
      icon = icon("syringe"),
      color = "yellow"
    )
  })
  
  
  output$TestedCases <- renderValueBox({
    
    valueBox(
      value = HTML(paste0("Tested cases <br>", prettyNum(tod_summary()$`Tested cases`[3], big.mark = ".", decimal.mark = ","), 
                          " (", tod_summary()$`Tested cases`[2], ")")),
      subtitle = HTML(paste0("<b>Total</b> ", prettyNum(tod_summary()$`Tested cases`[1], big.mark = ".", decimal.mark = ","))),
      icon = icon("syringe"),
      color = "red"
    )
  })
  
  output$LetRate <- renderValueBox({
    
    valueBox(
      value = HTML(paste0("Fatality rate <br>", prettyNum(tod_summary()$Letalita[1], big.mark = ".", decimal.mark = ","), "%")),
      subtitle = HTML(paste0("<b>since the beginning of the epidemic</b>")),
      icon = icon("skull"),
      color = "orange"
    )
  })
  
  output$MortRate <- renderValueBox({
    
    valueBox(
      value = HTML(paste0("Mortality rate <br>", prettyNum(tod_summary()$Mortalita[1], big.mark = ".", decimal.mark = ","), "%")),
      subtitle = HTML(paste0("<b>since the beginning of the epidemic</b>")),
      icon = icon("skull"),
      color = "yellow"
    )
  })
  
  output$PosRate <- renderValueBox({
    
    valueBox(
      value = HTML(paste0("Positivity rate <br>", prettyNum(tod_summary()$TassoPos[1], big.mark = ".", decimal.mark = ","), "%")),
      subtitle = HTML(paste0("<b>as for WHO standard</b>")),
      icon = icon("plus-square"),
      color = "red"
    )
  })
  
  # output$ICUStress <- renderValueBox({
  #   
  #   valueBox(
  #     value = paste0(tod_summary()$ICUStress[1], "%"),
  #     subtitle = HTML(paste0("<b>Saturazione terapie intensive</b>")),
  #     icon = icon("procedures"),
  #     color = "yellow"
  #   )
  # })
  
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
      prettyRadioButtons(inputId = "IncrPercSiNo", label = "", shape = "round", status = "danger", 
                         choices = list("Absolute", "Relative (%)"), icon = icon("check"), selected = "Absolute", inline = T)
    } else {
      return(NULL)
    }
  })
  
  # If region, then pick one
  output$DisplayRegRow1 <- renderUI({
    if(input$DonBarPerReg){
      pickerInput(inputId = "DisplayRegRow1", label = "Pick a region", choices = reg_choices,
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
      pickerInput(inputId = "SelRegModel", label = "Select a region", choices = reg_choices, selected = "Lombardia",width = "300px",
                  options = list(`actions-box` = FALSE, header = "Regions"))
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
  output$MapIta <- renderLeaflet({
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
  RegionRationIN <- reactive({input$RegionRatio})
  RegionByRatio <- reactive({input$Ratio_by_region})
  output$Bar_ratio <- renderPlotly({
    if(input$RatioIn != "ICU Stress"){
      bar_out <- plot_ratios(da = dati_reg(), is.reg = RegionByRatio(), reg = RegionRationIN(), type_of_ratio = input$RatioIn, 
                             plot_type = "barplot", capICU = capacitaICU, sfItareg = italy_sf_reg)
      
    }else {
      bar_out <- plot_ratios(da = dati_reg(), is.reg = F, reg = NULL, type_of_ratio = input$RatioIn, 
                             plot_type = "barplot", capICU = capacitaICU, sfItareg = italy_sf_reg)
      
    }
    return(bar_out)
    
  })
  
  output$TS_ratio <- renderPlotly({
    ts_rat_out <- plot_ratios(da = dati_reg(), is.reg = F, reg = NULL, type_of_ratio = input$RatioIn, plot_type = "ts", 
                              weeklyvar = input$VarWeekratio, capICU = capacitaICU, sfItareg = italy_sf_reg)
    if(input$Ratio_by_region){
      ts_rat_out <- plot_ratios(da = dati_reg(), is.reg = T, reg = input$RegionRatio, type_of_ratio = input$RatioIn, plot_type = "ts",
                                weeklyvar = input$VarWeekratio, capICU = capacitaICU, sfItareg = italy_sf_reg)
    }
    return(ts_rat_out)
  })
  
  ### VACCINI
  output$VacReg <- renderUI({
    if(input$VacIsReg){
      pickerInput(inputId = "VacReg", label = "Select a region", choices = reg_choices, selected = "Lombardia", width = "300px",
                  options = list(`actions-box` = FALSE, header = "Regioni"))
    }else{
      return(NULL)
    }
  })
  output$VacDonutSesso <- renderPlotly({
    if(input$VacIsReg){
      pp <- vaccine_donut(v_data_somm(), type = "Gender", reg = input$VacReg)
    }else{
      pp <- vaccine_donut(v_data_somm(), type = "Gender", reg = NULL) 
    }
    return(pp)
  })
  
  output$VacDonutCategoria <- renderPlotly({
    if(input$VacIsReg){
      pp <- vaccine_donut(v_data_somm(), type = "Category", reg = input$VacReg)
    }else{
      pp <- vaccine_donut(v_data_somm(), type = "Category", reg = NULL) 
    }
    return(pp)
  })
  
  output$VacDonutEta <- renderPlotly({
    if(input$VacIsReg){
      pp <- vaccine_donut(v_data_somm(), type = "Age", reg = input$VacReg)
    }else{
      pp <- vaccine_donut(v_data_somm(), type = "Age", reg = NULL) 
    }
    return(pp)
  })
  
  output$VaccineMap <- renderLeaflet({
    dd <- vaccine_map_data(v_data_summ(), type = input$VacMapType, ita_sf = italy_sf_reg, res_data = residents) 
    ppp <- dd[[1]] %>% st_set_crs(., "+proj=longlat +datum=WGS84")
    ttl <- dd[[2]]
    hover_add <- dd[[3]]
    pal <- colorBin("YlOrRd", domain = ppp$Value, bins = round(quantile(ppp$Value, probs = seq(0,1,length.out = 5)),2))
    ppp %>% 
      mutate(popup = str_c("<strong>", NAME, "</strong>: ", Value, hover_add,
                           ttl, Id1, "<br/>",
                           "Administered doses: ", dosi_somministrate) %>%
               map(htmltools::HTML)) %>% 
      leaflet() %>% addTiles() %>%
      setView(lng = 13, lat = 42, zoom = 5) %>%
      addPolygons(label=~popup, fillColor = ~pal(Value), weight = 1, smoothFactor = 0.5, color = "black", opacity = 1.0, fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")) %>% 
      addLegend(pal = pal, values = ~n, opacity = 0.4, title = "Doses", position = "bottomleft")
    
  })
  
  output$VacItaDT <- function(){
    if(input$VacIsReg){
      tab_out <- show_vaccine(v_data_reg() %>% filter(denominazione_regione == input$VacReg))
      dd <- v_data_reg()$data[1]
      cap <- input$VacReg
    }else{
      tab_out <- show_vaccine(v_data_ana())
      dd <- v_data_ana()$data[1]
      cap <- "Italy"
    }
    
    
    tab_out %>% dplyr::select(Value, `Administered doses`) %>% rename(` ` = Value) %>% 
      knitr::kable(format = "html", digits = 0, row.names = F, align = "c", caption = paste("Vaccines in", cap,"updated on ", dd)) %>% 
      kableExtra::kable_styling(bootstrap_options = "striped", full_width = F) %>% 
      kableExtra::group_rows("Age class", 1, 9) %>% 
      kableExtra::group_rows("Gender", 10, 11) %>% 
      kableExtra::group_rows("Category", 12, 14)
    
  }
  
  output$VaccineRawData <- renderDT({
    
    datatable(show_vaccine_reg(v_data_summ(), resdata = residents, TotPop = TotPop),
              caption = paste("Doses by region updated on", v_data_summ()$data[1]), rownames = F,
              options = list(dom = 'tpl', pageLength = 7, lengthMenu = c(7,14,21), columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    
  })
  
  # Icu data preparation
  
  # Modello terapia intensiva
  tICU$tb <- loadData(token = token)
  
  icu_data <- reactive({
    if(isolate(tdy()) == max(isolate(tICU$tb)$DataPred)){dataprep_terapie(dftoprep = dati_reg() %>% spread(Key, Value), resdata = residents, 
                                                                          capICULast = capacitaICU$Capienza[capacitaICU$data == max(isolate(tICU$tb)$DataPred)])}
  })
  
  if(isolate(tdy()) == max(isolate(tICU$tb)$DataPred)){
    showModal(modalDialog(
      HTML("<p align='center'>The app is updating..please be patient and wait without disconnecting!</p>"),
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
    tags$h2(tags$strong(paste("Prediction* of intensive care units** on the ", paste_eng_date(datasuTabella))))
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
  WWave <- eventReactive(input$updatemodel, {input$WhichWave})
  DWave <- eventReactive(input$updatemodel, {input$DateWave})
  
  
  # output$HelpCovs <- renderUI({
  #   if(input$VarForModel == "New positives"){
  #   helpText("Clicking the checkbox below, you can use the number of tested cases and the weekend effect for estimation purposes")
  #   } else{
  #     return(NULL)
  #   }
  # })
  # output$AddCovariates <- renderUI({
  #   if(input$VarForModel == "New positives"){
  #     prettyCheckbox(inputId = "AddCovariates", label = "Add covariates", value = F, status = "danger", icon = icon("check"))
  #   } else{
  #     return(F)
  #   }
  # })
  
  #AddCov <- eventReactive(input$updatemodel, {input$AddCovariates})  
  # Data for model
  output$ExcludeDaysModel <- renderUI({
    if(input$WhichWave == "I part"){
      sliderInput(inputId = "ExcludeDaysModel", label = "Fitting interval (exclude up to the last 15 days):", 
                  min = as.Date(input$DateWave)-14, max = as.Date(input$DateWave), value = as.Date(input$DateWave), width = "300px")
    }else{
      sliderInput(inputId = "ExcludeDaysModel", label = "Fitting interval (exclude up to the last 15 days):", 
                  min = tdy()-14, max = tdy(), value = tdy(), width = "300px")
    }
    
  })
  
  # Run the model
  outputModello <- eventReactive( input$updatemodel, {
    
    updateSliderInput(session, inputId = "SelDateModel", value = 1)
    
    withProgress(message = "In progress...", detail = "please be patient", value = 0.5, expr =  {             
      ## Your code
      if(RegModSiNo()){
        return(
          tryCatch(
            suppressWarnings(run_growth_model(da = data_formodel_prep(), reduce_obs = as.character(input$ExcludeDaysModel), reg = RegMSelected(), 
                                              wh = VMSelected(), horizon = 30, fam = FamMSelected(), 
                                              wave = WWave(), DataWave = DWave())),
            error = function(e){ return("The model could not converge. Try again!") } 
          )
        )
      }else{
        return(
          tryCatch(
            suppressWarnings(run_growth_model(da = data_formodel_prep(), reg = NULL, reduce_obs = as.character(input$ExcludeDaysModel),
                                              wh = VMSelected(), horizon = 30, fam = FamMSelected(), 
                                              wave = input$WhichWave, DataWave = DWave())),
            error = function(e){ return("The model could not converge. Try again!") } 
          )
        )
      }
    })
    
  })
  
  
  # Display bands and related messages
  output$Doyouwantbands <- renderUI({
    if(outputModello()$NoConv){
      HTML(paste("<h5><b>Possible local optimum. Confidence interval could be inaccurate (or not available)!</b></h5>"))
    }else{
      return(NULL)
    }
  })
   
  
  output$Showbands <- renderUI({
    if(outputModello()$NoConv){
      checkboxInput(inputId = "Showbands", label = "Add 95% confidence interval", value = F)
    }else{ return(NULL) }
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
    reg <- ifelse(RegModSiNo(), as.character(RegMSelected()), "Italy")
    plot_out_model(outputmod = outputModello(), horizon = input$SelDateModel, VarModel = VMSelected(), showbands = input$Showbands, reg = reg)
  })
  
  # Table model output
  output$DatPred <- renderDT({
    reg <- ifelse(RegModSiNo(), as.character(RegMSelected()), "Italy")
    DT_out_model(outputmod = outputModello(), horizon = 15, VarModel = VMSelected(), showbands = input$Showbands, reg = reg)
  })
  
  
}

shinyApp(ui = ui, server = server)