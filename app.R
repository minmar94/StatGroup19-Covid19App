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
require(plotly)
require(rgeos)
require(lme4)

rm(list=ls())


Sys.setlocale(locale = "C")

# Load functions and some data
source("Script/FunctionsDefAttempt.R")
source("Script/UsefulFunsApp2.R")
load("Data/ICU/PastICUPred.RData")


# Read aggregated Italian data up to today 
dati_Ita <- read_italian(path = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv")
today <- max(dati_Ita$data)

# Read regional data up to today
dati_reg <- read_regional(path = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")

# Leggo dati provinciali up o today
dati_prov <- read_province(path = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv")

# Leggo dati residenti
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
  sp::merge(dati_prov %>% dplyr::select(data, denominazione_provincia, denominazione_regione, `Cumulati positivi`),
            by.y = c("denominazione_regione", "denominazione_provincia"),
            by.x = c("NAME", "NAME_2"), all.x = T) %>% 
  gather(Key, Value, `Cumulati positivi`) %>% 
  dplyr::select(NAME_2, NAME, Key, data, Value, geometry)

# Calcolo quoziente di letalità
letalita_reg <- dati_reg %>% spread(Key, Value) %>% dplyr::select(data, denominazione_regione, Deceduti, `Cumulati positivi`) %>%
  filter(`Cumulati positivi`>0) %>% 
  group_by(denominazione_regione) %>% 
  summarise(Totdec = sum(Deceduti),
            Totcasi = sum(`Cumulati positivi`),
            letalita = Totdec/Totcasi)


# Creo il dataset di base che serve per il modello
data_formodel_prep <- prepdata_for_model(dftoprep = dati_reg %>% spread(Key, Value), resdata = residents)

# Calcolo tasso di mortalità
mortalita_reg <- data_formodel_prep %>% 
  dplyr::select(ti_orig, region, Deceduti, residents) %>% 
  group_by(region) %>% 
  summarise(Totdec = sum(Deceduti),
            Totres = residents[1],
            mortalita = Totdec/Totres)



# Preparo i dati per la sezione relativa alle terapie intensive
datasuTabella <- max(outmod_terapie_tab$DataPred)
outmod_terapie_plot <- outmod_terapie_tab %>% filter(DataPred < datasuTabella)
outmod_terapie_table <- outmod_terapie_tab %>% filter(DataPred == max(DataPred))

# Plot base
#basemap <- draw_map(dajoined = joined_reg, reg = NULL, varsel = "Cumulati positivi", datasel = today, dajoinedprov = joined_prov)
#basets <- do_ts(da = dati_Ita, reg = NULL, varsel = "Cumulati positivi", datasel = today, is.incrementi = F)

# ShinyApp ----------------------------------------------------------------
ui <- navbarPage(theme = shinytheme("united"),
                 title = "Analisi dell'epidemia di CoviD-19 in Italia",
                 
                 # Primo pannello: Overview
                 tabPanel(title = "Panoramica",
                          
                          div(style="margin-top:-3.5em",
                              
                              fluidRow(tags$hr(),
                                       column(width = 6,
                                              htmlOutput(outputId = "CurrentSitua"),
                                              style="padding:20px;"),
                                       column(width = 6, 
                                              htmlOutput(outputId = "Defins"), style="padding:20px;")
                              ),
                              
                              fluidRow(
                                column(11, htmlOutput(outputId = "COV19"), style="text-align:justify;padding:20px;")
                                
                              )
                          ), 
                          
                          tags$hr(),
                          
                          fluidPage(
                            
                            fluidRow(
                              sidebarLayout(
                                sidebarPanel(width = 3,
                                             radioButtons(
                                               inputId = "VarRow1", 
                                               label = "Scegli quale distribuzione visualizzare", 
                                               choices = list("Cumulati positivi", "Attualmente positivi"),
                                               selected = "Cumulati positivi", 
                                               inline = F),
                                             materialSwitch(
                                               inputId = "DonBarPerReg", 
                                               label = "Visualizza per regione",
                                               value = F, 
                                               status = "warning"
                                             ),
                                             uiOutput(outputId = "DisplayRegRow1")
                                ),
                                mainPanel(
                                  splitLayout(
                                    cellWidths = c("49%","60%"),
                                    plotlyOutput(outputId = "DonughtPlot"),
                                    plotlyOutput(outputId = "BarStackPos")
                                  )
                                  
                                )
                                
                              )
                            ),
                            tags$hr(),
                            
                            fluidRow(
                              sidebarLayout(
                                sidebarPanel(width = 3,
                                  pickerInput(inputId = "SelIdxforTSandMap", 
                                              label = "Seleziona una variabile", 
                                              choices = list("Cumulati positivi", "Attualmente positivi", 
                                                             "Attualmente ricoverati", "Ricoverati con sintomi","Terapia intensiva",
                                                             "Isolamento domiciliare","Dimessi guariti", "Deceduti", "Tamponi"),
                                              width = "fit",
                                              selected = "Totale casi"),
                                  materialSwitch(inputId = "TSRegioneSiNo", 
                                                 label = "Visualizza la serie per regione",
                                                 value = F),
                                  uiOutput(outputId = "SelRegTSandMap"),
                                  checkboxInput(inputId = "IncrementiSiNo", 
                                                label = "Visualizza serie degli incrementi (differenze prime)", 
                                                value = F),
                                  uiOutput(outputId = "IncrPercSiNo"),
                                  uiOutput(outputId = "SelDateTSandMap")
                                ),
                                mainPanel(
                                  splitLayout(
                                    cellWidths = c("49%", "60%"),
                                    plotlyOutput(outputId = "TSIncrPerc", height = "450px"),
                                    plotlyOutput(outputId = "MapIta", height = "450px")
                                  )
                                )
                              )),
                            
                            tags$hr()#,
                            
                            # fluidRow(
                            #   tags$h2(tags$strong("Cos'è la COVID-19?")),
                            #   column(12, textOutput(outputId = "COV19"), style="text-align:justify"),
                            #   tags$hr()
                            # )
                            )
                          ),
                 
                 tabPanel("Modello", 
                          fluidPage(
                            
                            
                            tags$h2(tags$strong("Modello lineare generalizzato di Richards*")),
                            tags$h5("*per maggiori dettagli sulla metodologia si rimanda al seguente ", 
                                    tags$a(href = "http://afarcome.altervista.org/growthGLM.pdf" , "link")), 
                            sidebarLayout(
                              sidebarPanel(
                                tags$h3(tags$strong("Parametri del modello")),
                                pickerInput(inputId = "VarForModel", 
                                            label = "Seleziona una variabile",
                                            choices = list("Cumulati positivi", "Attualmente positivi", "Attualmente ricoverati", 
                                                           "Terapia intensiva",
                                                           "Ricoverati con sintomi", "Dimessi guariti",
                                                           "Isolamento domiciliare", "Deceduti"),
                                            selected = "Cumulati positivi"), 
                                radioButtons(inputId = "ModelFamily",
                                             label = "Scegli la distribuzione",
                                             choices = list("Poisson", "Binomiale Negativa"),
                                             selected = "Poisson"),
                                materialSwitch(inputId = "ModRegioneSiNo", 
                                               label = "Modello a livello regione", 
                                               value = F, status = "danger"),
                                uiOutput(outputId = "SelRegModel"),
                                actionBttn(inputId = "updatemodel", label = "Stima il modello", style = "pill", color = "warning"),
                                helpText("Scegliere i parametri, poi cliccare per stimare il modello e attendere che raggiunga la convergenza"),
                                tags$br(),
                                uiOutput(outputId = "Doyouwantbands"),
                                uiOutput(outputId = "Showbands"),
                                uiOutput(outputId = "CannotPlotBands"),
                                tags$br(),
                                sliderInput(
                                  inputId = "SelDateModel",
                                  label = "Orizzonte di previsione (giorni):",
                                  width = 250,
                                  value = 1,
                                  min = 1,
                                  max = 15
                                ), 
                                helpText("Scorrere per vedere come cambia la curva di previsione"),
                                tags$br(),
                                htmlOutput("SummaryModel")
                              ),
                              
                              mainPanel(tags$style(type="text/css",
                                                   ".shiny-output-error { visibility: hidden; }",
                                                   ".shiny-output-error:before { visibility: hidden; }"
                              ), 
                              verticalLayout(
                                fluidRow(
                                  splitLayout(cellWidths = c("49%", "49%"), plotlyOutput("PredCumCases"), plotlyOutput("PredNewCases"))
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
                 tabPanel("Previsione terapie intensive", 
                          fluidPage(
                            tags$h2(tags$strong(paste("Previsione* posti occupati in terapia intensiva per il giorno ", 
                                                      day(datasuTabella), 
                                                      my_month(stri_trans_totitle(month(datasuTabella, label = T, abbr = F))), 
                                                      year(datasuTabella)))),
                            tags$h5("*per maggiori dettagli sulla metodologia si rimanda al seguente ", 
                                    tags$a(href = "http://afarcome.altervista.org/ICU.predictions.pdf" , "link")), 
                            fluidRow(DTOutput("ICuTab"), 
                                     helpText("Cliccare su 'Download' per scaricare la tabella con le previsioni"),
                                     downloadButton(outputId = "DownICUTomorrow", label = "Download")),
                            tags$hr(),
                            fluidRow(
                              sidebarLayout(
                                sidebarPanel(
                                  
                                  helpText(tags$h4(tags$strong("Confronto previsioni con valori osservati nella data selezionata*")),
                                           tags$h5("Prima data disponibile: 17 Marzo 2020")),
                                  dateInput(inputId = "ICubarDate", label = "Seleziona un giorno", value = datasuTabella-1,
                                            min = "2020-03-17", max = datasuTabella-1, width = "250px"),
                                  uiOutput("CovICUModel"),
                                  helpText("Cliccare su 'Download' per scaricare la tabella con le previsioni della data selezionata"),
                                  downloadButton(outputId = "DownICUWhatever", label = "Download")
                                  #plotOutput("HistICU")
                                ), 
                                mainPanel(plotlyOutput("BarplotIcu"))))
                          )),
                 tabPanel("Info", div(style="margin-top:-2.5em",
                                      includeMarkdown("InfoandCredits.md")))
                 
)

server <- function(input, output, session){
  
  # Cos'è la COVID19
  output$COV19 <- renderUI({HTML(paste0("<h2><b>Cos'è la COVID-19?</b></h2><br>","La COVID-19 (dall'inglese COronaVIrus Disease 2019) o malattia respiratoria acuta da SARS-CoV-2 (dall'inglese Severe Acute Respiratory Syndrome Coronavirus 2),
  è una malattia infettiva respiratoria causata dal virus denominato SARS-CoV-2 appartenente alla famiglia dei coronavirus. 
  Una persona infetta può presentare sintomi dopo un periodo di incubazione che può variare tra 2 e 14 giorni circa, durante i quali può comunque essere contagiosa.
  Per limitarne la trasmissione devono essere prese precauzioni, come adottare un'accurata igiene personale, lavarsi frequentemente le mani e indossare mascherine. 
  Coloro che ritengono di essere infetti devono rimanere in quarantena, indossare una mascherina chirurgica e chiamare immediatamente un medico al fine di ricevere appropriate indicazioni."))})
  
  # Definizioni
  output$Defins <- renderUI(HTML("</br></br><h3><b>Definizioni importanti</b></h3>
  <ul><li><b>Cumulati positivi =</b>  attualmente positivi + deceduti + dismessi guariti </li>
  <li><b>Attualmente positivi =</b> ricoverati con sintomi + terapia intensiva + isolamento domiciliare</li>
  <li><b>Attualmente ricoverati =</b> ricoverati con sintomi + terapia intensiva</li>
  <li><b>Tasso di mortalità =</b> deceduti/popolazione</li>
  <li><b>Tasso di letalità =</b> deceduti/cumulati positivi</li></ul>"))
  
  # Situazione attuale
  output$CurrentSitua <- renderUI({
    
    HTML(print_current_situa(da = dati_Ita, dati_letalita = letalita_reg, dati_mortalita = mortalita_reg))
    
  })
  
  # Dynamic inputs
  output$SelDateIncr <- renderUI# input dinamici
  output$SelDateTSandMap <- renderUI({
    sliderTextInput(
      inputId = "SelDateTSandMap",
      label = "Seleziona un giorno",
      grid = FALSE,
      force_edges = TRUE,
      width = 250,
      selected = max(dati_reg$data),
      choices = seq(min(dati_reg$data), max(dati_reg$data),1)
    )
  })
  
  # Se si vogliono visualizzare gli incrementi, allora scegliere tra assoluti o percentuali
  output$IncrPercSiNo <- renderUI({
    
    if(input$IncrementiSiNo){
      radioButtons(inputId = "IncrPercSiNo", 
                   label = "", 
                   choices = list("Assoluti", "Percentuali (%)"), 
                   selected = "Assoluti") 
    } else {
      return(NULL)
    }
    
  })
  
  # Se si vuole andare per regione, allora scegliere una o più regioni
  output$DisplayRegRow1 <- renderUI({
    if(input$DonBarPerReg){
      pickerInput(inputId = "DisplayRegRow1",
                  label = "Seleziona una regione",
                  choices = list("Nord" = list("Lombardia", "Liguria", "Piemonte", "Valle d'Aosta",
                                               "Emilia-Romagna", "Friuli Venezia Giulia", "Veneto", "Trentino-Alto Adige"),
                                 "Centro" = list("Lazio", "Marche", "Toscana", "Umbria"),
                                 "Sud" = list("Abruzzo", "Basilicata", "Calabria",
                                              "Campania", "Molise", "Puglia"),
                                 "Isole" = list("Sardegna", "Sicilia")),
                  selected = "Lombardia",
                  width = "fit", 
                  inline = F,
                  options = list(
                    `actions-box` = FALSE,
                    header = "Regioni"
                  ))
    }else{
      return(NULL)
    }
    
  })
  
  
  # Se si vuole andare per regione, allora scegliere una o più regioni
  output$SelRegTSandMap <- renderUI({
    if(input$TSRegioneSiNo){
      pickerInput(inputId = "SelRegTSandMap",
                  label = "Seleziona una o più regione (max. 5)",
                  choices = list("Nord" = list("Lombardia", "Liguria", "Piemonte", "Valle d'Aosta",
                                               "Emilia-Romagna", "Friuli Venezia Giulia", "Veneto", "Trentino-Alto Adige"),
                                 "Centro" = list("Lazio", "Marche", "Toscana", "Umbria"),
                                 "Sud" = list("Abruzzo", "Basilicata", "Calabria",
                                              "Campania", "Molise", "Puglia"),
                                 "Isole" = list("Sardegna", "Sicilia")),
                  selected = c("Lombardia", "Piemonte"),
                  multiple = T,
                  width = "370px", 
                  options = list(
                    `actions-box` = FALSE,
                    header = "Regioni",
                    `max-options` = 5
                  ))
    }else{
      return(NULL)
    }
    
  })
  
  
  
  output$SelRegModel <- renderUI({
    if(input$ModRegioneSiNo){
      pickerInput(inputId = "SelRegModel",
                  label = "Seleziona una regione",
                  choices = list("Nord" = list("Lombardia", "Liguria", "Piemonte", "Valle d'Aosta",
                                               "Emilia Romagna", "Friuli V. G.", "Veneto", "TrentinoAltoAdige"),
                                 "Centro" = list("Lazio", "Marche", "Toscana", "Umbria"),
                                 "Sud" = list("Abruzzo", "Basilicata", "Calabria",
                                              "Campania", "Molise", "Puglia"),
                                 "Isole" = list("Sardegna", "Sicilia")),
                  selected = "Lombardia",
                  options = list(
                    `actions-box` = FALSE,
                    header = "Regioni"
                  ))
    }else{
      return(NULL)
    }
    
  })
  
  
  # Grafico a ciambella: distribuzione attualmente positivi
  output$DonughtPlot <- renderPlotly({
    
      do_ciambella(da = dati_reg, is.reg = input$DonBarPerReg, reg = input$DisplayRegRow1, variable = input$VarRow1)
    
  })
  
  
  # Barplot: distribuzione totale casi
  output$BarStackPos <- renderPlotly({
    
    do_barplot_ts(da = dati_reg, is.reg = input$DonBarPerReg, reg = input$DisplayRegRow1, variable = input$VarRow1)
    
    
  })
  
  
  # Selezione reactive per mappa
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
  
  # Mappa
  output$MapIta <- renderPlotly({
    
    mmmap <- draw_map(dajoined = joined_reg, reg = NULL, dajoinedprov = joined_prov,
                      varsel = as.character(variable()), datasel = as.character(datetoselect()))
    if(input$TSRegioneSiNo){
      mmmap <- draw_map(dajoined = joined_reg, reg = as.character(Regselected()), dajoinedprov = joined_prov,
                        varsel = as.character(variable()), datasel = as.character(datetoselect()))
    }

    return(mmmap)
    
  })
  
  output$TSIncrPerc <- renderPlotly({
    
    do_ts(da = dati_reg, is.reg = input$TSRegioneSiNo, reg = as.character(Regselected()), 
                       varsel = as.character(variable()), datasel = as.character(datetoselect()), 
                       is.incrementi = input$IncrementiSiNo, tipo.incremento = input$IncrPercSiNo)
   
  })
  
  
  ###### Modello
  outputModello <- eventReactive( input$updatemodel, {
    
    #showModal(modalDialog("In progress.. Cliccare su 'Dismiss' quando compaiono i risultati!"))
    
    withProgress(message = "In progress...", value = 0.5, expr =  {             
      ## Your code
      if(input$ModRegioneSiNo){
        return(
          tryCatch(
            suppressWarnings(run_growth_model(da = data_formodel_prep, reg = input$SelRegModel, 
                                              wh = input$VarForModel, horizon = 30, fam = input$ModelFamily)),
            error = function(e){
              return("IL MODELLO NON HA RAGGIUNTO LA CONVERGENZA. RIPROVA!")
              
            } 
          )
        )
      }else{
        return(
          tryCatch(
            suppressWarnings(run_growth_model(da = data_formodel_prep, reg = NULL, 
                                              wh = input$VarForModel, horizon = 30, fam = input$ModelFamily)),
            error = function(e){
              return("IL MODELLO NON HA RAGGIUNTO LA CONVERGENZA. RIPROVA!")
            } 
          )
        )
      }
    })
    
    
    #removeModal()
    
  })
  
  
  # 
  output$Doyouwantbands <- renderUI({
    if(outputModello()$NoConv){
      HTML(paste("<h5><b>Possibile ottimo locale. Le bande di confidenza potrebbero non essere affidabili!</b></h5>"))
    }else{
      return(NULL)
    }
  })
  # 
  output$Showbands <- renderUI({
    if(outputModello()$NoConv){
      checkboxInput(
        inputId = "Showbands",
        label = "Aggiungi bande di confidenza al 95%",
        value = F
      )
    }else{
      return(NULL)
    }
  })
  
  output$CannotPlotBands <- renderUI({
    if(input$Showbands & sum(is.na(outputModello()$stderrs[[1]]) | is.nan(outputModello()$stderrs[[1]]))>0){
      HTML(paste("<b>Non è possibile stampare le bande!</b> \n"))
    }else{
      return(NULL)
    }
  })
  
  
  # Summary Italia
  output$SummaryModel <- reactive({
    
    if(!is.character(outputModello())){
      if(input$ModRegioneSiNo){
        return(
          summary_out_model(outputmod = outputModello(), VarModel = input$VarForModel, resdata = residents, 
                            reg = input$SelRegModel, is.reg = T)
        )
      }else{
        summary_out_model(outputmod = outputModello(), VarModel = input$VarForModel, resdata = residents, is.reg = F, reg = NULL)
      }
      
    }else{
      return(HTML(outputModello()))
    }
    
    
  })
  
  # Plot nuovi cumulati Italia
  output$PredCumCases <- renderPlotly({
    
    plot_mod1 <- plot_out_model(outputmod = outputModello(), horizon = input$SelDateModel, what = "Cumulati",
                                VarModel = input$VarForModel, showbands = input$Showbands)
    
    plot_mod1
    
  })
  
  # Plot nuovi predetti Italia
  output$PredNewCases <- renderPlotly({
    
    plot_mod2 <- plot_out_model(outputmod = outputModello(), horizon = input$SelDateModel, 
                                what = "Nuovi", VarModel = input$VarForModel, showbands = input$Showbands)
    
    plot_mod2
    
  })
  
  output$DatPred <- renderDT({
    
    DT_out_model(outputmod = outputModello(), horizon = input$SelDateModel, VarModel = input$VarForModel, showbands = input$Showbands)
    
  })
  
  
  
  #### Previsione terapie intensive
  output$ICuTab <- renderDT({
    
    outmod_terapie_table %>% dplyr::select(-DataPred) %>% 
      datatable(rownames = F, options = list(dom = 'tp', pageLength = 10, scrollX = T, 
                                             columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    
  })
  
  
  output$DownICUTomorrow <- downloadHandler(
    filename = function() {
      paste0("PrevisioneTerapieIntensive_", datasuTabella,".csv")
    },
    content = function(file) {
      write.csv(outmod_terapie_table, file, row.names = FALSE)
    }
  )
  
  dataICUbar <- reactive({
    
    levels(outmod_terapie_plot$Regione)[5] <- "Emilia-Romagna"
    levels(outmod_terapie_plot$Regione)[6] <- "Friuli Venezia Giulia"
    levels(outmod_terapie_plot$Regione)[17] <- "Trentino-Alto Adige"
    
    dplot <- outmod_terapie_plot %>% 
      rename(denominazione_regione = Regione, data = DataPred) %>% 
      left_join(dati_reg %>% spread(Key, Value) %>% 
                  filter(data <= max(outmod_terapie_plot$DataPred)) %>% 
                  dplyr::select(data, starts_with("Tera"), denominazione_regione), 
                by = c("denominazione_regione", "data")) 
    
    dplot
    
  })
  
  output$BarplotIcu <- renderPlotly({
    
    dataICUbar() %>% 
      filter(data == as.character(input$ICubarDate)) %>% 
      plot_ly(x = ~denominazione_regione, y = ~`Terapia intensiva`, type = "bar", 
              name = "Osservati", marker = list(color = "firebrick")) %>% 
      add_trace(y = ~Previsione, name = "Previsti", marker = list(color = "darkorange")) %>% 
      layout(title = paste("Posti occupati in terapia intensiva del ", 
                           day(as.character(input$ICubarDate)), 
                           my_month(stri_trans_totitle(month(as.character(input$ICubarDate), label = T, abbr = F))), 
                           year(as.character(input$ICubarDate))),
             xaxis = list(title = ""),
             yaxis = list(title = "N. Posti"))
    
  })
  
  output$CovICUModel <- renderUI({
    
    dplot2 <- dataICUbar() %>% filter(data == as.character(input$ICubarDate))
    quanteIn <- sum(dplot2$`Terapia intensiva` >= dplot2$`Limite Inferiore` & dplot2$`Terapia intensiva` <= dplot2$`Limite Superiore`)
    
    capts <- paste("per", quanteIn, "regioni su 20 il valore osservato è contenuto nell'intervallo di previsione.")
    HTML(paste("In data ", as.character(input$ICubarDate), ", ",capts))
    
  })
  
  output$DownICUWhatever <- downloadHandler(
    filename = function() {
      paste0("PrevisioneTerapieIntensive_", as.character(input$ICubarDate),".csv")
    },
    content = function(file) {
      write.csv(dataICUbar() %>% filter(data == as.character(input$ICubarDate)), file, row.names = FALSE)
    }
  )
  
}


shinyApp(ui = ui, server = server)