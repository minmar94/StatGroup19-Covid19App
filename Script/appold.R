# Packages
require(tidyverse)
require(magrittr)
require(shiny)
require(shinythemes)
require(shinyWidgets)
require(ggiraph)
require(lubridate)
require(DT)
require(stringi)
require(MASS)
require(rmutil)
require(nplr)
require(tscount)
require(foreach)
require(doSNOW)
require(cowplot)
require(sf)
require(sp)
require(rmarkdown)

rm(list=ls())

source("Script/_growthGLM.r")


paste_signpercent <- function(x_oggi,x_ieri){
  simbolo <- ifelse(x_oggi-x_ieri>0, "+", "")
  incr <- ((x_oggi-x_ieri)/x_ieri)*100
  return(paste0(simbolo, round(incr, 2),"% rispetto a ieri"))
} 


run_growth_model <- function(da, reg="Molise", wh="Totale casi", horizon = 10, fam="Poisson") {
  
  dat <- da[da$region==reg,]
  
  whidx <- which(colnames(dat)==wh)
  pc <- dat[, whidx]
  ti <- dat$ti
  
  if(any(pc==0)) {
    w <- min(which(pc!=0))
    pc <- pc[-c(1:w)]
    ti <- ti[-c(1:w)]
    ti <- ti-min(ti)+1
    }
    mti <- max(ti)        
    timax <- mti+horizon
  mnt <- ifelse(any(diff(pc)<0), F, T)
  np=growthGLM(pc,ti,timax=timax,family=fam,maxiter=5000,monotone=mnt)
  
  ps <- np$pars
  y <- np$linPred
  x <- 1:timax
  
  cc1<-data.frame(x1=c(x[2:mti],rep(NA,length(x[-c(1:mti)]))),
                  pc=c(diff(pc),rep(NA,length(x[-c(1:mti)]))), x=x[-1],y=diff(y))
  cc<-data.frame(x1=c(x[1:mti],rep(NA,length(x[-c(1:mti)]))),
                 pc=c(pc,rep(NA,length(x[-c(1:mti)]))), x=x,y=y)
 

  
  return(list(cc=cc, cc1=cc1, R2 = round(np$R2, 4))) 
}




ui <- navbarPage(theme = shinytheme("united"),
                 title = "Analisi dell'epidemia di CoviD-19 in Italia",
                 
                 # Primo pannello: Overview
                 tabPanel(title = "Overview", 
                          
                          div(style="margin-top:-3.5em",
                              fluidRow(tags$hr(),
                                column(width = 8,
                                       htmlOutput(outputId = "headSummary"), htmlOutput(outputId = "CurrentSitua"),
                                       style="padding:20px;")
                                #column(width = 4, ggiraphOutput("TSIncrPerc")),
                              )
                          ), tags$hr(),
                          
                          fluidPage(
                            
                            sidebarLayout(sidebarPanel(pickerInput(inputId = "SelIdxforMap", 
                                                                   label = "Seleziona una variabile", 
                                                                   choices = list("Totale casi", "Totale positivi", "Nuovi positivi",
                                                                                  "Totale ricoverati", "Ricoverati con sintomi","Terapia intensiva",
                                                                                  "Isolamento domiciliare","Dismessi/Guariti", "Deceduti", "Tamponi"),
                                                                   selected = "Nuovi positivi"), 
                                                       materialSwitch(inputId = "ProvinciaSiNo", 
                                                                      label = "Mappa a livello provinciale", 
                                                                      value = F, 
                                                                      status = "danger"),
                                                       helpText("Per i dati provinciali è disponibile solo il numero di casi totali"),
                                                       materialSwitch(inputId = "IncrPercSiNo", 
                                                                      label = "Visualizza incrementi assoluti", 
                                                                      value = F, 
                                                                      status = "danger"),
                                                       uiOutput(outputId = "SelDateBar")),
                                          mainPanel(splitLayout(plotOutput(outputId = "MapIta", width = "100%", height = "350px"), 
                                                                ggiraphOutput("TSIncrPerc", width = "100%", height = "350px")))),
                            # fluidRow(column(pickerInput(inputId = "SelIdxforMap", 
                            #                                  label = "Seleziona una variabile", 
                            #                                  choices = list("Totale casi", "Totale positivi", "Nuovi positivi",
                            #                                                 "Totale ricoverati", "Ricoverati con sintomi","Terapia intensiva",
                            #                                                 "Isolamento domiciliare","Dismessi/Guariti", "Deceduti", "Tamponi"),
                            #                                  selected = "Nuovi positivi"), 
                            #                 materialSwitch(inputId = "ProvinciaSiNo", 
                            #                                label = "Mappa a livello provinciale", 
                            #                                value = F, 
                            #                                status = "danger"),
                            #                 helpText("Per i dati provinciali è disponibile solo il numero di casi totali"),
                            #                      mainPanel(plotOutput(outputId = "MapIta")), width = 6),
                            #          column(uiOutput(outputId = "SelDateBar"),
                            #                 mainPanel(ggiraphOutput(outputId = "BarplotIta")), width = 6)
                            #                     ), 
                            tags$hr(),
                            
                            fluidRow(
                              column(6, tags$strong("Cos'è la COVID-19?"), textOutput(outputId = "COV19"), style="text-align:justify"),
                              column(6, tags$strong("Vocabulary"), htmlOutput(outputId = "Defins"), style="text-align:justify")
                            ))),
                          tabPanel("Modeling", 
                                   fluidPage(
                                     fluidRow(tags$h2(tags$strong("Modello su scala nazionale")), 
                                              sidebarLayout(sidebarPanel(pickerInput(inputId = "VarForModelIta",
                                                                                     label = "Seleziona una variabile",
                                                                                     choices = list("Totale casi", "Totale positivi", "Totale ricoverati",
                                                                                                            "Ricoverati con sintomi","Terapia intensiva",
                                                                                                            "Isolamento domiciliare", "Deceduti"),
                                                                                             selected = "Totale casi"),
                                                                                 uiOutput(outputId = "SelDateModelIta"), htmlOutput("SummaryModelIta")),
                                                                    mainPanel(tags$style(type="text/css",
                                                                                         ".shiny-output-error { visibility: hidden; }",
                                                                                         ".shiny-output-error:before { visibility: hidden; }"
                                                                    ), verticalLayout(
                                                                      fluidRow(
                                                                        splitLayout(cellWidths = c("49%", "49%"), ggiraphOutput("PredCumCasesIta"), ggiraphOutput("PredNewCasesIta"))
                                                                      ), tags$hr(),
                                                                      fluidRow(
                                                                        splitLayout(cellWidths = c("49%", "49%"), DTOutput("DatPredCumIta"), DTOutput("DatPredNewIta"))
                                                                      )
                                                                    )))),
                                     fluidRow(tags$h2(tags$strong("Modello per regione")), 
                                              sidebarLayout(sidebarPanel(pickerInput(inputId = "VarForModel", 
                                                         label = "Seleziona una variabile",
                                                         choices = list("Totale casi", "Totale positivi", "Totale ricoverati",
                                                                        "Ricoverati con sintomi","Terapia intensiva",
                                                                        "Isolamento domiciliare", "Deceduti"),
                                                         selected = "Totale casi"), 
                                             pickerInput(inputId = "SelRegModel", 
                                                         label = "Seleziona una regione",
                                                         choices = list("Northern Italy" = list("Lombardia", "Liguria", "Piemonte", "Valle d'Aosta",
                                                                                                "Emilia Romagna", "Friuli V. G.", "Veneto", "TrentinoAltoAdige"),
                                                                        "Central Italy" = list("Lazio", "Marche", "Toscana", "Umbria"),
                                                                        "Sourhern Italy" = list("Abruzzo", "Basilicata", "Calabria",
                                                                                                "Campania", "Molise", "Puglia"),
                                                                        "Insular" = list("Sardegna", "Sicilia")),
                                                         selected = "Lombardia",
                                                         options = list(
                                                           `actions-box` = FALSE,
                                                           header = "Regions"
                                                         )), uiOutput(outputId = "SelDateModel"), htmlOutput("SummaryModel")),
                                             mainPanel(tags$style(type="text/css",
                                                                  ".shiny-output-error { visibility: hidden; }",
                                                                  ".shiny-output-error:before { visibility: hidden; }"
                                             ), verticalLayout(
                                               fluidRow(
                                                 splitLayout(cellWidths = c("49%", "49%"), ggiraphOutput("PredCumCases"), ggiraphOutput("PredNewCases"))
                                               ), tags$hr(),
                                               fluidRow(
                                                 splitLayout(cellWidths = c("49%", "49%"), DTOutput("DatPredCum"), DTOutput("DatPredNew"))
                                               )
                                             )))))),
                          tabPanel("Credits", div(style="margin-top:-2.5em", includeMarkdown("InfoandCredits.md")))
                 
)
                 


server <- function(input, output){
  
  # Read aggregated Italian data up to today 
  CoviD_Ita <- reactive({
    
    dat_Ita <- read_csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv") %>% 
      mutate(data = date(data)) %>% dplyr::select(-note_it, -note_en)
    
    colnames(dat_Ita)[-c(1,2)] <- c("Ricoverati con sintomi", "Terapia intensiva", "Totale ricoverati",
                                    "Isolamento domiciliare", "Totale positivi", "Variazione totale positivi", "Nuovi positivi", 
                                    "Dismessi/Guariti", "Deceduti",  "Totale casi", "Tamponi")
    
    dat_Ita
  })
  
  # Read regional data up to today
  CoviD_Ita_Reg <- reactive({
    
    # Leggo i dati sui casi totali per Regione
    dati_reg <- read_csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv") %>% 
      dplyr::select(-note_it, -note_en) %>% 
      # Creo la regione Trentino che appare sottoforma di province autonome
      mutate(denominazione_regione = ifelse(denominazione_regione %in% c("P.A. Bolzano", "P.A. Trento"),
                                            "Trentino-Alto Adige", denominazione_regione),
             data = date(data)) %>% 
      gather(Key, Value, ricoverati_con_sintomi:tamponi) %>% 
      group_by(Key, data, denominazione_regione) %>% 
      summarise(Value = sum(Value)) %>% 
      ungroup() 
    
    dati_reg$Key <- factor(dati_reg$Key, levels = unique(dati_reg$Key), 
                           labels = c("Deceduti", "Dismessi/Guariti", "Isolamento domiciliare", "Nuovi positivi", "Ricoverati con sintomi", 
                                      "Tamponi", "Terapia intensiva", "Totale casi", "Totale ricoverati", "Totale positivi", 
                                      "Variazione totale positivi"))
    
    dati_reg
  })
  
  
  Covid_Ita_Prov <- reactive({
    
    dati_prov <- read_csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv") %>% 
      dplyr::select(-note_it, -note_en) %>%
      mutate(denominazione_regione = ifelse(denominazione_regione %in% c("P.A. Bolzano", "P.A. Trento"),
                                            "Trentino-Alto Adige", denominazione_regione),
             data = date(data)) %>% 
      drop_na(sigla_provincia)
    
    dati_prov
    
  })
  
  
  output$COV19 <- renderText({"La COVID19 (dall'inglese COronaVIrus Disease 2019) o malattia respiratoria acuta da SARS-CoV-2 (dall'inglese Severe acute respiratory syndrome coronavirus 2), è una malattia infettiva respiratoria causata dal virus denominato SARS-CoV-2 appartenente alla famiglia dei coronavirus. 
Una persona infetta può presentare sintomi dopo un periodo di incubazione che può variare tra 2 e 14 giorni circa, durante i quali può comunque essere contagiosa. Per limitarne la trasmissione devono essere prese precauzioni, come adottare un'accurata igiene personale, lavarsi frequentemente le mani e indossare mascherine. Coloro che ritengono di essere infetti devono rimanere in quarantena, indossare una mascherina chirurgica e chiamare immediatamente un medico al fine di ricevere appropriate indicazioni."})
  
  output$Defins <- renderUI(HTML("<ul><li><b>Pazienti attualmente positivi=</b> ricoverati con sintomi + terapia intensiva + isolamento domiciliare</li>
                                      <li><b>Nuovi attualmente positivi=</b> totale attualmente positivi del giorno corrente - totale attualmente positivi del giorno precedente</li>
                                 <li><b>Nuovi positivi=</b> totale casi giorno corrente - totale casi giorno precedente</li>
                                 <li><b>Totale casi=</b> totale attualmente positivi + totale deceduti + totale guariti</li></ul>"))
  
  
  output$headSummary <- reactive({
    today <- max(CoviD_Ita()$data)
    paste0(h2("Situazione italiana relativa all'epidemia CoviD-19 ad oggi*:"),
           h5("*I dati sono aggiornati alle ore 18pm del",
              day(today), stri_trans_totitle(month(today, label = T, abbr = F)), year(today)))
  })
  
  output$CurrentSitua <- reactive({
    
    today <- max(CoviD_Ita()$data) 
    dat_today <- CoviD_Ita() %>% filter(data %in% c(today, today-1))
    
    attualmente_positivi_ultimo <- dat_today$`Totale positivi`[2]
    incr_attualmente_positivi <- paste_signpercent(dat_today$`Totale positivi`[2], dat_today$`Totale positivi`[1])
    
    deceduti_ultimo <- dat_today$Deceduti[2]
    incr_deceduti <- paste_signpercent(dat_today$Deceduti[2], dat_today$Deceduti[1])
    
    dimessi_guariti_ultimo <- dat_today$`Dismessi/Guariti`[2]
    incr_guariti <- paste_signpercent(dat_today$`Dismessi/Guariti`[2], dat_today$`Dismessi/Guariti`[1])
    
    totale_casi_ultimo <- dat_today$`Totale casi`[2]
    incr_totale_casi <- paste_signpercent(dat_today$`Totale casi`[2], dat_today$`Totale casi`[1])
    
    ricov_sintomi_ultimo <- dat_today$`Ricoverati con sintomi`[2]
    incr_ricoverati <- paste_signpercent(dat_today$`Ricoverati con sintomi`[2], dat_today$`Ricoverati con sintomi`[1])
    
    isol_domic_ultimo <- dat_today$`Isolamento domiciliare`[2]
    incr_isolamento <- paste_signpercent(dat_today$`Isolamento domiciliare`[2], dat_today$`Isolamento domiciliare`[1])
    
    terapia_intens_ultimo <- dat_today$`Terapia intensiva`[2]
    incr_terapia <- paste_signpercent(dat_today$`Terapia intensiva`[2], dat_today$`Terapia intensiva`[1])
    
    paste0("<ul>",
           "<li><b>Pazienti attualmente positivi: </b>", attualmente_positivi_ultimo,"(",incr_attualmente_positivi,")","</li>",
           "<li><b>Ricoverati con sintomi: </b>", ricov_sintomi_ultimo, "(",incr_ricoverati,")","</li>",
           "<li><b>Isolamento domiciliare: </b>", isol_domic_ultimo, "(",incr_isolamento,")","</li>",
           "<li><b>Terapia intensiva: </b>", terapia_intens_ultimo, "(",incr_terapia,")","</li>",
           "<li><b>Totale casi: </b>", totale_casi_ultimo, "(",incr_totale_casi,")","</li>",
           "<li><b>Dimessi/Guariti: </b>", dimessi_guariti_ultimo,"(",incr_guariti,")","</li>",
           "<li><b>Decessi: </b>", deceduti_ultimo, "(",incr_deceduti,")","</li>",
           "</ul>"
    )
  })
  
  output$SelDateBar <- renderUI({
    sliderTextInput(
      inputId = "SelDateBar",
      label = "Seleziona un giorno",
      grid = FALSE,
      force_edges = TRUE,
      width = 250,
      selected = max(CoviD_Ita_Reg()$data),
      choices = seq(min(CoviD_Ita_Reg()$data), max(CoviD_Ita_Reg()$data),1)
    )
  })
  
  output$SelDateModel <- renderUI({
    sliderInput(
      inputId = "SelDateModel",
      label = "Orizzonte di previsione (giorni):",
      width = 250,
      value = 1,
      min = 1,
      max = 30
    )
  })
  
  output$SelDateModelIta <- renderUI({
    sliderInput(
      inputId = "SelDateModelIta",
      label = "Orizzonte di previsione (giorni):",
      width = 250,
      value = 1,
      min = 1,
      max = 30
    )
  })
  
  
  output$TSIncrPerc <- renderggiraph({
    
    dat_Ita_incrperc <-  CoviD_Ita() %>% 
      #dplyr::select(data, `Ricoverati con sintomi`, `Terapia intensiva`, `Isolamento domiciliare`, `Dismessi/Guariti`, Deceduti, `Totale casi`) %>% 
      mutate_if(is.numeric, .funs = function(x) (c(0, diff(x)))/x)
    dat_Ita_incrperc <- dat_Ita_incrperc[-1,] %>% 
      filter(data <= as.character(input$SelDateBar))
    ylabel <- "Incrementi percentuali"
    
    if(input$IncrPercSiNo){
      dat_Ita_incrperc <-  CoviD_Ita() %>% 
        #dplyr::select(data, `Ricoverati con sintomi`, `Terapia intensiva`, `Isolamento domiciliare`, `Dismessi/Guariti`, Deceduti, `Totale casi`) %>% 
        mutate_if(is.numeric, .funs = function(x) c(0, diff(x)))
      dat_Ita_incrperc <- dat_Ita_incrperc[-1,] %>% 
        filter(data <= as.character(input$SelDateBar))
      ylabel <- "Incrementi assoluti"
    }
    
    incr_plot <- dat_Ita_incrperc %>% 
      gather(Key, Value, -data, -stato) %>% 
      filter(Key == input$SelIdxforMap) %>% 
      ggplot(aes(x=data, y = Value, colour = I("firebrick"))) + geom_line_interactive() + geom_point_interactive() + 
      #scale_color_brewer(direction = 1, palette = "YlOrRd") +
      labs(x="", y = input$SelIdxforMap, colour = "", caption = paste(ylabel,"su scala nazionale")) +
      scale_x_date(date_labels = "%d/%m", date_breaks = "7 days", expand = c(0,0)) +
      theme_bw() +
      theme(text = element_text(size = 14))
    
    ggiraph(ggobj = incr_plot)
    
  })
  
  output$MapIta <- renderPlot({
    
    # Aggrego per regione
    dati_reg_formap <- CoviD_Ita_Reg() %>%
      filter(Key == as.character(input$SelIdxforMap), data == as.character(input$SelDateBar)) 
    italy_sf <- read_rds("Data/gadm36_ITA_1_sp.rds") %>% st_as_sf() %>% 
      dplyr::select(NAME_1, geometry) %>% 
      mutate(NAME_1 = factor(NAME_1, labels = c("Abruzzo", "Puglia", "Basilicata", "Calabria", "Campania", "Emilia-Romagna", "Friuli Venezia Giulia",
                                                "Lazio" ,"Liguria" , "Lombardia", "Marche", "Molise", "Piemonte", "Sardegna", "Sicilia", "Toscana",
                                                "Trentino-Alto Adige", "Umbria", "Valle d'Aosta", "Veneto")))
    ppp <- italy_sf %>% 
      sp::merge(dati_reg_formap, by.y = "denominazione_regione", by.x = "NAME_1")  
    
    fill_label <- as.character(input$SelIdxforMap)
    
    if(input$ProvinciaSiNo){
      dati_reg_formap <- Covid_Ita_Prov() %>% filter(data == as.character(input$SelDateBar)) %>% 
        dplyr::select(data, denominazione_regione, denominazione_provincia, totale_casi) %>% 
        rename(Value = totale_casi)
      
      italy_sf <- read_rds("Data/gadm36_ITA_2_sp.rds") %>% st_as_sf() %>% 
        dplyr::select(NAME_1, NAME_2, geometry)
      
      italy_sf$NAME_2[italy_sf$NAME_2 %in% c("Forli' - Cesena", "Mantua", "Padua", 
                                             "Monza and Brianza", "Pesaro E Urbino", "Syracuse",
                                             "Florence", "Reggio Di Calabria", "Reggio Nell'Emilia")] <- c("Reggio di Calabria", "Forlì-Cesena", "Reggio nell'Emilia",
                                                                                                           "Mantova", "Monza e della Brianza", "Pesaro e Urbino",
                                                                                                           "Siracusa",  "Firenze",  "Padova")
      
      ppp <- italy_sf %>% 
        sp::merge(dati_reg_formap, by.y = "denominazione_provincia", by.x = "NAME_2", all.x = T)  
      
      fill_label <- "Totale casi"
    } 
    
    map_plot1 <- ppp %>% ggplot() + geom_sf(aes(fill = Value, colour = I("black"))) +
      scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "grey") +
      labs(fill = fill_label, caption = paste("Mappa dell'Italia aggiornata al", as.character(input$SelDateBar))) + 
      theme_light() +
      theme(axis.text = element_blank(), text = element_text(size = 14))
    
    map_plot1
  })
  
  # output$BarplotIta <- renderggiraph({
  #   
  #   dati_forbar <- CoviD_Ita_Reg() %>% 
  #     group_by(data, Key) %>% 
  #     summarise(Tot = sum(Value)) %>%
  #     ungroup() %>% 
  #     filter(Key %in% c("Ricoverati con sintomi", "Terapia intensiva", "Isolamento domiciliare")) %>% 
  #     filter(data <= as.character(input$SelDateBar))
  #   
  #   barplot_1 <- dati_forbar %>% 
  #     ggplot(aes(x = data, y = Tot, fill = Key)) +
  #     geom_bar_interactive(stat = "identity") +
  #     labs(x = "", y = "Totale dei pazienti positivi", fill = "") +
  #     scale_x_date(date_labels = "%d/%m", date_breaks = "3 days", expand = c(0,0)) +
  #     scale_y_continuous(breaks = scales::pretty_breaks(), expand = c(0,0)) +
  #     scale_fill_manual(values = c("darkorange", "red2", "brown")) +
  #     theme_light() +
  #     theme(legend.position = "top", text = element_text(size = 14), legend.box = "horizontal")
  #   ggiraph(ggobj = barplot_1)
  #   
  # })
  
  data_formodel_prep <- reactive({
    
    residents <- read.csv("Data/residenti2019.csv",header=TRUE,sep=",")
    ag <- CoviD_Ita_Reg() %>% spread(Key, Value) %>% arrange(denominazione_regione)
    
    fa <- factor(ag$data)
    fa2 <- factor(ag$denominazione_regione)
    ti <- unclass(fa)
    ti_orig <- attr(ti, "levels")
    
    levels(fa2)[6] <- "Friuli V. G."
    levels(fa2)[5] <- "Emilia Romagna"
    levels(fa2)[17] <- "TrentinoAltoAdige"
    
    residents[,1] <- as.character(residents[,1])
    residents[10,1] <- "Valle d'Aosta"
    
    ma <- match(residents[,1],levels(fa2))
    residents <- residents[which(!is.na(ma)),]
    ma <- match(fa2,residents[,1])
    
    fa2 <- as.character(fa2)
    
    da <- data.frame(ag %>% dplyr::select(-data, -denominazione_regione, -Tamponi, -`Variazione totale positivi`, -`Dismessi/Guariti`, -`Nuovi positivi`),
                     ti=as.numeric(ti),region=fa2,residents=residents[ma,2])
    
    da$region <- factor(as.character(da$region))
    colnames(da) <- c("Deceduti", "Isolamento domiciliare", "Ricoverati con sintomi", 
                      "Terapia intensiva", "Totale casi", "Totale ricoverati",  "Totale positivi", "ti", "region", "residents")
    
    list(da=da, ti_orig=ti_orig)
    
    
  })
  
  outputModello <- reactive({
    
    outmod <- suppressWarnings(run_growth_model(da = da, reg = "Molise", wh = "Totale casi", horizon = 30))
    
    list(outmod, data_formodel_prep()[[2]])
    
  })
  
  outputModelloIta <- reactive({
    
    dd2 <- aggregate(data_formodel_prep()[[1]] %>% dplyr::select(-region, -ti, -residents), list(data_formodel_prep()[[1]]$ti),sum)
    colnames(dd2)[1] <- "ti"
    pc <- dd2[,as.character(input$VarForModelIta)]
    mti <- max(dd2$ti)
    timax <- mti + 30
    mnt <- ifelse(any(diff(pc)<0), F, T)
    np <- suppressWarnings(growthGLM(pc, dd2[,"ti"], timax = timax, family="Poisson",maxiter=5000, monotone=mnt))
    
    ps <- np$pars
    y <- np$linPred
    x <- 1:timax
    
    ti_orig <- data_formodel_prep()[[2]]
    
    
    cc1<-data.frame(x1=c(x[2:mti],rep(NA,length(x[-c(1:mti)]))),
                    pc=c(diff(pc),rep(NA,length(x[-c(1:mti)]))), x=x[-1],y=diff(y))
    cc<-data.frame(x1=c(x[1:mti],rep(NA,length(x[-c(1:mti)]))),
                   pc=c(pc,rep(NA,length(x[-c(1:mti)]))), x=x,y=y)
    
    orig_date_obs <- date(ti_orig[na.omit(cc$x1)])
    horiz_len <- sum(is.na(cc$x1))
    date_forecast <- seq(min(orig_date_obs), max(orig_date_obs) + horiz_len, 1)
    
    cc$x <- date_forecast
    cc$x1 <- c(date_forecast[1:(length(cc$x)-horiz_len)], rep(NA, horiz_len))
    
    orig_date_obs <- date(ti_orig[na.omit(cc1$x1)])
    horiz_len <- sum(is.na(cc1$x1))
    date_forecast <- seq(min(orig_date_obs), max(orig_date_obs) + horiz_len, 1)
    
    cc1$x <- date_forecast
    cc1$x1 <- c(date_forecast[1:(length(cc1$x)-horiz_len)], rep(NA, horiz_len))
    
    list(cc=cc, cc1=cc1, R2 = np$R2 %>% round(4))
    
  })
  
  output$PredCumCases <- renderggiraph({
    
    omod <- outputModello()[[1]]
    ti_orig <- outputModello()[[2]]
                               
    orig_date_obs <- date(ti_orig[na.omit(omod[[1]]$x1)])
    horiz_len <- sum(is.na(omod[[1]]$x1))
    date_forecast <- seq(min(orig_date_obs), max(orig_date_obs) + horiz_len, 1)
    
    omod[[1]]$x <- date_forecast
    omod[[1]]$x1 <- c(date_forecast[1:(length(omod[[1]]$x)-horiz_len)], rep(NA, horiz_len))
    
    plot_mod1 <- ggplot(omod[[1]] %>% 
                          filter(x <= max(x1, na.rm = T) + input$SelDateModel)) +
      geom_line(aes(x=x1,y=pc), alpha=0.2)+geom_point(aes(x=x1,y=pc))+
      geom_line(aes(x=x,y=y),col="red", size = 1.1)+
      labs(x = "", y = "Cumulati") +
      theme_bw() +
      theme(text = element_text(size = 14))
    
    ggiraph(ggobj = plot_mod1)
    
  })
  
  output$PredCumCasesIta <- renderggiraph({
    
    plot_mod1Ita <- ggplot(outputModelloIta()[[1]] %>% 
                          filter(x <= max(x1, na.rm = T) + input$SelDateModelIta)) +
      geom_line(aes(x=x1,y=pc), alpha=0.2)+geom_point(aes(x=x1,y=pc))+
      geom_line(aes(x=x,y=y),col="red", size = 1.1)+
      labs(x = "", y = "Cumulati") +
      theme_bw() +
      theme(text = element_text(size = 14))
    
    ggiraph(ggobj = plot_mod1Ita)
    
  })
  
  output$PredNewCases <- renderggiraph({
    
    omod <- outputModello()[[1]]
    ti_orig <- outputModello()[[2]]
    
    orig_date_obs <- date(ti_orig[na.omit(omod[[2]]$x1)])
    horiz_len <- sum(is.na(omod[[2]]$x1))
    date_forecast <- seq(min(orig_date_obs), max(orig_date_obs) + horiz_len, 1)
    
    omod[[2]]$x <- date_forecast
    omod[[2]]$x1 <- c(date_forecast[1:(length(omod[[2]]$x)-horiz_len)], rep(NA, horiz_len))
    
    plot_mod2 <- ggplot(omod[[2]] %>% 
                          filter(x <= max(x1, na.rm = T) + input$SelDateModel)) +
      geom_line(aes(x=x1,y=pc), alpha=0.2)+geom_point(aes(x=x1,y=pc)) +
      geom_line(aes(x=x,y=y),col="red", size = 1.1)+
      labs(x = "", y = "Giornalieri") +
      theme_bw() +
      theme(text = element_text(size = 14))
    
    ggiraph(ggobj = plot_mod2)
    
  })
  
  output$PredNewCasesIta <- renderggiraph({
    
    
    plot_mod2Ita <- ggplot(outputModelloIta()[[2]] %>% 
                          filter(x <= max(x1, na.rm = T) + input$SelDateModelIta)) +
      geom_line(aes(x=x1,y=pc), alpha=0.2)+geom_point(aes(x=x1,y=pc)) +
      geom_line(aes(x=x,y=y),col="red", size = 1.1)+
      labs(x = "", y = "Giornalieri") +
      theme_bw() +
      theme(text = element_text(size = 14))
    
    ggiraph(ggobj = plot_mod2Ita)
    
  })
  
  output$DatPredCum <- renderDT({
    
    omod <- outputModello()[[1]]
    ti_orig <- outputModello()[[2]]
    
    orig_date_obs <- date(ti_orig[na.omit(omod[[1]]$x1)])
    horiz_len <- sum(is.na(omod[[1]]$x1))
    date_forecast <- seq(min(orig_date_obs), max(orig_date_obs) + horiz_len, 1)
    
    omod[[1]]$x <- date_forecast
    omod[[1]]$x1 <- c(date_forecast[1:(length(omod[[1]]$x)-horiz_len)], rep(NA, horiz_len))
    
    dt_out <- omod[[1]] %>% as_tibble() %>% 
      filter(between(x, max(x1, na.rm = T) -2, max(x1, na.rm = T) + input$SelDateModel)) %>% 
      dplyr::select(x, pc, y) %>% 
      mutate(y = round(y)) %>% 
      rename(Data = x, Osservati = pc, Predetti = y)
    
    datatable(dt_out, rownames = FALSE, caption = "Casi cumulati", options = list(dom = 'tp', pageLength = 5, scrollX = T,
                                                                                  columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    
  })
  
  output$DatPredNew <- renderDT({
    
    omod <- outputModello()[[1]]
    ti_orig <- outputModello()[[2]]
    
    orig_date_obs <- date(ti_orig[na.omit(omod[[2]]$x1)])
    horiz_len <- sum(is.na(omod[[2]]$x1))
    date_forecast <- seq(min(orig_date_obs), max(orig_date_obs) + horiz_len, 1)
    
    omod[[2]]$x <- date_forecast
    omod[[2]]$x1 <- c(date_forecast[1:(length(omod[[2]]$x)-horiz_len)], rep(NA, horiz_len))
    
    dt_out2 <- omod[[2]] %>% as_tibble() %>% 
      filter(between(x, max(x1, na.rm = T) -2, max(x1, na.rm = T) + input$SelDateModel)) %>% 
      dplyr::select(x, pc, y) %>% 
      mutate(y = round(y)) %>% 
      rename(Data = x, Osservati = pc, Predetti = y)
    
    datatable(dt_out2, rownames = FALSE, caption = "Nuovi casi giornalieri", options = list(dom = 'tp', pageLength = 5,
                                                                                            columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    
  })
  
  output$SummaryModel <- reactive({
    
    omod <- outputModello()[[1]]
    ti_orig <- data_formodel_prep()[[2]]
    
    orig_date_obs <- date(ti_orig[1:length(na.omit(omod[[2]]$x1))])
    horiz_len <- sum(is.na(omod[[2]]$x1))
    date_forecast <- seq(min(orig_date_obs), max(orig_date_obs) + horiz_len, 1)
    
    omod[[2]]$x <- date_forecast[-1]
    omod[[2]]$x1 <- c(date_forecast[2:(length(omod[[2]]$x)-horiz_len+1)], rep(NA, horiz_len))
    
    est_pick <- omod[[2]]$x[which.max(omod[[2]]$y)]
    estmax_val <- round(omod[[2]]$y[omod[[2]]$x==est_pick])
    obsmax_val <- ifelse(est_pick<=max(omod[[2]]$x1, na.rm=T), round(omod[[2]]$pc[omod[[2]]$x==est_pick]), NA)
    
    pdate <- paste0(day(est_pick), " ", stri_trans_totitle(month(est_pick, label = T, abbr = F)), " ",  year(est_pick))
    paste0(
      "<ul>",
      "<li><b>Punto di flessione (picco della curva stimato): </b>", pdate,"</li>",
      "<li><b>Massimo stimato di casi giornalieri: </b>", estmax_val, ifelse(!is.na(obsmax_val), paste0(" (osservati ", obsmax_val, ")"), ""),"</li>",
      "<li><b>Goodness of fit</b> (R2): ", outputModello()[[1]][[3]], "</li>",
      "</ul>"
    )
    
  })
  
  output$SummaryModelIta <- reactive({
    
    omod <- outputModelloIta()[[2]]
    ti_orig <- data_formodel_prep()[[2]]
    
    orig_date_obs <- date(ti_orig[1:na.omit()])
    horiz_len <- sum(is.na(omod$x1))
    date_forecast <- seq(min(orig_date_obs), max(orig_date_obs) + horiz_len, 1)
    
    omod$x <- date_forecast[-1]
    omod$x1 <- c(date_forecast[2:(length(omod$x)-horiz_len+1)], rep(NA, horiz_len))
    
    est_pick <- omod$x[which.max(omod$y)]
    estmax_val <- round(omod$y[omod$x==est_pick])
    obsmax_val <- ifelse(est_pick<=max(omod$x1, na.rm=T), round(omod$pc[omod$x==est_pick]), NA)
    
    pdate <- paste0(day(est_pick), " ", stri_trans_totitle(month(est_pick, label = T, abbr = F)), " ",  year(est_pick))
    paste0(
      "<ul>",
      "<li><b>Punto di flessione (picco della curva stimato): </b>", pdate,"</li>",
      "<li><b>Massimo stimato di casi giornalieri: </b>", estmax_val, ifelse(!is.na(obsmax_val), paste0(" (osservati ", obsmax_val, ")"), ""),"</li>",
      "<li><b>Goodness of fit</b> (R2): ", outputModelloIta()[[3]], "</li>",
      "</ul>"
    )
    
  })

}

shinyApp(ui = ui, server = server)