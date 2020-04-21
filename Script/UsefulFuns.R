
# # Useful functions ------------------------------------------------------
# Read italian data
read_italian <- function(path){
  
  dati_Ita <- read_csv(path) %>% 
    mutate(data = date(data)) %>% dplyr::select(-note_it, -note_en, -casi_testati)
  
  colnames(dati_Ita)[-c(1,2)] <- c("Ricoverati con sintomi", "Terapia intensiva", "Attualmente ricoverati",
                                  "Isolamento domiciliare", "Diagnosticati positivi", "Variazione totale positivi", "Nuovi positivi", 
                                  "Dimessi guariti", "Deceduti",  "Cumulati positivi", "Tamponi")
  
  return(dati_Ita)
  
}

# Read and prepare regional data
read_regional <- function(path){
  
  dati_reg <- read_csv(path) %>% 
    dplyr::select(-note_it, -note_en) %>% 
    # Creo la regione Trentino che appare sottoforma di province autonome
    mutate(denominazione_regione = ifelse(denominazione_regione %in% c("P.A. Bolzano", "P.A. Trento"), "Trentino-Alto Adige", denominazione_regione),
           data = date(data)) %>% 
    gather(Key, Value, ricoverati_con_sintomi:tamponi) %>% 
    group_by(Key, data, denominazione_regione) %>% 
    dplyr::summarise(Value = sum(Value)) %>% 
    ungroup() 
  
  dati_reg$Key <- factor(dati_reg$Key, levels = unique(dati_reg$Key), 
                         labels = c("Deceduti", "Dimessi guariti", "Isolamento domiciliare", "Nuovi positivi", "Ricoverati con sintomi", 
                                    "Tamponi", "Terapia intensiva", "Cumulati positivi", "Attualmente ricoverati", "Diagnosticati positivi", 
                                    "Variazione totale positivi"))
  
  return(dati_reg)
}

# Read province data
read_province <- function(path){
  
  dati_prov <- read_csv(path) %>% 
    dplyr::select(-note_it, -note_en) %>%
    mutate(denominazione_regione = ifelse(denominazione_regione %in% c("P.A. Bolzano", "P.A. Trento"), "Trentino-Alto Adige", denominazione_regione),
           data = date(data)) %>% 
    drop_na(sigla_provincia) %>% 
    rename(`Cumulati positivi` = totale_casi)
  
  
  
}

# Read italian data shapefile for map
read_shape_italy <- function(province = F){
  
  italy_sf <- read_rds("Data/gadm36_ITA_1_sp.rds") %>% st_as_sf() %>% dplyr::select(NAME_1, geometry) %>% 
    mutate(NAME_1 = factor(NAME_1, labels = c("Abruzzo", "Puglia", "Basilicata", "Calabria", "Campania", "Emilia-Romagna", "Friuli Venezia Giulia",
                                              "Lazio" ,"Liguria" , "Lombardia", "Marche", "Molise", "Piemonte", "Sardegna", "Sicilia", "Toscana",
                                              "Trentino-Alto Adige", "Umbria", "Valle d'Aosta", "Veneto")))
  colnames(italy_sf)[1] <- "NAME" 
  
  if(province){
    
    italy_sf <- read_rds("Data/gadm36_ITA_2_sp.rds") %>% st_as_sf() %>% dplyr::select(NAME_1, NAME_2, geometry)
    
    # Sistemo etichette
    italy_sf$NAME_1[italy_sf$NAME_1 == "Apulia"] <- "Puglia"
    italy_sf$NAME_1[italy_sf$NAME_1 == "Sicily"] <- "Sicilia"
    italy_sf$NAME_1[italy_sf$NAME_1 == "Friuli-Venezia Giulia"] <- "Friuli Venezia Giulia"
    
    italy_sf$NAME_2[italy_sf$NAME_2 %in% c("Forli' - Cesena", "Mantua", "Padua", 
                                                     "Monza and Brianza", "Pesaro E Urbino", "Syracuse",
                                                     "Florence", "Reggio Di Calabria", "Reggio Nell'Emilia")] <- c("Reggio di Calabria", "Forlì-Cesena", "Reggio nell'Emilia",
                                                                                                                   "Mantova", "Monza e della Brianza", "Pesaro e Urbino",
                                                                                                                   "Siracusa",  "Firenze",  "Padova")
    colnames(italy_sf)[1] <- "NAME"
    
  }
  
  return(italy_sf)
  
}

# Genera print per situazione attuale
print_current_situa <- function(da, dati_letalita, dati_mortalita){
  today <- max(da$data)
  dat_today <- da %>% dplyr::filter(data %in% c(today, today-1))
  
  attualmente_positivi_ultimo <- dat_today$`Diagnosticati positivi`[2]
  incr_attualmente_positivi <- paste_signpercent(dat_today$`Diagnosticati positivi`[2], dat_today$`Diagnosticati positivi`[1])
  
  deceduti_ultimo <- dat_today$Deceduti[2]
  incr_deceduti <- paste_signpercent(dat_today$Deceduti[2], dat_today$Deceduti[1])
  
  dimessi_guariti_ultimo <- dat_today$`Dimessi guariti`[2]
  incr_guariti <- paste_signpercent(dat_today$`Dimessi guariti`[2], dat_today$`Dimessi guariti`[1])
  
  totale_casi_ultimo <- dat_today$`Cumulati positivi`[2]
  incr_totale_casi <- paste_signpercent(dat_today$`Cumulati positivi`[2], dat_today$`Cumulati positivi`[1])
  
  ricov_sintomi_ultimo <- dat_today$`Ricoverati con sintomi`[2]
  incr_ricoverati <- paste_signpercent(dat_today$`Ricoverati con sintomi`[2], dat_today$`Ricoverati con sintomi`[1])
  
  isol_domic_ultimo <- dat_today$`Isolamento domiciliare`[2]
  incr_isolamento <- paste_signpercent(dat_today$`Isolamento domiciliare`[2], dat_today$`Isolamento domiciliare`[1])
  
  terapia_intens_ultimo <- dat_today$`Terapia intensiva`[2]
  incr_terapia <- paste_signpercent(dat_today$`Terapia intensiva`[2], dat_today$`Terapia intensiva`[1])
  
  totale_ricoverati_ultimo <- dat_today$`Attualmente ricoverati`[2]
  incr_ricoverati <- paste_signpercent(dat_today$`Attualmente ricoverati`[2], dat_today$`Attualmente ricoverati`[1])
  
  
  quoz_let_ita <- round(sum(dati_letalita$Totdec)/sum(dati_letalita$Totcasi),4)*100
  
  tasso_mort_ita <- round(sum(dati_mortalita$Totdec)/sum(dati_mortalita$Totres),4)*100
  
  return(
    paste0(h2("Situazione italiana relativa all'epidemia COVID-19 ad oggi*:"),
           h5("*I dati sono aggiornati alle ore 18pm del",
              day(today), my_month(stri_trans_totitle(month(today, label = T, abbr = F))), year(today)),
           "<ul>",
           "<li><b>Cumulati positivi: </b>", totale_casi_ultimo, "(",incr_totale_casi,")","</li>",
           "<li><b>Diagnosticati positivi: </b>", attualmente_positivi_ultimo,"(",incr_attualmente_positivi,")","</li>",
           "<li><b>Attualmente ricoverati: </b>", totale_ricoverati_ultimo, "(",incr_ricoverati,")","</li>",
           "<li><b>Posti occupati in terapia intensiva: </b>", terapia_intens_ultimo, "(",incr_terapia,")","</li>",
           "<li><b>Ricoverati con sintomi: </b>", ricov_sintomi_ultimo, "(",incr_ricoverati,")","</li>",
           "<li><b>Isolamento domiciliare: </b>", isol_domic_ultimo, "(",incr_isolamento,")","</li>",
           "<li><b>Dimessi guariti: </b>", dimessi_guariti_ultimo,"(",incr_guariti,")","</li>",
           "<li><b>Deceduti: </b>", deceduti_ultimo, "(",incr_deceduti,")","</li>",
           "<li><b>Tasso di mortalita': </b>", tasso_mort_ita, "%","</li>",
           "<li><b>Tasso di letalita': </b>", quoz_let_ita, "%","</li>",
           "</ul>",
           collapse = ""
    )
  )
}


# Do donut plot
do_ciambella <- function(da, reg = NULL, variable = "Totale casi"){
  
  colors <- c("darkorange", "firebrick", "salmon")
  
  if(variable == "Cumulati positivi"){
    vartosel <- c("Deceduti", "Diagnosticati positivi", "Dimessi guariti")
  }
  if(variable == "Diagnosticati positivi"){
    vartosel <- c("Isolamento domiciliare", "Ricoverati con sintomi", "Terapia intensiva")
  }
  
  if(is.null(reg)){
    dperdonut <- da %>% 
      gather(Key, Value, vartosel) %>% 
      filter(data == max(data)) %>% 
      mutate(percentage = round(Value/sum(Value),4)*100,
             lab.pos = cumsum(percentage)-.5*percentage) %>%
      mutate(stato = "Italia") %>% 
      rename(denominazione_regione = stato)
  }else{
    dperdonut <- da %>% 
      filter(denominazione_regione == reg, data == max(data), 
             Key %in% vartosel) %>% 
      mutate(percentage = round(Value/sum(Value),4)*100,
             lab.pos = cumsum(percentage)-.5*percentage) 
  }
  
  dperdonut %>% 
    plot_ly(labels = ~Key, values = ~percentage, 
            type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            hole = 0.6,
            text = ~paste(Key, "\n", percentage, '%'),
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1))) %>% 
    layout(title = paste0("Distribuzione ", variable,  " in ", "<b>",unique(dperdonut$denominazione_regione), "</b>"), 
           showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
}


# Do barplot TS
do_barplot_ts <- function(da, reg = NULL, variable = "Cumulati positivi"){
  
  if(variable == "Cumulati positivi"){
    vartosel <- c("Diagnosticati positivi", "Dimessi guariti", "Deceduti")
  }
  if(variable == "Diagnosticati positivi"){
    vartosel <- c("Ricoverati con sintomi", "Terapia intensiva", "Isolamento domiciliare")
  }
  
  if(is.null(reg)){
    dperbar <- da %>%
      mutate(stato = "Italia") %>% 
      rename(denominazione_regione = stato) %>% 
      dplyr::select(data, denominazione_regione, vartosel)
    
    colnames(dperbar)[-c(1,2)] <- c("Y1", "Y2", "Y3")
    
  }else{
    dperbar <- da %>% 
      filter(denominazione_regione == reg, Key %in% vartosel) %>% 
      spread(Key, Value) %>% 
      dplyr::select(data, denominazione_regione, vartosel)
    
    colnames(dperbar)[-c(1,2)] <- c("Y1", "Y2", "Y3")
  }
  
  dperbar %>% 
    plot_ly(x = ~data, y = ~Y1, type = "bar", name = vartosel[1], color = I("firebrick")) %>% 
    add_trace(y = ~Y2, name = vartosel[2], color = I("salmon")) %>% 
    add_trace(y = ~Y3, name = vartosel[3], color = I("darkorange")) %>% 
    layout(title = paste0("Serie storica ", variable, "\n", "<b>",unique(dperbar$denominazione_regione), "</b>"),
           xaxis = list(title = ""),
           yaxis = list(title = 'Conteggi'), barmode = 'stack')
  
  
}


# Do map
draw_map <- function(dajoined, reg = NULL, varsel, datasel, dajoinedprov){
  
  annots <- list(x = 1, y = -0.1, text = as.character(datasel), 
                 showarrow = F, xref='paper', yref='paper', 
                 xanchor='right', yanchor='auto', xshift=0, yshift=0,
                 font=list(size=15, color="black"))
  if(is.null(reg)){
    p <- plot_ly(data = dajoined %>% 
                   filter(Key == as.character(varsel), data == as.character(datasel)), 
                 stroke = I("black"),
                 split = ~NAME, color = ~Value, colors = "YlOrRd", alpha = 1, 
                 text = ~paste0(NAME, "\n", Value, " persone"),
                 hoveron = "fills",
                 hoverinfo = "text", 
                 showlegend = F) %>%
      layout(legend = list(orientation = "h"), annotations = annots) %>% 
      colorbar( title = list(text = paste0("<b>", as.character(varsel), "</b>")), len = 0.6)
  }else{
    
    if(varsel == "Cumulati positivi"){
      p <- plot_ly(data = dajoinedprov %>% 
                     filter(Key == as.character(varsel), data == as.character(datasel), NAME %in% reg), 
                   stroke = I("black"),
                   split = ~NAME_2, color = ~Value, colors = "YlOrRd", alpha = 1, 
                   text = ~paste0(NAME_2, " (",Key, ")","\n", Value, " persone"),
                   hoveron = "fills",
                   hoverinfo = "text", showlegend = F) %>% 
        layout(legend = list(orientation = "h"), annotations = annots)  %>% 
        colorbar( title = list(text = paste0("<b>", as.character(varsel), "</b>")), len = 0.6)
    }else{
      p <- plot_ly(data = dajoined %>% 
                     filter(Key == as.character(varsel), data == as.character(datasel), NAME %in% reg), 
                   stroke = I("black"),
                   split = ~NAME, color = ~Value, colors = "YlOrRd", alpha = 1, 
                   text = ~paste0(NAME, " (",Key, ")","\n", Value, " persone"),
                   hoveron = "fills",
                   hoverinfo = "text", showlegend = F) %>% 
        layout(legend = list(orientation = "h"), annotations = annots)  %>% 
        colorbar( title = list(text = paste0("<b>", as.character(varsel), "</b>")), len = 0.6)
      if(length(reg) == 1){
        p <- p %>% hide_colorbar()
      }
    }
    
  }
  
  
  return(p)
  
  
}


# Do Time series
do_ts <- function(da, reg = NULL, varsel, datasel, is.incrementi=F, tipo.incremento = "Assoluti"){
  
  if(is.null(reg)){
    
    dts <- da %>% 
      filter(data <= as.character(datasel)) %>% 
      mutate(denominazione_regione = "Italia") %>% 
      dplyr::select(data, denominazione_regione, varsel) %>% 
      gather(Key, Value, varsel)
    
  }else{
    
    dts <- da %>% 
      filter(data <= as.character(datasel), Key == varsel, denominazione_regione %in% reg) %>% 
      dplyr::select(data, denominazione_regione, Key, Value)
    
  }
  
  ylabel <- "Conteggi"
  
  if(is.incrementi){
    dts <- dts %>% 
      arrange(denominazione_regione) %>% 
      group_split(denominazione_regione) %>% 
      map_dfr(function(x){
        x %<>% 
          mutate(IncrAss = c(0, diff(Value)),
                 IncrPerc = round((c(0, diff(Value))/Value)*100,4)) %>% 
          ungroup() 
      })
    ylabel <- ifelse(tipo.incremento == "Assoluti", "Incrementi assoluti (rispetto al giorno precedente)",
                     "Incrementi % (rispetto al giorno precedente)") 
    ysel <- ifelse(tipo.incremento == "Assoluti", "IncrAss", "IncrPerc") 
    dts <- dts %>% dplyr::select(data, denominazione_regione, Key, ysel) %>% rename(Value = ysel)
  }
  
  
  dts %>% 
    plot_ly(x = ~data, y = ~Value, type = 'scatter', mode = 'lines+markers', 
            color = ~denominazione_regione, colors = "YlOrRd") %>% 
    layout(xaxis = list(title = ""), yaxis = list(title = ylabel), title = varsel)
  
}

# Converto month english to ita
my_month <- function(eng_month){
  
  months_eng <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  
  months_ita <- c("Gennaio", "Febbraio", "Marzo", "Aprile", "Maggio", "Giugno", "Luglio", "Agosto", "Settembre", "Ottobre","Novembre", "Dicembre")
  
  return(months_ita[which(eng_month == months_eng)])
  
}


# Print sign header
paste_signpercent <- function(x_oggi,x_ieri){
  simbolo <- ifelse(x_oggi-x_ieri>0, "+", "")
  incr <- ((x_oggi-x_ieri)/x_ieri)*100
  return(paste0(simbolo, round(incr, 2),"% rispetto a ieri"))
} 


# Prepare data for model regional level
prepdata_for_model <- function(dftoprep, resdata){
  ag <- dftoprep %>% 
    mutate(denominazione_regione = ifelse(denominazione_regione == "Trentino-Alto Adige", "TrentinoAltoAdige", 
                                          ifelse(denominazione_regione == "Friuli Venezia Giulia", "Friuli V. G.",
                                                 ifelse(denominazione_regione == "Emilia-Romagna", "Emilia Romagna",
                                                 denominazione_regione)))) %>% 
    arrange(denominazione_regione, data)
  
  # Aggiusto le etichette
  resdata[10,1] <- "Valle d'Aosta"
  
  da <- ag %>% 
    left_join(resdata, by = c("denominazione_regione" = "Territorio")) %>% 
    mutate(ti = as.numeric(unclass(factor(data))), denominazione_regione = factor(denominazione_regione)) %>% 
    rename(ti_orig = data, region = denominazione_regione, residents = totale) 
  
  return(da)
}

# function for running the model 
run_growth_model <- function(da, reg=NULL, wh="Cumulati positivi", horizon = 10, fam="Poisson") {


  ti_orig_out <- da$ti_orig %>% unique()

  if(is.null(reg)){
    dat <- aggregate(da %>% dplyr::select(-region, -ti, -residents, -ti_orig), list(da$ti), sum)
    colnames(dat)[1] <- "ti"

  }else{
    dat <- da[da$region==reg,]
  }

  # Seleziono variabile da modellare
  whidx <- which(colnames(dat)==wh)
  pc <- dat[, whidx, drop = T]
  ti <- dat$ti


  if(any(pc==0)) {
    w <- min(which(pc!=0))
    pc <- pc[-c(1:w)]
    ti <- ti[-c(1:w)]
    ti <- ti-min(ti)+1
    ti_orig_out <- dat$ti_orig[-c(1:w)]
  }

  mti <- max(ti)
  timax <- mti+horizon
  diffpc <- diff(pc)
  mnt <- ifelse(any(diffpc[(length(diffpc)-3):length(diffpc)]<0), F, T)
  rrs <- ifelse(mnt, 250, 500)

  np <- tryCatch(
    growthGLM(count = pc, ti = ti, timax = timax, family = fam, maxiter = 10000, monotone = mnt, runs = rrs),
    error = function(e){return("Error")}
    )

  if(!mnt){

    np2 <- tryCatch(
      growthGLMr(count = pc, ti = ti, timax = timax, family = fam, maxiter = 10000, monotone = mnt, runs = rrs),
      error = function(e){return("Error")}
    )

    # Prediction dei cumulati
    ymirr <- np$linPred
    ymirrred <- np2$linPred

    # Creo tempi fittizi numerici
    x <- seq(min(ti_orig_out), max(ti_orig_out) + horizon, 1)
    x1 <- c(ti_orig_out[2:mti], rep(NA, horizon))

    pc_out <- c(diff(pc), rep(NA, horizon))

    # Data frame con osservati e predetti nuovi
    cc1<-data.frame(x1 = x1, pc = pc_out, x = x[-1], y = diff(ymirr), ymirrred = diff(ymirrred))

    # Data frame con osservati e predetti cumulati
    cc<-data.frame(x1 = c(x1[1] - 1, x1), pc = c(pc,rep(NA,horizon)), x = x, y = ymirr, ymirrred = ymirrred)

    ooo <- list(cc=cc, cc1=cc1, R2 = list(round(np$R2, 4), round(np2$R2, 4)),
                pars = list(np$pars, np2$pars), stderrs = list(np$se, np2$se), monot = mnt)

  }else{

    # Prediction dei cumulati
    y <- np$linPred
    # Creo tempi fittizi numerici
    x <- seq(min(ti_orig_out), max(ti_orig_out) + horizon, 1)
    x1 <- c(ti_orig_out[2:mti], rep(NA, horizon))

    pc_out <- c(diff(pc), rep(NA, horizon))

    # Data frame con osservati e predetti nuovi
    cc1<-data.frame(x1 = x1,
                    # Creo gli osservati facendo le differenze prime dei cumulati
                    pc = pc_out,
                    x = x[-1], y = diff(y))

    # Data frame con osservati e predetti cumulati
    cc<-data.frame(x1 = c(x1[1] - 1, x1),
                   pc = c(pc,rep(NA,horizon)),
                   x = x, y = y)

    ooo <- list(cc=cc, cc1=cc1, R2 = list(round(np$R2, 4)), pars = list(np$pars), stderrs = list(np$se), monot = mnt)

  }

  return(ooo)
}


# run_growth_model <- function(da, reg=NULL, wh="Totale casi", horizon = 10, fam="Poisson") {
#
#   source("Script/FunctionsDef.R")
#   ti_orig_out <- da$ti_orig %>% unique()
#
#   if(is.null(reg)){
#     dat <- aggregate(da %>% dplyr::select(-region, -ti, -residents, -ti_orig), list(da$ti), sum)
#     colnames(dat)[1] <- "ti"
#
#   }else{
#     dat <- da[da$region==reg,]
#   }
#
#   # Seleziono variabile da modellare
#   whidx <- which(colnames(dat)==wh)
#   pc <- dat[, whidx, drop = T]
#   ti <- dat$ti
#
#
#   if(any(pc==0)) {
#     w <- min(which(pc!=0))
#     pc <- pc[-c(1:w)]
#     ti <- ti[-c(1:w)]
#     ti <- ti-min(ti)+1
#     ti_orig_out <- dat$ti_orig[-c(1:w)]
#   }
#
#   mti <- max(ti)
#   timax <- mti+horizon
#   diffpc <- diff(pc)
#   mnt <- ifelse(any(diffpc[(length(diffpc)-3):length(diffpc)]<0), F, T)
#   rrs <- ifelse(mnt, 250, 500)
#
#   np <- tryCatch(
#     growthGLM(count=pc, ti = ti, monotone = mnt, nmirror = 9, tiMax = NA, family = fam, maxiter = 10000, runs = rrs, tPred = timax),
#     error = function(e){return("Error")}
#   )
#
#   if(!mnt){
#
#     np2 <- tryCatch(
#       growthGLM(count=pc, ti = ti, monotone = mnt, nmirror = 5, tiMax = NA, family = fam, maxiter = 10000, runs = rrs, tPred = timax),
#       error = function(e){return("Error")}
#     )
#
#     # Prediction dei cumulati
#     ymirr <- np$linPred
#     ymirrred <- np2$linPred
#
#     # Creo tempi fittizi numerici
#     x <- seq(min(ti_orig_out), max(ti_orig_out) + horizon, 1)
#     x1 <- c(ti_orig_out[2:mti], rep(NA, horizon))
#
#     pc_out <- c(diff(pc), rep(NA, horizon))
#
#     # Data frame con osservati e predetti nuovi
#     cc1<-data.frame(x1 = x1, pc = pc_out, x = x[-1], y = diff(ymirr), ymirrred = diff(ymirrred))
#
#     # Data frame con osservati e predetti cumulati
#     cc<-data.frame(x1 = c(x1[1] - 1, x1), pc = c(pc,rep(NA,horizon)), x = x, y = ymirr, ymirrred = ymirrred)
#
#     ooo <- list(cc=cc, cc1=cc1, R2 = list(round(np$R2, 4), round(np2$R2, 4)),
#                 pars = list(np$pars, np2$pars), stderrs = list(np$se, np2$se), monot = mnt)
#
#   }else{
#
#     # Prediction dei cumulati
#     y <- np$linPred
#     # Creo tempi fittizi numerici
#     x <- seq(min(ti_orig_out), max(ti_orig_out) + horizon, 1)
#     x1 <- c(ti_orig_out[2:mti], rep(NA, horizon))
#
#     pc_out <- c(diff(pc), rep(NA, horizon))
#
#     # Data frame con osservati e predetti nuovi
#     cc1<-data.frame(x1 = x1,
#                     # Creo gli osservati facendo le differenze prime dei cumulati
#                     pc = pc_out,
#                     x = x[-1], y = diff(y))
#
#     # Data frame con osservati e predetti cumulati
#     cc<-data.frame(x1 = c(x1[1] - 1, x1),
#                    pc = c(pc,rep(NA,horizon)),
#                    x = x, y = y)
#
#     ooo <- list(cc=cc, cc1=cc1, R2 = list(round(np$R2, 4)), pars = list(np$pars), stderrs = list(np$se), monot = mnt)
#
#   }
#
#   return(ooo)
# }


# Plot result model (return plot)
plot_out_model <- function(outputmod, hz = 1, what = "Cumulati", VarModel = "Cumulati positivi", addReduMirr = F){
  
  if(what == "Cumulati"){
    dd <- outputmod$cc
    ylabel <- "Totali giornalieri"
    if(VarModel %in% c("Deceduti", "Cumulati positivi")){
      ylabel <- "Cumulati"
    }
  } 
  if(what == "Nuovi"){
    dd <- outputmod$cc1
    ylabel <- "Variazioni giornaliere"
    if(VarModel %in% c("Deceduti", "Cumulati positivi")){
      ylabel <- "Totali giornalieri"
    }
  } 
  
  # Picco stimato
  # l_pck <- round(min(dd$x) + exp(outputmod$pars[4]-1.96*outputmod$stderrs[4]))
  # u_pck <- round(min(dd$x) + exp(outputmod$pars[4]+1.96*outputmod$stderrs[4]))
  # est_pick <- round(min(dd$x) + exp(outputmod$pars[4]))
  
  if(outputmod$monot){
    pp <- plot_ly(dd %>% filter(x <= max(x1, na.rm = T) + hz), 
                  x=~x1, y = ~pc, type = 'scatter', mode = 'lines+markers', name = "Osservati", color = I("black"), alpha = 0.7) %>% 
      add_trace(x=~x, y = ~y, name = "Richards", color = I("red"), mode = 'lines', alpha = 1, line = list(width = 3)) %>% 
      layout(xaxis = list(title = ""), yaxis = list(title = ylabel))
  } else {
    
    if(addReduMirr){
      
      pp <- plot_ly(dd %>% filter(x <= max(x1, na.rm = T) + hz), 
                    x=~x1, y = ~pc, type = 'scatter', mode = 'lines+markers', name = "Osservati", color = I("black"), alpha = 0.7) %>% 
        add_trace(x=~x, y = ~y, name = "Mirr. Richards", color = I("red3"), mode = 'lines', alpha = 1, line = list(width = 3)) %>% 
        add_trace(x=~x, y = ~ymirrred, name = "Mirr. Reduced", color = I("skyblue3"), mode = 'lines', alpha = 1, line = list(width = 3)) %>% 
        layout(xaxis = list(title = ""), yaxis = list(title = ylabel))
      
      
    } else {
      
      pp <- plot_ly(dd %>% filter(x <= max(x1, na.rm = T) + hz), 
                    x=~x1, y = ~pc, type = 'scatter', mode = 'lines+markers', name = "Osservati", color = I("black"), alpha = 0.7) %>% 
        add_trace(x=~x, y = ~y, name = "Mirr. Richards", color = I("red3"), mode = 'lines', alpha = 1, line = list(width = 3)) %>% 
        layout(xaxis = list(title = ""), yaxis = list(title = ylabel))
      
    }
  }
  
  return(pp)
  
}


# Table output model (return table)
DT_out_model <- function(outputmod, hz = 1, VarModel = "Cumulati positivi") {
  
  sketch  <-  htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Data'),
        th(colspan = 2, 'Totali'),
        th(colspan = 2, 'Variazioni')
      ),
      tr(
        lapply(rep(c('Osservati', 'Previsti'), 2), th)
      )
    )
  ))
  
  ddt <- bind_cols(outputmod$cc %>% 
                     dplyr::select(x1, pc, x, y) %>% 
                     set_colnames(value = c("Data", "Totali.Osservati", "Data2", "Totali.Previstit")),
                   outputmod$cc1 %>% 
                     dplyr::select(x1, pc, x, y) %>% 
                     add_row(data.frame(x1=outputmod$cc[1,1], pc=NA, x=outputmod$cc[1,1], y=NA), .before = T) %>%  
                     set_colnames(value = c("Data", "Variazioni.Osservati", "Data2", "Variazioni.Previsti")) %>% 
                     dplyr::select(-Data, -Data2)) %>% 
    filter(between(Data2, max(Data, na.rm = T) -2, max(Data, na.rm = T) + hz)) %>% 
    dplyr::select(-Data) %>% 
    mutate_if(is.numeric, round) 
  
  #cpt <- ifelse(what == "Cumulati", "Casi cumulati", "Nuovi casi giornalieri")
  
  ddtt <- datatable(ddt[,c(2, 1, 3:5)], rownames = FALSE, #caption = cpt, 
            options = list(dom = 'tp', pageLength = 5, scrollX = T, columnDefs = list(list(className = 'dt-center', targets = "_all"))),
            container = sketch)
  
  
  if(VarModel %in% c("Deceduti", "Dimessi guariti", "Cumulati positivi")){
    
    sketch  <-  htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Data'),
          th(colspan = 2, 'Cumulati'),
          th(colspan = 2, 'Totali')
        ),
        tr(
          lapply(rep(c('Osservati', 'Previsti'), 2), th)
        )
      )
    ))
    
    ddt <- bind_cols(outputmod$cc %>% set_colnames(value = c("Data", "Cumulati.Osservati", "Data2", "Cumulati.Previsti")),
                     outputmod$cc1 %>% add_row(data.frame(x1=outputmod$cc[1,1], pc=NA, x=outputmod$cc[1,1], y=NA), .before = T) %>%  
                       set_colnames(value = c("Data", "Totali.Osservati", "Data2", "Totali.Previsti")) %>% 
                       dplyr::select(-Data, -Data2)) %>% 
      filter(between(Data2, max(Data, na.rm = T) -2, max(Data, na.rm = T) + hz)) %>% 
      dplyr::select(-Data) %>% 
      mutate_if(is.numeric, round) 
    
    #cpt <- ifelse(what == "Cumulati", "Casi cumulati", "Nuovi casi giornalieri")
    
    ddtt <- datatable(ddt[,c(2, 1, 3:5)], rownames = FALSE, #caption = cpt, 
                      options = list(dom = 'tp', pageLength = 5, scrollX = T, columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                      container = sketch)
  }
  
  return(ddtt)
  
}

# print summary model (return html code)
summary_out_model <- function(outputmod, varest, resdata, is.reg = F, reg = NULL, monotone){
  
  resdata[10,1]="Valle d'Aosta"
  
  if(is.reg & !is.null(reg)) mx_sel <- resdata[resdata$Territorio == reg, 2]
  if(!is.reg) mx_sel <- 6*10^7
  
  omod <- outputmod$cc1
  ti_orig_out <- c(outputmod$cc1$x1[1]-1, outputmod$cc1$x1 %>% na.omit())
  
  # Picco stimato
  if(monotone){
    l_pck <- round(min(ti_orig_out) + exp(outputmod$pars[[1]][4]-1.96*outputmod$stderrs[[1]][4]))
    u_pck <- round(min(ti_orig_out) + exp(outputmod$pars[[1]][4]+1.96*outputmod$stderrs[[1]][4]))
    est_pick <- round(min(ti_orig_out) + exp(outputmod$pars[[1]][4]))
  }else{
    l_pck <- round(min(ti_orig_out) + exp(outputmod$pars[[1]][6]-1.96*outputmod$stderrs[[1]][6]))
    u_pck <- round(min(ti_orig_out) + exp(outputmod$pars[[1]][6]+1.96*outputmod$stderrs[[1]][6]))
    est_pick <- round(min(ti_orig_out) + exp(outputmod$pars[[1]][6]))
  }
  
  # Valore stimato nel giorno del picco
  estmax_val <- round(omod$y[omod$x==est_pick])
  # Se picco già avvenuto, metti valore osservato
  obsmax_val <- ifelse(est_pick<=max(omod$x1, na.rm=T), round(omod$pc[omod$x==est_pick]), NA)
  
  pdate_l <- paste0(day(l_pck), " ", my_month(stri_trans_totitle(month(l_pck, label = T, abbr = F))), " ",  year(l_pck))
  pdate <- paste0(day(est_pick), " ", my_month(stri_trans_totitle(month(est_pick, label = T, abbr = F))), " ",  year(est_pick))
  pdate_u <- paste0(day(u_pck), " ", my_month(stri_trans_totitle(month(u_pck, label = T, abbr = F))), " ",  year(u_pck))
  
  if(varest %in% c("Deceduti", "Cumulati positivi")){
      
    # Asintoto cumulati
    asi_est_l <- round(exp(outputmod$pars[[1]][1]-1.96*outputmod$stderrs[[1]][1])+exp(outputmod$pars[[1]][2]-1.96*outputmod$stderrs[[1]][2]))
    idx <- min(which(is.na(outputmod$cc$pc)))-1
    asi_est_l <- ifelse(asi_est_l < outputmod$cc$pc[idx], outputmod$cc$pc[idx], asi_est_l)
    asi_est_u <- round(exp(outputmod$pars[[1]][1]+1.96*outputmod$stderrs[[1]][1])+exp(outputmod$pars[[1]][2]+1.96*outputmod$stderrs[[1]][2])) 
    asi_est_u <- ifelse(asi_est_u > mx_sel, mx_sel, asi_est_u)
    asi_est <- round(exp(outputmod$pars[[1]][1])+exp(outputmod$pars[[1]][2])) 
    
    out_string <- paste0(
      "<ul>",
      "<li><b>Picco della curva stimato: </b>", pdate, paste0(" (non prima del ", pdate_l, ", non dopo il ", pdate_u, ")"), "</li>",
      "<li><b>Massimo stimato di casi giornalieri: </b>", estmax_val, ifelse(!is.na(obsmax_val), paste0(" (osservati ", obsmax_val, ")"), ""),"</li>",
      "<li><b>Asintoto stimato casi cumulati: </b>", asi_est, paste0(" (non meno di ", asi_est_l, ", non piu' di ", asi_est_u, ")"),"</li>", 
      "<li><b>Bonta' di adattamento: </b> ", outputmod$R2[[1]], "</li>",
      "</ul>",
      collapse = ""
    )
  } else{
    out_string <- paste0(
      "<ul>",
      "<li><b>Picco della curva stimato: </b>", pdate, paste0(" (non prima del ", pdate_l, ", non dopo il ", pdate_u, ")"), "</li>",
      "<li><b>Massimo stimato di casi giornalieri: </b>", estmax_val, ifelse(!is.na(obsmax_val), paste0(" (osservati ", obsmax_val, ")"), ""),"</li>",
      "<li><b>Bonta' di adattamento:</b> ", outputmod$R2[[1]], "</li>",
      "</ul>",
      collapse = ""
    )
    
  }
  
  return(out_string)
}

# dcat <- list("Northern Italy" = c("Lombardia", "Liguria", "Piemonte", "Valle d'Aosta",
#                                      "Emilia-Romagna", "Friuli Venezia Giulia", "Veneto", "Trentino-Alto Adige"),
#              "Central Italy" = c("Lazio", "Marche", "Toscana", "Umbria"),
#              "Sourhern Italy" = c("Abruzzo", "Basilicata", "Calabria",
#                                      "Campania", "Molise", "Puglia"),
#              "Insular" = c("Sardegna", "Sicilia")) %>%
#   unlist(use.names = T)
# 
# 
# dcat <- data.frame(cbind(Macro = gsub("[[:digit:]]","",names(dcat)), denominazione_regione = as.character(dcat)))
# 
# dati_reg$Key <- factor(dati_reg$Key, levels = c("Deceduti", "Dismessi/Guariti", "Nuovi positivi",
#                                                 "Ricoverati con sintomi", "Terapia intensiva", "Isolamento domiciliare",
#                                                 "Totale ricoverati", "Totale positivi",  "Totale casi",
#                                                 "Variazione totale positivi", "Tamponi"))
# 
# 
# 
# hm <- dati_reg %>% 
#   # spread(Key, Value) %>% 
#   # group_by(denominazione_regione) %>% 
#   # mutate_at(.vars = c("Deceduti", "Totale casi"), .funs = function(x) c(0, diff(x))) %>% 
#   # gather(Key, Value, -data, -denominazione_regione) %>% 
#   #left_join(dcat, by = "denominazione_regione") %>%
#   group_by(denominazione_regione, Key) %>%
#   mutate(Value = Value/max(Value)) %>%
#   #mutate(Value = log1p(Value)) %>%
#   filter(!(Key %in% c("Variazione totale positivi","Tamponi"))) %>%
#   ggplot(aes(x = data, y = denominazione_regione, fill = Value)) + geom_raster() + scale_fill_distiller(direction = 1, palette = "YlOrRd") +
#   scale_x_date(expand = c(0,0), date_labels = "%d/%m", date_breaks = "7 days") +
#   scale_y_discrete(expand = c(0,0), limits = rev(dcat$denominazione_regione)) +
#   #geom_vline(xintercept = date("2020-03-08")) +
#   facet_wrap(~Key) +
#   labs(x = "", y = "", fill = "") +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 10), legend.position = "bottom")
# 
# hm
# ggplotly(hm)

# jpeg("www/HeatmapMortalita.jpg", height = 750, width = 1200)
# pdf("www/HeatmapMortalita.pdf", height = 7, width = 12)
# data_formodel_prep %>%
#   dplyr::select(ti_orig, region, Deceduti, residents) %>%
#   group_by(region) %>%
#   mutate(Deceduti = c(0, diff(Deceduti)),
#          mortalita = round(Deceduti/residents, 6)*100) %>%
#   ungroup() %>% 
#   left_join(dcat, by = c("region"="denominazione_regione")) %>%
#   ggplot(aes(x = ti_orig, y = region, fill = mortalita)) +
#   geom_tile() + scale_fill_distiller(direction = 1, palette = "YlOrRd") +
#   scale_x_date(expand = c(0,0), date_labels = "%d/%m", date_breaks = "7 days") +
#   scale_y_discrete(expand = c(0,0), limits = rev(dcat$denominazione_regione)) +
#   #geom_vline(xintercept = date("2020-03-08")) +
#   labs(x = "", y = "", fill = "", title = "Tasso di mortalità (%)") +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 10), legend.position = "bottom") +
#   guides(fill = guide_colourbar(barheight = 0.8, barwidth = 10))
# dev.off()
# # 
# pdf("www/HeatmapLetalita.pdf", height = 7, width = 12)
# dati_reg %>% spread(Key, Value) %>% 
#   dplyr::select(data, denominazione_regione, Deceduti, `Totale casi`) %>%
#   group_by(denominazione_regione) %>%
#   # mutate(Deceduti = c(0, diff(Deceduti)),
#   #        `Totale casi` = c(0, diff(`Totale casi`))) %>%
#   #filter(`Totale casi`>0) %>%
#   mutate(Letalita = ifelse(`Totale casi`>0, (Deceduti/`Totale casi`)*100,0)) %>%
#   #filter(Letalita <= 1) %>%
#   left_join(dcat, by = "denominazione_regione") %>%
#   ggplot(aes(x = data, y = denominazione_regione, fill = Letalita)) +
#   geom_raster() + scale_fill_distiller(direction = 1, palette = "YlOrRd") +
#   scale_x_date(expand = c(0,0), date_labels = "%d/%m", date_breaks = "7 days") +
#   scale_y_discrete(expand = c(0,0), limits = rev(dcat$denominazione_regione)) +
#   #geom_vline(xintercept = date("2020-03-08")) +
#   labs(x = "", y = "", fill = "", title = "Quoziente di letalità (%)") +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 10), legend.position = "bottom")
# dev.off()
# Build map default

# plot_leaflet_map <- function(df, variable, datetoselect){
#   
#   dd <- df %>% dplyr::filter(Key == variable, data == datetoselect)
#   
#   pal <- colorBin("YlOrRd", domain = dd$Value, bins = 6)
#   
#   leaflet(data = dd) %>% 
#     clearTiles() %>% 
#     clearShapes() %>%
#     clearControls() %>% 
#     addTiles() %>% 
#     addPolygons(
#       fillColor = ~pal(Value),
#       weight = 2,
#       opacity = 1,
#       color = "black",
#       dashArray = "1",
#       fillOpacity = 1,
#       highlight = highlightOptions(
#         weight = 5,
#         fillOpacity = 1,
#         bringToFront = TRUE),
#       label = print_labels(dd),
#       labelOptions = labelOptions(
#         style = list("font-weight" = "normal", padding = "3px 8px"),
#         textsize = "15px",
#         direction = "auto")) %>%
#     addLegend(pal = pal, values = ~Value, opacity = 1, title = variable, position = "topright", na.label = "Missing")
#   
# }
