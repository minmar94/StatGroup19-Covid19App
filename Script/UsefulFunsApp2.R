# # Useful functions ------------------------------------------------------
# Paste date in english
paste_eng_date <- function(date){
  dd <- day(date)
  mm <- month(date, label = T, abbr = F)
  yy <-year(date)
  
  l1 <- ifelse(dd %in% c(1, 21, 31), "st", ifelse(dd %in% c(2, 22), "nd", ifelse(dd %in% c(3, 23), "rd", "th")))
  outmonth <- paste0(dd, l1, " of ", mm, " ", yy)
  return(outmonth)
}

# Read italian data
read_italian <- function(path){
  
  dati_Ita <- read_csv(path) %>% 
    mutate(data = date(data)) %>% dplyr::select(-note_it, -note_en) 
  
  colnames(dati_Ita)[-c(1,2)] <- c("Hospitalized with symptoms", "Intensive care", "Current hospitalized",
                                   "Home isolation", "Current positives", "Variation Current positives", "New positives", 
                                   "Discharged recovered", "Deceased",  "Cumulative positives", "Swabs", "Tested cases")
  
  return(dati_Ita)
  
}

# Read and prepare regional data
read_regional <- function(path){
  
  dati_reg <- read_csv(path, guess_max = 1500) %>% 
    dplyr::select(-note_it, -note_en) %>% 
    # 
    mutate(denominazione_regione = ifelse(denominazione_regione %in% c("P.A. Bolzano", "P.A. Trento"), "Trentino-Alto Adige",
                                          denominazione_regione),
           data = date(data)) %>% 
    gather(Key, Value, ricoverati_con_sintomi:casi_testati) %>% 
    group_by(Key, data, denominazione_regione) %>% 
    dplyr::summarise(Value = sum(Value)) %>% 
    ungroup() 
  
  dati_reg$Key <- factor(dati_reg$Key, levels = unique(dati_reg$Key), 
                         labels = c("Tested cases","Deceased", "Discharged recovered", "Home isolation", "New positives", 
                                    "Hospitalized with symptoms", 
                                    "Swabs", "Intensive care", "Cumulative positives", "Current hospitalized", "Current positives", 
                                    "Variation Current positives"))
  
  return(dati_reg)
}

# Read province data
read_province <- function(path){
  
  dati_prov <- read_csv(path) %>% 
    dplyr::select(-note_it, -note_en) %>%
    mutate(denominazione_regione = ifelse(denominazione_regione %in% c("P.A. Bolzano", "P.A. Trento"), "Trentino-Alto Adige", denominazione_regione),
           data = date(data)) %>% 
    drop_na(sigla_provincia) %>% 
    rename(`Cumulative positives` = totale_casi)
  
  
  
}

# Read italian data shapefile for map
read_shape_italy <- function(province = F){
  
  italy_sf <- read_rds("Data/gadm36_ITA_1_sp.rds") %>% 
    st_as_sf() %>% dplyr::select(NAME_1, geometry) %>% 
    mutate(NAME_1 = factor(NAME_1, labels = c("Abruzzo", "Puglia", "Basilicata", "Calabria", "Campania", "Emilia-Romagna", "Friuli Venezia Giulia",
                                              "Lazio" ,"Liguria" , "Lombardia", "Marche", "Molise", "Piemonte", "Sardegna", "Sicilia", "Toscana",
                                              "Trentino-Alto Adige", "Umbria", "Valle d'Aosta", "Veneto")))
  colnames(italy_sf)[1] <- "NAME" 
  
  if(province){
    
    italy_sf <- read_rds("Data/gadm36_ITA_2_sp.rds") %>% 
      st_as_sf() %>% dplyr::select(NAME_1, NAME_2, geometry)
    
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

# Add residents to map
add_residents_tomap <- function(d_sf, resdata){
  
  # Modify labels
  resdata[10,1] <- "Valle d'Aosta"
  resdata[30,1] <- "Trentino-Alto Adige"
  resdata[43,1] <- "Friuli Venezia Giulia"
  resdata[48,1] <- "Emilia-Romagna"
  
  resdata$Territorio[resdata$Territorio %in% c("AscoliPiceno", "ProvinciaAutonomaTrento", "MonzaedellaBrianza", "ReggiodiCalabria",
                            "ProvinciaAutonomaBolzano/Bozen", "ViboValentia", "LaSpezia", "ReggionellEmilia",
                            "Massa-Carrara")] <- c("La Spezia", "Monza e della Brianza", "Bolzano", "Trento", "Reggio nell'Emilia",
                                                   "Massa Carrara", "Ascoli Piceno", "Reggio di Calabria", "Vibo Valentia")
  resdata %<>% rename(residenti = totale)
  
  return(d_sf %>% sp::merge(resdata, by.y = "Territorio", by.x = 1))
  
}

# Not actually in use
print_current_situa <- function(da, TotPop){
  today <- max(da$data)
  dat_today <- da %>% dplyr::filter(data %in% c(today, today-1))
  
  attualmente_positivi_ultimo <- dat_today$`Current positives`[2]
  incr_attualmente_positivi <- paste_signpercent(dat_today$`Current positives`[2], dat_today$`Current positives`[1])
  
  deceduti_ultimo <- dat_today$Deceased[2]
  incr_deceduti <- paste_signpercent(dat_today$Deceased[2], dat_today$Deceased[1])
  
  dimessi_guariti_ultimo <- dat_today$`Discharged recovered`[2]
  incr_guariti <- paste_signpercent(dat_today$`Discharged recovered`[2], dat_today$`Discharged recovered`[1])
  
  totale_casi_ultimo <- dat_today$`Cumulative positives`[2]
  incr_totale_casi <- paste_signpercent(dat_today$`Cumulative positives`[2], dat_today$`Cumulative positives`[1])
  
  ricov_sintomi_ultimo <- dat_today$`Hospitalized with symptoms`[2]
  incr_ricoverati <- paste_signpercent(dat_today$`Hospitalized with symptoms`[2], dat_today$`Hospitalized with symptoms`[1])
  
  isol_domic_ultimo <- dat_today$`Home isolation`[2]
  incr_isolamento <- paste_signpercent(dat_today$`Home isolation`[2], dat_today$`Home isolation`[1])
  
  terapia_intens_ultimo <- dat_today$`Intensive care`[2]
  incr_terapia <- paste_signpercent(dat_today$`Intensive care`[2], dat_today$`Intensive care`[1])
  
  totale_ricoverati_ultimo <- dat_today$`Current hospitalized`[2]
  incr_currhosp <- paste_signpercent(dat_today$`Current hospitalized`[2], dat_today$`Current hospitalized`[1])
  
  
  quoz_let_ita <- round(dat_today$Deceased[2]/dat_today$`Cumulative positives`[2],4)*100
  
  tasso_mort_ita <- round(dat_today$Deceased[2]/TotPop,4)*100
  
  return(
    paste0(tags$h2(tags$strong("Italian current situation about the COVID-19 pandemic*")),
           h5("*most recent update: ", paste_eng_date(today)),
           "<ul>",
           "<li><b>Cumulative positives: </b>", totale_casi_ultimo, " (",incr_totale_casi,")","</li>",
           "<li><b>Current positives: </b>", attualmente_positivi_ultimo," (",incr_attualmente_positivi,")","</li>",
           "<li><b>Current hospitalized: </b>", totale_ricoverati_ultimo, " (",incr_currhosp,")","</li>",
           "<li><b>Intensive care units: </b>", terapia_intens_ultimo, " (",incr_terapia,")","</li>",
           "<li><b>Hospitalized with symptoms: </b>", ricov_sintomi_ultimo, " (",incr_ricoverati,")","</li>",
           "<li><b>Home isolation: </b>", isol_domic_ultimo, " (",incr_isolamento,")","</li>",
           "<li><b>Discharged recovered: </b>", dimessi_guariti_ultimo," (",incr_guariti,")","</li>",
           "<li><b>Deceased: </b>", deceduti_ultimo, " (",incr_deceduti,")","</li>",
           "<li><b>Death rate: </b>", tasso_mort_ita, "%","</li>",
           "<li><b>Fatality rate: </b>", quoz_let_ita, "%","</li>",
           "</ul>",
           collapse = ""
    )
  )
}


# Do donut plot
do_ciambella <- function(da, is.reg = F, reg, variable = "Cumulative positives"){
  
  colors <- c("firebrick", "darkorange", "salmon")
  
  if(variable == "Cumulative positives"){
    vartosel <- c("Deceased", "Current positives", "Discharged recovered")
  }
  if(variable == "Current positives"){
    vartosel <- c("Home isolation", "Hospitalized with symptoms", "Intensive care")
  }
  
  if(!is.reg){
    dperdonut <- da %>% 
      mutate(Key = as.character(Key)) %>% 
      group_by(data, Key) %>% 
      summarise(Value = sum(Value)) %>% 
      ungroup() %>% 
      filter(data == max(data), Key %in% vartosel) %>% 
      arrange(Key) %>% 
      mutate(percentage = round(Value/sum(Value),4)*100,
             lab.pos = cumsum(percentage)-.5*percentage) %>%
      mutate(stato = "Italy") %>% 
      rename(denominazione_regione = stato)
  }else{
    dperdonut <- da %>% 
      mutate(Key = as.character(Key)) %>% 
      filter(denominazione_regione == reg, data == max(data), 
             Key %in% vartosel) %>% arrange(Key) %>% 
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
    layout(title = paste0("Distribution of ", tolower(variable),  " in ", "<b>",unique(dperdonut$denominazione_regione), "</b>", " today"), 
           showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
}


# Do barplot TS
do_barplot_ts <- function(da, is.reg = F, reg, variable = "Cumulative positives"){
  
  if(variable == "Cumulative positives"){
    vartosel <- c("Deceased", "Current positives", "Discharged recovered")
  }
  if(variable == "Current positives"){
    vartosel <- c("Hospitalized with symptoms", "Home isolation", "Intensive care")
  }
  
  if(!is.reg){
    dperbar <- da %>%
      mutate(Key = as.character(Key)) %>% 
      group_by(data, Key) %>% 
      summarise(Value = sum(Value)) %>% 
      ungroup() %>% spread(Key, Value) %>% 
      mutate(stato = "Italy") %>% 
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
    plot_ly(x = ~data, y = ~Y1, type = "bar", name = vartosel[1], color = I("darkorange")) %>% 
    add_trace(y = ~Y2, name = vartosel[2], color = I("firebrick")) %>% 
    add_trace(y = ~Y3, name = vartosel[3], color = I("salmon")) %>% 
    layout(title = paste0("Distribution of ", tolower(variable), " over time","\n", "<b>",unique(dperbar$denominazione_regione), "</b>"),
           xaxis = list(title = ""), legend = list(x = 0, y = 1),
           yaxis = list(title = 'Counts'), barmode = 'stack')
  
  
}


# Do map
draw_map <- function(dajoined, reg = NULL, varsel = "Cumulative positives", datasel = today()-1, dajoinedprov, is.density = F){
  
  # annots <- list(x = 1, y = -0.1, text = as.character(datasel), 
  #                showarrow = F, xref='paper', yref='paper', 
  #                xanchor='right', yanchor='auto', xshift=0, yshift=0,
  #                font=list(size=15, color="black"))
  
  leg_lab <- as.character(varsel)
  
  add_people <- " people"
  
  if(is.density){
    dajoined$Value <- round(dajoined$Value/dajoined$residenti*100, 4)
    dajoinedprov$Value <- round(dajoinedprov$Value/dajoinedprov$residenti*100, 4)
    leg_lab <- paste(leg_lab, "(%)")
    add_people <- "% of the residents"
  }
  
  if(is.null(reg)){
    p <- plot_ly(data = dajoined %>% 
                   filter(Key == as.character(varsel), data == as.character(datasel)), 
                 stroke = I("black"),
                 split = ~NAME, color = ~Value, colors = "YlOrRd", alpha = 1, 
                 text = ~paste0(NAME, "\n", Value, add_people),
                 hoveron = "fills",
                 hoverinfo = "text", 
                 showlegend = F) %>%
      layout(title = HTML(paste0("<b>",as.character(datasel),"</b>"))) %>% 
      colorbar( title = list(text = paste0("<b>", leg_lab, "</b>")), len = 0.6)
  }else{
    
    if(varsel == "Cumulative positives"){
      p <- plot_ly(data = dajoinedprov %>% 
                     filter(Key == as.character(varsel), data == as.character(datasel), NAME %in% reg), 
                   stroke = I("black"),
                   split = ~NAME_2, color = ~Value, colors = "YlOrRd", alpha = 1, 
                   text = ~paste0(NAME_2," (",NAME, ")","\n", Value, add_people),
                   hoveron = "fills",
                   hoverinfo = "text", showlegend = F) %>% 
        layout(title = HTML(paste0("<b>",as.character(datasel),"</b>")))  %>% 
        colorbar( title = list(text = paste0("<b>", leg_lab, "</b>")), len = 0.6)
    }else{
      p <- plot_ly(data = dajoined %>% 
                     filter(Key == as.character(varsel), data == as.character(datasel), NAME %in% reg), 
                   stroke = I("black"),
                   split = ~NAME, color = ~Value, colors = "YlOrRd", alpha = 1, 
                   text = ~paste0(NAME, "\n", Value, add_people),
                   hoveron = "fills",
                   hoverinfo = "text", showlegend = F) %>% 
        layout(title = HTML(paste0("<b>",as.character(datasel),"</b>")))  %>% 
        colorbar( title = list(text = paste0("<b>", leg_lab, "</b>")), len = 0.6)
      if(length(reg) == 1){
        p <- p %>% hide_colorbar()
      }
    }
    
  }
  
  # p %<>% layout(images = list(list(source = "www/StatGroup19Pic.jpg",xref = "x",
  #                                  yref = "y",
  #                                  x = 0,
  #                                  y = 1,
  #                                  sizex = 0.2,
  #                                  sizey = 0.2,
  #                                  opacity = 0.4,
  #                                  layer = "below"))) 
  return(p)
  
  
}


# Do Time series
do_ts <- function(da, is.reg = F, reg, varsel, datasel, is.incrementi=T, lag_incr = "daily variations", tipo.incremento = "Absolute"){
  
  if(!is.reg){
    
    dts <- da %>% 
      mutate(Key = as.character(Key)) %>% 
      group_by(data, Key) %>% 
      summarise(Value = sum(Value)) %>% 
      ungroup() %>% 
      filter(data <= as.character(datasel), Key == varsel) %>% 
      arrange(Key) %>% 
      mutate(denominazione_regione = "Italy") %>% drop_na()
    
  }else{
    
    dts <- da %>% 
      filter(data <= as.character(datasel), Key == varsel, denominazione_regione %in% reg) %>% 
      dplyr::select(data, denominazione_regione, Key, Value) %>% drop_na()
    
  }
  
  ylabel <- "Counts"
  
  if(is.incrementi){
    dts %<>% 
      arrange(denominazione_regione) %>% 
      group_split(denominazione_regione) 
    
    if(lag_incr == "daily variations"){
      dts %<>%  
        map_dfr(function(x) {
          x %<>% 
            mutate(IncrAss = c(0, diff(Value)),
                   IncrPerc = round((c(0, diff(Value))/Value)*100,4)) %>% 
            ungroup() 
        })
    }
    if(lag_incr == "weekly variations" & varsel %in% c("Cumulative positives", "Discharged recovered", "Deceased")){
      dts %<>%  
        map_dfr(function(x){
          x %<>% mutate(week = week(data)) %>% 
        group_by(week) %>% 
        summarise(data = last(data), Key = last(Key), Value = last(Value),
                   denominazione_regione = last(denominazione_regione)) %>% 
        mutate(IncrAss = c(0, diff(Value)),
               IncrPerc = round((c(0, diff(Value))/Value)*100,4)) %>% 
        ungroup() 
        })
    }
  
    ylabel <- ifelse(tipo.incremento == "Absolute", "Absolute variations",
                     "Relative (%) variations") 
    
    if(tipo.incremento == "Absolute"){
      dts <- dts %>% dplyr::select(data, denominazione_regione, Key, IncrAss) %>% rename(Value = IncrAss)
    } else {
      dts <- dts %>% dplyr::select(data, denominazione_regione, Key, IncrPerc) %>% rename(Value = IncrPerc) %>% 
        filter(data > min(data))
    }
    
    
  }
  colors <- c("firebrick", "red", "orange3", "orange","gold")[1:length(reg)]
  
  dts %>% 
    plot_ly(x = ~data, y = ~Value, type = 'scatter', mode = 'lines+markers', 
            color = ~denominazione_regione, colors = colors) %>% 
    layout(xaxis = list(title = ""), yaxis = list(title = ylabel), title = HTML(paste0("Time series of <b>",tolower(varsel),"</b>")),
           legend = list(orientation = "h"))
  
}


# Print sign header
paste_signpercent <- function(x_oggi,x_ieri){
  simbolo <- ifelse(x_oggi-x_ieri>0, "+", "")
  incr <- ((x_oggi-x_ieri)/x_ieri)*100
  return(paste0(simbolo, round(incr, 2),"% w.r.t. yesterday"))
} 


# Prepare data for model regional level
prepdata_for_model <- function(dftoprep, resdata){
  ag <- dftoprep %>% 
    mutate(denominazione_regione = ifelse(denominazione_regione == "Trentino-Alto Adige", "TrentinoAltoAdige", 
                                          ifelse(denominazione_regione == "Friuli Venezia Giulia", "Friuli V. G.",
                                                 ifelse(denominazione_regione == "Emilia-Romagna", "Emilia Romagna",
                                                        denominazione_regione)))) %>% 
    arrange(denominazione_regione, data)
  
  # Label
  resdata[10,1] <- "Valle d'Aosta"
  
  da <- ag %>% 
    left_join(resdata, by = c("denominazione_regione" = "Territorio")) %>% 
    mutate(ti = as.numeric(unclass(factor(data))), denominazione_regione = factor(denominazione_regione)) %>% 
    rename(ti_orig = data, region = denominazione_regione, residents = totale) 
  
  return(da)
}

# Run the model
run_growth_model <- function(da, reg=NULL, wh="Cumulative positives", horizon = 10, fam="Poisson", reduce_obs) {
  
  ti_orig_out <- da$ti_orig %>% unique()
  
  if(is.null(reg)){
    dat <- aggregate(da %>% dplyr::select(-region, -ti, -residents, -ti_orig), list(da$ti), sum)
    colnames(dat)[1] <- "ti"
    
  }else{
    dat <- da[da$region==reg,]
  }
  
  # Pick the variable to model
  whidx <- which(colnames(dat)==wh)
  pc_all <- dat[, whidx, drop = T]
  pc <- dat[ti_orig_out<=reduce_obs, whidx, drop = T]
  ti <- dat$ti[ti_orig_out<=reduce_obs]
  
  
  if(any(pc==0)) {
    w <- min(which(pc!=0))
    pc_all <- pc_all[-c(1:w)]
    pc <- pc[-c(1:w)]
    ti <- ti[-c(1:w)]
    ti <- ti-min(ti)+1
    ti_orig_out <- dat$ti_orig[-c(1:w)]
  }
  
  mti <- max(ti)
  timax <- mti+horizon
  # 
  diffpc <- diff(pc)
  lastidx <- (length(pc)-19):length(pc) 
  beta1 <- coef(lm(pc[lastidx]~ti[lastidx]))[2]
  
  mnt <- as.logical(ifelse(beta1<0, F, T))
  if(!is.null(reg) & wh == "New positives"){
    if(reg == "Molise") mnt <- FALSE
  } 
  rrs <- 500
  
  np <- tryCatch(
    growthGLM(count = pc, ti = ti, monotone = mnt, tk = NA, family = fam, maxiter = 2000, runs = rrs, tPred = timax, nBoot = 10000,
              #,nmirror=8
              ),
    #warning = function(w){print("Something happened")},
    error = function(e){return("Error")}
  )
  
    
  # Prediction
  y <- cbind(np$low, np$linPred, np$up)
  colnames(y) <- c("ly", "y", "uy")
  # ydiff <- cbind(np$lowdiff, diff(np$linPred), np$updiff)
  # colnames(ydiff) <- c("ly", "y", "uy")
  #
  x <- seq(min(ti_orig_out), max(ti_orig_out[ti_orig_out<=reduce_obs]) + horizon, 1)
  x1 <- c(ti_orig_out[2:mti], rep(NA, horizon))
  tall <- c(ti_orig_out, rep(NA, nrow(y)-length(pc_all)))
  #pc_out <- c(diff(pc), rep(NA, horizon))
  
  # Data frame observed and predicted variation
  #cc1<-data.frame(x1 = x1, pc = pc_out, x = x[-1], ydiff)
  
  # Data frame observed and predicted total
  cc<-data.frame(x1 = c(x1[1] - 1, x1), pc = c(pc,rep(NA,horizon)), 
                 pc_all = c(pc_all,rep(NA,nrow(y)-length(pc_all))), t_all = tall,
                 x = x, y)
  
  ooo <- list(cc=cc, R2 = list(round(np$R2, 4)), pars = list(np$pars), stderrs = list(np$se), monot = mnt, 
              fam = fam, NoConv = np$NoConv, BandsError = np$BandsError)
    
  
  return(ooo)
}


# Plot result model (return plot) new
plot_out_model <- function(outputmod, horizon = 15, what = "Cumulati", VarModel = "Cumulative positives", showbands = T, reg = "Italy"){
  
  ses <- outputmod$stderrs[[1]]
  
  if(what == "Cumulati"){
    dd <- outputmod$cc #%>% 
      # mutate(uy = ifelse(x<=max(x1, na.rm = T), NA, uy),
      #        ly = ifelse(x<=max(x1, na.rm = T), NA, ly))
    ylabel <- "Daily counts"
    if(VarModel %in% c("Deceased", "Discharged recovered", "Cumulative positives")){
      ylabel <- "Cumulative counts"
    }
  }
  # if(what == "Nuovi"){
  #   dd <- outputmod$cc1 #%>% 
  #     # mutate(uy = ifelse(x<=max(x1, na.rm = T), NA, uy),
  #     #        ly = ifelse(x<=max(x1, na.rm = T), NA, ly))
  #   ylabel <- "Daily variations"
  #   if(VarModel %in% c("Deceased", "Discharged recovered", "Cumulative positives")){
  #     ylabel <- "Daily counts"
  #   }
  # }
  
  namemodel <- ifelse(outputmod$monot, "Richards", "Mirrored Richards")
  idx <- min(which(is.na(dd$pc_all)))-1
  pp <- plot_ly(dd,
                x=~t_all, y = ~pc_all, name = 'Observed', color = I("black"),
                type = 'scatter', mode = 'lines+markers', alpha = 0.6) %>%
    add_trace(data = dd %>% filter(x <= max(x1, na.rm = T) + horizon),
              x=~x, y = ~y, name = namemodel, type = "scatter",
              mode = 'lines', alpha = 1,
              line = list(width = 3, color = "red")) %>%
    layout(title = paste0("<b align='center'>Fitted and observed values for ","\n", VarModel, " - ",reg, "</b>"), 
           xaxis = list(title = ""), yaxis = list(title = ylabel), legend = list(x = 0, y = 1))
  
  if(!outputmod$NoConv){
    if(sum(is.na(ses)|is.nan(ses)) == 0){
      pp <- plot_ly(data = dd %>% filter(x <= max(x1, na.rm = T) + horizon),
                    x=~x, y = ~y, type = "scatter", mode = 'lines',
                    line = list(width = 3, color = "red"), name = namemodel) %>%
        add_ribbons(data = dd %>% filter(x <= max(x1, na.rm = T) + horizon),
                    ymin = ~ly, ymax = ~uy, name = '99% Conf. Int.', color = I('rgba(250,128,114, 0.8)'),
                    line = list(width = 0)) %>% 
        add_trace(data = dd, x=~t_all, y = ~pc_all,  name = "Observed", alpha = 0.7, mode = 'lines+markers', 
                  line = list(color = "black", width=0.5), marker = list(color = "black")) %>%
        layout(title = paste0("<b align='center'>Fitted and observed values for ","\n", VarModel, " - ",reg,"</b>"), 
               xaxis = list(title = ""), yaxis = list(title = ylabel), legend = list(x = 0, y = 1))
    }
  }  else{
    if(sum(is.na(ses)|is.nan(ses)) == 0 & showbands){
      pp <- plot_ly(dd %>% filter(x <= max(x1, na.rm = T) + horizon), 
                   x=~x, y = ~y, type = "scatter", mode = 'lines',
                   line = list(width = 3, color = "red"), name = namemodel) %>%
        add_ribbons(ymin = ~ly, ymax = ~uy, name = '99% Conf. Int.', color = I('rgba(250,128,114, 0.8)'),
                    line = list(width = 0)) %>% 
        add_trace(data = dd, x=~t_all, y = ~pc_all,  name = "Observed", alpha = 0.7, mode = 'lines+markers', 
                  line = list(color = "black", width=0.5), marker = list(color = "black")) %>%
        layout(title = paste0("<b align='center'>Fitted and observed values for ","\n", VarModel, " - ", reg, "</b>"), 
               xaxis = list(title = ""), yaxis = list(title = ylabel), legend = list(x = 0, y = 1))
    } 
  }
  
  
  return(pp)
  
}



DT_out_model <- function(outputmod, horizon = 15, VarModel = "Cumulative positives", showbands = F, reg = "Italy") {
  
  ses <- outputmod$stderrs[[1]]
  
  capts_out <-  htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: left;',
    'Table: ', htmltools::em(paste0('Predictions for ', tolower(VarModel), ' - ', reg,'.'))
  )
  
  
  ddt <- outputmod$cc %>% dplyr::select(t_all, pc_all, x, y) %>%
                     set_colnames(value = c("Date", "Observed", "Data2", "Predicted")) %>%
    filter(between(Data2, max(Date, na.rm = T) -2, max(Date, na.rm = T) + horizon)) %>%
    dplyr::select(-Date) %>%
    mutate_if(is.numeric, round) %>% 
    rename(Date = Data2) %>% 
    dplyr::select(Date, Observed, Predicted)
  
  if(!outputmod$NoConv){
    if(sum(is.na(ses)|is.nan(ses)) == 0){
      
      ddt <- bind_cols(ddt, outputmod$cc %>% 
                         filter(x %in% ddt$Date) %>% 
                         dplyr::select(ly, uy) %>% 
                         set_colnames(value = c("Min.", "Max."))) %>%
        mutate_if(is.numeric, round) %>% 
        dplyr::select(Date, Observed, Predicted, `Min.`, `Max.`)
    }
    
  } else{
    if(sum(is.na(ses)|is.nan(ses)) == 0 & showbands){
      
      ddt <- bind_cols(ddt, outputmod$cc %>% 
                         filter(x %in% ddt$Date) %>% 
                         dplyr::select(ly, uy) %>% 
                         set_colnames(value = c("Min.", "Max."))) %>%
        mutate_if(is.numeric, round) %>% 
        dplyr::select(Date, Observed, Predicted, `Min.`, `Max.`)
    }
  }
  
  
  
  ddtt <- datatable(ddt, rownames = FALSE, class = 'cell-border stripe',
                    options = list(dom = 'tp', pageLength = 5, scrollX = T, columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                    #container = sketch, 
                    caption = capts_out)
  
  
  return(ddtt)
  
}

# print summary model (return html code)
summary_out_model <- function(outputmod, VarModel = "Cumulative positives", resdata, is.reg = F, reg = NULL){
  
  famout <- outputmod$fam
  
  resdata[10,1]="Valle d'Aosta"
  
  if(is.reg & !is.null(reg)) mx_sel <- resdata[resdata$Territorio == reg, 2]
  if(!is.reg) mx_sel <- 6*10^7
  
  # omod <- outputmod$cc1
  # 
  ti_orig_out <- na.omit(outputmod$cc$t_all)
  # ti <- as.numeric(unclass(factor(ti_orig_out)))
  # 
  est_pick <- ti_orig_out[which.max(c(outputmod$cc$pc_all[1],diff(outputmod$cc$y)))]
  # estmax_val_l <- round(omod$ly[which.max(omod$y)])
  # estmax_val <- round(max(omod$y))
  # estmax_val_u <- round(omod$uy[which.max(omod$y)])
  # obsmax_val <- omod$pc[which.max(omod$y)]
  
  
  # 
  tasso_come <- ifelse(outputmod$monot, "Richards", "Mirrored Richards")
  
  # 
  pdate <- paste_eng_date(est_pick)
 
  coverage <- ((sum((outputmod$cc$ly <= outputmod$cc$pc) & (outputmod$cc$uy >= outputmod$cc$pc), na.rm = T))/sum(!is.na(outputmod$cc$pc))) %>% round(2)
    
  # Asintoto cumulati
  asi_est_l <- exp(outputmod$pars[[1]][1]-1.96*outputmod$stderrs[[1]][1])
  errs_l <- (outputmod$pars[[1]][length(outputmod$pars[[1]])]-1.96*outputmod$stderrs[[1]][length(outputmod$pars[[1]])]) %>% exp
  #errs <- outputmod$pars[[1]][length(outputmod$pars[[1]])] %>% exp()
  asi_est_l <- (asi_est_l-1.96*ifelse(famout == "Poisson",sqrt(asi_est_l),sqrt(asi_est_l + (asi_est_l^2)/errs_l))) %>% round
  idx <- min(which(is.na(outputmod$cc$pc)))-1
  cond <- asi_est_l < outputmod$cc$pc[idx]
  asi_est_l <- ifelse(cond, outputmod$cc$pc[idx], asi_est_l)
  
  
  asi_est_u <- exp(outputmod$pars[[1]][1]+1.96*outputmod$stderrs[[1]][1])
  asi_est_u <- (asi_est_u+1.96*ifelse(famout == "Poisson",sqrt(asi_est_u),sqrt(asi_est_u + (asi_est_u^2)/errs_l))) %>% round
  asi_est_u <- ifelse(asi_est_u > mx_sel, mx_sel, asi_est_u)
  asi_est <- ifelse(cond, outputmod$cc$pc[idx], round(exp(outputmod$pars[[1]][1])))
  
  
  if(VarModel %in% c("Deceased", "Discharged recovered", "Cumulative positives") & !(is.na(outputmod$stderrs[[1]][1])) & (asi_est_u>=asi_est_l)){
    
    
    out_string <- paste0(
      paste0('<h3 align = "center"><b>', famout, " - ", tasso_come, ': </b></h3>'),
      "<br/>",#'<ul>',
      '<p align="center"><b>Estimated peak of daily variations: </b></p>', '<p align="center">',pdate, "</p>", "<br/>",
      '<p align="center"><b>Estimated upper asymptote of cumulative cases: </b></p>', 
      "<p align='center'>", asi_est, paste0(" (not less than ", asi_est_l, ", no more than ", asi_est_u, ")"),"</p>", "<br/>",
      '<p align="center"><b>Goodness of fit: </b></p>', '<p align="center">', outputmod$R2[[1]], '</p>',"<br/>",
      '<p align="center"><b>Coverage: </b></p>', '<p align="center">', coverage*100,"%", "</p>",
      collapse = ""
    )
  } else{
    out_string <- paste0(
      paste0('<h3 align="center"><b>', famout, " - ", tasso_come, ': </b></h3>'),
      "<br/>",
      '<p align="center"><b>Estimated peak of daily variations: </b></p>', '<p align="center">', pdate, "</p><br/>",
      '<p align="center"><b>Goodness of fit:</b></p>', '<p align="center">', outputmod$R2[[1]],"</p><br/>",
      '<p align="center"><b>Coverage: </b></p>', "<p align='center'>", coverage*100,"%", "</p>",
      collapse = ""
    )
    
  }
  
  return(HTML(out_string))
}


# Return current situa
return_current_situa <- function(da, TotPop){
  today <- max(da$data)
  dat_today <- da %>% dplyr::filter(data %in% c(today, today-1))
  
  attualmente_positivi_ultimo <- dat_today$`Current positives`[2]
  incr_attualmente_positivi <- paste_signpercent(dat_today$`Current positives`[2], dat_today$`Current positives`[1])
  
  deceduti_ultimo <- dat_today$Deceased[2]
  incr_deceduti <- paste_signpercent(dat_today$Deceased[2], dat_today$Deceased[1])
  
  dimessi_guariti_ultimo <- dat_today$`Discharged recovered`[2]
  incr_guariti <- paste_signpercent(dat_today$`Discharged recovered`[2], dat_today$`Discharged recovered`[1])
  
  totale_casi_ultimo <- dat_today$`Cumulative positives`[2]
  incr_totale_casi <- paste_signpercent(dat_today$`Cumulative positives`[2], dat_today$`Cumulative positives`[1])
  
  ricov_sintomi_ultimo <- dat_today$`Hospitalized with symptoms`[2]
  incr_ricoverati <- paste_signpercent(dat_today$`Hospitalized with symptoms`[2], dat_today$`Hospitalized with symptoms`[1])
  
  isol_domic_ultimo <- dat_today$`Home isolation`[2]
  incr_isolamento <- paste_signpercent(dat_today$`Home isolation`[2], dat_today$`Home isolation`[1])
  
  terapia_intens_ultimo <- dat_today$`Intensive care`[2]
  incr_terapia <- paste_signpercent(dat_today$`Intensive care`[2], dat_today$`Intensive care`[1])
  
  totale_ricoverati_ultimo <- dat_today$`Current hospitalized`[2]
  incr_currhosp <- paste_signpercent(dat_today$`Current hospitalized`[2], dat_today$`Current hospitalized`[1])
  
  swabs_ultimo <- dat_today$Swabs[2]
  incr_swabs <- paste_signpercent(dat_today$Swabs[2], dat_today$Swabs[1])
  
  new_pos_ultimo <- dat_today$`New positives`[2]
  incr_newpos <- paste_signpercent(dat_today$`New positives`[2], dat_today$`New positives`[1])
  
  tested_ultimo <- dat_today$`Tested cases`[2]
  incr_tests <- paste_signpercent(dat_today$`Tested cases`[2], dat_today$`Tested cases`[1])
  
  quoz_let_ita <- round(dat_today$Deceased[2]/dat_today$`Cumulative positives`[2],4)*100
  
  tasso_mort_ita <- round(dat_today$Deceased[2]/TotPop,4)*100
  
  list_out <- list("Cumulative positives" = c(totale_casi_ultimo, incr_totale_casi),
                   "Current positives" = c(attualmente_positivi_ultimo, incr_attualmente_positivi),
                   "Current hospitalized" = c(totale_ricoverati_ultimo, incr_currhosp),
                   "Intensive care" = c(terapia_intens_ultimo, incr_terapia),
                   "Hospitalized with symptoms" = c(ricov_sintomi_ultimo, incr_ricoverati), 
                   "Home isolation" = c(isol_domic_ultimo, incr_isolamento),
                   "New positives" = c(new_pos_ultimo, incr_newpos),
                   "Discharged recovered" = c(dimessi_guariti_ultimo, incr_guariti),
                   "Deceased" = c(deceduti_ultimo, incr_deceduti),
                   "Swabs" = c(swabs_ultimo, incr_swabs),
                   "Tested cases" = c(tested_ultimo, incr_tests),
                   "Letalita" = quoz_let_ita, 
                   "Mortalita" = tasso_mort_ita)
  return(
   list_out
  )
}


# Show raw data
show_raw_data <- function(daita, dareg){
  Dt_out_raw <- daita %>% 
    rename(Date = data, Region = stato) %>% 
    mutate(Region = "Italy",
           `New deaths` = c(Deceased %>% first, diff(Deceased))) %>% 
    dplyr::select(Date, Region, `Cumulative positives`, `New positives`, `Deceased`, `New deaths`, 
                  `Current positives`, `Current hospitalized`, `Hospitalized with symptoms`, 
                  `Intensive care`, `Home isolation`, `Discharged recovered`, Swabs) %>% 
    bind_rows(dareg %>% 
                spread(Key, Value) %>% 
                rename(Date = data, Region = denominazione_regione) %>% 
                group_by(Region) %>% 
                mutate(`New deaths` = c(Deceased %>% first, diff(Deceased))) %>% 
                ungroup() %>% 
                dplyr::select(Date, Region, `Cumulative positives`, `New positives`, `Deceased`, `New deaths`, 
                              `Current positives`, `Current hospitalized`, `Hospitalized with symptoms`, 
                              `Intensive care`, `Home isolation`, `Discharged recovered`, Swabs)
              
    ) 
  
  
  Dt_out_raw$Region <- factor(Dt_out_raw$Region,
                               levels = c("Lombardia", "Liguria", "Piemonte", "Valle d'Aosta",
                                          "Emilia-Romagna", "Friuli Venezia Giulia", "Veneto", "Trentino-Alto Adige",
                                          "Lazio", "Marche", "Toscana", "Umbria",
                                          "Abruzzo", "Basilicata", "Calabria",
                                          "Campania", "Molise", "Puglia","Sardegna", "Sicilia"))
  
  Dt_out_raw %<>% arrange(desc(Date), Region) 
  
  return(Dt_out_raw)
}

table_raw_data <- function(tab, datesel, resdata){
  
  Capienza <- tibble(Region = sort(as.character(unique(tab$Region))), 
                         `Capacity ICU` = c(115,49,107,(506+80),(539+90),127,(557+118),186,(1200+208),(154+39),31,560,289,123,392,(394+70),(115+42),70,45,(600+338)))
  resdata[c(10, 30, 43, 48),1] <- c("Valle d'Aosta", "Trentino-Alto Adige", "Friuli Venezia Giulia", "Emilia-Romagna")
  
  raw_app <- tab %>% group_by(`Region`) %>% 
    arrange(Date) %>% 
    mutate(Incrpos = c(`New positives`[1], diff(`New positives`)),
           Incrdeaths = c(`New deaths`[1], diff(`New deaths`))) %>% 
    ungroup() %>% 
    arrange(desc(Date)) %>% 
    dplyr::select(-`Current hospitalized`, -`Hospitalized with symptoms`, -`Home isolation`, -`Discharged recovered`, -Swabs) %>% 
    filter(`Region`!="Italy") %>% 
    left_join(Capienza, by = "Region") %>% 
    left_join(resdata, by = c("Region" = "Territorio")) %>% rename(Residents = totale) %>% 
    mutate(`Cum. Pos. (x 1000 residents)` = round((`Cumulative positives`/Residents)*1000,2),
           `Deceas. (x 1000 residents)` = round((`Deceased`/Residents)*1000,2),
           `Curr. Pos. (x 1000 residents)` = round((`Current positives`/Residents)*1000,2),
           `ICU/capacity (%)` = round((`Intensive care`/`Capacity ICU`)*100,2))
  
  
  # if(is.density){
  #   raw_app[,3:8] <- round(raw_app[,3:8]/raw_app$totale*100,3)
  # } 
  dt_out <- raw_app %>% 
    filter(Date == datesel) %>% 
    dplyr::select(-Date) %>% 
    datatable(class = "cell-border stripe", 
              caption = "The yellow and grey cells reflect a decrease in the number of new positives and new deaths w.r.t. the previous day; viceversa, the red and black ones.",
              rownames = F, filter = "top", extensions = 'ColReorder',
              options = list(
                colReorder = TRUE,
                initComplete = JS(
                  "function(settings, json) {",
                  "$('body').css({'font-family': 'Calibri'});",
                  "}"
                ), pageLength = 20,
                dom = 'tp', columnDefs = list(
                  list(className = 'dt-center', targets = "_all"),
                  list(visible = F, targets = c(7, 8,9,10))
                ))) %>% 
    formatStyle("Region", fontWeight = "bold", fontSize = "120%", target = "row") %>% 
    formatStyle(columns = "New positives",  valueColumns = "Incrpos",
                backgroundColor = styleInterval(0, c("lemonchiffon", "tomato")), color = "black") %>% 
    formatStyle(columns = "New deaths", valueColumns = "Incrdeaths",
                backgroundColor = styleInterval(0, c("grey", "black")), color = "white") %>% 
    formatCurrency(columns = colnames(raw_app)[3:8],
                   currency = "", interval = 3, mark = ",", digits = 0)
  
  return(dt_out)
}

# Decrees list
get_decrees <- function() {
  decrees <- list(
    list(
      x = as.Date("2020-01-30"),
      name = 'Ministry of Health Ordinance 30th January 2020 <img src="checkMark.png" alt="Check mark" height="42" width="42">',
      label = "Ministry of Health Ordinance",
      description = "Preventive measures against new Coronavirus (2019 - nCoV)",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/02/01/20A00738/sg",
      status = "active"
    ),
    list(
      x = as.Date("2020-01-31"),
      name = "Resolution of the Council of Ministers 31st January 2020",
      label = "Resolution of the Council of Ministers",
      description = "Declaration of the emergency state as a result of the health risk 
associated with the outbreak of diseases caused by transmissible viral agents.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/02/01/20A00737/sg",
      status = "active"
    ),
    list(
      x = as.Date("2020-02-23"),
      name = "Prime Minister Decree (DPCM) 23rd February 2020",
      label = "Prime Minister Decree (DPCM)",
      description = "Urgent measures for containment and management of the epidemiological emergency caused by COVID-2019 .",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/02/23/20A01228/sg",
      status = "active"
    ),
    list(
      x = as.Date("2020-02-25"),
      name = " Prime Minister Decree (DPCM) 25th February 2020",
      label = "Prime Minister Decree (DPCM)",
      description = "New measures concerning the organization of sport events, the organization of school activities and
higher education, health prevention in prisons, the regulation of access to driving tests, 
the organization of cultural activities and tourism.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/02/25/20A01278/sg",
      status = "active"
    ),
    list(
      x = as.Date("2020-02-28"),
      name = "Decree-Law (Dl) 28th February 2020",
      label = "Decree-Law",
      description = "Measures for the improvement of the National Health Service (SSN), civil protection and security, 
as well as support for public and private work and for families and businesses; provisions in the areas of justice, 
transport, agriculture and sport, entertainment and culture, schools and universities; suspension of payment obligations
for taxes and contributions, other tax obligations and tax incentives.",
      link = "http://www.governo.it/node/14225",
      status = "active"
    ),
    list(
      x = as.Date("2020-03-01"),
      name = " Prime Minister Decree (DPCM) 1st March 2020",
      label = "Prime Minister Decree (DPCM)",
      description = "Further measures aimed at uniformly regulating the framework of interventions and 
throughout the country in the implementation of prophylaxis programmes.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/03/01/20A01381/sg",
      status = "inactive"
    ),
    list(
      x = as.Date("2020-03-02"),
      name = " Prime Minister Decree (DPCM) 2nd March 2020",
      label = "Prime Minister Decree (DPCM)",
      description = " Emergency support measures for families, workers and enterprises related to the 
epidemiological emergency caused by COVID-19.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/03/02/20G00026/sg",
      status = "active"
    ),
    list(
      x = as.Date("2020-03-04"),
      name = " Prime Minister Decree (DPCM) 4th March 2020",
      label = "Prime Minister Decree (DPCM)",
      description = "Further implementing provisions of Decree-Law No 6 of February 23rd 2020 on emergency measures
for the containment and management of the COVID-19 epidemiological emergency, covering the whole country.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/03/01/20A01381/sg",
      status = "inactive"
    ),
    list(
      x = as.Date("2020-03-08"),
      name = " Prime Minister Decree (DPCM) 8th March 2020",
      label = "Prime Minister Decree (DPCM)",
      description = "With reference to the DPCM issued on 8th March 2020, the Interior Minister has issued 
Directive no. 14606 of 08/03/2020 intended for the Prefects in order to implement controls 
in areas with reinforced containment.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/03/08/20A01522/sg",
      status = "active"
    ),
    list(
      x = as.Date("2020-03-09"),
      name = "Decree-Law (Dl) 9th March 2020, n. 14",
      label = "Decree-Law",
      description = " Urgent measures to strengthen the National Health Service in relation to the COVID-19 emergency.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/03/09/20G00030/sg",
      status = "active"
    ),
    list(
      x = as.Date("2020-03-09"),
      name = " Prime Minister Decree (DPCM) 9th March 2020",
      label = "Prime Minister Decree (DPCM)",
      description = "The measures referred to in art. 1 of the Dpcm 8th March 2020 have been extended to the entire national territory.
Furthermore, any form of gathering of people in public places or places open to the public has been prohibited.
Finally, the letter d of art.1 of the Dpcm 8th March 2020 relating to sporting events and manifestations is amended.
These provisions take effect from 10th March 2020 and are effective until 3rd April 2020.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/03/09/20A01558/sg",
      status = "active"
    ),
    list(
      x = as.Date("2020-03-11"),
      name = " Prime Minister Decree (DPCM) 11th Marzo 2020",
      label = "Prime Minister Decree (DPCM)",
      description = "Closure of all commercial and retail activities, with the exception of grocery stores, basic necessities, 
pharmacies and parapharmacies. The provisions shall take effect from 12 March 2020 and shall be effective until 25th March 2020.
With the entry into force of the decree, the measures set out in the Dpcm 8th March 2020 and
Dpcm 9th March 2020 will cease to have effect, where incompatible.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/03/11/20A01605/sg",
      status = "active"
    ),
    list(
      x = as.Date("2020-03-17"),
      name = "Decree-Law (Dl) 17th March 2020 n. 18 #CuraItalia",
      label = "Decree-Law",
      description = " Measures with the aim of strengthening the National Health Service (SSN) and
providing economic support for families, workers and businesses.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/03/17/20G00034/sg",
      status = "active"
    ),
    list(
      x = as.Date("2020-03-20"),
      name = "'Ministry of Health Ordinance 20th March 2020",
      label = "Ministry of Health Ordinance",
      description = " New restrictions throughout Italy, valid until 25th March. In particular, closure of parks and public gardens.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/03/20/20A01797/sg",
      status = "active"
    ),
    list(
      x = as.Date("2020-03-22"),
      name = " Prime Minister Decree (DPCM) 22nd Marzo 2020",
      label = "Prime Minister Decree (DPCM)",
      description = " The regulation provides for the closure of non-essential or strategic production activities, 
food stores, pharmacies, basic necessities shops and essential services shall remain open.
In addition, the decree forbids all individuals to relocate or move by public or private means of transportation in towns 
other than where they are located, except for proven work needs, absolute urgency or for health reasons.
The provisions are effective from 23rd March 2020 and shall be effective until 3 April 2020.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/03/22/20A01807/sg",
      status = "active",
      flag = TRUE
    ),
    list(
      x = as.Date("2020-03-25"),
      name = "Minister for the Economic Development Decree 25th March 2020",
      label = "Minister for the Economic Development Decree",
      description = " Companies which had not been suspended by the Prime Ministerial Decree of March 22nd, 2020 and which, 
as a result of this decree, will have to suspend their operations are allowed to complete the activities 
required for the suspension, including the shipment of goods in stock, until March 28th, 2020.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/03/26/20A01877/sg",
      status = "active",
      flag = FALSE
    ),
    list(
      x = as.Date("2020-03-25"),
      name = " Decree-law (Dl) 25th March 2020, n. 19",
      label = "Decree-Law",
      description = " The decree provides that, in order to contain and avoid health risks and the spread of contagion, 
one or more of the measures set out in the decree may be adopted, on specific areas of the national territory or on all of it, 
for predetermined periods, each for a period not exceeding thirty days, which may be repeated and amended several times 
until the end of the state of emergency, set at 31st July 2020 by the resolution passed by the Council of Ministers 
on 31st January 2020. The implementation of the measures may be modulated upwards or downwards 
according to the epidemiological trend of the aforementioned virus, one or more of the measures provided by the decree itself,
according to criteria of specific adequacy and principles of proportionality to the risk actually present.", 
      link = "https://www.gazzettaufficiale.it/eli/id/2020/03/25/20G00035/sg",
      status = "active",
      flag = FALSE
    ),
    list(
      x = as.Date("2020-04-01"),
      name = " Prime Minister Decree (DPCM) 1st April 2020",
      label = "Prime Minister Decree (DPCM)",
      description = "The decree extends to the 13th of April 2020 the effectiveness of the provisions of the Dpcm 
of the 8th, 9th, 11th and 22nd of March 2020, as well as those provided for by the Ordinance of the Minister of Health 
of the 20th of March 2020 and the Ordinance of the 28th of March 2020 adopted by the Minister of Health in agreement 
with the Minister of Infrastructures and Transportation still effective on the 3rd of April.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/04/02/20A01976/sg",
      status = "active",
      flag = FALSE
    ),
    list(
      x = as.Date("2020-04-10"),
      name = " Prime Minister Decree (DPCM)  10th April 2020",
      label = "Prime Minister Decree (DPCM)",
      description = "The decree establishes the creation of a Committee of experts in economic and social matters. 
The Committee, led by Vittorio Colao and composed of experts in economic and social matters, will have the task, 
in agreement with the Technical and Scientific Committee, of developing the necessary measures for a gradual recovery
in the various sectors of social, economic and productive activities, also through the identification of 
new organizational and relational models, taking into account the needs of containment and emergency prevention.",
      link = "http://www.governo.it/sites/new.governo.it/files/documenti/documenti/Notizie-allegati/covid-19/DPCM_comitato_txt_20200410.pdf",
      status = "active",
      flag = FALSE
    ),
    list(
      x = as.Date("2020-04-10"),
      name = " Prime Minister Decree (DPCM) 10th April 2020",
      label = "Prime Minister Decree (DPCM)",
      description = " the restrictive measures taken so far in order to contain the epidemiological crisis
caused by Covid-19 are extended until the 3rd of May. With the new Dpcm, as of the 14th of April, 
the opening of paper stationers, bookshops and clothing stores for children and babies will be allowed and silviculture 
and the wooden industry will be included among the allowed production activities. From the 14th of April, 
the date of entry into force of the Dpcm 10 April 2020, the Dpcm of 8th, 9th, 11th and 22nd March 
and 1st April 2020 will cease to have effect.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/04/11/20A02179/sg",
      status = "active",
      flag = FALSE
    ),
    list(
      x = as.Date("2020-04-26"),
      name = " Prime Minister Decree (DPCM) 26th April 2020",
      label = "Prime Minister Decree (DPCM)",
      description = "Italian First Minister Conte announced at a press conference the measures to contain the Covid-19 emergency 
in the so-called 'phase two'. Among the novelties introduced by the new Dpcm, whose measures will take effect from 
the 4th of May and for the following two weeks: the reopening of manufacturing, construction, real estate brokerage 
and wholesale trade activities; take-away catering is allowed without exception to the obligation of respecting 
the interpersonal safety distance of at least one meter, the prohibition of consuming the products inside 
the premises and the prohibition of stopping in the immediate vicinity of the premises.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/04/27/20A02352/sg",
      status = "active",
      flag = TRUE
    ),
    list(
      x = (as.Date("2020-05-16")),
      name = "Decree-Law (Dl) 16th May 2020 #RilanciaItalia",
      label = "Decree-Law",
      description = "Urgent measures concerning health, work support and economy, besides social policies related to the 
epidemiological COVID-19 emergency through the allocation of 55 billions.",
      link = "http://www.governo.it/it/articolo/comunicato-stampa-del-consiglio-dei-ministri-n-45/14602",
      status = "active",
      flag = TRUE
    ),
    list(
      x = (as.Date("2020-05-17")),
      name = " Prime Minister Decree (DPCM) 17th May 2020",
      label = "Prime Minister Decree (DPCM)",
      description = "Implementing provisions of the decree-law 25 marzo 2020, n. 19, 
including urgent measures to face the epidemiological emergency of COVID-19, and of the decreee-law 16 maggio 2020, n. 33, 
including further urgent measures (20A02717).",
      link = "http://www.governo.it/sites/new.governo.it/files/DPCM_20200517_txt.pdf",
      status = "active",
      flag = TRUE
    )
  )
  
  
  return(decrees)
}


# Decree plot
plot_decree <- function(decrees){
  
  df <- map(decrees, function(x){
    dd <- data.frame(Date = x$x, Name = x$name, Label = x$label, Description = x$description, Link = x$link)
    #colnames(dd) <- c("Date", "Name", "Label", "Descr", "Link")
    return(dd)
  }) %>% reduce(.f = rbind) %>% as_tibble() %>% mutate(position = c(rep(c(1,-1),11),1))
  
  
  
  js <- "
function(el, x) {
  el.on('plotly_click', function(d) {
    var point = d.points[0];
    var url = point.data.customdata[point.pointIndex];
    window.open(url);
  });
}"
  urls <- df$Link %>% as.character()
  p <- df %>% 
    plot_ly(x = ~Date, y = ~0, color = ~Label, colors = c("firebrick", "red", "orange3", "orange","gold"),
            text = ~paste0(Date, "\n<b>", Name, ":</b>\n", Description), 
            type = "scatter", mode = "markers", showlegend = T,
            marker = list(size = 20, line = list(color = "black", width = 2)), 
            hoverinfo = "text", customdata = urls) %>% 
    layout(title = paste0("<b>Timeline of Ministerial Decrees related to COVID-19 epidemic</b>", "\n",
                          "Click on the events to read the official documents"), 
           legend = list(x = 0.5, title = "", orientation = "h", xanchor = 'center'),
           xaxis = list(title = "", showgrid=F), yaxis = list(title = "",
                                                              showgrid = FALSE,
                                                              showline = FALSE,
                                                              showticklabels = FALSE)) %>% htmlwidgets::onRender(js)
  
  return(p)
}



plot_ratios <- function(da, is.reg = F, reg = NULL, type_of_ratio = "Positivity", plot_type){

  if(!is.reg){
    da_plot <- da %>% spread(Key, Value) %>%
      dplyr::select(-denominazione_regione) %>%
      group_by(data) %>% summarise_all(.funs = "sum") %>%
      mutate(denominazione_regione = "Italy")
    nm <- "Italy"
  }else{
    da_plot <- da %>% spread(Key, Value) %>%
      filter(denominazione_regione %in% reg)
    nm <- reg
  }

  da_plot %<>%
    group_by(denominazione_regione) %>%
    mutate(`Daily swabs` = c(Swabs[1], diff(Swabs)), `Daily tests` = c(`Tested cases`[1], diff(`Tested cases`)),
           `Positivity1` = `New positives`/`Daily swabs`,
           `Positivity2` = `New positives`/`Daily tests`,
           `Healing` = `Discharged recovered`/`Cumulative positives`,
           `Fatality` = Deceased/`Cumulative positives`,
           `Severity` = `Intensive care`/`Current hospitalized`
    ) %>%
    ungroup()
  col_tosel_bar <- c("New positives","Daily swabs", "Daily tests")
  if(type_of_ratio == "Severity") col_tosel_bar <-  c("Intensive care","Current hospitalized")
  if(type_of_ratio == "Fatality") col_tosel_bar <-  c("Deceased","Cumulative positives")
  if(type_of_ratio == "Healing") col_tosel_bar <-  c("Discharged recovered","Cumulative positives")

  nvar <- rlang::syms(col_tosel_bar)

  if(plot_type == "barplot"){
    ttl <- paste0("Composition of ", tolower(type_of_ratio), " rate", "\n <b>",nm,"</b>")
    if(type_of_ratio == "Positivity"){
      da_plot %<>%
        dplyr::select(data, denominazione_regione, nvar[[1]], nvar[[2]], nvar[[3]])
      
      p_out <- plot_ly(x = da_plot$data, y = da_plot[,3,drop = T], type = "bar",
                       name = as.character(nvar[[1]]), marker = list(color = "darkorange")) %>%
        add_trace(y = da_plot[,4,drop = T], name = as.character(nvar[[2]]),
                  marker = list(color = "firebrick")) %>% 
        add_trace(y = da_plot[,5,drop = T], name = as.character(nvar[[3]]),
                  marker = list(color = "grey")) %>% 
        layout(title =  ttl,  xaxis = list(title = ""), legend = list(x = 0, y = 1),
               yaxis = list(title = "Counts"))
    } else{
      da_plot %<>%
        dplyr::select(data, denominazione_regione, nvar[[1]], nvar[[2]])
      
      p_out <- plot_ly(x = da_plot$data, y = da_plot[,3,drop = T], type = "bar",
                       name = as.character(nvar[[1]]), marker = list(color = "darkorange")) %>%
        add_trace(y = da_plot[,4,drop = T], name = as.character(nvar[[2]]),
                  marker = list(color = "firebrick")) %>% 
        layout(title =  ttl,  xaxis = list(title = ""), legend = list(x = 0, y = 1),
               yaxis = list(title = "Counts"), autosize = F)
      
    }
  
  }
  
  if(plot_type == "ts"){
    ttl <- paste0("Time series of ", tolower(type_of_ratio), " rate", "\n <b>",nm,"</b>")
    if(type_of_ratio == "Positivity"){
      da_plot %<>%
        dplyr::select(data, denominazione_regione, `Positivity1`, `Positivity2`)
      p_out <- plot_ly(x = da_plot$data, y = da_plot[,3,drop=T]*100,
                       text = paste0(da_plot$data, "\n", round(da_plot[,3,drop=T]*100,2),"%"),
                       hoverinfo = "text", name = "with dayly swabs",
                       type = "scatter",mode="lines+markers",
                       marker = list(color = "firebrick"), line = list(color = "firebrick")) %>%
        add_trace(y = ~da_plot[,4,drop=T]*100, type = "scatter",mode="lines+markers",
                  marker = list(color = "grey"), line = list(color = "grey"), name = "with daily tests") %>% 
        layout(title =  ttl, xaxis = list(title = ""), legend = list(x = 0, y = 1),
               yaxis = list(title = paste0(type_of_ratio, " rate (%)")))
    } else{
      da_plot %<>%
        dplyr::select(data, denominazione_regione, rlang::syms(type_of_ratio)[[1]])
      p_out <- plot_ly(x = da_plot$data, y = da_plot[,3,drop=T]*100,
                       text = paste0(da_plot$data, "\n", round(da_plot[,3,drop=T]*100,2),"%"),
                       hoverinfo = "text",
                       type = "scatter",mode="lines+markers",
                       marker = list(color = "firebrick"), line = list(color = "firebrick")) %>%
        layout(title =  ttl, xaxis = list(title = ""), legend = list(x = 0, y = 1),
               yaxis = list(title = paste0(type_of_ratio, " rate (%)")))
    }
    
  }

  return(p_out)

}
