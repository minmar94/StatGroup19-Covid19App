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

# Substitute region code in vaccine data
subst_reg_code <- function(lab){
  lab[lab == "ABR"] <- "Abruzzo"
  lab[lab == "BAS"] <- "Basilicata"
  lab[lab == "CAL"] <- "Calabria"
  lab[lab == "CAM"] <- "Campania"
  lab[lab == "EMR"] <- "Emilia-Romagna"
  lab[lab == "FVG"] <- "Friuli Venezia Giulia"
  lab[lab == "LAZ"] <- "Lazio"
  lab[lab == "LIG"] <- "Liguria"
  lab[lab == "LOM"] <- "Lombardia"
  lab[lab == "MAR"] <- "Marche"
  lab[lab == "MOL"] <- "Molise"
  lab[lab == "PAB"] <- "Trentino-Alto Adige"
  lab[lab == "PAT"] <- "Trentino-Alto Adige"
  lab[lab == "PIE"] <- "Piemonte"
  lab[lab == "PUG"] <- "Puglia"
  lab[lab == "SAR"] <- "Sardegna"
  lab[lab == "SIC"] <- "Sicilia"
  lab[lab == "TOS"] <- "Toscana"
  lab[lab == "UMB"] <- "Umbria"
  lab[lab == "VDA"] <- "Valle d'Aosta"
  lab[lab == "VEN"] <- "Veneto"
  return(lab)
}

# Help button
helpbttn <- function(){
  showModal(modalDialog(
    title = HTML("</br></br><h3><b>Important information:</b></h3>"),
    HTML("
  <ul><li><b>Cumulative positives =</b>  current positives + deceased + discharged recovered </li>
  <li><b>Current positives =</b> hospitalized with symptoms + intensive care + home isolation</li>
  <li><b>New positives =</b> cumulative positives of today - cumulative positives of yesterday</li>
  <li><b>Current hospitalized =</b> hospitalized with symptoms + intensive care</li>
  <li><b>Death rate =</b> deceased/population</li>
  <li><b>Fatality rate =</b> deceased/cumulative positives</li>
  <li><b>NB: </b> the data, as it is reported by Italian Protezione Civile, refers to the number of people that have been found positive to SARS-Cov-2. 
           This does not imply that the same number of people developed COVID-19 disease.</li>
  <li><b>NB: </b>Tested cases identify diagnostic swabs. The difference between 'swabs' and 'tested cases' corresponds to control swabs, which are done on the same individual to confirm the healing or for other necessities.</li>
  <li><b>NB: </b>The term epidemic refers to a disease that affects a large number of people within a community, population, or region. On the other hand, a pandemic is an epidemic that is spread over multiple countries or continents.</li>
           </ul>"), 
    easyClose = TRUE
  ))
}
whatiscovid <- function(){
  HTML("<h2><b>What is COVID-19?</b></h2>","Coronavirus disease 2019 (COVID-19) is an infectious disease caused by Severe Acute Respiratory Syndrome Coronavirus 2 (SARS-CoV-2). The disease was first identified in December 2019 in Wuhan, the capital of China's Hubei province, and it has spread globally. The first confirmed case of what was then an unknown coronavirus was traced back to November 2019 in Hubei. Common symptoms include fever, cough, and shortness of breath. Other symptoms may include fatigue, muscle pain, diarrhoea, sore throat, loss of smell, and abdominal pain. The time from exposure to onset of symptoms is typically around five days but may range from two to fourteen days. While the majority of cases result in mild symptoms, some progress to viral pneumonia and multi-organ failure.")
}

# Print sign header
paste_signpercent <- function(x_oggi,x_ieri){
  simbolo <- ifelse(x_oggi-x_ieri>0, "+", "")
  incr <- ((x_oggi-x_ieri)/x_ieri)*100
  return(paste0(simbolo, round(incr, 2),"% w.r.t. yesterday"))
} 

# SAVE ICU DROPBOX
saveData <- function(data, token) {
  
  # Create a unique file name
  fileName <- "PastICUPred.csv"
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(file = filePath, path = "DataICUCovid19", dtoken = token)
}

loadData <- function(token) {
  # Read all the files into a list
  # Concatenate all data together into one data.frame
  data <- drop_read_csv(file = "DataICUCovid19/PastICUPredIta.csv", dtoken = token, skip = 1) %>% 
    as_tibble() %>% 
    set_colnames(value = c("Region", "Prediction", "Lower bound", "Upper bound", "Capacity", "DataPred")) %>% 
    mutate(DataPred = as.Date(DataPred))
  return(data)
}


# Read data functions -----------------------------------------------------
# Read italian data
read_italian <- function(path){
  
  dati_Ita <- data.table::fread(path) %>% as_tibble() %>% group_by(data) %>% summarise_all(first) %>% ungroup %>% 
    # Convert datatime into date
    mutate(data = date(data)) %>% 
    # Select only necessary variables
    dplyr::select(data:casi_testati, totale_positivi_test_molecolare:tamponi_test_antigenico_rapido, 
                  -starts_with("note"), -starts_with("casi_da"), -starts_with("variazione")) %>% 
    mutate_if(is.numeric, abs)
  
  # Set new colnames
  colnames(dati_Ita)[-c(1,2)] <- c("Hospitalized with symptoms", "Intensive care", "Current hospitalized",
                                   "Home isolation", "Current positives", "New positives", 
                                   "Discharged recovered", "Deceased",  "Cumulative positives", "Swabs", "Tested cases", 
                                   "PosTestMol", "PosTestAnti", "TampMol", "TampAnti")
  
  # Correct input values
  dati_Ita$`Tested cases`[dati_Ita$data == "2020-12-03"] <- 13264937
  dati_Ita$`Tested cases`[dati_Ita$data == "2020-12-05"] <- 13435624
  
  return(dati_Ita)
  
}

# Read and prepare regional data
read_regional <- function(path){
  
  dati_reg <- data.table::fread(path) %>% as_tibble() %>% 
    # Unselect not needed variables
    dplyr::select(-note, -variazione_totale_positivi, -casi_da_sospetto_diagnostico, -casi_da_screening, -lat, -long) %>% 
    mutate_if(is.numeric, abs) %>% 
    # Creo la regione Trentino che appare sottoforma di province autonome
    mutate(denominazione_regione = ifelse(denominazione_regione %in% c("P.A. Bolzano", "P.A. Trento"), "Trentino-Alto Adige", denominazione_regione),
           data = date(data)) %>% 
    gather(Key, Value, ricoverati_con_sintomi:casi_testati) %>% 
    group_by(Key, data, denominazione_regione) %>% 
    dplyr::summarise(Value = sum(Value)) %>% 
    ungroup()
  
  dati_reg$Key <- factor(dati_reg$Key, levels = unique(dati_reg$Key), 
                         labels = c("Tested cases","Deceased", "Discharged recovered", "Home isolation", "New positives", "Hospitalized with symptoms", 
                                    "Swabs", "Intensive care", "Cumulative positives", "Current hospitalized", "Current positives"))
  
  # Correct input values
  dati_reg$Value[dati_reg$Key == "Tested cases" & dati_reg$denominazione_regione == "Campania" & dati_reg$data == "2020-12-03"] <- 1118787
  dati_reg$Value[dati_reg$Key == "Tested cases" & dati_reg$denominazione_regione == "Molise" & dati_reg$data == "2020-12-05"] <- 89725
  return(dati_reg)
}

# Read province data
read_province <- function(path){
  
  dati_prov <- data.table::fread(path, na.strings = ",NA") %>% as_tibble() %>% 
    dplyr::select(-note) %>%
    mutate(denominazione_regione = ifelse(denominazione_regione %in% c("P.A. Bolzano", "P.A. Trento"), "Trentino-Alto Adige", denominazione_regione),
           data = date(data)) %>% 
    filter(sigla_provincia != "") %>% 
    rename(`Cumulative positives` = totale_casi) 
}

# Read residents data
read_residents <- function(path){
  
  residents <- data.table::fread(input = path, sep = ",") %>% as_tibble()
  residents$Territorio <- as.character(residents$Territorio)
  # Correct some region labels
  residents %<>% 
    mutate(
      Territorio = ifelse(Territorio == "Valle Aosta", "Valle d'Aosta", Territorio),
      Territorio = ifelse(Territorio == "Emilia Romagna", "Emilia-Romagna", Territorio),
      Territorio = ifelse(Territorio == "Friuli V. G.", "Friuli Venezia Giulia", Territorio),
      Territorio = ifelse(Territorio == "TrentinoAltoAdige", "Trentino-Alto Adige", Territorio),
    ) 
  residents$Territorio[residents$Territorio %in% c("AscoliPiceno", "ProvinciaAutonomaTrento", "MonzaedellaBrianza", "ReggiodiCalabria",
                                                   "ProvinciaAutonomaBolzano/Bozen", "ViboValentia", "LaSpezia", "ReggionellEmilia",
                                                   "Massa-Carrara")] <- c("La Spezia", "Monza e della Brianza", "Bolzano", "Trento", "Reggio nell'Emilia",
                                                                          "Massa Carrara", "Ascoli Piceno", "Reggio di Calabria", "Vibo Valentia")
  
  return(residents)
}

# Read vaccine data at the finest resolution and aggregates
read_vaccines_1 <- function(path){
  
  out <- read_csv(path) 
  dd <- max(out$data_somministrazione)
  
  out %<>% mutate_at(vars(area), subst_reg_code)
  
  out %<>% 
    dplyr::select(area, fascia_anagrafica, starts_with("sesso"), starts_with("categoria")) %>% 
    gather(Key, Value, -area, -fascia_anagrafica) %>% 
    group_by(area, fascia_anagrafica, Key) %>% 
    summarise(Value = sum(Value)) %>% 
    ungroup() %>% 
    rename(denominazione_regione = area) %>% 
    spread(Key, Value) %>% 
    mutate(data = dd)
  return(out)
}

# Read vaccine data at the finest resolution
read_vaccines_2 <- function(path){
  
  out <- read_csv(path) 
  out %<>% mutate_at(vars(area), subst_reg_code)
  
  out %<>% 
    dplyr::select(data_somministrazione, area, fascia_anagrafica, starts_with("sesso"), starts_with("categoria")) %>% 
    gather(Key, Value, -data_somministrazione, -area, -fascia_anagrafica) %>% 
    group_by(data_somministrazione, area, Key, fascia_anagrafica) %>% 
    summarise(Value = sum(Value)) %>% 
    ungroup() %>% 
    rename(data = data_somministrazione, denominazione_regione = area) %>% 
    spread(Key, Value)
  return(out)
}

# Read doses data
read_vaccines_latest_summ <- function(path){
  
  out <- read_csv(path) 
  
  out %<>% mutate_at(vars(area), subst_reg_code)
  
   out %<>% 
    gather(Key, Value, -ultimo_aggiornamento, -area) %>% 
    group_by(ultimo_aggiornamento, area, Key) %>% 
    summarise(Value = mean(Value)) %>% 
    ungroup() %>% 
    rename(data = ultimo_aggiornamento, denominazione_regione = area) %>% 
    spread(Key, Value)
  return(out)
}

# Read aggregated vaccine data at the mpst recent update by age class
read_vaccines_latest_ana <- function(path){
  
  out <- read_csv(path) 
  
  out %<>% rename(data = ultimo_aggiornamento)
  return(out)
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


# Overview  ---------------------------------------------------------------
# Return current situa
return_current_situa <- function(da, TotPop, CapICUTot){
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
  
  ICUStress <- round(terapia_intens_ultimo/CapICUTot, 4)*100
  
  PosRate <- round(new_pos_ultimo/abs(diff(c(dat_today$Swabs[2], dat_today$Swabs[1]))), 4)*100
  
  PosTestMol <- dat_today$PosTestMol[2] - dat_today$PosTestMol[1]
  PosTestAnti <- dat_today$PosTestAnti[2] - dat_today$PosTestAnti[1]
  TampMol <- dat_today$TampMol[2] - dat_today$TampMol[1]
  TampAnti <- dat_today$TampAnti[2] - dat_today$TampAnti[1]
  
  list_out <- list("Cumulative positives" = c(totale_casi_ultimo, incr_totale_casi),
                   "Current positives" = c(attualmente_positivi_ultimo, incr_attualmente_positivi, dat_today$`Current positives`[2] - dat_today$`Current positives`[1]),
                   "Current hospitalized" = c(totale_ricoverati_ultimo, incr_currhosp, dat_today$`Current hospitalized`[2] - dat_today$`Current hospitalized`[1]),
                   "Intensive care" = c(terapia_intens_ultimo, incr_terapia, dat_today$`Intensive care`[2] - dat_today$`Intensive care`[1]),
                   "Hospitalized with symptoms" = c(ricov_sintomi_ultimo, incr_ricoverati, dat_today$`Hospitalized with symptoms`[2] - dat_today$`Hospitalized with symptoms`[1]), 
                   "Home isolation" = c(isol_domic_ultimo, incr_isolamento, dat_today$`Home isolation`[2] - dat_today$`Home isolation`[1]),
                   "New positives" = c(new_pos_ultimo, incr_newpos, PosTestMol, PosTestAnti),
                   "Discharged recovered" = c(dimessi_guariti_ultimo, incr_guariti, dat_today$`Discharged recovered`[2] - dat_today$`Discharged recovered`[1]),
                   "Deceased" = c(deceduti_ultimo, incr_deceduti, dat_today$`Deceased`[2] - dat_today$`Deceased`[1]),
                   "Swabs" = c(swabs_ultimo, incr_swabs, dat_today$`Swabs`[2] - dat_today$`Swabs`[1], TampMol, TampAnti),
                   "Tested cases" = c(tested_ultimo, incr_tests, dat_today$`Tested cases`[2] - dat_today$`Tested cases`[1]),
                   "Letalita" = quoz_let_ita, 
                   "Mortalita" = tasso_mort_ita, 
                   "TassoPos" = PosRate, 
                   "ICUStress" = ICUStress)
  return(
    list_out
  )
}

# Show raw data
show_raw_data <- function(daita, dareg){
  
  Dt_out_raw <- daita %>% rename(Date = data, Region = stato) %>% 
    mutate(Region = "Italy",
           `New deaths` = c(Deceased %>% first, diff(Deceased))) %>% 
    dplyr::select(Date, Region, `Cumulative positives`, `New positives`, `Deceased`, `New deaths`, 
                  `Current positives`, `Current hospitalized`, `Hospitalized with symptoms`, 
                  `Intensive care`, `Home isolation`, `Discharged recovered`, Swabs, `Tested cases`) %>% 
    bind_rows(dareg %>% spread(Key, Value) %>% 
                rename(Date = data, Region = denominazione_regione) %>% 
                group_by(Region) %>%  mutate(`New deaths` = c(Deceased %>% first, diff(Deceased))) %>% 
                ungroup() %>% 
                dplyr::select(Date, Region, `Cumulative positives`, `New positives`, `Deceased`, `New deaths`, 
                              `Current positives`, `Current hospitalized`, `Hospitalized with symptoms`, 
                              `Intensive care`, `Home isolation`, `Discharged recovered`, Swabs, `Tested cases`)) 
  
  
  Dt_out_raw$Region <- factor(Dt_out_raw$Region, levels = c("Lombardia", "Liguria", "Piemonte", "Valle d'Aosta",  "Emilia-Romagna", "Friuli Venezia Giulia", "Veneto", "Trentino-Alto Adige",
                                                            "Lazio", "Marche", "Toscana", "Umbria","Abruzzo", "Basilicata", "Calabria","Campania", "Molise", "Puglia","Sardegna", "Sicilia"))
  
  Dt_out_raw %<>% arrange(desc(Date), Region) 
  
  return(Dt_out_raw)
}

table_raw_data <- function(tab, datesel, resdata, capICU){
  
  capICU %<>% filter(data <= datesel) %>% rename(Region = denominazione_regione, Date = data)
  
  raw_app <- tab %>% mutate(Region = as.character(Region)) %>% 
    group_by(`Region`) %>% arrange(Date) %>% 
    mutate(Incrpos = c(`New positives`[1], diff(`New positives`)),
           Incrdeaths = c(`New deaths`[1], diff(`New deaths`))) %>% 
    ungroup() %>% 
    arrange(desc(Date)) %>% 
    dplyr::select(-`Current hospitalized`, -`Hospitalized with symptoms`, -`Current positives`,
                  -`Home isolation`, -`Discharged recovered`, -Swabs) %>% 
    filter(`Region`!="Italy") %>% 
    left_join(capICU, by = c("Region", "Date")) %>% 
    left_join(resdata, by = c("Region" = "Territorio")) %>% rename(Residents = totale) %>% 
    mutate(`Cum. Pos. (x 1000 residents)` = round((`Cumulative positives`/Residents)*1000,2),
           `Deceas. (x 1000 residents)` = round((`Deceased`/Residents)*1000,2),
           `Tested cases (x 1000 residents)` = round((`Tested cases`/Residents)*1000,2),
           `Fatality rate (%)` = round((Deceased/`Cumulative positives`)*100, 2),
           `ICU/capacity (%)` = round((`Intensive care`/`Capienza`)*100,2))
  
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
                dom = 't', columnDefs = list(
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
      x = (as.Date("2020-02-24")),
      name = "Minister of Economics and Finance Decree 24th February 2020",
      label = "Minister of Economics and Finance Decree",
      description = "Suspension of the deadlines for the fulfillment of tax obligations in favor of the taxpayers concerned
from the epidemiological emergency from COVID-19. (20A01299)",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/02/26/20A01299/sg",
      status = "active",
      flag = TRUE
    ),
    list(
      x = as.Date("2020-01-30"),
      name = 'Minister of Health Ordinance 30th January 2020 <img src="checkMark.png" alt="Check mark" height="42" width="42">',
      label = "Minister of Health Ordinance",
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
      name = "'Minister of Health Ordinance 20th March 2020",
      label = "Minister of Health Ordinance",
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
epidemiological COVID-19 emergency through the allocation of 55 billions of euros.",
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
    ),
    list(
      x = (as.Date("2020-06-03")),
      name = "Minister of Economics and Finance Decree 3rd June 2020",
      label = "Minister of Economics and Finance Decree",
      description = "Technicalities for the involvement of the Health Card System for the purpose of implementing prevention measures
in the context of public health measures linked to the COVID-19 emergency.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/06/08/20A03083/sg",
      status = "active",
      flag = TRUE
    ),
    list(
      x = (as.Date("2020-06-30")),
      name = "Minister of Health Ordinance 30th June 2020",
      label = "Minister of Health Ordinance",
      description = "Further urgent measures to limit and manage the spread of the second epidemic wave of COVID-19",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/07/02/20A03561/sg",
      status = "active",
      flag = TRUE
    ),
    list(
      x = (as.Date("2020-07-09")),
      name = "Minister of Health Ordinance 9th July 2020",
      label = "Minister of Health Ordinance",
      description = "Further urgent measures to limit and manage the spread of the second epidemic wave of COVID-19",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/07/10/20A03744/sg",
      status = "active",
      flag = TRUE
    ),
    list(
      x = (as.Date("2020-07-14")),
      name = " Prime Minister Decree (DPCM) 14th July 2020",
      label = "Prime Minister Decree (DPCM)",
      description = "Further instructions following DPCM 25th March 2020 and 16th May 2020. Reopening of discotheques.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/07/14/20A03814/sg",
      status = "active",
      flag = TRUE
    ),
    list(
      x = (as.Date("2020-08-07")),
      name = " Prime Minister Decree (DPCM) 7th August 2020",
      label = "Prime Minister Decree (DPCM)",
      description = "Further instructions following DPCM 25th March 2020 and 16th May 2020.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/08/08/20A04399/sg",
      status = "active",
      flag = TRUE
    ),
    list(
      x = (as.Date("2020-09-07")),
      name = "Prime Minister Decree (DPCM) 7th September 2020",
      label = "Prime Minister Decree (DPCM)",
      description = "Further instructions following DPCM 25th March 2020 and 16th May 2020.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/09/07/20A04814/sg",
      status = "active",
      flag = TRUE
    ),
    list(
      x = (as.Date("2020-10-07")),
      name = "Prime Minister Decree (DPCM) 7th October 2020",
      label = "Prime Minister Decree (DPCM)",
      description = "Urgent measures related to the extension of the COVID-19 epidemiological State of Emergency and 
implementation of EU directive 2020/739 del 3 giugno 2020.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/10/07/20G00144/sg",
      status = "active",
      flag = TRUE
    ),
    list(
      x = (as.Date("2020-10-13")),
      name = "Prime Minister Decree (DPCM) 13th October 2020",
      label = "Prime Minister Decree (DPCM)",
      description = "Further instructions following DPCM 25th March 2020 and 16th May 2020.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/10/13/20A05563/sg",
      status = "active",
      flag = TRUE
    ),
    list(
      x = (as.Date("2020-10-18")),
      name = "Prime Minister Decree (DPCM) 18th October 2020",
      label = "Prime Minister Decree (DPCM)",
      description = "Further instructions following DPCM 25th March 2020 and 16th May 2020.",
      link = "https://www.gazzettaufficiale.it/eli/gu/2020/10/18/258/sg/pdf",
      status = "active",
      flag = TRUE
    ),
    list(
      x = (as.Date("2020-10-25")),
      name = "Prime Minister Decree (DPCM) 25th October 2020",
      label = "Prime Minister Decree (DPCM)",
      description = "Further instructions following DPCM 25th March 2020 and 16th May 2020.",
      link = "https://www.gazzettaufficiale.it/eli/gu/2020/10/25/265/sg/pdf",
      status = "active",
      flag = TRUE
    ),
    list(
      x = (as.Date("2020-10-28")),
      name = "Decree-Law (Dl) 28th October 2020",
      label = "Decree-Law",
      description = "Further urgent measures to support health, firms, workers, 
justice and security, related to COVID-19 epidemic emergency.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/10/28/20G00166/sg",
      status = "active",
      flag = TRUE
    ),
    list(
      x = (as.Date("2020-11-03")),
      name = "Prime Minister Decree (DPCM) 3rd November 2020",
      label = "Prime Minister Decree (DPCM)",
      description = "Further instructions following DPCM 25th March 2020 and 16th May 2020.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/11/04/20A06109/sg",
      status = "active",
      flag = TRUE
    ),
    list(
      x = (as.Date("2020-11-09")),
      name = "Decree-Law (Dl) 9th November 2020 #DecretoRistoriBis",
      label = "Decree-Law",
      description = "Further urgent measures for health protection, workers and firms support, justice and public security.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/11/09/20G00170/sg",
      status = "active",
      flag = TRUE
    ),
    list(
      x = (as.Date("2020-11-23")),
      name = "Decree-Law (Dl) 23rd November 2020 #DecretoRistoriTer",
      label = "Decree-Law",
      description = "Further urgent measures for health protection, workers and firms support, justice and public security.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/11/23/20G00175/sg",
      status = "active",
      flag = TRUE
    ),
    list(
      x = (as.Date("2020-11-30")),
      name = "Decree-Law 30th November 2020 #DecretoRistoriQuater",
      label = "Decree-Law",
      description = "Further urgent measures for health protection, workers and firms support, justice and public security.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/11/30/20G00183/sg",
      status = "active",
      flag = TRUE
    ),
    list(
      x = (as.Date("2020-12-03")),
      name = "Prime Minister Decree (DPCM) 3rd December 2020",
      label = "Prime Minister Decree (DPCM)",
      description = "Further instructions following DPCM 25th March 2020, 16th May 2020 and 14th July 2020.",
      link = "https://www.gazzettaufficiale.it/eli/id/2020/12/03/20A06767/sg",
      status = "active",
      flag = TRUE
    ),
    list(
      x = (as.Date("2020-12-05")) ,
      name = 'Minister of Health Ordinance del 5 Dicembre 2020',
      label = "Minister of Health Ordinance",
      description = "Further urgent measures to limit and manage the spread of the second epidemic wave of COVID-19. Classification of
Campania, Toscana, Valle d'Aosta e della Provincia autonoma di Bolzano.",
      link = "https://www.trovanorme.salute.gov.it/norme/dettaglioAtto?id=77516",
      status = "active"
    ),
    list(
      x = (as.Date("2020-12-06")) ,
      name = 'Minister of Health Ordinance 5th December 2020',
      label = "Minister of Health Ordinance",
      description = "Further urgent measures to limit and manage the spread of the second epidemic wave of COVID-19. Classification of Emilia-Romagna, 
Friuli Venezia Giulia, Marche, Puglia e Umbria.",
      link = "https://www.trovanorme.salute.gov.it/norme/dettaglioAtto?id=77517",
      status = "active"
    ),
    list(
      x = (as.Date("2020-12-11")) ,
      name = "Minister of Health Ordinance 11th December 2020",
      label = "Minister of Health Ordinance",
      description = "Further urgent measures to limit and manage the spread of the second epidemic wave of COVID-19. Classification of 
Abruzzo, Basilicata, Calabria, Lombardia e Piemonte.",
      link = "https://www.trovanorme.salute.gov.it/norme/dettaglioAtto?id=77661",
      status = "active"
    ),
    list(
      x = (as.Date("2021-01-05")),
      name = "Decree-Law (Dl) 5th January 2021 #DecretoPonte",
      label = "Decree-Law",
      description = "Further urgent measures to limit and manage the spread of the second epidemic wave of COVID-19.",
      link = "https://www.gazzettaufficiale.it/eli/id/2021/01/05/21G00001/sg",
      status = "active",
      flag = TRUE
    ),
    list(
      x = (as.Date("2021-01-14")) ,
      name = "Prime Minister Decree (DPCM) 14th January 2021",
      label = "Prime Minister Decree (DPCM)",
      description = "Further instructions following DPCM 25th March 2020, 16th May 2020 and 14th July 2020.",
      link = "https://www.gazzettaufficiale.it/eli/id/2021/01/15/21A00221/sg",
      status = "active"
    )
  )
  
  
  return(decrees)
}

# Decree plot
plot_decree <- function(decrees){
  
  if(length(decrees)%%2 == 0){
    lpos <- rep(c(1,-1),length(decrees)/2)
  }else{
    lpos <- c(rep(c(1,-1),floor(length(decrees)/2)),1)
  }
  
  df <- map(decrees, function(x){
    dd <- data.frame(Date = x$x, Name = x$name, Label = x$label, Description = x$description, Link = x$link)
    #colnames(dd) <- c("Date", "Name", "Label", "Descr", "Link")
    return(dd)
  }) %>% reduce(.f = rbind) %>% as_tibble() %>% mutate(position = lpos)
  
  
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
    plot_ly(x = ~Date, y = ~0, color = ~Label, colors = c("firebrick", "red", "orange3", "orange","gold", "grey50"),
            text = ~paste0(Date, "\n<b>", Name, ":</b>\n", Description), type = "scatter", mode = "markers", showlegend = T,
            marker = list(size = 20, line = list(color = "black", width = 2)), hoverinfo = "text", customdata = urls) %>% 
    layout(title = paste0("<b>Timeline of the government actions related to COVID-19 epidemic</b>", "\n", "Click on the events to read the official documents"), 
           legend = list(x = 0.5, title = "", orientation = "h", xanchor = 'center'),
           xaxis = list(title = "", showgrid=F), yaxis = list(title = "", showgrid = FALSE, showline = FALSE, showticklabels = FALSE)) %>% htmlwidgets::onRender(js)
  return(p)
}

# Ratio plot
plot_ratios <- function(da, is.reg = F, reg = NULL, type_of_ratio = "Positivity", plot_type, weeklyvar = F, capICU,
                        sfItareg = NULL){
  
  
  if(!is.reg){
    da_plot <- da %>% spread(Key, Value) %>% left_join(capICU, by = c("denominazione_regione", "data")) %>% 
      dplyr::select(-denominazione_regione) %>%
      group_by(data) %>% summarise_all(.funs = "sum") %>%
      mutate(denominazione_regione = "Italy")
    nm <- "Italy"
  }else{
    da_plot <- da %>% spread(Key, Value) %>% left_join(capICU, by = c("denominazione_regione", "data")) %>%
      filter(denominazione_regione %in% reg)
    nm <- reg
  }
  
  if(type_of_ratio == "ICU Stress"){
    dmap <- sfItareg %>% 
      sp::merge(
        da %>% spread(Key, Value) %>% left_join(capICU, by = c("denominazione_regione", "data")) %>% 
          mutate(`ICU Stress` = `Intensive care`/Capienza) %>% 
          dplyr::select(data, denominazione_regione, `ICU Stress`, Capienza),
        by.y = "denominazione_regione", by.x = "NAME"
      ) %>% arrange(data, NAME)
  }
  
  da_plot %<>% 
    mutate(`Daily swabs` = c(Swabs[1], diff(Swabs)), 
           `Daily tests` = c(`Tested cases`[1], diff(`Tested cases`)),
           `Daily tests` = ifelse(`Daily tests` > `Daily swabs`, `Daily swabs`, `Daily tests`),
           `Positivity1` = `New positives`/`Daily swabs`,
           `Positivity2` = `New positives`/`Daily tests`,
           `Healing` = `Discharged recovered`/`Cumulative positives`,
           `Fatality` = Deceased/`Cumulative positives`,
           `Severity` = `Intensive care`/`Current hospitalized`,
           `Seriousness` = `Intensive care`/`Current positives`,
           `ICU Stress` = `Intensive care`/`Capienza`
    ) %>% 
    ungroup() %>% 
    mutate_at(.vars = vars(`Positivity1`:`ICU Stress`), function(x){
      x[x<0] <- NA
      x[x>100] <- NA
      return(x)
    }) %>% 
    mutate_at(.vars = vars(`Daily swabs`,`Daily tests`), function(x){
      x[x<0] <- NA
      return(x)
    })
  
  col_tosel_bar <- c("New positives","Daily swabs", "Daily tests")
  if(type_of_ratio == "Severity") col_tosel_bar <-  c("Intensive care","Current hospitalized")
  if(type_of_ratio == "Fatality") col_tosel_bar <-  c("Deceased","Cumulative positives")
  if(type_of_ratio == "Healing") col_tosel_bar <-  c("Discharged recovered","Cumulative positives")
  if(type_of_ratio == "Seriousness'") col_tosel_bar <-  c("Intensive care","Current positives")
  if(type_of_ratio == "ICU Stress") col_tosel_bar <-  c("Intensive care","Capienza")
  
  nvar <- rlang::syms(col_tosel_bar)
  
  if(plot_type == "barplot"){
    ttl <- paste0("Composition of ", tolower(type_of_ratio), " rate", "\n <b>",nm,"</b>")
    if(type_of_ratio == "Positivity"){
      da_plot %<>% dplyr::select(data, denominazione_regione, nvar[[1]], nvar[[2]], nvar[[3]])
      p_out <- plot_ly(x = da_plot$data, y = da_plot[,3,drop = T], type = "bar", name = as.character(nvar[[1]]), marker = list(color = "darkorange")) %>%
        add_trace(y = da_plot[,4,drop = T], name = as.character(nvar[[2]]), marker = list(color = "firebrick")) %>% 
        add_trace(y = da_plot[,5,drop = T], name = as.character(nvar[[3]]), marker = list(color = "grey")) %>% 
        layout(title =  ttl,  xaxis = list(title = ""), legend = list(x = 0, y = 1), yaxis = list(title = "Counts"), barmode = 'stack')
    } else{
      da_plot %<>% dplyr::select(data, denominazione_regione, nvar[[1]], nvar[[2]])
      p_out <- plot_ly(x = da_plot$data, y = da_plot[,3,drop = T], type = "bar", name = as.character(nvar[[1]]), marker = list(color = "darkorange")) %>%
        add_trace(y = da_plot[,4,drop = T], name = as.character(nvar[[2]]), marker = list(color = "firebrick")) %>% 
        layout(title =  ttl,  xaxis = list(title = ""), legend = list(x = 0, y = 1), yaxis = list(title = "Counts"), autosize = F, barmode = 'stack')
      if(type_of_ratio == "ICU Stress"){
        p_out <- plot_ly(data = dmap %>% filter(data == max(data)), stroke = I("black"), 
                         split = ~NAME, color = ~`ICU Stress`*100, colors = "YlOrRd", alpha = 1,
                         text = ~paste0(NAME, "\n", type_of_ratio, " = ", round(`ICU Stress`*100,2), "%", 
                                        "\n", "Capacity = ", Capienza),
                         hoveron = "fills", 
                         hoverinfo = "text", showlegend = F) %>%
          layout(title = HTML(paste0("<b> ICU occupancy rate", "\n", as.character(max(dmap$data)),"</b>"))) %>% 
          colorbar( title = "", len = 0.6, limits = c(0,100)) %>% 
          partial_bundle()
      }
    }
  }
  
  if(plot_type == "ts"){
    ttl <- paste0("Time series of ", tolower(type_of_ratio), " rate", "\n <b>",nm,"</b>")
    if(type_of_ratio == "Positivity"){
      da_plot %<>% dplyr::select(data, denominazione_regione, `Positivity1`, `Positivity2`)
      if(weeklyvar){
        da_plot %<>% mutate(Week = week(data)) %>% group_by(Week) %>% 
          mutate_if(is.numeric, mean, na.rm = T) %>% 
          mutate(data = last(data), Week = last(Week)) %>% ungroup %>% distinct() %>% 
          #mutate_at(.vars = vars(`Positivita'1`, `Positivita'2`), .funs = function(x) c(0, diff(x))) %>% 
          dplyr::select(-Week)
      }
      p_out <- plot_ly(x = da_plot$data, y = da_plot[,3,drop=T]*100,
                       text = paste0(format(da_plot$data, "%b %d, %Y"), "\n", round(da_plot[,3,drop=T]*100,2),"%"),
                       hoverinfo = "text", name = "with dayly swabs", type = "scatter",mode="lines+markers", marker = list(color = "firebrick"), line = list(color = "firebrick")) %>%
        add_trace(y = ~da_plot[,4,drop=T]*100, type = "scatter",mode="lines+markers", marker = list(color = "grey"), line = list(color = "grey"), name = "with daily tests") %>% 
        layout(title =  ttl, xaxis = list(title = ""), legend = list(x = 0, y = 1), yaxis = list(title = paste0(type_of_ratio, " rate (%)")))
    } else{
      da_plot %<>% dplyr::select(data, denominazione_regione, rlang::syms(type_of_ratio)[[1]])
      if(weeklyvar){
        da_plot %<>% mutate(Week = week(data)) %>% group_by(Week) %>% 
          mutate_if(is.numeric, mean, na.rm = T) %>% 
          mutate(data = last(data), Week = last(Week)) %>% ungroup %>% distinct() %>% 
          #mutate_at(.vars = vars(`Positivita'1`, `Positivita'2`), .funs = function(x) c(0, diff(x))) %>% 
          dplyr::select(-Week)
      }
      p_out <- plot_ly(x = da_plot$data, y = da_plot[,3,drop=T]*100,
                       text = paste0(format(da_plot$data, "%b %d, %Y"), "\n", round(da_plot[,3,drop=T]*100,2),"%"),
                       hoverinfo = "text", type = "scatter",mode="lines+markers", marker = list(color = "firebrick"), line = list(color = "firebrick")) %>%
        layout(title =  ttl, xaxis = list(title = ""), legend = list(x = 0, y = 1), yaxis = list(title = paste0(type_of_ratio, " rate (%)")))
    }
  }
  return(p_out)
}


# Add residents to map
add_residents_tomap <- function(d_sf, resdata, tomerge){
  
  resdata %<>% rename(residenti = totale)
  
  return(d_sf %>% sp::merge(resdata, by.y = "Territorio", by.x = tomerge))
  
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
      summarise(Value = sum(Value)) %>% ungroup() %>% 
      filter(data == max(data), Key %in% vartosel) %>% 
      arrange(Key) %>% 
      mutate(percentage = round(Value/sum(Value),4)*100, lab.pos = cumsum(percentage)-.5*percentage) %>%
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
    plot_ly(labels = ~Key, values = ~percentage, type = 'pie', textposition = 'inside',
            textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text', hole = 0.6, text = ~paste(Key, "\n", percentage, '%'),
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
  
  leg_lab <- as.character(varsel)
  
  add_people <- " people"
  valtoround <- 0
  if(is.density){
    valtoround <- 5
    dajoined$Value <- round(dajoined$Value/dajoined$residenti*100, 4)
    dajoinedprov$Value <- round(dajoinedprov$Value/dajoinedprov$residenti*100, 4)
    leg_lab <- paste(leg_lab, "(%)")
    add_people <- "% of the residents"
  }
  
  if(is.null(reg)){
    datamap <- dajoined %>% filter(Key == as.character(varsel), data == as.character(datasel))
  }else{
    if(varsel == "Cumulative positives"){
      datamap <- dajoinedprov %>% filter(Key == as.character(varsel), data == as.character(datasel), NAME %in% reg) %>% mutate(NAME = paste0(NAME_2, " (", NAME, ")"))
    }else{
      datamap <- dajoined %>% 
        filter(Key == as.character(varsel), data == as.character(datasel), NAME %in% reg)
    }
    
  }
  
  # p <- plot_ly(data = datamap, stroke = I("black"),
  #              split = ~NAME, color = ~Value, colors = "YlOrRd", alpha = 1, type = "scatter",
  #              text = ~paste0(NAME, "\n", Value, add_people), hoveron = "fills", hoverinfo = "text", showlegend = F) %>% 
  #   layout(title = HTML(paste0("<b>",as.character(datasel),"</b>")))  %>% 
  #   colorbar( title = list(text = paste0("<b>", leg_lab, "</b>")), len = 0.6)
  # if(length(reg) == 1){
  #   p <- p %>% hide_colorbar()
  # }
  
  pal <- colorBin("YlOrRd", datamap$Value, bins = round(quantile(datamap$Value, seq(0,1,length.out = 5)),valtoround), na.color = "grey")
  ttltop <- tags$div(HTML(paste0("<b>",as.character(datasel),"</b>")))
  
  p <- datamap %>% st_set_crs(., "+proj=longlat +datum=WGS84") %>% 
    mutate(popup = str_c("<strong>", NAME, "</strong>: </br>", Value, add_people) %>%
             map(htmltools::HTML)) %>% 
    leaflet() %>% 
    addTiles() %>%
    setView(lng = 12.8, lat = 41.8, zoom = 5) %>%
    addPolygons(label=~popup, fillColor = ~pal(Value), weight = 1, smoothFactor = 0.5, color = "black", opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")) %>% 
    addLegend(pal = pal, values = ~n, opacity = 0.4, title = leg_lab, position = "bottomleft") %>% 
    addControl(ttltop, "topright")
  
  return(p)

}

# Do Time series
do_ts <- function(da, is.reg = F, reg, varsel, datasel, is.incrementi=T, lag_incr = "daily variations", tipo.incremento = "Absolute"){
  
  if(!is.reg){
    dts <- da %>% mutate(Key = as.character(Key)) %>% 
      group_by(data, Key) %>% summarise(Value = sum(Value)) %>% 
      ungroup() %>% 
      filter(data <= as.character(datasel), Key == varsel) %>% 
      arrange(Key) %>% 
      mutate(denominazione_regione = "Italy") %>% drop_na()
  }else{
    dts <- da %>% 
      filter(data <= as.character(datasel), Key == varsel, denominazione_regione %in% reg) %>% dplyr::select(data, denominazione_regione, Key, Value) %>% drop_na()
  }
  
  ylabel <- "Counts"
  
  if(is.incrementi){
    dts %<>% arrange(denominazione_regione) %>% group_split(denominazione_regione) 
    
    if(lag_incr == "daily variations"){
      dts %<>%  
        map_dfr(function(x) {
          x %<>% 
            mutate(IncrAss = c(0, diff(Value)),
                   IncrPerc = round((c(0, diff(Value))/Value)*100,4)) %>% 
            ungroup() 
        })
    }
    if(lag_incr == "weekly variations"){
      if(varsel %in% c("Cumulative positives", "Discharged recovered", "Deceased", "Swabs", "Tested cases")){
        dts %<>%  
          map_dfr(function(x){
            x %<>% mutate(Y = year(data), week = week(data)) %>% 
              group_by(Y, week) %>% 
              summarise(data = last(data), Key = last(Key), Value = last(Value),
                        denominazione_regione = last(denominazione_regione)) %>% 
              mutate(IncrAss = c(0, diff(Value)),
                     IncrPerc = round((c(0, diff(Value))/Value)*100,4)) %>% 
              ungroup() 
          }) 
      } else{
        dts %<>%  
          map_dfr(function(x){
            x %<>% mutate(Y = year(data), week = week(data)) %>% 
              group_by(Y, week) %>% 
              summarise(data = last(data), Key = last(Key), Value = mean(Value, na.rm = T),
                        denominazione_regione = last(denominazione_regione)) %>% 
              mutate(IncrAss = c(0, diff(Value)),
                     IncrPerc = round((c(0, diff(Value))/Value)*100,4)) %>% 
              ungroup() 
          })
      }
      
    }
    
    ylabel <- ifelse(tipo.incremento == "Absolute", "Absolute variations",
                     "Relative variations (%)") 
    
    if(tipo.incremento == "Absolute"){
      dts <- dts %>% dplyr::select(data, denominazione_regione, Key, IncrAss) %>% rename(Value = IncrAss)
    } else {
      dts <- dts %>% dplyr::select(data, denominazione_regione, Key, IncrPerc) %>% rename(Value = IncrPerc) %>% filter(data > min(data))
    }
  }
  
  colors <- c("firebrick", "red", "orange3", "orange","gold")[1:length(reg)]
  
  dts %>% 
    plot_ly(x = ~data, y = ~Value, type = 'scatter', mode = 'lines+markers', color = ~denominazione_regione, colors = colors) %>% 
    layout(xaxis = list(title = ""), yaxis = list(title = ylabel), title = HTML(paste0("Time series of <b>",tolower(varsel),"</b>")), legend = list(orientation = "h"))
  
}

# Model -------------------------------------------------------------------
prepdata_for_model <- function(dftoprep, resdata){
  
  da <- dftoprep %>% arrange(denominazione_regione, data) %>% 
    left_join(resdata, by = c("denominazione_regione" = "Territorio")) %>% 
    mutate(ti = as.numeric(unclass(factor(data)))) %>% 
    rename(ti_orig = data, region = denominazione_regione, residents = totale) 
  
  return(da)
}


run_growth_model <- function(da, reg=NULL, wh="New positives", horizon = 10, fam="Poisson", 
                             alpha=0.05, reduce_obs, wave = "I part", DataWave = "2020-07-19") {
  
  # da = data_formodel_prep
  # reg = NULL
  # fam = "Negative Binomial"
  # horizon = 30
  # alpha = 0.01
  # wave = "II part"
  # wh = "New positives"
  # reduce_obs = "2020-11-07"
  # NoCov = T
  # DataWave = "2020-07-31"
  # 
  
  
  #ti_orig_out <- da$ti_orig %>% unique()
  
  if(is.null(reg)){
    dat <- da %>% select(-region, -ti) %>% group_by(ti_orig) %>% summarise_at(.vars = 1:12, .funs = sum) 
  }else{
    dat <- da[da$region==reg,]
  }
  
  dat %<>% mutate(`New deceased` = abs(c(Deceased[1], diff(Deceased))),
                  `New discharged recovered` = abs(c(`Discharged recovered`[1], diff(`Discharged recovered`))),
                  NewTested = abs(c(`Tested cases`[1], diff(`Tested cases`))))
  dat <- dat[-1, ]
  
  if(wave == "I part"){
    dat %<>% filter(ti_orig<=DataWave) %>% mutate(ti=as.numeric(ti_orig-min(ti_orig))+1)
    nSet <- as.numeric(as.Date(DataWave)+horizon-as.Date("2020-02-25")+1)
    cc <- as.data.frame(matrix(NA, 
                               nrow = nSet, 
                               ncol = 8)) 
  }else{
    dat %<>% filter(ti_orig>DataWave) %>% mutate(ti=as.numeric(ti_orig-min(ti_orig))+1)
  }
  
  # Adding covariates
  X <- cbind(rep(1, nrow(dat)), !weekdays(dat$ti_orig)%in%c("Saturday", "Sunday"))
  # X <- as.matrix(rep(1, nrow(dat)))
  # if(NoCov & wh == "New positives"){
  #   dat %<>% filter(!is.na(NewTested)) %>% mutate(ti=as.numeric(ti-min(ti))+1)
  #   X <- cbind(rep(1, nrow(dat)), 
  #              scale(dat$NewTested), 
  #              !weekdays(dat$ti_orig)%in%c("Saturday", "Sunday"))
  # }
  
  # if(wave == "I wave"){
  #   dat <- dat[ti_orig_out<=DataWave, ]
  #   ti_orig_out <- seq(min(ti_orig_out[-1]), as.Date(DataWave), 1)
  #   cc <- as.data.frame(matrix(NA, nrow = as.numeric(as.Date(DataWave)+horizon-as.Date("2020-02-25")+1), ncol = 8)) 
  #  
  # }else{
  #   dat <- dat[ti_orig_out>DataWave, ]
  #   ti_orig_out <- seq(as.Date(DataWave)+1, max(ti_orig_out), 1)
  #   cc <- as.data.frame(matrix(NA, nrow = as.numeric(max(ti_orig_out)+horizon-as.Date(DataWave)+1+1), ncol = 8)) 
  # }
  # 
  
  # Pick the variable to model
  whidx <- which(colnames(dat)==wh)
  pc_allobs <- dat[, whidx, drop = T]
  pc_fit <- (dat %>% filter(ti_orig<=reduce_obs))[, whidx, drop = T]
  ti_fit <- dat %>% filter(ti_orig<=reduce_obs) %$% ti
  ti_orig_fit <- dat %>% filter(ti_orig<=reduce_obs) %$% ti_orig
  ti_orig_hor <- seq(min(ti_orig_fit), as.Date(reduce_obs)+horizon, 1)
  X_fit <- as.matrix(X[ti_fit,])
  
  # Max fit and horizon
  mti <- max(ti_fit)
  timax <- mti+horizon
  
  np <- tryCatch(
    growthGLM(di = pc_fit, 
              ti = ti_fit, alpha = alpha,
              family = fam, tPred = timax, X = X_fit,
              maxiter = 2000, runs = 500, nBoot = 5000),
    error = function(e){return("Error")}
  )
  
  # Predictions
  fitTib <- tibble(ti_orig=ti_orig_fit, pc=pc_fit)
  horTib <- tibble(ti_orig=ti_orig_hor, ly=np$lowdiff, yhat=c(np$linPredDiff), uy=np$updiff)
  # Correct upper bound
  if(is.null(reg)){
    Pop <- da %>% distinct(region, residents) %$% sum(residents) 
    horTib$uy[horTib$uy>Pop] <- Pop 
  }else{
    Pop <- da$residents[da$region == reg][1]
    horTib$uy[horTib$uy>Pop] <- Pop 
  }
  cc <- fitTib %>% 
    full_join(dat %>% dplyr::select(ti_orig,  rlang::sym(wh)), by="ti_orig") %>% 
    full_join(horTib, by="ti_orig") %>% 
    set_colnames(value = c("x1", "pc", "pc_all", "ly", "y", "uy"))
  ooo <- list(cc=cc, R2 = round(np$R2diff, 4), pars = np$pars, 
              stderrs = np$se, fam = fam,
              NoConv = np$NoConv)
  
  #outputmod <- ooo
  return(ooo)
}


# Plot result model (return plot) new
plot_out_model <- function(outputmod, horizon = 15, VarModel = "New positives", showbands = T, reg = "Italy"){
  
  # VarModel = wh
  # horizon = 1
  # showbands = T
  # reg = reg
  # 
  # 
  ses <- outputmod$stderrs
  dd <- outputmod$cc
  ylabel <- "Daily counts"
  
  namemodel <- "Richards"
  idx <- min(which(is.na(dd$pc_all)))-1
  ttl <- paste0("<b align='center'>Fitted and observed values for ","\n", VarModel, " - ",reg, "</b>") 
 
  pp <- plot_ly(dd, 
                x=~x1, y = ~pc_all, name = 'Observed', color = I("black"), type = 'scatter', mode = 'lines+markers', alpha = 0.6) %>%
    add_trace(data = dd %>% filter(x1 <= x1[min(which(dd$pc %>% is.na)) + horizon]), x=~x1, y = ~y, name = namemodel, type = "scatter",
              mode = 'lines', alpha = 1, line = list(width = 3, color = "red")) %>%
    layout(title = ttl, xaxis = list(title = ""), yaxis = list(title = ylabel), legend = list(x = 0, y = 1))
  
  if(!outputmod$NoConv){
    if(sum(is.na(ses)|is.nan(ses)) == 0){
      pp <- plot_ly(data = dd %>% filter(x1 <= x1[min(which(dd$pc %>% is.na)) + horizon]), x=~x1, y = ~y, type = "scatter", mode = 'lines',
                    line = list(width = 3, color = "red"), name = namemodel) %>%
        add_ribbons(data = dd %>%filter(x1 <= x1[min(which(dd$pc %>% is.na)) + horizon]),
                    ymin = ~ly, ymax = ~uy, name = '95% Conf. Int.', color = I('rgba(250,128,114, 0.8)'), line = list(width = 0)) %>% 
        add_trace(data = dd, x=~x1, y = ~pc_all,  name = "Observed", alpha = 0.7, mode = 'lines+markers', 
                  line = list(color = "black", width=0.5), marker = list(color = "black")) %>%
        layout(title = ttl, xaxis = list(title = ""), yaxis = list(title = ylabel), legend = list(x = 0, y = 1))
    }
  }  else{
    if(sum(is.na(ses)|is.nan(ses)) == 0 & showbands){
      pp <- plot_ly(dd %>% filter(x1 <= x1[min(which(dd$pc %>% is.na)) + horizon]), x=~x1, y = ~y, type = "scatter", mode = 'lines',
                    line = list(width = 3, color = "red"), name = namemodel) %>%
        add_ribbons(ymin = ~ly, ymax = ~uy, name = '95% Conf. Int.', color = I('rgba(250,128,114, 0.8)'), line = list(width = 0)) %>% 
        add_trace(data = dd, x=~x1, y = ~pc_all,  name = "Observed", alpha = 0.7, mode = 'lines+markers', 
                  line = list(color = "black", width=0.5), marker = list(color = "black")) %>%
        layout(title = ttl, xaxis = list(title = ""), yaxis = list(title = ylabel), legend = list(x = 0, y = 1))
    } 
  }
  
  return(pp)
  
}

# Datatable model output
DT_out_model <- function(outputmod, horizon = 15, VarModel = "New positives", showbands = F, reg = "Italy") {
  
  ses <- outputmod$stderrs
  
  capts_out <-  htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: left;', 'Table: ', htmltools::em(paste0('Predictions for ', tolower(VarModel), ' - ', reg,'.'))
  )
  
  outputmod$cc %<>% mutate(t_all = x1)
  outputmod$cc$t_all[outputmod$cc$x1>=outputmod$cc$x1[min(which(is.na(outputmod$cc$pc_all)))]] <- NA
  ddt <- outputmod$cc %>% 
    dplyr::select(t_all, pc_all, x1, y) %>% set_colnames(value = c("Date", "Observed", "Data2", "Predicted")) %>%
    filter(between(Data2, max(Date, na.rm = T) -2, max(Date, na.rm = T) + horizon)) %>%
    dplyr::select(-Date) %>%
    mutate_if(is.numeric, round) %>% 
    rename(Date = Data2) %>% 
    dplyr::select(Date, Observed, Predicted)
  
  if(!outputmod$NoConv){
    if(sum(is.na(ses)|is.nan(ses)) == 0){
      ddt <- bind_cols(ddt, outputmod$cc %>% filter(x1 %in% ddt$Date) %>% dplyr::select(ly, uy) %>% set_colnames(value = c("Min.", "Max."))) %>%
        mutate_if(is.numeric, round) %>% 
        dplyr::select(Date, Observed, Predicted, `Min.`, `Max.`)
    }
  } else{
    if(sum(is.na(ses)|is.nan(ses)) == 0 & showbands){
      
      ddt <- bind_cols(ddt, outputmod$cc %>% filter(x1 %in% ddt$Date) %>% dplyr::select(ly, uy) %>% set_colnames(value = c("Min.", "Max."))) %>%
        mutate_if(is.numeric, round) %>% 
        dplyr::select(Date, Observed, Predicted, `Min.`, `Max.`)
    }
  }
  
  ddtt <- datatable(ddt, rownames = FALSE, class = 'cell-border stripe',  caption = capts_out,
                    options = list(dom = 'tpl', pageLength = 5, scrollX = T, lengthMenu = c(5,10,15,20),
                                   columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  
  
  return(ddtt)
  
}

# print summary model (return html code)
summary_out_model <- function(outputmod, VarModel = "New positives", resdata, is.reg = F, reg = "Italy"){
  
  famout <- outputmod$fam
  
  mx_sel <- 6*10^7
  if(is.reg) mx_sel <- resdata[resdata$Territorio == reg, 2]
  
  # 
  ti_orig_out <- outputmod$cc$x1[outputmod$cc$x1<outputmod$cc$x1[min(which(is.na(outputmod$cc$pc_all)))]]
  # 
  if(which.max(outputmod$cc$y)> length(ti_orig_out)){
    est_pick <- max(ti_orig_out) + which.max(outputmod$cc$y) - length(ti_orig_out)
  }else{
    est_pick <- ti_orig_out[which.max(outputmod$cc$y)]
  }
  tpos_max <- floor(outputmod$pars[2] + log10(outputmod$pars[3])/exp(outputmod$pars[1]))
  if(tpos_max>0 & tpos_max <= length(outputmod$cc$x1)) est_pick <- as.Date(outputmod$cc$x1[tpos_max])
  #
  tasso_come <- "Richards"
  # 
  pdate <- paste_eng_date(est_pick)
 
  coverage <- ((sum((outputmod$cc$ly <= outputmod$cc$pc_all) & (outputmod$cc$uy >= outputmod$cc$pc_all), na.rm = T))/sum(!is.na(outputmod$cc$pc_all))) %>% round(2)
    
  # Asintoto cumulati
  # asi_est_l <- exp(outputmod$pars[1]-1.96*outputmod$stderrs[1])
  # errs_l <- (outputmod$pars[length(outputmod$pars)]-1.96*outputmod$stderrs[length(outputmod$pars)]) %>% exp
  # asi_est_l <- (asi_est_l-1.96*ifelse(famout == "Poisson",sqrt(asi_est_l),sqrt(asi_est_l + (asi_est_l^2)/errs_l))) %>% round
  # idx <- min(which(is.na(outputmod$cc$pc)))-1
  # cond <- asi_est_l < outputmod$cc$pc[idx]
  # asi_est_l <- ifelse(cond, outputmod$cc$pc[idx], asi_est_l)
  # 
  # asi_est_u <- exp(outputmod$pars[1]+1.96*outputmod$stderrs[1])
  # asi_est_u <- (asi_est_u+1.96*ifelse(famout == "Poisson",sqrt(asi_est_u),sqrt(asi_est_u + (asi_est_u^2)/errs_l))) %>% round
  # asi_est_u <- ifelse(asi_est_u > mx_sel, mx_sel, asi_est_u)
  # asi_est <- ifelse(cond, outputmod$cc$pc[idx], round(exp(outputmod$pars[1])))
  
  
  # if(VarModel %in% c("Deceased", "Discharged recovered", "Cumulative positives") & !(is.na(outputmod$stderrs[1])) & (asi_est_u>=asi_est_l)){
  #   out_string <- paste0(
  #     paste0('<h3 align = "center"><b>', famout, " - ", tasso_come, ': </b></h3>'),
  #     "<br/>",
  #     '<p align="center"><b>Estimated peak of daily variations: </b></p>', '<p align="center">',pdate, "</p>", "<br/>",
  #     '<p align="center"><b>Estimated upper asymptote of cumulative cases: </b></p>', 
  #     "<p align='center'>", asi_est, paste0(" (not less than ", asi_est_l, ", no more than ", asi_est_u, ")"),"</p>", "<br/>",
  #     '<p align="center"><b>Goodness of fit: </b></p>', '<p align="center">', outputmod$R2, '</p>',"<br/>",
  #     '<p align="center"><b>Coverage: </b></p>', '<p align="center">', coverage*100,"%", "</p>",
  #     collapse = ""
  #   )
  # } else{
    out_string <- paste0(
      paste0('<h3 align="center"><b>', famout, " - ", tasso_come, ': </b></h3>'),
      "<br/>",
      '<p align="center"><b>Day of the peak: </b></p>', '<p align="center">', pdate, "</p><br/>",
      '<p align="center"><b>Goodness of fit:</b></p>', '<p align="center">', outputmod$R2,"</p><br/>",
      '<p align="center"><b>Coverage: </b></p>', "<p align='center'>", coverage*100,"%", "</p>",
      collapse = ""
    )
 # }
  return(HTML(out_string))
}



# Vaccine section ---------------------------------------------------------
vaccine_map <- function(v_data, type = "Administered", ita_sf, res_data){
  
  da_map <- ita_sf %>% sp::merge(v_data, by.y = "denominazione_regione", by.x = "NAME") %>% 
    add_residents_tomap(resdata = res_data, tomerge = "NAME") %>% 
    mutate(percentuale_residenti = round(dosi_somministrate/residenti*1000, 1))
  #dplyr::select(NAME, starts_with("percentuale"), geometry) %>% 
  
  if(type == "Administered"){
    ttl1 <- ""
    hover_add <- ""
    da_map2 <- da_map %>% dplyr::select(NAME, starts_with("dosi")) %>% 
      mutate(Value = dosi_somministrate, Id1 = "")
  }
  if(type == "Administered/Delivered"){
    ttl1 <- "Dosi consegnate: "
    hover_add <- "% \n"
    da_map2 <- da_map %>% dplyr::select(NAME, starts_with("dosi"), percentuale_somministrazione) %>% 
      rename(Value = percentuale_somministrazione, Id1 = dosi_consegnate)
  }
  if(type == "Administered/Residents"){
    ttl1 <- "Residents: "
    hover_add <- " (x 1000 res.) \n"
    da_map2 <- da_map %>% dplyr::select(NAME, residenti, dosi_somministrate, percentuale_residenti) %>% 
      rename(Value = percentuale_residenti, Id1 = residenti)
  }  
  
  
  p <- plot_ly(data = da_map2, stroke = I("black"), 
               split = ~NAME, color = ~Value, colors = "YlOrRd", alpha = 1,
               text = ~paste0(NAME,": ", Value, hover_add, 
                              ttl1, Id1, "\n", 
                              "Administered doses: ", dosi_somministrate),
               hoveron = "fills", hoverinfo = "text", showlegend = F) %>%
    layout(title = HTML(paste0("<b>",type,"</b>"))) %>% 
    colorbar( title = list(text = paste0("<b>", "Doses", "</b>")), len = 0.6)
  return(p)
}

vaccine_map_data <- function(v_data, type = "Administered", ita_sf, res_data){
  
  da_map <- ita_sf %>% sp::merge(v_data, by.y = "denominazione_regione", by.x = "NAME") %>% 
    add_residents_tomap(resdata = res_data, tomerge = "NAME") %>% 
    mutate(percentuale_residenti = round(dosi_somministrate/residenti*1000, 1))
  #dplyr::select(NAME, starts_with("percentuale"), geometry) %>% 
  
  if(type == "Administered"){
    ttl1 <- ""
    hover_add <- ""
    da_map2 <- da_map %>% dplyr::select(NAME, starts_with("dosi")) %>% 
      mutate(Value = dosi_somministrate, Id1 = "")
  }
  if(type == "Administered/Delivered"){
    ttl1 <- "Delivered doses: "
    hover_add <- "% <br/>" 
    da_map2 <- da_map %>% dplyr::select(NAME, starts_with("dosi"), percentuale_somministrazione) %>% 
      rename(Value = percentuale_somministrazione, Id1 = dosi_consegnate)
  }
  if(type == "Administered/Residents"){
    ttl1 <- "Residents: "
    hover_add <- " (x 1000 res.) <br/>"
    da_map2 <- da_map %>% dplyr::select(NAME, residenti, dosi_somministrate, percentuale_residenti) %>% 
      rename(Value = percentuale_residenti, Id1 = residenti)
  }  
  
  
  # p <- plot_ly(data = da_map2, stroke = I("black"), 
  #              split = ~NAME, color = ~Value, colors = "YlOrRd", alpha = 1,
  #              text = ~paste0(NAME,": ", Value, hover_add, 
  #                             ttl1, Id1, "\n", 
  #                             "Dosi somministrate: ", dosi_somministrate),
  #              hoveron = "fills", hoverinfo = "text", showlegend = F) %>%
  #   layout(title = HTML(paste0("<b>",type,"</b>"))) %>% 
  #   colorbar( title = list(text = paste0("<b>", "Dosi", "</b>")), len = 0.6)
  return(list(da_map2, ttl1, hover_add))
}

vaccine_donut <- function(v_data, type = "Gender", reg = NULL){
  #v_data <- isolate(v_data_somm())
  da_plot <- v_data %>% group_by(denominazione_regione) %>% summarise_if(is.numeric, sum)
  if(type == "Gender"){
    da_plot %<>% dplyr::select(-starts_with("categoria"))
    if(!is.null(reg)){
      da_plot %<>% filter(denominazione_regione == reg)
    } else{
      da_plot %<>% add_row(denominazione_regione = "Italy", 
                           sesso_femminile = sum(da_plot$sesso_femminile),
                           sesso_maschile = sum(da_plot$sesso_maschile)) %>% 
        filter(denominazione_regione == "Italy")
    }
    p_out <- da_plot %>% 
      mutate(TOT = sesso_femminile + sesso_maschile,
             Male = round(sesso_maschile/TOT*100, 2), 
             Female = round(sesso_femminile/TOT*100, 2)) %>% 
      gather(Gender, Value, Male:Female) %>% 
      plot_ly(labels = ~Gender, values = ~Value, type = 'pie', textposition = 'inside',
              textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text', hole = 0.6,
              text = ~paste(Gender, "\n", Value, '%'),
              marker = list(colors = c("skyblue3", "salmon"), line = list(color = '#FFFFFF', width = 1))) %>% 
      layout(title = paste0("Gender",  "<b>", "\n", unique(da_plot$denominazione_regione), "</b>", "\n"), 
             showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  }
  if(type == "Category"){
    da_plot %<>% dplyr::select(-starts_with("sesso"))
    
    nms <- gsub(replacement = "", x = colnames(v_data %>% dplyr::select(starts_with("categoria"))), pattern = "categoria") %>% 
      gsub(pattern = "_", replacement = " ", x = .) %>% 
      stri_trans_totitle() %>% str_trim()
    
    if(!is.null(reg)){
      da_plot %<>% filter(denominazione_regione == reg)  %>% 
        set_colnames(value = c("denominazione_regione", nms))
    } else{
      valtoadd <- da_plot %>% dplyr::select(starts_with("categoria")) %>% colSums() %>% t %>% as.data.frame()
      colnames(valtoadd) <- nms
      colnames(da_plot)[-1] <- nms
      da_plot <- tibble(denominazione_regione = "Italy", valtoadd) 
    }
    TOT <- rowSums(da_plot[,-1])
    p_out <- da_plot %>% 
      mutate_at(2:ncol(.), function(x){ round(x/TOT*100, 2)}) %>% 
      gather(Category, Value, -denominazione_regione) %>% 
      plot_ly(labels = ~Category, values = ~Value, type = 'pie', textposition = 'inside',
              textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text', hole = 0.6,
              text = ~paste(Category, "\n", Value, '%'),
              marker = list(colors = ~Category, line = list(color = '#FFFFFF', width = 1))) %>% 
      layout(title = paste0("Category",  "<b>", "\n", unique(da_plot$denominazione_regione), "</b>", "\n"), 
             showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  }
  if(type == "Age"){
    da_plot <- v_data %>% 
      group_by(denominazione_regione, fascia_anagrafica) %>% 
      summarise_if(is.numeric, sum) %>% 
      dplyr::select(denominazione_regione, fascia_anagrafica, starts_with("sesso")) %>% 
      group_by(denominazione_regione, fascia_anagrafica) %>% 
      nest() %>% ungroup() %>% 
      mutate(TOT_Age = map_dbl(.$data, rowSums), fascia_anagrafica = 
               factor(fascia_anagrafica, levels = rev(unique(v_data$fascia_anagrafica)), ordered = T)) %>% 
      dplyr::select(-data) %>% 
      group_by(denominazione_regione) %>% 
      mutate(TOT = sum(TOT_Age), percentage = round(TOT_Age/TOT*100, 2))
    if(!is.null(reg)){
      da_plot %<>% filter(denominazione_regione == reg)
    } else{
      da_plot %<>%
        group_by(fascia_anagrafica) %>% 
        summarise(TOT_Age = sum(TOT_Age), TOT = sum(TOT), percentage = round(TOT_Age/TOT*100, 2)) %>% 
        mutate(denominazione_regione = "Italy") %>% 
        dplyr::select(denominazione_regione, fascia_anagrafica:percentage)
    }
    p_out <- da_plot %>% 
      plot_ly(labels = ~fascia_anagrafica, values = ~percentage, type = 'pie', textposition = 'inside',
              textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text', hole = 0.6, sort = F,
              text = ~paste(fascia_anagrafica, "\n", percentage, '%'),
              marker = list(colors = RColorBrewer::brewer.pal(9, "Paired"), line = list(color = '#FFFFFF', width = 1))) %>% 
      layout(title = paste0("Age class",  "<b>", "\n", unique(da_plot$denominazione_regione), "</b>", "\n"), 
             showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  }
  return(p_out)
  
}

show_vaccine <- function(v_data){
  #v_data = isolate(v_data_reg())
  dd <- unique(v_data$data) %>% as.Date()
  # Età
  tab_eta <- v_data %>% group_by(fascia_anagrafica) %>% 
    summarise(`Administered doses` = sesso_maschile + sesso_femminile) %>% 
    rename(`Age class'` = fascia_anagrafica) %>% 
    gather(Key, Value, -`Administered doses`)
  # Sesso
  tab_sesso <- tibble(`Administered doses` = v_data %>% dplyr::select(starts_with("sesso")) %>% colSums() %>% as.numeric(),
                      Key = c("Gender", "Gender"),
                      Value = c("Male", "Female"))
  # Categoria
  nms <- gsub(replacement = "", x = colnames(v_data %>% dplyr::select(starts_with("categoria"))), pattern = "categoria") %>% 
    gsub(pattern = "_", replacement = " ", x = .) %>% 
    stri_trans_totitle() %>% str_trim()
  
  tab_categoria <- tibble(`Administered doses` = v_data %>% dplyr::select(starts_with("categoria")) %>% colSums() %>% as.numeric(),
                          Key = rep("Category", length(nms)),
                          Value = nms)
  
  tab_out <- bind_rows(tab_eta, tab_sesso, tab_categoria)
  return(tab_out)
  
}

show_vaccine_reg <- function(v_data, resdata, TotPop){
  
  tab_out <- v_data %>% left_join(resdata, by = c("denominazione_regione" = "Territorio")) %>% 
    mutate(
      `Admin. (x 1000 res.)` = round(dosi_somministrate/totale*1000,2)
    ) %>% 
    rename(Region = denominazione_regione, Delivered = dosi_consegnate, `Admin.` = dosi_somministrate,
           `Admin. (%)` = percentuale_somministrazione) %>% 
    dplyr::select(-data, -totale)
  tab_out <- bind_rows(tibble(Region = "Italy", 
                              Delivered = round(sum(tab_out$Delivered)), 
                              `Admin.` = round(sum(tab_out$`Admin.`)),
                              `Admin. (%)` = round(`Admin.`/Delivered*100,2),
                              `Admin. (x 1000 res.)` = round(`Admin.`/TotPop*1000,2)),
                       tab_out)
  
  return(tab_out)
  
}


# ICU section -------------------------------------------------------------
dataprep_terapie <- function(dftoprep, resdata, capICULast){
  
  da <- dftoprep %>% arrange(denominazione_regione, data) %>% dplyr::select(data, denominazione_regione, `Intensive care`) %>% 
    left_join(resdata, by = c("denominazione_regione" = "Territorio")) %>% 
    mutate(data = as.numeric(unclass(factor(data))), denominazione_regione = factor(denominazione_regione)) %>% 
    rename(ti = data, region = denominazione_regione, icu = `Intensive care`, residents = totale) 
  
  mx <- max(da$ti)-15
  da <- da[da$ti>mx,]
  da$ti <- da$ti-mx
  
  da.pred <- da[which(da$ti==max(da$ti)),]
  da.pred$ti <- max(da$ti)+1
  da <- da[order(da$region,da$ti),]
  da.pred <- da.pred[order(da.pred$region),]
  
  # Capacity
  #da.pred$capienza <- capICU$`PL in Terapia Intensiva`
  da.pred$capienza <- capICULast
  da$capienza <- rep(da.pred$capienza,each=max(da$ti))
  
  return(list(da = da, dapred = da.pred))
}

fup <- function(dat){
  y <- dat$icu
  x <- cbind(dat$ti,dat$ti^2/100,dat$ti^3/250)
  tsw3 <- tsglm(y,xreg=x[,1:3],link="log",dist="poisson")
  tsw2 <- tsglm(y,xreg=x[,1:2],link="log",dist="poisson")
  tsw1 <- tsglm(y,xreg=x[,1],link="log",dist="poisson")
  tsw <- tsglm(y,link="log",dist="poisson")
  newx <- NULL
  if(BIC(tsw)>BIC(tsw1)){
    tsw <- tsw1; newx=data.frame(ti=max(x[,1])+1)
  }
  if(BIC(tsw)>BIC(tsw2)){
    tsw <- tsw2
    newx <- data.frame(ti=max(x[,1])+1,ti2=(max(x[,1])+1)^2/100)
  }
  if(BIC(tsw)>BIC(tsw3)){
    tsw <- tsw3
    mx <- max(x[,1])+1
    newx <- data.frame(ti=mx,ti2=mx^2/100,ti3=mx^3/250)
  }
  
  pr <- predict(tsw, newxreg=newx,level=1-0.01)
  
  outvec <- c(pr$pred,pr$interval)
  names(outvec) <- c("est", "lb", "ub")
  return(outvec)
}

modello_terapia_intensiva <- function(dat = da, dattopred = da.pred){
  
  fit2 <- glmer(icu~ti+I(ti^2/100)+offset(log(residents))+((1+ti)|region)+((0+I(ti^2/100))|region),data=dat,family=poisson)
  pr2 <- exp(predict(fit2,dattopred))
  
  ba <- dat %>% group_split(region) %>% sapply(FUN = fup, simplify = T) %>% t
  
  d2 <- dat[dat$ti<max(dat$ti),]
  fit.loss <- glmer(icu~ti+I(ti^2/100)+offset(log(residents))+((1+ti)|region)+((0+I(ti^2/100))|region),data=d2,family=poisson)
  pr.loss <- exp(predict(fit.loss,d2))
  ba.loss <- d2 %>% group_split(region) %>% sapply(FUN = fup, simplify = T) %>% t
  
  do <- function(x,b,l,da){
    w <- exp(x)/(1+exp(x))
    abs(b*w+l*(1-w)-da$icu)^2
  }
  
  optW <- rep(NA,nrow(dattopred))
  for(j in 1:length(optW)) {
    op <- optimize(function(x) do(x ,ba.loss[,1],pr.loss,d2)[j],c(-15,15))
    optW[j] <- exp(op$min)/(1+exp(op$min))
  }
  
  jnk <- list(preds=round(pr2*(1-optW)+ba[,1]*optW),tsci=ba[,-1],optW=optW)
  
  pred <- data.frame(region=levels(dat$region),prediction=jnk$preds)
  
  cl <- makeCluster(3, type = "SOCK")
  registerDoSNOW(cl)
  
  res2 <- matrix(NA, nrow = 500, ncol = nrow(dattopred))
  res2 <- foreach(j = 1:500, .packages = "lme4", .combine = "rbind")  %dopar% {
    ws <- sample(nrow(dat),nrow(dat),replace=TRUE)
    daws <- dat[ws,]
    fit2 <- glmer(icu~ti+I(ti^2/100)+offset(log(residents))+((1+ti)|region)+((0+I(ti^2/100))|region),data=daws,family=poisson)
    pr2 <- exp(predict(fit2,dattopred))
    return(pr2)
  }
  
  stopCluster(cl)
  
  qu12 <- apply(res2, 2, quantile, probs = c(0.01/2, 1-0.01/2), na.rm=TRUE)
  pred$prLow.Bonf <- apply(cbind(round(qu12[1,]),jnk$tsci[,1]),1,min)
  pred$prUp.Bonf <- apply(cbind(round(qu12[2,]),jnk$tsci[,2]),1,max)
  pred$prUp.Bonf <- pmin(pred$prUp.Bonf,dattopred$capienza)
  pr.oggi <- pred
  pr.oggi$capienza <- dattopred$capienza 
  
  colnames(pr.oggi) <- c("Region", "Prediction", "Lower bound", "Upper bound", "Capacity")
  
  return(pr.oggi)
  
}

tab_tomor_ICU <- function(tab1, tab2){
  capts_out <-  htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: left;',
    'Table: ', htmltools::em(HTML(paste0("ICU predictions by region, 99% confidence bounds and overall capacity. Best case, HSS Pressure and Worst case are obtained by dividing the lower bounds, the prediction and the upper bounds by the capacity. THey reflect the pressure on the regional health care system.<br/>'Prediction' is yellow if an improvement is expected w.r.t. yesterday, viceversa is for the red. 'HSS Pressure (%)' is coloured proportionally to the number of occupancies.")))
  )
  #   
  varwrtoday <- tab1$Prediction - (tab2 %>% filter(DataPred == max(DataPred)) %$% Prediction ) 
  tabICU <- tab1 %>%
    mutate(`Best case (%)` = round(`Lower bound`/Capacity*100,2),
           `HSS Pressure (%)` = round(Prediction/Capacity*100, 2),
           `Worst case (%)` = round(`Upper bound`/Capacity*100,2),
           IsBetterPred = varwrtoday) %>%
    dplyr::select(-DataPred) %>%
    mutate(
      Region = as.character(Region),
      Region = ifelse(Region == "TrentinoAltoAdige", "Trentino-Alto Adige", Region),
      Region = ifelse(Region == "Friuli V. G.", "Friuli Venezia Giulia", Region)
    )

  out <- tabICU %>%
    datatable(rownames = F, caption = capts_out,
              options = list(dom = 'tpl', pageLength = 10, scrollX = T, lengthMenu = c(10, 15, 20),
                             columnDefs = list(list(className = 'dt-center', targets = "_all"), list(visible = F, targets = 8)))) %>%
    formatStyle(columns = "Prediction", valueColumns = "IsBetterPred",fontWeight = "bold",
                backgroundColor = styleInterval(0, c("lemonchiffon", "salmon")), color = "black") %>%
    formatStyle(columns = "HSS Pressure (%)", background = styleColorBar(c(0,100), "salmon"), backgroundSize = '98% 88%',
                backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>%
    formatStyle(columns = "Region", fontWeight = "bold")
  
  return(out)
}

prep_bar_ICU <- function(tab1, datireg){
  dataICUbar <- tab1 %>%
    mutate(Region = as.character(Region),
           Region = ifelse(Region == "Emilia Romagna", "Emilia-Romagna", Region),
           Region = ifelse(Region == "Friuli V. G.", "Friuli Venezia Giulia", Region),
           Region = ifelse(Region == "TrentinoAltoAdige", "Trentino-Alto Adige", Region)) %>%
    rename(denominazione_regione = Region, data = DataPred) %>%
    left_join(datireg %>% spread(Key, Value) %>% filter(data <= max(tab1$DataPred)) %>% dplyr::select(data, starts_with("Int"), denominazione_regione),
              by = c("denominazione_regione", "data"))
  return(dataICUbar)
}

barplot_ICU <- function(tab1,datasel){
  
  out <- tab1 %>% filter(data == datasel) %>%
    plot_ly(x = ~denominazione_regione, y = ~`Intensive care`, type = "bar", name = "Observed", marker = list(color = "firebrick")) %>%
    add_trace(y = ~Prediction, name = "Predicted", marker = list(color = "darkorange")) %>%
    layout(title = paste0("Intensive care units on the ", paste_eng_date(datasel)), legend = list(x = 0, y = 1), xaxis = list(title = ""), yaxis = list(title = "ICUs"))
  
  return(out)
  
}
