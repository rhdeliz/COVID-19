#Written by Rafael Deliz-Aguirre
#For educational purposes only

#Required columns
#" Location"
# "State"
# "Population"
# "Date"
# "Cases"

#Optional columns
# "FIPS"
# "Deaths"

#INPUT
RUN_FOR = "Texas"
#RUN_FOR = "US Counties"
#RUN_FOR = "US States"
LOCATIONS = NA #NA if none
STATE = NA #NA if none
  
PredictionDays = 60

if(RUN_FOR == "Texas") {
  #https://dshs.texas.gov/coronavirus/additionaldata.aspx
  DIRECTORY = "~/COVID-19/Texas/"
  TABLE = "Texas COVID-19 Case Count Data by County.xlsx"
}

if(RUN_FOR == "US Counties") {
  DIRECTORY = "~/COVID-19/USA"
  
  # Puerto Rico Population
  # https://www.census.gov/data/datasets/time-series/demo/popest/2010s-total-puerto-rico-municipios.html
  PR_POPULATION = "prm-est2019-annres.xlsx"
  
  # US Counties
  # https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html#par_textimage_739801612
  COUNTIES_POPULATION = "co-est2019-annres.xlsx"
  
  # COVID-19 Cases
  # https://github.com/nytimes/covid-19-data
  CASES = "us-counties.csv"
}

if(RUN_FOR == "US States") {
  DIRECTORY = "~/COVID-19/USA"
  
  # States Population
  # https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html#par_textimage_1574439295
  STATES_POPULATION = "nst-est2019-01.xlsx"
  
  # COVID-19 Cases
  # https://github.com/nytimes/covid-19-data
  CASES = "us-states.csv"
}

# LIBRARIES
library(openxlsx)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(parallel)
library(stringr)
library(sf)
library(cowplot)
library(rcartocolor)
library(spData)
library(tigris)
#library(reshape2) #Ensure installed

# CUSTOM FUNCTIONS
#For getting characters n letters left
substrLeft = function(text, num_char) {
  substr(text, 1, num_char)
}
#For getting characters n letters right
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#Detect local minima
localMinima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

setwd(DIRECTORY)

if(RUN_FOR == "US Counties") {
  # US Counties
  USCountiesPopulation <- 
    read.xlsx(
      COUNTIES_POPULATION,
      startRow = 5
    )
  
  USCountiesPopulation <- USCountiesPopulation[1:3142,]
  
  USCountiesPopulation <-
    USCountiesPopulation %>%
    select(
      United.States,
      "328239523"
    ) %>%
    mutate(
      United.States = str_replace(United.States, ".", ""),
      County = gsub("(.*),.*", "\\1", United.States),
      State = substrRight(United.States, nchar(United.States) - nchar(County) - 2),
      County = str_replace(County, " County", ""),
      County = str_replace(County, " Parish", ""),
      County = str_replace(County, "New York", "New York City")
    ) %>%
    select(
      State,
      County,
      "328239523"
    )
  
  names(USCountiesPopulation) <- c("State", "County", "Population")
  
  # Puerto Rico Population
  PuertoRicoPopulation <- 
    read.xlsx(
      PR_POPULATION,
      startRow = 4
    )
  
  # Select county name and population
  PuertoRicoPopulation <-
    PuertoRicoPopulation %>%
    select(
      X1,
      "2019"
    ) %>%
    mutate(
      X1 = str_replace(X1, ".", ""),
      X1 = str_replace(X1, " Municipio, Puerto Rico", ""),
      X1 = str_replace(X1, "á", "a"),
      X1 = str_replace(X1, "é", "e"),
      X1 = str_replace(X1, "í", "i"),
      X1 = str_replace(X1, "ó", "o"),
      X1 = str_replace(X1, "ú", "u"),
      X1 = str_replace(X1, "ñ", "n"),
      X1 = str_replace(X1, "ü", "u"),
      State = "Puerto Rico"
    ) %>%
    select(
      State,
      X1,
      "2019"
    )
  
  names(PuertoRicoPopulation) <- c("State", "County", "Population")
  
  PuertoRicoPopulation <- PuertoRicoPopulation[2:79,]
  
  USCountiesPopulation <- rbind(USCountiesPopulation, PuertoRicoPopulation)
  remove(PuertoRicoPopulation)
  
  #Assign unique ID to county
  USCountiesPopulation <-
    USCountiesPopulation %>%
    mutate(
      Location = paste(County, State, sep = ", ")
    )
  
  # COVID Data
  Cases <-
    read.csv(CASES, header = T)
  
  names(Cases) <- c("Date", "County", "State", "FIPS", "Cases", "Deaths")
  
  Cases <-
    Cases %>%
    mutate(
      Location = paste(County, State, sep = ", ")
    ) %>%
    select(-c(
      "County",
      "State"
    ))
  
  Input <- merge(USCountiesPopulation, Cases, by = "Location")
  remove(USCountiesPopulation, Cases)
  
  Input <-
    Input %>%
    arrange(
      State,
      Location,
      Date
    ) %>%
    select(-c(
      County
    ))
}

if(RUN_FOR == "US States") {
  # States Population
  StatesPopulation <- 
    read.xlsx(
      STATES_POPULATION,
      startRow = 4
    )
  
  # Select state name and population
  StatesPopulation <-
    StatesPopulation %>%
    select(
      X1,
      "2019"
    ) %>%
    mutate(
      X1 = str_replace(X1, ".", ""),
      X1 = str_replace(X1, "uerto Rico","Puerto Rico")
    )
  
  names(StatesPopulation) <- c("State", "Population")
  
  StatesPopulation <- StatesPopulation[6:57,]
  
  # COVID Data
  Cases <-
    read.csv(CASES, header = T)
  
  names(Cases) <- c("Date", "State","FIPS","Cases", "Deaths")
  
  Input <- merge(Cases, StatesPopulation, by = "State")
  remove(Cases, StatesPopulation)
  
  Input <-
    Input %>%
    mutate(
      Location = State,
      Location = State
    ) %>%
    arrange(
      State,
      Date
    )
}

if(RUN_FOR == "Texas") {
  Input <-
    read.xlsx(
      TABLE,
      startRow = 3,
      rowNames = T
    )
  
  Dates <- colnames(Input)
  Dates <- str_replace(Dates, "Cases.", "")
  Dates <- gsub("[*].*$","",Dates)
  Dates <- gsub("&#13;","",Dates)
  Dates <- gsub("[.]","",Dates)
  Dates <- paste(Dates, "-2020", sep="")
  Dates <- as.Date(Dates, "%m-%d-%Y") 
  colnames(Input) <- Dates
  
  Population = Input[1:254, 1]
  Location = rownames(Input)[1:254]
  
  Population <- cbind(Location, Population)
  remove(Location)
  
  Input <- Input[1:254, 2:(NCOL(Input)-1)]
  Input <- t(Input)
  Input <- reshape2::melt(Input)
  names(Input) <- c("Date","Location", "Cases")
  
  Input <- merge(Input, Population, by = "Location")
  remove(Population)
  
  Input$FIPS = NA
  Input$State = "Texas"
  Input$Deaths = NA
}

ReferenceTable <-
  Input %>%
  select(
    Location,
    State,
    Population,
    FIPS
  ) %>%
  distinct()

if(is.na(LOCATIONS)){
  LOCATIONS <- unique(Input$Location)
} else{
  LOCATIONS
}

if(is.na(STATE)){
  STATE <- unique(Input$State)
} else{
  STATE
}

Input <- 
  Input %>%
  filter(
    Location %in% LOCATIONS,
    State %in% STATE
  )

#CASES
CasesLocationFx <- function(LocationX){
  tryCatch({
    
    GeographicLocation = ReferenceTable$Location[LocationX]
    
    LocationInput <-
      Input %>%
      filter(
        Location == GeographicLocation,
        Cases > 0
      ) %>%
      mutate(
        Date = as.Date(Date)
      ) %>%
      arrange(
        Date
      ) %>%
      mutate(
        Day = Date - min(Date),
        Day = ifelse(is.na(Day), 0, Day),
        NewCases = Cases - lag(Cases),
        NewCases = NewCases,
        NewCases = ifelse(is.na(NewCases), 1, NewCases),
        Population = as.numeric(Population),
        CasesRatio = Cases/Population*100000,
        NewCasesRatio = NewCases/Population*100000
      )
    
    LocationInput 
  },  error = function(e) {print(paste("ERROR with LocationFx. Location =", Location))}
  )
}
#Loop
nLocations <- 1:NROW(ReferenceTable)
CasesExport <- mclapply(nLocations, CasesLocationFx)
CasesExport <- CasesExport[(which(sapply(CasesExport,is.list), arr.ind=TRUE))]
CasesExport <- do.call(bind_rows, CasesExport)

#Resort data
CasesExport <-
  CasesExport%>%
  select(
    Location,
    State,
    Population,
    Date,
    Day,
    everything()
  )

#Save actual cases table
write.csv(CasesExport, "CasesTable.csv", row.names = F)

#Ranking of locations
CasesExportReport <-
  CasesExport %>%
  group_by(
    Location,
    Population
  ) %>%
  summarize(
    Cases = max(Cases),
    CasesRatio = max(CasesRatio),
    MaxNewCases = max(NewCases),
    MaxNewCasesRatio = max(NewCasesRatio)
  ) %>%
  ungroup() %>%
  mutate(
    CasesRank = rank(-Cases),
    CasesRatioRank = rank(-CasesRatio),
    MaxNewCasesRank = rank(-MaxNewCases),
    MaxNewCasesRatioRank = rank(-MaxNewCasesRatio),
  )

write.csv(CasesExportReport, "Report.csv", row.names = F)

#PREDICTIONS
PredictionLocationFx <- function(LocationX){
  tryCatch({
   
    GeographicLocation = ReferenceTable$Location[LocationX]
    
    print(paste("Working on", GeographicLocation, "LocationX =", LocationX))
    
    FIPS = ReferenceTable$FIPS[LocationX]
    Population = ReferenceTable$Population[LocationX]
    State = ReferenceTable$State[LocationX]
    
    LocationInput <-
      CasesExport %>%
      filter(
        Location == GeographicLocation
      ) %>%
      mutate(
        Population = as.numeric(Population),
        SmoothedCases = rollmean(Cases, 3, align = c("center"), fill = NA),
        SmoothedCases = rollmean(SmoothedCases, 5, align = c("center"), fill = NA),
        SmoothedCases = rollmean(SmoothedCases, 11, align = c("center"), fill = NA),
        SmoothedCases = rollmean(SmoothedCases, 3, align = c("center"), fill = NA),
        SmoothedCases = rollmean(SmoothedCases, 5, align = c("center"), fill = NA),
        SmoothedCases = rollmean(SmoothedCases, 11, align = c("center"), fill = NA),
        SmoothedCases = rollmean(SmoothedCases, 3, align = c("center"), fill = NA),
        SmoothedCases = rollmean(SmoothedCases, 5, align = c("center"), fill = NA),
        SmoothedCases = rollmean(SmoothedCases, 11, align = c("center"), fill = NA),
        SmoothedNewCases = SmoothedCases - lag(SmoothedCases)
      ) %>%
      select(
        Location,
        State,
        Population,
        Date,
        Day,
        Cases,
        SmoothedNewCases
      )
    
    WaveDay <- LocationInput %>%drop_na()
    WaveDay <- localMinima(WaveDay$SmoothedNewCases)
    WaveDay <-  WaveDay + 27
    WaveDay <-
      if(NROW(WaveDay)==1) {
        if(WaveDay != 28){
          WaveDay = WaveDay
        } else{
          WaveDay = NA
        }
      } else{
        WaveDay = WaveDay[2:NROW(WaveDay)]
      }
    
    WaveFx <- function(WaveX){
      tryCatch({
        WaveStart <- ifelse(WaveX==0, 1, WaveDay[WaveX])
        WaveEnd <- ifelse(is.na(WaveDay[WaveX+1]), max(LocationInput$Day)+1+PredictionDays, WaveDay[WaveX+1])
        
        PredictionsLocationInput <-
          LocationInput %>%
          filter(
            Day >= WaveStart,
            Day < WaveEnd
          )
        
        StartDay <- min(PredictionsLocationInput$Day)
        StartCases <- min(PredictionsLocationInput$Cases)
        
        PredictionsLocationInput <-
          PredictionsLocationInput %>%
          mutate(
            Day = Day - StartDay + 1,
            Cases = Cases - StartCases + 1
          )
        
        Start <- min(PredictionsLocationInput$Date)
        
        ZeroCases <- matrix(rep(0, len = (NCOL(PredictionsLocationInput)*14)), ncol = NCOL(PredictionsLocationInput))
        ZeroCases <- as_tibble(ZeroCases)
        names(ZeroCases) <- names(PredictionsLocationInput)
        ZeroCases$Day <- (min(PredictionsLocationInput$Day)-14):(min(PredictionsLocationInput$Day)-1)
        
        DailyPredictionLocationInput <- rbind(ZeroCases, PredictionsLocationInput)
        remove(ZeroCases)
        
        DailyPredictionLocationInput$Day = DailyPredictionLocationInput$Day+15
        
        #Cases Prediction
        DOSE_RESPONSE <- drc::drm(Cases ~ Day, data = DailyPredictionLocationInput, fct = drc::LL.4())
        PredictionCases <- expand.grid(conc=exp(seq(log(10), log(320), length=1000)))
        pm2 <- predict(DOSE_RESPONSE, newdata = PredictionCases, interval="confidence")
        PredictionCases$Cases <- pm2[,1]
        PredictionCases$Cases <- PredictionCases$Cases + StartCases
        PredictionCases$CasesMin <- pm2[,2]
        PredictionCases$CasesMin <- PredictionCases$CasesMin + StartCases
        PredictionCases$CasesMax <- pm2[,3]
        PredictionCases$CasesMax <- PredictionCases$CasesMax + StartCases
        PredictionCases$Day <- PredictionCases$conc-15 + StartDay
        PredictionCases$conc <- NULL
        PredictionCases$Date <- Start + PredictionCases$Day
        
        PredictionCasesTable <-
          PredictionCases %>%
          mutate(
            Day = round(Day)
          ) %>%
          filter(
            Day >= WaveStart,
            Day < WaveEnd
          ) %>%
          group_by(
            Day
          ) %>%
          summarize(
            Cases = mean(Cases),
            CasesMin = min(CasesMin),
            CasesMax = max(CasesMax),
            Cases = Cases,
            Wave = WaveX + 1,
            Location = GeographicLocation
          )
        
        PredictionCasesTable$Date <- min(LocationInput$Date) + PredictionCasesTable$Day
        
        PredictionCasesTable <-
          PredictionCasesTable %>%
          mutate(
            Population = as.numeric(Population),
            CasesRatio = Cases/Population*100000,
            CasesRatioMin = CasesMin/Population*100000,
            CasesRatioMax = CasesMax/Population*100000,
            
            NewCases = Cases - lag(Cases),
            NewCasesMin = CasesMin - lag(CasesMin),
            NewCasesMax = CasesMax - lag(CasesMax),
            
            NewCasesRatio = CasesRatio - lag(CasesRatio),
            NewCasesRatioMin = CasesRatioMin - lag(CasesRatioMin),
            NewCasesRatioMax = CasesRatioMax - lag(CasesRatioMax),
          ) %>%
          select(
            Date,
            Day,
            everything()
          )
        
        PredictionCasesTable
      },  error = function(e) {print(paste("ERROR with WaveFx WaveX =", WaveX))}
      )
    }
    #Loop
    nWaves <- 0:NROW(WaveDay)
    WavesTable <- mclapply(nWaves, WaveFx)
    WavesTable <- WavesTable[(which(sapply(WavesTable,is.list), arr.ind=TRUE))]
    WavesTable <- do.call(bind_rows, WavesTable)
    
    PredictionCasesExport <-
      WavesTable %>%
      mutate(
        Population = Population,
        State = State,
        FIPS = FIPS
      ) %>%
      select(
        Location,
        State,
        FIPS,
        Population,
        Date,
        Day,
        Wave,
        CasesMin,
        Cases,
        CasesMax,
        NewCasesMin,
        NewCases,
        NewCasesMax,
        CasesRatioMin,
        CasesRatio,
        CasesRatioMax,
        NewCasesRatioMin,
        NewCasesRatio,
        NewCasesRatioMax
      )
    
    PredictionCasesExport 
  },  error = function(e) {print(paste("ERROR with LocationFx. Location =", LocationX))}
  )
}
PredictionCasesExport <- mclapply(nLocations, PredictionLocationFx)
PredictionCasesExport <- PredictionCasesExport[(which(sapply(PredictionCasesExport,is.list), arr.ind=TRUE))]
PredictionCasesExport <- do.call(bind_rows, PredictionCasesExport)

PredictionCasesExport <-
  PredictionCasesExport %>%
  group_by(
    Location
  ) %>%
  mutate(
    NewCasesMin = rollmean(NewCasesMin, 3, align = c("center"), fill = NA),
    NewCasesMin = rollmean(NewCasesMin, 7, align = c("center"), fill = NA),
    
    NewCases = rollmean(NewCases, 3, align = c("center"), fill = NA),
    NewCases = rollmean(NewCases, 7, align = c("center"), fill = NA),
    
    NewCasesMax = rollmean(NewCasesMax, 3, align = c("center"), fill = NA),
    NewCasesMax = rollmean(NewCasesMax, 7, align = c("center"), fill = NA),
    
    NewCasesRatioMin = rollmean(NewCasesRatioMin, 3, align = c("center"), fill = NA),
    NewCasesRatioMin = rollmean(NewCasesRatioMin, 7, align = c("center"), fill = NA),
    
    NewCasesRatio = rollmean(NewCasesRatio, 3, align = c("center"), fill = NA),
    NewCasesRatio = rollmean(NewCasesRatio, 7, align = c("center"), fill = NA),
    
    NewCasesRatioMax = rollmean(NewCasesRatioMax, 3, align = c("center"), fill = NA),
    NewCasesRatioMax = rollmean(NewCasesRatioMax, 7, align = c("center"), fill = NA)
    
  )

write.csv(PredictionCasesExport, "PredictionsTable.csv", row.names = F)
