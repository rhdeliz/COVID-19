#Written by Rafae Deliz-Aguirre
#For showing cases, projection over time

DIRECTORY = "~/COVID-19/Texas"
#LOCATIONS = c("Puerto Rico", "Texas", "New York", "California")
#LOCATIONS = c("Webb", "Dallas","Harris","Bexar")
LOCATIONS = c("Webb")
#Load libraries
library(stringr)
library(gganimate)
library(av)
library(dplyr)
library(parallel)

#Import data
setwd(DIRECTORY)
CasesExport <- read.csv("CasesTable.csv", header = T)
PredictionCasesExport <- read.csv("PredictionsTable.csv", header = T)

#Create export folder
FOLDER = paste(DIRECTORY, "CasesVideo", sep ="/")
dir.create(FOLDER)
setwd(FOLDER)

#Wave Date
WaveDate <-
  PredictionCasesExport %>%
  filter(
    Location %in% LOCATIONS
  ) %>%
  filter(
    Wave > 1
  ) %>%
  group_by(
    Location,
    Wave
  ) %>%
  summarize(
    Day = min(Day),
    Date = min(Date),
    Date = as.Date(Date)
  )

CasesExport <-
  CasesExport %>%
  filter(
    Location %in% LOCATIONS
  ) %>%
  mutate(
    NewCases = ifelse(NewCases<0, NA, NewCases),
    NewCasesRatio = ifelse(NewCasesRatio<0, NA, NewCasesRatio),
    Date = as.Date(Date)
  )

PredictionCasesExport <-
  PredictionCasesExport %>%
  filter(
    Location %in% LOCATIONS
  ) %>%
  mutate(
    CasesMin = ifelse(CasesMin < 0, 0, CasesMin),
    Cases = ifelse(Cases < 0, 0, Cases),
    CasesMax = ifelse(CasesMax < 0, 0, CasesMax),
    Date = as.Date(Date)
  )

DayFx <- function(DayX) {
  
  PlottingTable <-
    CasesExport %>%
    filter(
      Day <= DayX
    )
  
  PredictionCasesTable <-
    PredictionCasesExport %>%
    filter(
      Day <= DayX
    )
  
  Test <- ifelse(max(CasesExport$Day) < DayX, T, F)

  DayCases <-
    ifelse(
      Test == F,
      paste(str_pad(max(PlottingTable$Cases), 4, pad = "0"), " Cases + ", sep = ""),
      paste(str_pad(round(max(PredictionCasesTable$Cases, na.rm = T)), 4, pad = "0"), " Predicted Cases + ", sep = "")
    )

  NewCases <-
    ifelse(
      Test == F,
      paste(str_pad(round(tail(PlottingTable$NewCases, n= 1)), 3, pad = "0"), " New Cases", sep = ""),
      paste(str_pad(round(tail(PredictionCasesTable$NewCases, n = 1, na.rm = T)), 3, pad = "0"), " Predicted New Cases", sep = "")
    )

  DayN = ifelse(DayX < 13, 0, DayX - 12)
  
  PlotTitle <-
    paste(
      "Day ", str_pad(DayN, 3, pad = "0"), ": ",
      DayCases,
      NewCases,
      sep = "")
  
  ggplot() +
    geom_ribbon(
      data = PredictionCasesTable,
      aes(
        x = Date,
        ymin = CasesMin,
        ymax = CasesMax
      ),
      alpha=0.2) +
    geom_line(
      data = PredictionCasesTable,
      aes(
        x = Date,
        y = Cases,
        group = Location,
        color = "Predicted Cases"
      )
    ) +
    geom_line(
      data = PlottingTable,
      aes(
        x = Date,
        y = Cases,
        group = Location,
        color = "Actual Cases"
      ),
    ) +
    geom_vline(
      data = WaveDate,
      aes(
        xintercept = Date
      )
    ) +
    scale_colour_manual(values=c("black","red"))+
    scale_y_continuous(breaks= scales::pretty_breaks())+
    theme_classic() +
    labs(
      title = PlotTitle,
      subtitle = "Twitter @RHDeliz • delizaguirre.com/corona",
      y = "Cases",
      x = "Date",
      color = "Data"
    ) +
    facet_wrap(
      ~Location
    ) +
    theme(
      legend.position = "bottom"
    ) +
    ggsave(
      paste(str_pad(DayX, 4, pad = "0"), ".png"),
      height = 4.5,
      width = 8
    )
}
tictoc::tic()
nDays = 1:max(PredictionCasesExport$Day)
Cases <- mclapply(nDays, DayFx)
tictoc::toc()

av_encode_video(list.files(FOLDER, "*.png"), framerate = 14, output = "Cases.mp4")

