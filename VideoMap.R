#Written by Rafael Deliz-Aguirre
#For showing cases and projection over time in a map

#DON'T FORGET TO PASTE CSVs

DIRECTORY = "~/COVID-19/USA" #Or USA
LOCATIONS = "Texas"
LOCATIONS = NA
#TYPE = "County"
TYPE = "State"
LEGEND_POSITION = "right"
HEIGHT = 4
WIDTH = 2

library(av)
library(colorspace)
library(cowplot)
library(dplyr)
library(ggplot2)
library(ggnewscale)
library(openxlsx)
library(parallel)
library(sf)    
library(tidyr)
library(tidyverse)
library(zoo)
library(lubridate)

setwd(DIRECTORY)
CasesExport <- read.csv("CasesTable.csv", header = T)
PredictionCasesExport <- read.csv("PredictionsTable.csv", header = T)

if(TYPE == "County") {
  Coordinates <-
    tigris::counties(LOCATIONS, cb = T, resolution = "500k")
}

if(TYPE == "State") {
  Coordinates <-
    tigris::states(cb = T, resolution = "500k")
}

if(is.na(unique(CasesExport$FIPS))) {
  CasesExport <- merge(Coordinates, CasesExport, by.x = "NAME", by.y = "Location")
  PredictionCasesExport <- merge(Coordinates, PredictionCasesExport, by.x = "NAME", by.y = "Location")
} else {
  CasesExport <- merge(Coordinates, CasesExport, by.x = "GEOID", by.y = "FIPS")
  PredictionCasesExport <- merge(Coordinates, PredictionCasesExport, by.x = "GEOID", by.x = "FIPS")
}

PredictionCasesExport <-
  PredictionCasesExport %>%
  filter(
    Date > max(CasesExport$Date)
  )

Data <- bind_rows(CasesExport, PredictionCasesExport)

CASES_MAX = max(Data$Cases)
CASES_MAX = round(CASES_MAX)

FOLDER = paste(DIRECTORY, "Video", sep ="/")
dir.create(FOLDER)
setwd(FOLDER)

ColorScale <- c("#e5e5e5")
names(ColorScale) <- c("Data Not Available")

ActualDayFx <- function(DayX) {
  
  Plot <-
    Data %>%
    filter(
      Day == DayX
    )
  
  #Date
  DATE <- Plot$Date[1]
  DATE <- as.Date(DATE)
  DATE <- format(DATE,"%B %d, %Y")
  
  #Missing in all
  ALL_LOCATIONS <- unique(Coordinates$NAME)
  MISSING <- match(ALL_LOCATIONS, unique(Plot$NAME))
  MISSING_LOCATIONS <- cbind(ALL_LOCATIONS, MISSING)
  MISSING_LOCATIONS <- as.data.frame(MISSING_LOCATIONS)
  
  MISSING_LOCATIONS <-
    MISSING_LOCATIONS %>%
    mutate(
      MISSING = is.na(MISSING)
    ) %>%
    filter(
      MISSING == TRUE
    ) %>%
    arrange(
      ALL_LOCATIONS
    )
  
  MissingAll <- merge(Coordinates, MISSING_LOCATIONS,  by.x = "NAME", by.y = "ALL_LOCATIONS")
  
  ggplot(
  ) +
    geom_sf(
      data = MissingAll,
      aes(
        fill = "Data Not Available"
      )
    ) +
    scale_fill_manual(
      values = ColorScale,
      name = ""
    ) +
    new_scale_fill() +
    geom_sf(
      data = Plot,
      aes(
        fill = CasesRatio/1000
      )
    ) +
    # scale_x_continuous(
    #   limits = c(-67.2, -65.3)
    # ) +
    scale_fill_distiller(
      palette = "PuRd",
      direction = 1,
      limits = c(0, CASES_MAX/1000),
      #breaks = c(0, 1,2, 3),
      #labels = c(0, 1000,2000, 3000),
      #labels = c(0, 750, paste("≥", round(CASES_MAX))),
      na.value = "#67001f",
      name = "COVID-19 Positive Cases\n(per 1000 people)"
    ) +
    labs(
      title = paste(LOCATIONS, "Actual Cases on", DATE),
      subtitle = "Twitter @RHDeliz • delizaguirre.com/corona"
    ) +
    theme_classic(
      base_size = 24
    ) +
    theme(
      legend.position = LEGEND_POSITION,
      legend.key.height = unit(HEIGHT,"line"),
      legend.key.width = unit(WIDTH,"line"),
      axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank()
    ) +
    ggsave(
      paste(str_pad(DayX, 4, pad = "0"), ".png"),
      height = 9,
      width = 16
    )
  
}
nDays <- unique(CasesExport$Day)
ActualDays <- lapply(nDays, ActualDayFx)

PredictedDayFx <- function(DayX) {
  
  Plot <-
    Data %>%
    filter(
      Day == DayX
    )
  
  #Date
  DATE <- Plot$Date[1]
  DATE <- as.Date(DATE)
  DATE <- format(DATE,"%B %d, %Y")
  
  #Missing in all
  ALL_LOCATIONS <- unique(Coordinates$NAME)
  MISSING <- match(ALL_LOCATIONS, unique(Plot$NAME))
  MISSING_LOCATIONS <- cbind(ALL_LOCATIONS, MISSING)
  MISSING_LOCATIONS <- as.data.frame(MISSING_LOCATIONS)
  MISSING_LOCATIONS <-
    MISSING_LOCATIONS %>%
    mutate(
      MISSING = is.na(MISSING)
    ) %>%
    filter(
      MISSING == TRUE
    ) %>%
    arrange(
      ALL_LOCATIONS
    )
  
  MissingAll <- merge(Coordinates, MISSING_LOCATIONS,  by.x = "NAME", by.y = "ALL_LOCATIONS")

  ggplot(
  ) +
    geom_sf(
      data = MissingAll,
      aes(
        fill = "Data Not Available"
      )
    ) +
    scale_fill_manual(
      values = ColorScale,
      name = ""
    ) +
    new_scale_fill() +
    geom_sf(
      data = Plot,
      aes(
        fill = CasesRatio/1000
      )
    ) +
    # scale_x_continuous(
    #   limits = c(-67.2, -65.3)
    # ) +
    scale_fill_distiller(
      palette = "PuRd",
      direction = 1,
      limits = c(0, CASES_MAX/1000),
      #breaks = c(0, 1,2, 3),
      #labels = c(0, 1000,2000, 3000),
      #labels = c(0, 750, paste("≥", round(CASES_MAX))),
      na.value = "#67001f",
      name = "COVID-19 Positive Cases\n(per 1000 people)"
    ) +
    labs(
      title = paste(LOCATIONS, "Predicted Cases for", DATE),
      subtitle = "Twitter @RHDeliz • delizaguirre.com/corona"
    ) +
    theme_classic(
      base_size = 24
    ) +
    theme(
      legend.position = LEGEND_POSITION,
      legend.key.height = unit(HEIGHT,"line"),
      legend.key.width = unit(WIDTH,"line"),
      axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank()
    )  +
    ggsave(
      paste(str_pad(DayX, 4, pad = "0"), ".png"),
      height = 9,
      width = 16
    )
  
}
nDays <- (max(CasesExport$Day)+1):max(PredictionCasesExport$Day)
PredictedDays <- lapply(nDays, PredictedDayFx)

av_encode_video(list.files(FOLDER, '*.png'), framerate = 20, output = 'Locations.mp4')
