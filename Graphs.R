#Written by Rafael Deliz-Aguirre
#For drawing plots of prevalence and incidence

DIRECTORY = "~/COVID-19/Texas" #Or Texas
#LOCATIONS = c("Puerto Rico", "Texas", "New York", "California")
LOCATIONS = c("Webb")

#Load libraries
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

#Import data
setwd(DIRECTORY)
CasesExport <- read.csv("CasesTable.csv", header = T)
PredictionCasesExport <- read.csv("PredictionsTable.csv", header = T)

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

PlottingTable <-
  CasesExport %>%
  filter(
    Location %in% LOCATIONS
  ) %>%
  mutate(
    NewCases = ifelse(NewCases<0, NA, NewCases),
    NewCasesRatio = ifelse(NewCasesRatio<0, NA, NewCasesRatio),
    Date = as.Date(Date)
  )

Max <-
  PlottingTable %>%
  filter(
    Day == max(Day)
  ) %>%
  arrange(
    CasesRatio
  )

PredictionCasesExport <-
  PredictionCasesExport %>%
  filter(
    Location %in% LOCATIONS
  ) %>%
  mutate(
    Date = as.Date(Date)
  )

LinearCases <-
  ggplot() +
  geom_ribbon(
    data = PredictionCasesExport,
    aes(
      x = Date,
      ymin = CasesMin,
      ymax = CasesMax
    ),
    alpha=0.2) +
  geom_line(
    data = PredictionCasesExport,
    aes(
      x = Date,
      y = Cases,
      color = "Predicted Cases"
    )
  ) +
  geom_line(
    data = PlottingTable,
    aes(
      x = Date,
      y = Cases,
      color = "Actual Cases"
    )
  ) +
  scale_colour_manual(values=c("black","red"))+
  geom_vline(
    data = WaveDate,
    aes(
      xintercept = Date
    )
  ) +
  theme_classic(
    base_size = 16
  ) +
  labs(
    title = paste("COVID-19 Positive Cases"),
    y = "Cases",
    x = "Date"
  ) +
  facet_wrap(
    ~Location,
    scale = "free_y"
  ) +
  theme(
    legend.position = "bottom"
  )

LinearNewCases <-
  ggplot() +
  geom_ribbon(
    data = PredictionCasesExport,
    aes(
      x = Day-13,
      ymin = NewCasesMin,
      ymax = NewCasesMax),
    alpha=0.2) +
  geom_line(
    data = PredictionCasesExport,
    aes(
      x = Day-13,
      y = NewCases,
      color = "Predicted Cases"
    ),
    color = "red"
  ) +
  geom_line(
    data = PlottingTable,
    aes(
      x = Day-13,
      y = NewCases,
      color = "Actual Cases"
    )
  ) +
  scale_colour_manual(values=c("black","red"))+
  geom_vline(
    data = WaveDate,
    aes(
      xintercept = Day-13
    )
  ) +
  theme_classic(
    base_size = 16
  ) + 
  labs(
    title = "New COVID-19 Daily Cases",
    y = "New Cases",
    x = "Days since First Local Case (Patient Zero)",
    color = "Data"
  ) +
  facet_wrap(
    ~Location,
    scale = "free_y"
  ) +
  theme(
    legend.position = "bottom"
  )

pdf("NonTechnicalPrediction.pdf", height = 9, width = 16)
gridExtra::grid.arrange(LinearCases, LinearNewCases, ncol =2)
dev.off()

LinearCases <-
  ggplot() +
  geom_ribbon(
    data = PredictionCasesExport,
    aes(
      x = Date,
      ymin = CasesMin,
      ymax = CasesMax),
    alpha=0.2) +
  geom_line(
    data = PredictionCasesExport,
    aes(
      x = Date,
      y = Cases
    ),
    color = "red"
  ) +
  geom_line(
    data = PlottingTable,
    aes(
      x = Date,
      y = Cases
    )
  ) +
  geom_vline(
    data = WaveDate,
    aes(
      xintercept = Date
    )
  ) +
  theme_classic(
    base_size = 16
  ) +
  labs(
    title = "Prevalence",
    y = "Cases",
    x = "Date"
  ) +
  facet_wrap(
    ~Location,
    scale = "free_y"
  )

LogCases <-
  ggplot() +
  geom_ribbon(
    data = PredictionCasesExport,
    aes(
      x = Day,
      ymin = CasesMin,
      ymax = CasesMax),
    alpha=0.2) +
  geom_line(
    data = PredictionCasesExport,
    aes(
      x = Day,
      y = Cases
    ),
    color = "red"
  ) +
  geom_line(
    data = PlottingTable,
    aes(
      x = Day,
      y = Cases
    )
  ) +
  geom_vline(
    data = WaveDate,
    aes(
      xintercept = Day
    )
  ) +
  theme_classic(
    base_size = 16
  ) +
  scale_y_log10() +
  labs(
    title = "Prevalence (log)",
    y = "Cases",
    x = "Days since First Local Case (Patient Zero)"
  ) +
  facet_wrap(
    ~Location,
    scale = "free_y"
  )

LinearNewCases <-
  ggplot() +
  geom_ribbon(
    data = PredictionCasesExport,
    aes(
      x = Date,
      ymin = NewCasesMin,
      ymax = NewCasesMax),
    alpha=0.2) +
  geom_line(
    data = PredictionCasesExport,
    aes(
      x = Date,
      y = NewCases
    ),
    color = "red"
  ) +
  geom_line(
    data = PlottingTable,
    aes(
      x = Date,
      y = NewCases
    )
  ) +
  geom_vline(
    data = WaveDate,
    aes(
      xintercept = Date
    )
  ) +
  theme_classic(
    base_size = 16
  ) +
  labs(
    title = "Daily Incidence",
    y = "New Cases",
    x = "Date"
  ) +
  facet_wrap(
    ~Location,
    scale = "free_y"
  )

logNewCases <-
  ggplot() +
  geom_ribbon(
    data = PredictionCasesExport,
    aes(
      x = Day,
      ymin = NewCasesMin,
      ymax = NewCasesMax),
    alpha=0.2) +
  geom_line(
    data = PredictionCasesExport,
    aes(
      x = Day,
      y = NewCases
    ),
    color = "red"
  ) +
  geom_line(
    data = PlottingTable,
    aes(
      x = Day,
      y = NewCases
    )
  ) +
  geom_vline(
    data = WaveDate,
    aes(
      xintercept = Day
    )
  ) +
  scale_y_log10()+
  theme_classic(
    base_size = 16
  ) +
  labs(
    title = "Daily Incidence (log)",
    y = "New Cases",
    x = "Days since First Local Case (Patient Zero)"
  ) +
  facet_wrap(
    ~Location,
    scale = "free_y"
  )

LinearNewCases <-
  ggplot() +
  geom_ribbon(
    data = PredictionCasesExport,
    aes(
      x = Date,
      ymin = NewCasesMin,
      ymax = NewCasesMax),
    alpha=0.2) +
  geom_line(
    data = PredictionCasesExport,
    aes(
      x = Date,
      y = NewCases
    ),
    color = "red"
  ) +
  geom_line(
    data = PlottingTable,
    aes(
      x = Date,
      y = NewCases
    )
  ) +
  geom_vline(
    data = WaveDate,
    aes(
      xintercept = Date
    )
  ) +
  theme_classic(
    base_size = 16
  ) +
  labs(
    title = "Daily Incidence",
    y = "New Cases",
    x = "Date"
  ) +
  facet_wrap(
    ~Location,
    scale = "free_y"
  )

LogNewCases <-
  ggplot() +
  geom_ribbon(
    data = PredictionCasesExport,
    aes(
      x = Day,
      ymin = NewCasesMin,
      ymax = NewCasesMax),
    alpha=0.2) +
  geom_line(
    data = PredictionCasesExport,
    aes(
      x = Day,
      y = NewCases
    ),
    color = "red"
  ) +
  geom_line(
    data = PlottingTable,
    aes(
      x = Day,
      y = NewCases
    )
  ) +
  geom_vline(
    data = WaveDate,
    aes(
      xintercept = Day
    )
  ) +
  theme_classic(
    base_size = 16
  ) +
  scale_y_log10() +
  labs(
    title = "Daily Incidence (log)",
    y = "New Cases",
    x = "Days since First Local Case (Patient Zero)"
  ) +
  facet_wrap(
    ~Location,
    scale = "free_y"
  )

pdf("TechnicalPrediction.pdf", height = 9, width = 16)
gridExtra::grid.arrange(
  LinearCases,
  LogCases,
  LinearNewCases,
  LogNewCases,
  ncol =2)
dev.off()

LinearCases <-
  ggplot() +
  geom_ribbon(
    data = PredictionCasesExport,
    aes(
      x = Date,
      ymin = CasesRatioMin,
      ymax = CasesRatioMax),
    alpha=0.2) +
  geom_line(
    data = PredictionCasesExport,
    aes(
      x = Date,
      y = CasesRatio
    ),
    color = "red"
  ) +
  geom_line(
    data = PlottingTable,
    aes(
      x = Date,
      y = CasesRatio
    )
  ) +
  geom_vline(
    data = WaveDate,
    aes(
      xintercept = Date
    )
  ) +
  theme_classic(
    base_size = 16
  ) +
  labs(
    title = "Prevalence",
    y = "Cases (per 100 000)",
    x = "Date"
  ) +
  facet_wrap(
    ~Location
  )

LogCases <-
  ggplot() +
  geom_ribbon(
    data = PredictionCasesExport,
    aes(
      x = Day,
      ymin = CasesRatioMin,
      ymax = CasesRatioMax),
    alpha=0.2) +
  geom_line(
    data = PredictionCasesExport,
    aes(
      x = Day,
      y = CasesRatio
    ),
    color = "red"
  ) +
  geom_line(
    data = PlottingTable,
    aes(
      x = Day,
      y = CasesRatio
    )
  ) +
  geom_vline(
    data = WaveDate,
    aes(
      xintercept = Day
    )
  ) +
  theme_classic(
    base_size = 16
  ) +
  scale_y_log10() +
  labs(
    title = "Prevalence (log)",
    y = "Cases (per 100 000)",
    x = "Days since First Local Case (Patient Zero)"
  ) +
  facet_wrap(
    ~Location
  )

LinearNewCases <-
  ggplot() +
  geom_ribbon(
    data = PredictionCasesExport,
    aes(
      x = Date,
      ymin = NewCasesRatioMin,
      ymax = NewCasesRatioMax),
    alpha=0.2) +
  geom_line(
    data = PredictionCasesExport,
    aes(
      x = Date,
      y = NewCasesRatio
    ),
    color = "red"
  ) +
  geom_line(
    data = PlottingTable,
    aes(
      x = Date,
      y = NewCasesRatio
    )
  ) +
  geom_vline(
    data = WaveDate,
    aes(
      xintercept = Date
    )
  ) +
  theme_classic(
    base_size = 16
  ) +
  labs(
    title = "Daily Incidence",
    y = "New Cases",
    x = "Date"
  ) +
  facet_wrap(
    ~Location
  )

logNewCases <-
  ggplot() +
  geom_ribbon(
    data = PredictionCasesExport,
    aes(
      x = Day,
      ymin = NewCasesRatioMin,
      ymax = NewCasesRatioMax),
    alpha=0.2) +
  geom_line(
    data = PredictionCasesExport,
    aes(
      x = Day,
      y = NewCasesRatio
    ),
    color = "red"
  ) +
  geom_line(
    data = PlottingTable,
    aes(
      x = Day,
      y = NewCasesRatio
    )
  ) +
  geom_vline(
    data = WaveDate,
    aes(
      xintercept = Day
    )
  ) +
  scale_y_log10()+
  theme_classic(
    base_size = 16
  ) +
  labs(
    title = "Daily Incidence (log)",
    y = "New Cases (per 100 000)",
    x = "Days since First Local Case (Patient Zero)"
  ) +
  facet_wrap(
    ~Location
  )

LinearNewCases <-
  ggplot() +
  geom_ribbon(
    data = PredictionCasesExport,
    aes(
      x = Date,
      ymin = NewCasesRatioMin,
      ymax = NewCasesRatioMax),
    alpha=0.2) +
  geom_line(
    data = PredictionCasesExport,
    aes(
      x = Date,
      y = NewCasesRatio
    ),
    color = "red"
  ) +
  geom_line(
    data = PlottingTable,
    aes(
      x = Date,
      y = NewCasesRatio
    )
  ) +
  geom_vline(
    data = WaveDate,
    aes(
      xintercept = Date
    )
  ) +
  theme_classic(
    base_size = 16
  ) +
  labs(
    title = "Daily Incidence",
    y = "New Cases (per 100 000)",
    x = "Date"
  ) +
  facet_wrap(
    ~Location
  )

LogNewCases <-
  ggplot() +
  geom_ribbon(
    data = PredictionCasesExport,
    aes(
      x = Day,
      ymin = NewCasesRatioMin,
      ymax = NewCasesRatioMax),
    alpha=0.2) +
  geom_line(
    data = PredictionCasesExport,
    aes(
      x = Day,
      y = NewCasesRatio
    ),
    color = "red"
  ) +
  geom_line(
    data = PlottingTable,
    aes(
      x = Day,
      y = NewCasesRatio
    )
  ) +
  geom_vline(
    data = WaveDate,
    aes(
      xintercept = Day
    )
  ) +
  theme_classic(
    base_size = 16
  ) +
  scale_y_log10() +
  labs(
    title = "Daily Incidence (log)",
    y = "New Cases (per 100 000)",
    x = "Days since First Local Case (Patient Zero)"
  ) +
  facet_wrap(
    ~Location,
  )

pdf("TechnicalPredictionRates.pdf", height = 9, width = 16)

gridExtra::grid.arrange(
  LinearCases,
  LogCases,
  LinearNewCases,
  LogNewCases,
  ncol =2)

dev.off()

pdf("linPrevalence.pdf",  height = 9, width = 16)
gridExtra::grid.arrange(
  LinearCases,
  LinearNewCases,
  ncol =2)
dev.off()

NewCasesPlottingTable <-
  PlottingTable %>%
  group_by(
    Location
  ) %>%
  mutate(
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
  )

NewCasesLin <-
  ggplot() +
  geom_line(
    data = PlottingTable,
    aes(
      x = Date,
      y = NewCases,
      color = "Raw New Cases"
    )
  ) +
  geom_line(
    data = NewCasesPlottingTable,
    aes(
      x = Date,
      y = SmoothedNewCases,
      color = "Smoothed New Cases"
    )
  ) +
  scale_colour_manual(values=c("black", "blue"))+
  geom_vline(
    data = WaveDate,
    aes(
      xintercept = Date
    ),
    color = "red"
  ) +
  theme_classic(
    base_size = 16
  ) +
  labs(
    title = paste("Daily COVID-19 Positive Cases"),
    y = "New Cases",
    x = "Date",
    color = "Data"
  ) +
  facet_wrap(
    ~Location,
    scale = "free_y"
  ) +
  theme(
    legend.position = "bottom"
  )

NewCasesLog <-
  ggplot() +
  geom_line(
    data = PlottingTable,
    aes(
      x = Date,
      y = NewCases,
      color = "Raw New Cases"
    )
  ) +
  geom_line(
    data = NewCasesPlottingTable,
    aes(
      x = Date,
      y = SmoothedNewCases,
      color = "Smoothed New Cases"
    )
  ) +
  scale_colour_manual(values=c("black", "blue"))+
  geom_vline(
    data = WaveDate,
    aes(
      xintercept = Date
    ),
    color = "red",
    linetype = "dashed"
  ) +
  geom_vline(
    data = WaveDate,
    aes(
      xintercept = Date
    ),
    color = "red"
  ) +
  theme_classic(
    base_size = 16
  ) +
  labs(
    title = paste("Daily COVID-19 Positive Cases (log)"),
    y = "New Cases",
    x = "Date",
    color = "Data"
  ) +
  facet_wrap(
    ~Location,
    scale = "free_y"
  ) +
  theme(
    legend.position = "bottom"
  ) +
  scale_y_log10()

pdf("NewCases.pdf", height = 9, width = 16)
gridExtra::grid.arrange(
  NewCasesLin,
  NewCasesLog,
  ncol =2)
dev.off()
