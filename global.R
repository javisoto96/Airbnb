library(readr)
library(shiny)
library(leaflet,quietly = T, warn.conflicts = F)
library(plotly)
library(dplyr)
library(lubridate)
library(gbm)
library(caret)
library(sqldf)
library(rpart)
library(wordcloud2)
library(tm)

datos_malaga <- read.csv("malagaDatos.csv",stringsAsFactors = FALSE,encoding = "UTF-8")

datos_barcelona <- read.csv("barcelonaDatos.csv",stringsAsFactors = FALSE,encoding = "UTF-8")

datos_valencia <- read.csv("valenciaDatos.csv",stringsAsFactors = FALSE,encoding = "UTF-8")

calendar_malaga <- read.csv("malagaCalendar.csv",stringsAsFactors = FALSE,encoding = "UTF-8")

calendar_barcelona <- read.csv("barcelonaCalendar.csv",stringsAsFactors = FALSE,encoding = "UTF-8")

calendar_valencia <- read.csv("valenciaCalendar.csv",stringsAsFactors = FALSE,encoding = "UTF-8")