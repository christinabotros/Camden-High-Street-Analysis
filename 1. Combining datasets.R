###### Christina Botros
###### 03/07/2021
###### Compiling 10 years of Camden Data 

library(sp)
library(reshape2)
library(geojsonio)
library(rgdal)
library(downloader)
library(maptools)
library(dplyr)
library(broom) 
library(stplanr)
library(ggplot2)
library(MASS)
library(sf)
library(tmap)
library(tmaptools)
library(stringr)
library(leaflet)
library(tidyverse)
library(here)
library(downloader)
library(readxl)
library(janitor)
library(survival)



library(dplyr)
library(data.table)
setwd("/nfs/cfs/home2/ucfn/ucfncab/DISSERTATION")

setwd("Whythawk - Camden")
files <- list.files(pattern = "*camden-location-report.csv")
combined_files <- bind_rows(lapply(files, fread))

write.csv(combined_files,"Whythawk-Camden-Combined.csv", row.names = FALSE)

#setwd("/nfs/cfs/home2/ucfn/ucfncab/DISSERTATION/AddressBasePlus")
#ABP <- list.files(pattern = "*.csv")
#ABPfull <- bind_rows(lapply(ABP, fread))

setwd("/nfs/cfs/home2/ucfn/ucfncab/DISSERTATION")

ABP <- read.csv("AddressBasePlus/TQ3080.csv", header=FALSE)
names(ABP)
CamABP<- ABP %>%
  filter(V62== "CAMDEN")

ABP <- read.csv("AddressBasePlus/TQ2585.csv", header=FALSE)
names(ABP) 
CamABP1 <- ABP %>%
  filter(V62== "CAMDEN")

ABP <- read.csv("AddressBasePlus/TQ2580.csv", header=FALSE)
names(ABP) 
CamABP2 <- ABP %>%
  filter(V62== "CAMDEN")

ABP <- read.csv("AddressBasePlus/TQ2085.csv", header=FALSE)
names(ABP) 
CamABP3 <- ABP %>%
  filter(V62== "CAMDEN")

ABP <- read.csv("AddressBasePlus/TQ2080.csv", header=FALSE)
names(ABP) 
CamABP4 <- ABP %>%
  filter(V62== "CAMDEN")

CAMDEN_ABP <- rbind(CamABP, CamABP1, CamABP2, CamABP3, CamABP4)
write.csv(CAMDEN_ABP,"AddressBasePlus-Camden-Complete.csv", row.names = FALSE)


rm(ABP, CamABP, CamABP1, CamABP2, CamABP3, CamABP4)


##### Match the locations in Whythawk with AddressBasePlus

as.integer(CAMDEN_ABP$V49)

sapply(CAMDEN_ABP$V49, class)

ABPWHY <- match(combined_files$location_code, CAMDEN_ABP$V49)
  
  erge(combined_files, CAMDEN_ABP, by.x="location_code", by.y="V49")





