


# Here you will load all the packages needed and also the working directory
          source("https://raw.githubusercontent.com/FedericoTossani/osprey/main/osprey_code_WD_packages.r")

setwd("C:/Users/fedet/OneDrive/Documenti/Lavoro/Danilo/Asinara")

data <- read.csv("Riepiloghi Tumbarino 2023 - Foglio4.csv")

data <- data%>%
          pivot_longer(cols = 2:46, names_to ="date", values_to = "captures")%>%
          na.omit()%>%
          mutate(date = gsub("X", "", date),
                 date = as.Date(date, format = "%d.%m.%Y"))


# MIGRATORI

rondine <- data%>%
          #filter(NOME.COMUNE == "Rondine")%>%
          ggplot(aes(x = date, fill = "NOME.COMUNE"))+
          geom_bar(position="stack")+
          theme_light()+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

luigrosso <-

codirossocomune <-

beccafico <-

canapinomaggiore <-

sterpazzola <-

pigliamosche <-

balianera <-


#RESIDENTI

cinciallegra <-

occhiocotto <-

cardellino <-

fanello <-

tottavilla <-

scricciolo <-


