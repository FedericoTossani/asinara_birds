


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

# Color Palette

col <- colorRampPalette(c("#5a8300", "#6474ff", "#4acf50", "#8d3ec4", "#1c8700", "#cb4cd0", "#d0cb3b", "#5a39b7", "#898c00", "#cf81ff", "#006c15", "#ff61ca", "#01b582", "#f11683", "#87d987", "#bd0027", "#19dbe5", "#f5473a", "#068eff", "#ffb24e", "#404e8f", "#ae7400", "#31c5ff", "#b13500", "#009067", "#a80040", "#006637", "#ffa3ed", "#506100", "#863079", "#d2c96e", "#813968", "#acb67e", "#8a364c", "#018880", "#ff7e72", "#476031", "#b89bce", #7d441d", "#df99b4", "#ff988e", "#f3a8a3"))

          
          all_plot <- data%>%
                    arrange(date, desc(captures))%>%
                    ggplot(aes(x = date, y = captures, fill = NOME.COMUNE))+
                    geom_col()+
                    theme_light()+
                    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

 rondine <- data%>%
          filter(NOME.COMUNE == "Rondine")%>%
          ggplot(aes(x = date, y = captures))+
          geom_col()+
          theme_light()+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Rondine", subtitle = "Hirundo rustica")
                          
luigrosso <- data%>%
          filter(NOME.COMUNE == "Luì grosso")%>%
          ggplot(aes(x = date, y = captures))+
          geom_col()+
          theme_light()+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Luì grosso", subtitle = "Phylloscopus trochilus")

codirossocomune <- data%>%
          filter(NOME.COMUNE == "Codirosso comune")%>%
          ggplot(aes(x = date, y = captures))+
          geom_col()+
          theme_light()+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Codirosso comune", subtitle = "Phoenicurus phoenicurus")

beccafico <- data%>%
          filter(NOME.COMUNE == "Beccafico")%>%
          ggplot(aes(x = date, y = captures))+
          geom_col()+
          theme_light()+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Beccafico", subtitle = "Sylvia borin")

canapinomaggiore <- data%>%
          filter(NOME.COMUNE == "Canapino maggiore")%>%
          ggplot(aes(x = date, y = captures))+
          geom_col()+
          theme_light()+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Canapino maggiore", subtitle = "Hippolais icterina")

sterpazzola <- data%>%
          filter(NOME.COMUNE == "Sterpazzola")%>%
          ggplot(aes(x = date, y = captures))+
          geom_col()+
          theme_light()+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Sterpazzola", subtitle = "Sylvia communis")

pigliamosche <- data%>%
          filter(NOME.COMUNE == "Pigliamosche")%>%
          ggplot(aes(x = date, y = captures))+
          geom_col()+
          theme_light()+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Pigliamosche", subtitle = "Muscicapa striata")

balianera <- data%>%
          filter(NOME.COMUNE == "Balia nera")%>%
          ggplot(aes(x = date, y = captures))+
          geom_col()+
          theme_light()+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Balia nera", subtitle = "Ficedula hypoleuca")+
          scale_x_continuous(limits = date, breaks = date)


#RESIDENTI

cinciallegra <-

occhiocotto <-

cardellino <-

fanello <-

tottavilla <-

scricciolo <-


