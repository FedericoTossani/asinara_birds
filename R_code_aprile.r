
###########
# Summary #
###########
#
###############
# 1. Packages #
###############

# First, create a list of required packages
list.of.packages <- c("tidyverse", "gridExtra", "stargazer", "lubridate", "ggpubr", "gganimate", "patchwork", "gifski")

# install required packages, if necessary, and load them
{
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  
  if(length(new.packages)) install.packages(new.packages)
  
  lapply(list.of.packages, require, character.only = TRUE)
}

##################
# 2. Data import #
##################

setwd("C:/Users/fedet/OneDrive/Documenti/R/asinara")

#-----------------------------------------------------------------------------------------#

dat_raw <- read.csv("data/ALLCAT.csv", stringsAsFactors = F)

str(dat_raw) 

#-----------------------------------------------------------------------------------------#

dat_loc_raw <- read.csv("data/ARCLOC.csv", stringsAsFactors = F)

str(dat_loc_raw)

#-----------------------------------------------------------------------------------------#

cod_euring <- read.csv("data/euring_sp_codes_2020.csv", stringsAsFactors = F)

str(cod_euring)

#-----------------------------------------------------------------------------------------#

data_23 <- read.csv("data/riepiloghi_asinara_23.csv", stringsAsFactors = F)

data_24 <- read.csv("data/riepiloghi_asinara_24.csv", stringsAsFactors = F)


##############
# 3. Dataset #
##############

# Nomi specie

sp_name <- 
  cod_euring %>%
  select(Code, Current.Name) %>%
  arrange(Code)%>%
  rename(nome = Current.Name)

#-----------------------------------------------------------------------------------------#

# Catture Tumbarino

dat_tumb <- 
  dat_raw%>%
  filter(LOCALITA.N.20.5 == "2") %>%
  arrange(DATA.C.8) %>%
  select(DATA.C.8, NUMERO.C.3, LOCALITA.N.20.5, RICATTURA.N.20.5, ORA.N.20.5, ANELLO.C.8, EURING.C.5, ALA_1.N.20.5, PESO.N.20.5, ALA_2.N.20.5, ETA.C.3, SESSO.C.1, GRASSO.C.1, TARSO.N.20.5, MUSCOLO.C.1)%>%
  left_join(sp_name, by = c("EURING.C.5" = "Code"))%>%         
  rename(data = DATA.C.8,
         num_prog = NUMERO.C.3,
         loc = LOCALITA.N.20.5,
         ricattura = RICATTURA.N.20.5,
         ora = ORA.N.20.5,
         anello = ANELLO.C.8,
         euring = EURING.C.5,
         c_max = ALA_1.N.20.5,
         peso = PESO.N.20.5,
         "3_rm" = ALA_2.N.20.5,
         eta = ETA.C.3,
         sex = SESSO.C.1,
         grasso = GRASSO.C.1,
         tarso = TARSO.N.20.5,
         muscolo = MUSCOLO.C.1)

dat_tumb$data <- ymd(dat_tumb$data)

dat_tumb <- dat_tumb%>%
  mutate(anno = year(data))%>%
  mutate(mese = month(data))%>%
  mutate(giorno = day(data))%>%
  unite(mese_gg, c(mese, giorno), sep="/", remove = F)

  
#-----------------------------------------------------------------------------------------#

# Dati 2023

new_data_23 <- pivot_longer(data_23, cols = 3:32, names_to = "data", values_to = "num")

new_data_23$data <- substr(new_data_23$data, 2, 11)


new_data_23 <- new_data_23%>%
    drop_na(num)%>%
    arrange(data)%>%
    rename(nome = Nome.scientifico)

new_data_23[new_data_23 == "Sylvia melanocephala"] <- "Curruca melanocephala"
new_data_23[new_data_23 == "Sylvia communis"] <- "Curruca communis"

new_data_23$data <- as.Date(new_data_23$data, format = "%d.%m.%Y", sep=".")

top_15_apr23 <- new_data_23%>%
    filter(nome %in% top_15$nome)

#-----------------------------------------------------------------------------------------#

# Dati 2024

new_data_24 <- pivot_longer(data_24, cols = 3:32, names_to = "data", values_to = "num")

new_data_24$data <- substr(new_data_24$data, 2, 11)


new_data_24 <- new_data_24%>%
    drop_na(num)%>%
    arrange(data)%>%
    rename(nome = NOME.SCIENTIFICO)

new_data_24[new_data_24 == "Sylvia melanocephala"] <- "Curruca melanocephala"
new_data_24[new_data_24 == "Sylvia communis"] <- "Curruca communis"

new_data_24$data <- as.Date(new_data_24$data, format = "%d.%m.%Y", sep=".")

top_15_apr24 <- new_data_24%>%
    filter(nome %in% top_15$nome)

#-----------------------------------------------------------------------------------------#
aprile <- dat_tumb[dat_tumb$mese_gg >= "4/1" & dat_tumb$mese_gg < "5/1", ]

aprile_since14 <- aprile[aprile$anno >= 2014 & aprile$anno != 2015, ]

sub_apr <- aprile_since14%>%
   select(anno, mese_gg, nome)%>%
  group_by(nome)%>%
  count(anno)%>%
  arrange(anno)


#-----------------------------------------------------------------------------------------#

# Numero di catture per specie

aprile_sp_count <- 
  aprile_since14 %>%
  count(nome) %>%
  arrange(desc(n))

#-----------------------------------------------------------------------------------------#

# Subset delle specie popolari

pop_sp <- aprile_sp_count%>%
  filter(n >= 100)

pop_sp_name <- pop_sp$nome

pop_aprile <- sub_apr%>%
  filter(nome %in% pop_sp_name)

top_15 <- aprile_sp_count[1:15, ]

top_15_apr <- aprile_since14%>%
       filter(nome %in% top_15$nome)%>%
       select(data, nome, anno)%>%
       group_by(data, nome)%>%
       count(nome)%>%
       mutate (anno = year(data),
               doy = yday(data))

col <- colorRampPalette(c("red", "blue", "orange", "green", "purple", "light blue", "yellow", "pink", "brown"))


theme_asinara <- theme_classic() + theme(panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"))

# 2014

apr14_plot <- top_15_apr%>%
  filter(anno == 2014)%>%
  ggplot(aes(x=data, y=n, fill = nome))+
  geom_col()+
  facet_wrap(~nome)+
  labs(title = "Aprile 2014",
       subtitle = "Plot delle 15 specie più catturate durante il mese di Aprile",
       x = "Data",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Aprile_2014.jpeg", plot = apr14_plot, width = 16, height = 7.69, dpi = 300)

apr14_plot2 <- top_15_apr%>%
  filter(anno == 2014)%>%
  ggplot(aes(x=data, y=n, fill = nome))+
  geom_col()+
  labs(title = "Aprile 2014",
       subtitle = "Plot delle 15 specie più catturate durante il mese di Aprile",
       x = "Data",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Aprile14.jpeg", plot = apr14_plot2, width = 16, height = 7.69, dpi = 300)
  
 # 2015

apr15_plot <- top_15_apr%>%
  filter(anno == 2015)%>%
  ggplot(aes(x=data, y=n, fill = nome))+
  geom_col()+
  facet_wrap(~nome)+
  labs(title = "Aprile 2015",
       subtitle = "Plot delle 15 specie più catturate durante il mese di Aprile",
       x = "Data",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Aprile_2015.jpeg", plot = apr15_plot, width = 16, height = 7.69, dpi = 300)

apr15_plot2 <- top_15_apr%>%
  filter(anno == 2015)%>%
  ggplot(aes(x=data, y=n, fill = nome))+
  geom_col()+
  labs(title = "Aprile 2015",
       subtitle = "Plot delle 15 specie più catturate durante il mese di Aprile",
       x = "Data",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Aprile15.jpeg", plot = apr15_plot2, width = 16, height = 7.69, dpi = 300)

# 2016

apr16_plot <- top_15_apr%>%
  filter(anno == 2016)%>%
  ggplot(aes(x=data, y=n, fill = nome))+
  geom_col()+
  facet_wrap(~nome)+
  labs(title = "Aprile 2016",
       subtitle = "Plot delle 15 specie più catturate durante il mese di Aprile",
       x = "Data",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Aprile_2016.jpeg", plot = apr16_plot, width = 16, height = 7.69, dpi = 300)

apr16_plot2 <- top_15_apr%>%
  filter(anno == 2016)%>%
  ggplot(aes(x=data, y=n, fill = nome))+
  geom_col()+
  labs(title = "Aprile 2016",
       subtitle = "Plot delle 15 specie più catturate durante il mese di Aprile",
       x = "Data",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Aprile16.jpeg", plot = apr16_plot2, width = 16, height = 7.69, dpi = 300)

# 2017

apr17_plot <- top_15_apr%>%
  filter(anno == 2017)%>%
  ggplot(aes(x=data, y=n, fill = nome))+
  geom_col()+
  facet_wrap(~nome)+
  labs(title = "Aprile 2017",
       subtitle = "Plot delle 15 specie più catturate durante il mese di Aprile",
       x = "Data",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Aprile_2017.jpeg", plot = apr17_plot, width = 16, height = 7.69, dpi = 300)

apr17_plot2 <- top_15_apr%>%
  filter(anno == 2017)%>%
  ggplot(aes(x=data, y=n, fill = nome))+
  geom_col()+
  labs(title = "Aprile 2017",
       subtitle = "Plot delle 15 specie più catturate durante il mese di Aprile",
       x = "Data",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Aprile17.jpeg", plot = apr17_plot2, width = 16, height = 7.69, dpi = 300)


# 2018

apr18_plot <- top_15_apr%>%
  filter(anno == 2018)%>%
  ggplot(aes(x=data, y=n, fill = nome))+
  geom_col()+
  facet_wrap(~nome)+
  labs(title = "Aprile 2018",
       subtitle = "Plot delle 15 specie più catturate durante il mese di Aprile",
       x = "Data",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Aprile_2018.jpeg", plot = apr18_plot, width = 16, height = 7.69, dpi = 300)

apr18_plot2 <- top_15_apr%>%
  filter(anno == 2018)%>%
  ggplot(aes(x=data, y=n, fill = nome))+
  geom_col()+
  labs(title = "Aprile 2018",
       subtitle = "Plot delle 15 specie più catturate durante il mese di Aprile",
       x = "Data",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Aprile18.jpeg", plot = apr18_plot2, width = 16, height = 7.69, dpi = 300)

# 2019

apr19_plot <- top_15_apr%>%
  filter(anno == 2019)%>%
  ggplot(aes(x=data, y=n, fill = nome))+
  geom_col()+
  facet_wrap(~nome)+
  labs(title = "Aprile 2019",
       subtitle = "Plot delle 15 specie più catturate durante il mese di Aprile",
       x = "Data",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Aprile_2019.jpeg", plot = apr19_plot, width = 16, height = 7.69, dpi = 300)

apr19_plot2 <- top_15_apr%>%
  filter(anno == 2019)%>%
  ggplot(aes(x=data, y=n, fill = nome))+
  geom_col()+
  labs(title = "Aprile 2019",
       subtitle = "Plot delle 15 specie più catturate durante il mese di Aprile",
       x = "Data",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Aprile19.jpeg", plot = apr19_plot2, width = 16, height = 7.69, dpi = 300)

# 2020

apr20_plot <- top_15_apr%>%
  filter(anno == 2020)%>%
  ggplot(aes(x=data, y=n, fill = nome))+
  geom_col()+
  facet_wrap(~nome)+
  labs(title = "Aprile 2020",
       subtitle = "Plot delle 15 specie più catturate durante il mese di Aprile",
       x = "Data",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Aprile_2020.jpeg", plot = apr20_plot, width = 16, height = 7.69, dpi = 300)

apr20_plot2 <- top_15_apr%>%
  filter(anno == 2020)%>%
  ggplot(aes(x=data, y=n, fill = nome))+
  geom_col()+
  labs(title = "Aprile 2020",
       subtitle = "Plot delle 15 specie più catturate durante il mese di Aprile",
       x = "Data",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Aprile20.jpeg", plot = apr20_plot2, width = 16, height = 7.69, dpi = 300)

# 2021

apr21_plot <- top_15_apr%>%
  filter(anno == 2021)%>%
  ggplot(aes(x=data, y=n, fill = nome))+
  geom_col()+
  facet_wrap(~nome)+
  labs(title = "Aprile 2021",
       subtitle = "Plot delle 15 specie più catturate durante il mese di Aprile",
       x = "Data",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Aprile_2021.jpeg", plot = apr21_plot, width = 16, height = 7.69, dpi = 300)

apr21_plot2 <- top_15_apr%>%
  filter(anno == 2021)%>%
  ggplot(aes(x=data, y=n, fill = nome))+
  geom_col()+
  labs(title = "Aprile 2021",
       subtitle = "Plot delle 15 specie più catturate durante il mese di Aprile",
       x = "Data",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Aprile21.jpeg", plot = apr21_plot2, width = 16, height = 7.69, dpi = 300)

# 2022

apr22_plot <- top_15_apr%>%
  filter(anno == 2022)%>%
  ggplot(aes(x=data, y=n, fill = nome))+
  geom_col()+
  facet_wrap(~nome)+
  labs(title = "Aprile 2022",
       subtitle = "Plot delle 15 specie più catturate durante il mese di Aprile",
       x = "Data",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Aprile_2022.jpeg", plot = apr22_plot, width = 16, height = 7.69, dpi = 300)

apr22_plot2 <- top_15_apr%>%
  filter(anno == 2022)%>%
  ggplot(aes(x=data, y=n, fill = nome))+
  geom_col()+
  labs(title = "Aprile 2022",
       subtitle = "Plot delle 15 specie più catturate durante il mese di Aprile",
       x = "Data",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Aprile22.jpeg", plot = apr22_plot2, width = 16, height = 7.69, dpi = 300)

# 2023

apr23_plot <- top_15_apr23%>%
  ggplot(aes(x=data, y=num, fill = nome))+
  geom_col()+
  facet_wrap(~nome)+
  labs(title = "Aprile 2023",
       subtitle = "Plot delle 15 specie più catturate durante il mese di Aprile",
       x = "Data",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Aprile_2023.jpeg", plot = apr23_plot, width = 16, height = 7.69, dpi = 300)


apr23_plot2 <- top_15_apr23%>%
  ggplot(aes(x=data, y=num, fill = nome))+
  geom_col()+
  labs(title = "Aprile 2023",
       subtitle = "Plot delle 15 specie più catturate durante il mese di Aprile",
       x = "Data",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Aprile23.jpeg", plot = apr23_plot2, width = 16, height = 7.69, dpi = 300)

# 2024

apr24_plot <- top_15_apr24%>%
  ggplot(aes(x=data, y=num, fill = nome))+
  geom_col()+
  facet_wrap(~nome)+
  labs(title = "Aprile 2024",
       subtitle = "Plot delle 15 specie più catturate durante il mese di Aprile",
       x = "Data",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Aprile_2024.jpeg", plot = apr24_plot, width = 16, height = 7.69, dpi = 300)


apr24_plot2 <- top_15_apr24%>%
  ggplot(aes(x=data, y=num, fill = nome))+
  geom_col()+
  labs(title = "Aprile 2024",
       subtitle = "Plot delle 15 specie più catturate durante il mese di Aprile",
       x = "Data",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Aprile24.jpeg", plot = apr24_plot2, width = 16, height = 7.69, dpi = 300)

#-----------------------------------------------------------------------------------------#

cap_plot <- top_15_apr%>%
  filter(nome == "Sylvia atricapilla")%>%
  filter(anno != 2015)%>%
  ggplot(aes(x=doy, y=n, fill = factor(anno)))+
  geom_col()+
  facet_wrap(~anno)+
  scale_fill_manual(values=c("red", "blue", "orange", "green", "purple", "light blue", "yellow", "pink", "brown"))+
  labs(title = "Sylvia atricapilla",
       subtitle = "Plot delle catture di Capinera durante il mese di Aprile (dal 2014 al 2022)",
       x = "Day of the year",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Capinera_aprile.jpeg", plot = cap_plot, width = 16, height = 7.69, dpi = 300)

cap_plot23 <- top_15_apr23%>%
  filter(nome == "Sylvia atricapilla")%>%
  ggplot(aes(x=doy, y=num, fill = nome))+
  geom_col()+
  labs(title = "Sylvia atricapilla",
       subtitle = "Plot delle catture di Capinera durante il mese di Aprile 2023",
       x = "Day of the year",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Capinera_aprile23.jpeg", plot = cap_plot23, width = 16, height = 7.69, dpi = 300)



lui_plot <- top_15_apr%>%
  filter(nome == "Phylloscopus trochilus")%>%
  ggplot(aes(x=doy, y=n, fill = factor(anno)))+
  geom_col()+
  facet_wrap(~anno)+
  scale_fill_manual(values=c("red", "blue", "orange", "green", "purple", "light blue", "yellow", "pink", "brown"))+
  labs(title = "Phylloscopus trochilus",
       subtitle = "Plot delle catture di Luì grosso durante il mese di Aprile (dal 2014 al 2022)",
       x = "Day of the year",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Lui_grosso_aprile.jpeg", plot = lui_plot, width = 16, height = 7.69, dpi = 300)


lui_plot23 <- top_15_apr23%>%
  filter(nome == "Phylloscopus trochilus")%>%
  ggplot(aes(x=doy, y=num, fill = nome))+
  geom_col()+
  labs(title = "Phylloscopus trochilus",
       subtitle = "Plot delle catture di Luì grosso durante il mese di Aprile 2023",
       x = "Day of the year",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("Lui_grosso_aprile23.jpeg", plot = lui_plot23, width = 16, height = 7.69, dpi = 300)
