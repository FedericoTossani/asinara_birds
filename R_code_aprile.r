
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

# Numero di catture per codice euring

tumb_sp_count <- 
  dat_tumb %>%
  count(euring) %>%
  left_join(sp_name, by = c("euring" = "Code"))%>%
  select("nome", everything())%>%
  arrange(desc(n))
  
#-----------------------------------------------------------------------------------------#

aprile <- dat_tumb[dat_tumb$mese_gg >= "4/1" & dat_tumb$mese_gg <= "4/30", ]

aprile_since14 <- aprile[aprile$anno >= 2014, ]


sub_apr <- aprile_since14%>%
   select(anno, mese_gg, nome)%>%
  group_by(nome)%>%
  count(anno)%>%
  arrange(anno)

#-----------------------------------------------------------------------------------------#

# Subset delle specie popolari

pop_sp <- tumb_sp_count%>%
  filter(n >= 100)

pop_sp_name <- pop_sp$nome

pop_prim <- sub_prim%>%
  filter(nome %in% pop_sp_name)

top_15 <- tumb_sp_count[1:15, ]


top_15_apr <- aprile_since14%>%
       filter(nome %in% top_15$nome)%>%
       select(data, nome, anno)%>%
       group_by(data, nome)%>%
       count(nome)%>%
       mutate (anno = year(data))


top_15_apr_plot <-
  ggplot(top_15_apr, aes(x=data, y=n, color = nome))+
  geom_line(size = 1, )+
  geom_point(size = 5.5)+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  #geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "15 Specie più comuni",
       subtitle = "Plot delle catture durante 25 anni di Progetto Piccole Isole",
       x = "Data",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
  
  

























