#################
# Asinara birds #
#################

###########
# Summary #
###########
#
###############
# 1. Packages #
###############

# First, create a list of required packages
list.of.packages <- c("tidyverse", "gridExtra", "stargazer", "lubridate", "ggtheme", "ggpubr", "gganimate", "patchwork", "gifski")

# install required packages, if necessary, and load them
{
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  
  if(length(new.packages)) install.packages(new.packages)
  
  lapply(list.of.packages, require, character.only = TRUE)
}

##################
# 2. Data import #
##################

dat_raw <- read.csv("data/ALLCAT.csv", stringsAsFactors = F)

str(dat_raw) 

#-----------------------------------------------------------------------------------------#

dat_loc_raw <- read.csv("data/ARCLOC.csv", stringsAsFactors = F)

str(dat_loc_raw)

#-----------------------------------------------------------------------------------------#

# dat_riepiloghi <- read.csv("data/riepiloghi_asinara_PPI_2022.csv", stringsAsFactors = F)

# str(dat_riepiloghi)

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




dim(dat_tumb)
str(dat_tumb)
n_distinct(dat_tumb$euring) # 122 diversi codici euring

#-----------------------------------------------------------------------------------------#

# aggiunta della colonna famiglia

# Accipitriformi
accipitridae <- c("Accipiter nisus", "Buteo buteo", "Circus aeruginosus")

# Bucerotiformi
upupidae <- c("Upupa epops")

# Caprimulgiformi
caprimulgidae <- c("Caprimulgus europaeus")

# Colombiformi
columbidae <- c("Columba livia", "Columba palumbus", "Streptopelia decaocto", "Streptopelia turtur")

# Coraciformi
alcedinidae <- c("Alcedo atthis")
coraciidae <- c("Coracias garrulus")
meropidae <- c("Merops apiaster")

# Cuculiformi
cuculidae <- c("Cuculus canorus")

# Falconiformi
falconidae <- c("Falco peregrinus", "Falco tinnunculus")

# Galliformi
phasianidae <- c("Alectoris barbara", "Alectoris rufa", "Coturnix coturnix")

# Caradriformi
laridae <- c("Larus michahellis")
scolopacidae <- c("Scolopax rusticola", "Tringa ochropus")

# Passeriformi
alaudidae <- c("Alauda arvensis", "Lullula arborea")
acrocephalidae <- c("Acrocephalus arundinaceus", "Acrocephalus schoenobaenus", "Acrocephalus scirpaceus", "Hippolais icterina", "Hippolais polyglotta", "Iduna opaca")
hirundinidae <- c("Cecropis daurica", "Delichon urbicum", "Hirundo rustica", "Riparia riparia")
motacillidae <- c("Anthus campestris", "Anthus pratensis", "Anthus trivialis", "Motacilla alba", "Motacilla cinerea", "Motacilla flava feldegg", "Motacilla flava flava", "Motacilla flava iberiae")
turdidae <- c("Turdus iliacus", "Turdus merula", "Turdus philomelos", "Turdus pilaris", "Turdus torquatus", "Turdus viscivorus")
sylviidae <- c("Curruca cantillans/iberiae/subalpina", "Curruca communis", "Curruca curruca", "Curruca hortensis", "Curruca melanocephala", "Curruca nisoria", "Curruca sarda", "Curruca subalpina", "Curruca undata", "Sylvia atricapilla", "Sylvia borin")
muscicapidae <- c("Erithacus rubecula", "Ficedula albicollis", "Ficedula hypoleuca", "Ficedula parva", "Ficedula semitorquata", "Luscinia megarhynchos", "Luscinia svecica", "Monticola saxatilis", "Monticola solitarius", "Muscicapa striata", "Oenanthe deserti", "Oenanthe hispanica/melanoleuca", "Oenanthe oenanthe", "Phoenicurus ochruros", "Phoenicurus phoenicurus", "Saxicola rubetra", "Saxicola rubicola")
phylloscopidae <- c("Phylloscopus bonelli/orientalis", "Phylloscopus collybita", "Phylloscopus fuscatus", "Phylloscopus inornatus", "Phylloscopus schwarzi", "Phylloscopus sibilatrix", "Phylloscopus trochilus")
paridae <- c("Cyanistes caeruleus", "Parus major", "Periparus ater")
oriolidae <- c("Oriolus oriolus")
prunellidae <- c("Prunella modularis")
regulidae <- c("Regulus ignicapilla", "Regulus regulus")
locustellidae <- c("Locustella naevia")
laniidae <- c("Lanius collurio", "Lanius senator")
corvidae <- c("Corvus cornix", "Corvus corone/cornix", "Pica pica")
sturnidae <- c("Sturnus unicolor", "Sturnus vulgaris")
passeridae <- c("Passer hispaniolensis", "Petronia petronia")
fringillidae <- c("Acanthis flammea/cabaret", "Carduelis carduelis", "Carpodacus erythrinus", "Chloris chloris", "Coccothraustes coccothraustes", "Fringilla coelebs", "Fringilla montifringilla", "Linaria cannabina", "Serinus serinus", "Spinus spinus")
emberizidae <- c("Emberiza calandra", "Emberiza cia", "Emberiza cirlus", "Emberiza hortulana", "Emberiza schoeniclus")
scotocercidae <- c("Cettia cetti")
troglodytidae <- c("Troglodytes troglodytes")

#Pelecaniformi
ardeidae <- c("Bubulcus ibis", "Egretta garzetta", "Ixobrychus minutus")

# Piciformi
picidae <- c("Jynx torquilla")

# Strigiformi
strigidae <- c("Asio otus", "Athene noctua", "Otus scops")
tytonidae <- c("Tyto alba")


dat_tumb%>%
  filter(nome %in% accipitridae)%>%
  mutate(famiglia = "Accipitridae")


#-----------------------------------------------------------------------------------------#

# Numero di catture per codice euring

tumb_sp_count <- 
  dat_tumb %>%
  count(euring) %>%
  left_join(sp_name, by = c("euring" = "Code"))%>%
  select("nome", everything())%>%
  arrange(desc(n))


tumb_sp_count

#-----------------------------------------------------------------------------------------#

# Catture per data

dat_grouped_by_date <-
  dat_tumb%>%
  count(data)%>%
  arrange(desc(data))

str(dat_grouped_by_date)
head(dat_grouped_by_date)
tail(dat_grouped_by_date)

#-----------------------------------------------------------------------------------------#

# Subset di primavera ed autunno

primavera <- dat_tumb[dat_tumb$mese_gg >= "4/1" & dat_tumb$mese_gg <= "5/15", ]
autunno <- dat_tumb[dat_tumb$mese_gg >= "10/1" & dat_tumb$mese_gg <= "10/31", ]

#autunno <- dat_tumb[dat_tumb$mese_gg == "10/31", ]

sub_prim <- primavera%>%
  select(anno, mese_gg, nome)%>%
  group_by(nome)%>%
  count(anno)%>%
  arrange(anno)

sub_aut <- autunno%>%
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

top_15_prim <- primavera%>%
       filter(nome %in% top_15$nome)%>%
       select(data, nome, anno, mese_gg)%>%
       mutate(num = 1)
      
top_15_prim_grouped <- top_15_prim%>%
  group_by(data, nome)%>%
  summarise(across(num,sum))%>%
  mutate (anno = year(data))%>%
  mutate(mese = month(data))%>%
  mutate(giorno = day(data))%>%
  unite(mese_gg, c(mese, giorno), sep="/", remove = F)

top_15_prim_plot <-
  ggplot(top_15_prim, aes(x=anno, y=n, color = nome))+
  geom_line(size = 1, )+
  #geom_point(size = 5.5)+
  #geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "15 Specie più comuni",
       subtitle = "Plot delle catture durante 25 anni di Progetto Piccole Isole",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("pop_prim.jpeg", plot = top_15_prim_plot)

pop_aut <- sub_aut%>%
  filter(nome %in% pop_sp_name)

#-----------------------------------------------------------------------------------------#

# Subset per ogni specie in primavera

# First let's create a theme to use in the plot

theme_asinara <- theme_classic() + theme(panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"))

pettirosso_prim <- primavera%>%
  filter(nome == "Erithacus rubecula")%>%
  count(mese_gg)%>%
  summarise(mean_per_day = mean(n), 
            total = sum(n))
pettirosso_aut <- autunno%>%
  filter(nome == "Erithacus rubecula")%>%
  count(mese_gg)%>%
  summarise(mean_per_day = mean(n), 
            total = sum(n))

E_rubecula <- sub_prim%>%
  filter(nome == "Erithacus rubecula")

E_rubecula_aut <- sub_aut%>%
  filter(nome == "Erithacus rubecula")

rubecula_prim_plot <-
  ggplot(E_rubecula, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Pettirosso (Erithacus rubecula, L. 1758)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Piccole Isole",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("pettirosso_prim.jpeg", plot = rubecula_prim_plot)

rubecula_aut_plot <-
  ggplot(E_rubecula_aut, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Pettirosso (Erithacus rubecula, L. 1758)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Autunno",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("pettirosso_aut.jpeg", plot = rubecula_aut_plot)

#-----------------------------------------------------------------------------------------#

S_atricapilla <- sub_prim%>%
  filter(nome == "Sylvia atricapilla")

S_atricapilla_aut <- sub_aut%>%
  filter(nome == "Sylvia atricapilla")

atricapilla_prim_plot <-
  ggplot(S_atricapilla, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Capinera (Sylvia atricapilla, L. 1758)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Piccole Isole",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("capinera_prim.jpeg", plot = atricapilla_prim_plot)

atricapilla_aut_plot <-
  ggplot(S_atricapilla_aut, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Capinera (Sylvia atricapilla, L. 1758)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Autunno",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("capinera_aut.jpeg", plot = atricapilla_aut_plot)

#-----------------------------------------------------------------------------------------#

P_trochilus <- sub_prim%>%
  filter(nome == "Phylloscopus trochilus")

P_trochilus_aut <- sub_aut%>%
  filter(nome == "Phylloscopus trochilus")

trochilus_prim_plot <-
  ggplot(P_trochilus, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Luì grosso (Phylloscopus trochilus, L. 1758)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Piccole Isole",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("lui_grosso_prim.jpeg", plot = trochilus_prim_plot)

trochilus_aut_plot <-
  ggplot(P_trochilus_aut, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Luì grosso (Phylloscopus trochilus, L. 1758)",
       subtitle = "Plot delle catture durante 25 anni di Progetto autunno",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("lui_grosso_aut.jpeg", plot = trochilus_aut_plot)

#-----------------------------------------------------------------------------------------#

S_borin <- sub_prim%>%
  filter(nome == "Sylvia borin")

S_borin_aut <- sub_aut%>%
  filter(nome == "Sylvia borin")

borin_prim_plot <-
  ggplot(S_borin, aes(x=anno, y=n))+
    geom_line(size = 1, color = "blue")+
    geom_point(size = 5.5, color = "blue")+
    geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
    geom_smooth(color = "red", se = F, method = "loess")+
    labs(title = "Beccafico (Sylvia borin, Boddaert 1783)",
         subtitle = "Plot delle catture durante 25 anni di Progetto Piccole Isole",
         x = "Anno",
         y = "Catture",
         caption = "Data source: Database catture di Tumbarino")+
    theme_asinara
ggsave("beccafico_prim.jpeg", plot = borin_prim_plot)

borin_aut_plot <-
  ggplot(S_borin_aut, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Beccafico (Sylvia borin, Boddaert 1783)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Autunno",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("beccafico_aut.jpeg", plot = borin_aut_plot)

#-----------------------------------------------------------------------------------------#

C_melanocephala <- sub_prim%>%
  filter(nome == "Curruca melanocephala")

C_melanocephala_aut <- sub_aut%>%
  filter(nome == "Curruca melanocephala")

melanocephala_prim_plot <-
  ggplot(C_melanocephala, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Occhiocotto (Curruca melanocephala, Gmelin 1789)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Piccole Isole",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("occhiocotto_prim.jpeg", plot = melanocephala_prim_plot)

melanocephala_aut_plot <-
  ggplot(C_melanocephala_aut, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Occhiocotto (Curruca melanocephala, Gmelin 1789)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Autunno",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("occhiocotto_aut.jpeg", plot = melanocephala_aut_plot)

#-----------------------------------------------------------------------------------------#

F_hypoleuca <- sub_prim%>%
  filter(nome == "Ficedula hypoleuca")

F_hypoleuca_aut <- sub_aut%>%
  filter(nome == "Ficedula hypoleuca")

hypoleuca_prim_plot <-
  ggplot(F_hypoleuca, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Balia nera (Ficedula hypoleuca, Pallas 1764)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Piccole Isole",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("balia_nera_prim.jpeg", plot = hypoleuca_prim_plot)

hypoleuca_aut_plot <-
  ggplot(F_hypoleuca_aut, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Balia nera (Ficedula hypoleuca, Pallas 1764)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Autunno",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("balia_nera_aut.jpeg", plot = hypoleuca_aut_plot)

#-----------------------------------------------------------------------------------------#

P_phoenicurus <- sub_prim%>%
  filter(nome == "Phoenicurus phoenicurus")

P_phoenicurus_aut <- sub_aut%>%
  filter(nome == "Phoenicurus phoenicurus")

phoenicurus_prim_plot <-
  ggplot(P_phoenicurus, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Codirosso comune (Phoenicurus phoenicurus, L. 1758)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Piccole Isole",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("codirosso_com_prim.jpeg", plot = phoenicurus_prim_plot)

phoenicurus_aut_plot <-
  ggplot(P_phoenicurus_aut, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Codirosso comune (Phoenicurus phoenicurus, L. 1758)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Autunno",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("codirosso_com_aut.jpeg", plot = phoenicurus_aut_plot)

#-----------------------------------------------------------------------------------------#

P_collybita <- sub_prim%>%
  filter(nome == "Phylloscopus collybita")

P_collybita_aut <- sub_aut%>%
  filter(nome == "Phylloscopus collybita")

collybita_prim_plot <-
  ggplot(P_collybita, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Luì piccolo (Phylloscopus collybita, Vieillot 1817)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Piccole Isole",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("lui_piccolo_prim.jpeg", plot = collybita_prim_plot)

collybita_aut_plot <-
  ggplot(P_collybita_aut, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Luì piccolo (Phylloscopus collybita, Vieillot 1817)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Autunno",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("lui_piccolo_aut.jpeg", plot = collybita_aut_plot)

#-----------------------------------------------------------------------------------------#

P_sibilatrix <- sub_prim%>%
  filter(nome == "Phylloscopus sibilatrix")

P_sibilatrix_aut <- sub_aut%>%
  filter(nome == "Phylloscopus sibilatrix")

sibilatrix_prim_plot <-
  ggplot(P_sibilatrix_aut, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Luì verde (Phylloscopus sibilatrix, Bechstein 1793)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Piccole Isole",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("lui_verde_prim.jpeg", plot = sibilatrix_prim_plot)

sibilatrix_aut_plot <-
  ggplot(P_sibilatrix_aut, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Luì verde (Phylloscopus sibilatrix, Bechstein 1793)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Autunno",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("lui_verde_aut.jpeg", plot = sibilatrix_aut_plot)

#-----------------------------------------------------------------------------------------#

H_rustica <- sub_prim%>%
  filter(nome == "Hirundo rustica")

H_rustica_aut <- sub_aut%>%
  filter(nome == "Hirundo rustica")

rustica_prim_plot <-
  ggplot(H_rustica, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Rondine (Hirundo rustica, L. 1758)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Piccole Isole",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("rondine_prim.jpeg", plot = rustica_prim_plot)

rustica_aut_plot <-
  ggplot(H_rustica_aut, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Rondine (Hirundo rustica, L. 1758)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Autunno",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("rondine_aut.jpeg", plot = rustica_aut_plot)

#-----------------------------------------------------------------------------------------#

T_philomelos <- sub_prim%>%
  filter(nome == "Turdus philomelos")

T_philomelos_aut <- sub_aut%>%
  filter(nome == "Turdus philomelos")

philomelos_prim_plot <-
  ggplot(T_philomelos, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Tordo bottaccio (Turdus philomelos, C.L.Brehm 1831)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Piccole Isole",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("tordo_bottaccio_prim.jpeg", plot = philomelos_prim_plot)

philomelos_aut_plot <-
  ggplot(T_philomelos_aut, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Tordo bottaccio (Turdus philomelos, C.L.Brehm 1831)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Autunno",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("tordo_bottaccio_aut.jpeg", plot = philomelos_aut_plot)

#-----------------------------------------------------------------------------------------#

M_striata <- sub_prim%>%
  filter(nome == "Muscicapa striata")

M_striata_aut <- sub_aut%>%
  filter(nome == "Muscicapa striata")

striata_prim_plot <-
  ggplot(M_striata, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Pigliamosche (Muscicapa striata, Pallas 1764)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Piccole Isole",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("pigliamosche_prim.jpeg", plot = striata_prim_plot)

striata_aut_plot <-
  ggplot(M_striata_aut, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Pigliamosche (Muscicapa striata, Pallas 1764)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Autunno",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("pigliamosche_aut.jpeg", plot = striata_aut_plot)

#-----------------------------------------------------------------------------------------#

P_major <- sub_prim%>%
  filter(nome == "Parus major")

P_major_aut <- sub_aut%>%
  filter(nome == "Parus major")

major_prim_plot <-
  ggplot(P_major, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Cinciallegra (Parus major, L. 1758)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Piccole Isole",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("cinciallegra_prim.jpeg", plot = major_prim_plot)

major_aut_plot <-
  ggplot(P_major_aut, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Cinciallegra (Parus major, L. 1758)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Autunno",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("cinciallegra_aut.jpeg", plot = major_aut_plot)

#-----------------------------------------------------------------------------------------#

C_communis <- sub_prim%>%
  filter(nome == "Curruca communis")

C_communis_aut <- sub_aut%>%
  filter(nome == "Curruca communis")

communis_prim_plot <-
  ggplot(C_communis, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Sterpazzola (Curruca communis, Latham 1787)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Piccole Isole",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("sterpazzola_prim.jpeg", plot = communis_prim_plot)

communis_aut_plot <-
  ggplot(C_communis_aut, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Sterpazzola (Curruca communis, Latham 1787)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Autunno",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("sterpazzola_aut.jpeg", plot = communis_aut_plot)

#-----------------------------------------------------------------------------------------#

T_merula <- sub_prim%>%
  filter(nome == "Turdus merula")

T_merula_aut <- sub_aut%>%
  filter(nome == "Turdus merula")

merula_prim_plot <-
  ggplot(T_merula, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Merlo (Turdus merula, L. 1758)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Piccole Isole",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("merlo_prim.jpeg", plot = merula_prim_plot)

merula_aut_plot <-
  ggplot(T_merula_aut, aes(x=anno, y=n))+
  geom_line(size = 1, color = "blue")+
  geom_point(size = 5.5, color = "blue")+
  geom_text(aes(label = round(n,5.5)), color = "white", size = 2)+
  geom_smooth(color = "red", se = F, method = "loess")+
  labs(title = "Merlo (Turdus merula, L. 1758)",
       subtitle = "Plot delle catture durante 25 anni di Progetto Autunno",
       x = "Anno",
       y = "Catture",
       caption = "Data source: Database catture di Tumbarino")+
  theme_asinara
ggsave("merlo_aut.jpeg", plot = merula_aut_plot)


# ------------------------------------------------------ #
L_senator <- sub_prim%>%
  filter(nome == "Lanius senator")

L_collurio <- sub_prim%>%
  filter(nome == "Lanius collurio")

S_borin <- sub_prim%>%
  filter(nome == "Sylvia borin")

S_borin <- sub_prim%>%
  filter(nome == "Sylvia borin")

##################
# 4. Data export #
##################

write.table(sp_name_join,"sp_name_join.txt",sep="\t",row.names=FALSE)

pdf("sp_tumbarino.pdf", title = "Specie e conteggi Tumbarino", paper = "a4")
grid.table(tumb_sp_count)
dev.off()

stargazer(tumb_sp_count,
          title = "Specie e conteggi Tumbarino",
          summary = FALSE,
          type = "text",
          digit.separator = ".",
          out = "sp_tumbarino.txt")


##############
# 5. GRAFICI #
##############


                                         

plot(dat_grouped_by_date[c(-1, -1667),])

# andamento specie

# facet_wrap() è la funzione che mi divide il grafico in tanti grafici singoli in base ad una variabile che decido

ggplot(pop_prim, aes(x=anno, y=n, color = nome))+
  geom_point(alpha = 0.4)+
  geom_smooth(se = F)
#facet_wrap(~ nome)

theme_asinara2 <- theme_classic() + theme(panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
                                          axis.text.x = element_text(angle = 90))


tumbarino_graph <- ggplot(top_15_prim_grouped, aes(x = mese_gg, y = num, size = num, colour = nome)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_size(range = c(2, 12)) +
  facet_wrap(~nome)+
  theme_asinara2+
  # Here comes the gganimate specific bits
  labs(title = 'Anno: {frame_time}', x = 'Mese / Giorno', y = 'Catture') +
  transition_time(top_15_prim_grouped$anno) 
  ease_aes('linear')
  
tumb_gif <- animate(tumbarino_graph, fps=30, duration = 20, end_pause = 30, res = 100)

anim_save("Tumbarino_gif.gif")

tumbarino_graph2 <- ggplot(top_15_prim_grouped, aes(x = nome, Y = num, fill = nome)) +
  geom_bar() +
  scale_size(range = c(2, 12)) +
  scale_color_viridis("magma")
  facet_wrap(~anno)+
  theme_asinara2

