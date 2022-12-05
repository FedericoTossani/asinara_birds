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
list.of.packages <- c("tidyverse", "gridExtra", "stargazer", "lubridate", "ggplot2")

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

dat_riepiloghi <- read.csv("data/riepiloghi_asinara_PPI_2022.csv", stringsAsFactors = F)
 
str(dat_riepiloghi)

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

if (condition) {
  expressions A
} else if (condition) {
  expressions B
} else if (condition) {
  expressions C
} else {
  expressions D
}


#-----------------------------------------------------------------------------------------#

# Numero di catture per codice euring
 
tumb_sp_count <- 
           dat_tumb %>%
              count(euring) %>%
              arrange(desc(n))%>%
              left_join(sp_name, by = c("euring" = "Code"))%>%
              select("nome", everything())
             
 
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
 
sub_prim <- primavera%>%
   select(anno, mese_gg, nome)%>%
   group_by(nome)%>%
   count(anno)%>%
   arrange(anno)

#-----------------------------------------------------------------------------------------#
 
# Subset di prim per specie 
 
pop_sp <- tumb_sp_count%>%
   filter(n >= 100)
 
pop_sp_name <- pop_sp$nome
   
pop_prim <- sub_prim%>%
   filter(nome %in% pop_sp_name)

#-----------------------------------------------------------------------------------------#
 
# Subset per ogni specie  
 
E_rubecula <- sub_prim%>%
   filter(nome == "Erithacus rubecula")

P_collybita <- sub_prim%>%
   filter(nome == "Phylloscopus collybita")
 
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
 
capinera <- 
  dat_tumb%>%
  filter(nome == "Sylvia atricapilla")%>%
  group_by(anno)%>%
  count(anno)
  #group_by(data)


 
 plot(capinera, main = "Capinera", xlim = c(1997, 2021), ylim = c(0, 200))
 
 
 # facet_wrap() è la funzione che mi divide il grafico in tanti grafici singoli in base ad una variabile che decido
   
 ggplot(pop_prim, aes(x=anno, y=n, color = nome))+
        geom_point(alpha = 0.4)+
        geom_smooth(se = F)
       #facet_wrap(~ nome)

 
 ggplot(S_borin, aes(x=anno, y=n))+
     geom_line()
