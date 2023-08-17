


# Here you will load all the packages needed and also the working directory
          source("https://raw.githubusercontent.com/FedericoTossani/osprey/main/osprey_code_WD_packages.r")

setwd("C:/Users/fedet/OneDrive/Documenti/Lavoro/Danilo/Asinara")

data <- read.csv("Riepiloghi Tumbarino 2023 - Foglio4.csv")

data <- data%>%
          pivot_longer(cols = 2:46, names_to ="date", values_to = "captures")%>%
          na.omit()%>%
          mutate(date = gsub("X", "", date),
                 date = as.Date(date, format = "%d.%m.%Y"))%>%
          rename(specie = NOME.COMUNE)


top10 %>%
          kable(format = 'latex', booktabs = TRUE) 


top10 <- data%>%
          group_by(specie)%>%
          summarize(totcaptures = sum(captures))%>%
          arrange(desc(totcaptures))

top10_date <- data%>%
          group_by(date)%>%
          summarize(totcaptures = sum(captures))%>%
          arrange(desc(totcaptures))


# Catture primavera 2023

# ggplot theme

theme_asinara <- theme_classic() + theme(panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"))

          
all_plot <- data%>%
          arrange(date)%>%
          ggplot(aes(x = date, y = captures, fill = specie))+
          geom_col()+
          scale_x_date(limits = c(as.Date("2023-04-01"), as.Date("2023-05-15")), date_breaks = "2 day", date_labels = "%b %d")+ 
          theme_asinara+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                legend.position = "bottom")+
          labs(title = "Progetto Piccole Isole 2023", subtitle = "Specie catturate durante il Progetto Piccole Isole 2023",x = "Data", y = "Numero di catture",)

baloon_plot <- data%>%
          arrange(date)%>%
          ggplot(aes(x = date, y = specie, size = captures))+
          geom_point()+
          scale_x_date(limits = c(as.Date("2023-04-01"), as.Date("2023-05-15")), date_breaks = "1 day", date_labels = "%b %d")+ 
          theme_asinara+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Progetto Piccole Isole 2023", subtitle = "Specie catturate durante il Progetto Piccole Isole 2023", x = "Data", y = "Specie")

 rondine <- data%>%
          filter(specie == "Rondine")%>%
          ggplot(aes(x = date, y = captures))+
          geom_col()+
          scale_x_date(limits = c(as.Date("2023-04-01"), as.Date("2023-05-15")), date_breaks = "1 day", date_labels = "%b %d")+
          scale_y_continuous(breaks = unique(data$captures))  + 
          theme_asinara+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Rondine", subtitle = "Hirundo rustica", x = "Data", y = "Numero di catture")
                          
luigrosso <- data%>%
          filter(specie == "Luì grosso")%>%
          ggplot(aes(x = date, y = captures))+
          geom_col()+
          scale_x_date(limits = c(as.Date("2023-04-01"), as.Date("2023-05-15")), date_breaks = "1 day", date_labels = "%b %d")+
          scale_y_continuous(breaks = unique(data$captures))  + 
          theme_asinara+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Luì grosso", subtitle = "Phylloscopus trochilus", x = "Data", y = "Numero di catture")

codirossocomune <- data%>%
          filter(specie == "Codirosso comune")%>%
          ggplot(aes(x = date, y = captures))+
          geom_col()+
          scale_x_date(limits = c(as.Date("2023-04-01"), as.Date("2023-05-15")), date_breaks = "1 day", date_labels = "%b %d")+
          scale_y_continuous(breaks = unique(data$captures))  + 
          theme_asinara+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Codirosso comune", subtitle = "Phoenicurus phoenicurus", x = "Data", y = "Numero di catture")

beccafico <- data%>%
          filter(specie == "Beccafico")%>%
          ggplot(aes(x = date, y = captures))+
          geom_col()+
          scale_x_date(limits = c(as.Date("2023-04-01"), as.Date("2023-05-15")), date_breaks = "1 day", date_labels = "%b %d")+
          scale_y_continuous(breaks = unique(data$captures))  + 
          theme_asinara+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Beccafico", subtitle = "Sylvia borin", x = "Data", y = "Numero di catture")

canapinomaggiore <- data%>%
          filter(specie == "Canapino maggiore")%>%
          ggplot(aes(x = date, y = captures))+
          geom_col()+
          scale_x_date(limits = c(as.Date("2023-04-01"), as.Date("2023-05-15")), date_breaks = "1 day", date_labels = "%b %d")+
          scale_y_continuous(breaks = unique(data$captures))  + 
          theme_asinara+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Canapino maggiore", subtitle = "Hippolais icterina", x = "Data", y = "Numero di catture")

sterpazzola <- data%>%
          filter(specie == "Sterpazzola")%>%
          ggplot(aes(x = date, y = captures))+
          geom_col()+
          scale_x_date(limits = c(as.Date("2023-04-01"), as.Date("2023-05-15")), date_breaks = "1 day", date_labels = "%b %d")+
          scale_y_continuous(breaks = unique(data$captures))  + 
          theme_asinara+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Sterpazzola", subtitle = "Sylvia communis", x = "Data", y = "Numero di catture")

pigliamosche <- data%>%
          filter(specie == "Pigliamosche")%>%
          ggplot(aes(x = date, y = captures))+
          geom_col()+
          scale_x_date(limits = c(as.Date("2023-04-01"), as.Date("2023-05-15")), date_breaks = "1 day", date_labels = "%b %d")+
          scale_y_continuous(breaks = unique(data$captures))  + 
          theme_asinara+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Pigliamosche", subtitle = "Muscicapa striata", x = "Data", y = "Numero di catture")

balianera <- data%>%
          filter(specie == "Balia nera")%>%
          ggplot(aes(x = date, y = captures))+
          geom_col()+
          scale_x_date(limits = c(as.Date("2023-04-01"), as.Date("2023-05-15")), date_breaks = "1 day", date_labels = "%b %d")+
          scale_y_continuous(breaks = unique(data$captures))  + 
          theme_asinara+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Balia nera", subtitle = "Ficedula hypoleuca", x = "Data", y = "Numero di catture")

luiverde <- data%>%
          filter(specie == "Luì verde")%>%
          ggplot(aes(x = date, y = captures))+
          geom_col()+
          scale_x_date(limits = c(as.Date("2023-04-01"), as.Date("2023-05-15")), date_breaks = "1 day", date_labels = "%b %d")+
          scale_y_continuous(breaks = unique(data$captures))  + 
          theme_asinara+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Luì verde", subtitle = "Phylloscopus sibilatrix", x = "Data", y = "Numero di catture")

pettirosso <- data%>%
          filter(specie == "Pettirosso")%>%
          ggplot(aes(x = date, y = captures))+
          geom_col()+
          scale_x_date(limits = c(as.Date("2023-04-01"), as.Date("2023-05-15")), date_breaks = "1 day", date_labels = "%b %d")+
          scale_y_continuous(breaks = unique(data$captures))  + 
          theme_asinara+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Pettirosso", subtitle = "Erithacus rubecula", x = "Data", y = "Numero di catture")

luipiccolo <- data%>%
          filter(specie == "Luì piccolo")%>%
          ggplot(aes(x = date, y = captures))+
          geom_col()+
          scale_x_date(limits = c(as.Date("2023-04-01"), as.Date("2023-05-15")), date_breaks = "1 day", date_labels = "%b %d")+
          scale_y_continuous(breaks = unique(data$captures))  + 
          theme_asinara+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Luì piccolo", subtitle = "Phylloscopus collybita", x = "Data", y = "Numero di catture")

usignolo <- data%>%
          filter(specie == "Usignolo")%>%
          ggplot(aes(x = date, y = captures))+
          geom_col()+
          scale_x_date(limits = c(as.Date("2023-04-01"), as.Date("2023-05-15")), date_breaks = "1 day", date_labels = "%b %d")+
          scale_y_continuous(breaks = unique(data$captures))  + 
          theme_asinara+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Usignolo", subtitle = "Luscinia megarhynchos", x = "Data", y = "Numero di catture")

capinera <- data%>%
          filter(specie == "Capinera")%>%
          ggplot(aes(x = date, y = captures))+
          geom_col()+
          scale_x_date(limits = c(as.Date("2023-04-01"), as.Date("2023-05-15")), date_breaks = "1 day", date_labels = "%b %d")+
          scale_y_continuous(breaks = c(1:22))  + 
          theme_asinara+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Capinera", subtitle = "Sylvia atricapilla", x = "Data", y = "Numero di catture")

cinciallegra <- data%>%
          filter(specie == "Cinciallegra")%>%
          ggplot(aes(x = date, y = captures))+
          geom_col()+
          scale_x_date(limits = c(as.Date("2023-04-01"), as.Date("2023-05-15")), date_breaks = "1 day", date_labels = "%b %d")+
          scale_y_continuous(breaks = unique(data$captures))  + 
          theme_asinara+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Cinciallegra", subtitle = "Parus major", x = "Data", y = "Numero di catture")


occhiocotto <- data%>%
          filter(specie == "Occhiocotto")%>%
          ggplot(aes(x = date, y = captures))+
          geom_col()+
          scale_x_date(limits = c(as.Date("2023-04-01"), as.Date("2023-05-15")), date_breaks = "1 day", date_labels = "%b %d")+
          scale_y_continuous(breaks = unique(data$captures))  + 
          theme_asinara+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Occhiocotto", subtitle = "Sylvia melanocoephala", x = "Data", y = "Numero di catture")


cardellino <- data%>%
          filter(specie == "Cardellino")%>%
          ggplot(aes(x = date, y = captures))+
          geom_col()+
          scale_x_date(limits = c(as.Date("2023-04-01"), as.Date("2023-05-15")), date_breaks = "1 day", date_labels = "%b %d")+
          scale_y_continuous(breaks = unique(data$captures))  + 
          theme_asinara+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(title = "Cardellino", subtitle = "Carduelis carduelis", x = "Data", y = "Numero di catture")

# Example list of plots (replace this with your list of plots)
          plot_list <- list(all_plot, baloon_plot, rondine, luigrosso, codirossocomune, beccafico, canapinomaggiore, sterpazzola, pigliamosche, balianera, luiverde, luipiccolo, pettirosso, usignolo, cinciallegra, occhiocotto, cardellino)  # Replace with your actual plots

# Directory where you want to save the plots
          output_directory <- "C:/Users/fedet/OneDrive/Documenti/Lavoro/Danilo/Asinara/plot_primavera23"

# Loop through the list of plots and save each one
          for (i in seq_along(plot_list)) {
            plot_filename <- paste0("plot_", i, ".png")
            ggsave(file.path(output_directory, plot_filename), plot = plot_list[[i]], width = 8, height = 6)
          }


