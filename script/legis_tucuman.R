library(tidyverse)
library(ggthemes)
library(ggparliament)
library(readxl)

diputados_tucuman <- read_xlsx("data/Tucuman.xlsx") %>% 
  mutate(party_long = case_when(
    party_short == "UCRHY" ~ "UCR-Hipólito Yrigoyen (1)", # Modificó para agregar tíldes faltantes
    party_short == "PSJ" ~ "Psj Recuperemos Tucumán (3)", 
    TRUE ~ party_long
  ) ) %>% 
  mutate(pct = round(seats/sum(seats)*100,1), 
         name = str_remove_all(party_long, "[^[:alpha:] & [:space:]]"), 
         name = str_squish(name)) %>% 
  select(name, everything()) %>% # Modifico party_long para tener toda la info en labels
  mutate(party_long = case_when(seats == 1 ~ paste0(name, " (", seats, " banca - ", pct, "%)"),
                                TRUE ~  paste0(name, " (", seats, " bancas - ", pct, "%)"))) %>%  
  select(year, country, house, party_long, party_short, seats, government , colour, orden)  %>% 
  print()




# VOY A CREAR EL OBJETO DE ggparliament con la data ordenada para el plot

data_diputados_tucuman<- ggparliament::parliament_data(diputados_tucuman, #datos originales
                                                       type = "semicircle", # forma del hemiciclo
                                                       parl_rows =3, # cantidad de filas
                                                       party_seats = diputados_tucuman$seats, # bancas 
                                                       plot_order = diputados_tucuman$orden) %>% #orden de partidos 
  mutate(colour = as.character(colour)) %>% # vector de texto para codigo HEX de colores asignados previamente
  as_tibble() %>%  
  print()




data_diputados_tucuman %>% as_tibble() %>% 
  ggplot(aes(x, y, colour = party_long)) +
  geom_parliament_seats(size = 7) + # tamaño de bancas (puntos)
  geom_highlight_government(government == 1, colour = "black", size = 8) + # circulo negro al oficialismo
  geom_parliament_bar(party = party_short, label = F) + # barra con proporción de bancas
  draw_majoritythreshold(n = 31, label = F, type = "semicircle") + # dinuja el limite de mayoría 
  scale_colour_manual(values = data_diputados_tucuman$colour,
                      limits = data_diputados_tucuman$party_long)  +
  guides(colour = guide_legend(nrow=7)) + # customiza etiquetas
  labs(title = "Diputados", 
       subtitle = "2019 - 2021", 
       colour = "Bloques") +
  theme_fivethirtyeight() +
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(), 
        legend.position = "bottom", 
        legend.text = element_text(size = 14))


