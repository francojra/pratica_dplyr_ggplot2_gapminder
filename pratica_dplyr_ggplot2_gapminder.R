
# Prática - Pacotes dplyr e ggplot2 --------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 25/05/22 ---------------------------------------------------------------------------------------------------------------------------

# Carregar pacotes ----------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(dados)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

tabela <- dados::dados_gapminder
view(tabela)

# Seleção de dados -------------------------------------------------------------------------------------------------------------------------

tab <- tabela %>%
  select(continente, ano, expectativa_de_vida) %>%
  filter(continente %in% c("Américas", "Europa"))
view(tab)

# Análises ---------------------------------------------------------------------------------------------------------------------------------

tab_1 <- tab %>%
  group_by(continente, ano) %>%
  summarise(media = mean(expectativa_de_vida), sd = sd(expectativa_de_vida),
            n = n(), se = sd / sqrt(n)) 
tab_1
view(tab_1)
glimpse(tab_1)
tab_1$ano <- as.factor(tab_1$ano)

# Gráfico ----------------------------------------------------------------------------------------------------------------------------------

ggplot(tab_1) +
  geom_col(aes(x = ano, y = media, fill = continente, group = ano)) +
  geom_errorbar(aes(x = ano, y = media, ymin = media - sd, ymax = media + sd)) 
  
