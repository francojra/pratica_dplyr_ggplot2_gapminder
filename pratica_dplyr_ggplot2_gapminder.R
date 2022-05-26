
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

ggplot(tab_1, aes(x = ano, y = media, col = continente,
                  ymin = media - se, ymax = media + se)) +
  geom_point(stat = "identity", position = position_dodge(),
             size = 3) +
  geom_errorbar(colour = "brown", size = 0.8,
                stat = "identity", position = position_dodge(),
                width = 0.1, alpha = 0.6) +
  scale_color_manual(values = c("forestgreen", "purple"),
                     name = "Continente") +
  coord_cartesian(ylim = c(50, 80)) +
  theme_minimal(base_size = 12) +
  labs(x = "Anos", y = "Expectativa de vida")
  
