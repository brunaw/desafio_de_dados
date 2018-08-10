### script desafio ###

# memory
rm(list = ls())

# packages
# devtools::install_github("conre3/pesqEle") # instalação do pacote;
# devtools::install_github('jtrecenti/ufshape') # instalação do pacote;

library(pesqEle)
library(ufshape)
library(tidyverse)
library(ggmap)

# data
pe <- pe_2018()
dplyr::glimpse(pe)

pe.uf <- pe %>% 
  group_by(info_uf) %>%
  summarise(n = n())
pe.uf

df_uf <- ufshape::df_uf
plot(df_uf)

df_uf$NM_ESTADO
df_uf$info_uf <- c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN",
                     "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP", "PR", "SC",
                     "MS", "MT", "GO", "DF", "RS")
pe.uf$info_uf
df_uf[, c(1, 5)]

n <- as.vector(as.matrix(left_join(data.frame(info_uf = df_uf$info_uf), pe.uf)[2]))
n <- ifelse(is.na(n) == TRUE, 0, n)
df_uf$N <- n

di <- cut(n, breaks = c(0, 5, 10, 20, 42))
df_uf$di <- factor(ifelse(is.na(di), 0, paste(di)), levels = c(0, levels(di)))

###---------------------------------------------------------------------------###

## plot
ggplot(df_uf) +
  geom_sf(aes(fill = di)) +
  scale_fill_brewer(name = "Nº Pesquisas") + 
  theme_minimal() +
  theme(axis.text = element_blank())

###---------------------------------------------------------------------------###
