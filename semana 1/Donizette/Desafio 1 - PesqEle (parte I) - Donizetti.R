library("pesqEle")
library("dplyr")
library("ggplot2")
library("magrittr")

pe <- pe_2018()
pe2 <- tbl_df(ftable(pe$info_uf))
pe2 <- pe2 %>% rename(SIGLA = Var1, FREQ = Freq) %>% filter(SIGLA != "BR")
pe2 <- pe2 %>% add_row(SIGLA = "AP", FREQ = 0)

# Dica: manter o código reproduzível sem colocar caminhos
# muito específicos
# cod_geo <- tbl_df(read.csv("Documentos/Projetos/cod_geo.csv"))
cod_geo <- tbl_df(read.csv("cod_geo.csv"))

arq <- inner_join(pe2,cod_geo)
arq2 <- arq %>% select(CD_GEOCUF,FREQ)
arq2$CD_GEOCUF <- as.character(arq2$CD_GEOCUF)
arq2$CORTE <- cut(arq2$FREQ, breaks = c(0,0.1,5,10,20,42), include.lowest = TRUE)
arq2$CORTE <- as.numeric(arq2$CORTE)

mapa <- arq2 %>% inner_join(ufshape::df_uf) %>% { ggplot(.) +
    geom_sf(aes(fill = CORTE))+
    scale_fill_continuous(name = "Nº de pesquisas", low = "white", high = "deepskyblue4",
                          guide = guide_legend(),
                          labels = c("0","(0,5]","(5,10]","(10,20]","(20,42]"))+
    theme_minimal() + theme(axis.text = element_text(colour = "white"))
}

mapa

