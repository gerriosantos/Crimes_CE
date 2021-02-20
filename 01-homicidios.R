
# Homicídios -----

##' Óbitos causados por agressão e intervenção legal. 
##' Dados adquiridos pelos códigos da CID-10:
##'  X85-Y09 (agressão) e também Y35-Y36 (intervenção legal).
##'   Fonte: TABNET/DATASUS.

rm(list = ls())
pacman::p_load('microdatasus', 'tidyverse', 'geobr', 'ribge')


# População 2000 a 2019 ----
pop_serie = c(2000:2019)
data <- data.frame()

for(i in pop_serie){
  if(i != 2007){
    p <- populacao_municipios(i) %>% filter(uf == 'CE') %>%
      mutate(ano = i) %>% select(ano, nom_mun = nome_munic,
                                 cod_mun = cod_munic6, pop = populacao) %>% data.frame
    #assign(paste0('p', i), p)
    #df <- data.frame(p)
    data <- rbind(data, p)}
}
# Duas formas de remover arquivos que ficam gravados no diretório.
lista <- list.files(pattern = '.xls|.zip')
lapply(lista, file.remove)
#unlink(paste(getwd(), "*.zip", sep = '/'))

d <- data.frame(subset(data, subset = ano == 2006)) %>% 
  mutate(ano = 2007)
data <- bind_rows(data,d) %>% mutate(id = paste(ano, cod_mun, sep = '-')) %>% 
  filter(ano >= 2000) %>% select(nom_mun, id, pop)
rm(p,i,pop_serie,d, lista)


# Base do SIM ----
dado <- readRDS('dado.RDS') %>% 
  mutate(cid10 = substr(CAUSABAS,1,3),
         ano = substr(DTOBITO,5,8),
         mes = substr(DTOBITO,3,4),
         CODMUNRES = substr(CODMUNRES,1,6),
         CODMUNRES = as.numeric(as.character(CODMUNRES)),
         id = paste(ano, CODMUNRES, sep = '-')) %>% 
  filter((ano >= 2000) & (cid10 == 'X85'| cid10 == 'X86' | cid10 == 'X87' | cid10 == 'X88' |
           cid10 == 'X89' | cid10 == 'X90' | cid10 == 'X91' | cid10 == 'X92' | cid10 == 'X93'|
           cid10 == 'X94' | cid10 == 'X95' | cid10 == 'X96' | cid10 == 'X97'|cid10 == 'X98' | cid10 == 'X99' |
           cid10 == 'Y00' | cid10 == 'Y01' | cid10 == 'Y02' | cid10 == 'Y03' |
           cid10 == 'Y04' | cid10 == 'Y05' | cid10 == 'Y06' | cid10 == 'Y07' | cid10 == 'Y08' |
           cid10 == 'Y09' | cid10 == 'Y35' | cid10 == 'Y36')) %>% left_join(data, by = 'id') %>% 
    group_by(ano, CODMUNRES, nom_mun, id) %>% summarise(n = n(), pop = first(pop)) %>%
    mutate(ano = as.numeric(ano), tx_morte = (n/pop)*100000) %>% na.omit()



# Cidades com maiores populações do CE -----

  ## Números absolutos de mortes

dado %>%
  filter((CODMUNRES == 230440 | CODMUNRES == 230370  | CODMUNRES == 230730 |
                         CODMUNRES == 230765 | CODMUNRES == 231290)) %>% 
  ggplot(aes(x = ano, y = n, group = nom_mun, color = nom_mun))+
  geom_line()+
  geom_point()+
  #geom_hline(yintercept = c(40,60), size = 0.3, color = 'Black', linetype = 'dashed')+
  scale_x_continuous(breaks = seq(2000,2019, by = 1))+
  scale_y_continuous(breaks = seq(0,3000, by = 100))+
  guides(guide_legend(order = 3))+
  #scale_color_manual(values = c("Blue", "gray20"))+
  theme_light()+
  labs(title = 'Homicídios -- 5 Maiores Cidades em termos Populacionais do Ceará - 2019',
       subtitle = 'CID10 (X85:Y09 e; Y35:Y36)', 
       y = 'Mortes', x = 'Anos', color = '',
       caption = 'Fonte: Dados do SIM-SUS')


  ## Mortes por 100 mil/hab
dado %>%
  filter((CODMUNRES == 230440 | CODMUNRES == 230370  | CODMUNRES == 230730 |
            CODMUNRES == 230765 | CODMUNRES == 231290)) %>% 
  ggplot(aes(x = ano, y = tx_morte, group = nom_mun, color = nom_mun))+
  geom_line()+
  geom_point()+
  geom_hline(aes(yintercept = 31.1, linetype = 'Média Nacional (100mil/hab)'), color = 'black')+
  scale_linetype_manual(name = '', values = 'dashed', 
                        guide = guide_legend(override.aes = list(color = c("black"))))+
  scale_x_continuous(breaks = seq(2000,2019, by = 1))+
  scale_y_continuous(breaks = seq(0,200, by = 5))+
  guides(guide_legend(order = 3))+
  #scale_color_manual(values = c("Blue", "gray20"))+
  theme_light()+
  labs(title = 'Homicídios (100mil/hab) -- 5 Maiores Cidades em termos Populacionais do Ceará', subtitle = 'CID10 (X85:Y09 e; Y35:Y36)', 
       y = 'Mortes', x = 'Anos', color = '',
       caption = 'Fonte: Dados do SIM-SUS') #+ facet_wrap(~nom_mun)




# Cidades com menores populações do CE -----

## Números absolutos de mortes

dado %>%
  filter((CODMUNRES == 230480 | CODMUNRES == 230510  | CODMUNRES == 230180 |
            CODMUNRES == 231123 | CODMUNRES == 230990)) %>% 
  ggplot(aes(x = ano, y = n, group = nom_mun, color = nom_mun))+
  geom_line()+
  geom_point()+
  #geom_hline(yintercept = c(40,60), size = 0.3, color = 'Black', linetype = 'dashed')+
  scale_x_continuous(breaks = seq(2000,2019, by = 1))+
  scale_y_continuous(breaks = seq(0,50, by = 1))+
  guides(guide_legend(order = 3))+
  #scale_color_manual(values = c("Blue", "gray20"))+
  theme_light()+
  labs(title = 'Homicídios -- 5 Menores Cidades em termos Populacionais do Ceará', 
       subtitle = 'CID10 (X85:Y09 e; Y35:Y36)', 
       y = 'Mortes', x = 'Anos', color = '',
       caption = 'Fonte: Dados do SIM-SUS') #+ facet_wrap(~nom_mun)



## Mortes por 100 mil/hab
dado %>%
  filter((CODMUNRES == 230480 | CODMUNRES == 230510  | CODMUNRES == 230180 |
            CODMUNRES == 231123 | CODMUNRES == 230990)) %>% 
  ggplot(aes(x = ano, y = tx_morte, group = nom_mun, color = nom_mun))+
  geom_line()+
  geom_point()+
  geom_hline(aes(yintercept = 31.1, linetype = 'Morte Brasil (100mil/hab)'), color = 'black')+
  scale_linetype_manual(name = '', values = 'dashed', 
                        guide = guide_legend(override.aes = list(color = c("black"))))+
  scale_x_continuous(breaks = seq(2000,2019, by = 1))+
  scale_y_continuous(breaks = seq(0,300, by = 5))+
  guides(guide_legend(order = 3))+
  #scale_color_manual(values = c("Blue", "gray20"))+
  theme_light()+
  labs(title = 'Homicídios (100mil/hab) -- 5 Menores Cidades em termos Populacionais do Ceará - 2019',
       subtitle = 'CID10 (X85:Y09 e; Y35:Y36)', 
       y = 'Mortes', x = 'Anos', color = '',
       caption = 'Fonte: Dados do SIM-SUS') #+ facet_wrap(~nom_mun)


# Coreau ----

dado %>%
  filter((CODMUNRES == 230400 | CODMUNRES == 230880  | CODMUNRES == 230050 |
            CODMUNRES == 230450 | CODMUNRES == 231290)) %>% 
  ggplot(aes(x = ano, y = tx_morte, group = nom_mun, color = nom_mun))+
  geom_line()+
  geom_point()+
  geom_hline(aes(yintercept = 31.1, linetype = 'Média Nacional (100mil/hab)'), color = 'black')+
  scale_linetype_manual(name = '', values = 'dashed', 
                        guide = guide_legend(override.aes = list(color = c("black"))))+
  scale_x_continuous(breaks = seq(2000,2019, by = 2))+
  scale_y_continuous(breaks = seq(0,100, by = 5))+
  guides(guide_legend(order = 3))+
  #scale_color_manual(values = c("Blue", "gray20"))+
  theme_light()+
  labs(title = 'Homicídios (100mil/hab) -- Coreaú', subtitle = 'cid10 (X85:Y09 e; Y35:Y36)', 
       y = 'Mortes', x = 'Anos', color = '',
       caption = 'Fonte: Dados do SIM-SUS') #+ facet_wrap(~nom_mun)







