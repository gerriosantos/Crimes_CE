

rm(list = ls())
pacman::p_load('microdatasus', 'tidyverse', 'geobr', 'ribge')

pop_serie = c(2000:2019)
data <- data.frame()

for(i in pop_serie){
  if(i != 2007){
    p <- populacao_municipios(i) %>% filter(uf == 'CE') %>%
      mutate(ano = i) %>% select(ano, nom_mun = nome_munic, cod_mun = cod_munic6, pop = populacao) %>% data.frame
    #assign(paste0('p', i), p)
    #df <- data.frame(p)
    data <- rbind(data, p)}
}
lista <- list.files(pattern = '.xls|.zip')
lapply(lista, file.remove)

d <- data.frame(subset(data, subset = ano == 2006)) %>% 
  mutate(ano = 2007)
data <- bind_rows(data,d) %>% mutate(id = paste(ano, cod_mun, sep = '-'))
rm(p,i,pop_serie,d)

popCE <- data %>% group_by(ano) %>% summarise(popT = sum(pop))




dado <- readRDS('dado.RDS') %>% 
  mutate(cid10 = substr(CAUSABAS,1,3),
         ano = substr(DTOBITO,5,8),
         ano = as.numeric(ano),
         mes = substr(DTOBITO,3,4),
         CODMUNRES = substr(CODMUNRES,1,6),
         CODMUNRES = as.numeric(as.character(CODMUNRES)),
         id = paste(ano, CODMUNRES, sep = '-')) %>% 
  filter((ano >= 2000) & (cid10 == 'X93'| cid10 == 'X94' | cid10 == 'X95' )) %>%
  group_by(ano) %>% summarise(n = n()) %>% left_join(popCE, by = 'ano') %>%
  mutate(tx_morte = (n/popT)*100000)



# Números Absolutos ----

ggplot(dado, aes(x = ano, y = n))+
  geom_line()+
  geom_point()+
  geom_vline(xintercept = c(2005,2010,2016), size = 0.3, color = 'Black', linetype = 'dashed')+
  scale_x_continuous(breaks = seq(1996,2019, by = 1))+
  scale_y_continuous(breaks = seq(0,6000, by = 100))+
  guides(guide_legend(order = 3))+
  #scale_color_manual(values = c("Blue", "gray20"))+
  theme_light()+
  labs(title = 'Armas de Fogo -- Ceará', subtitle = 'CID10 (X85:Y09 e; Y35:Y36)', 
       y = 'Mortes', x = 'Anos', color = '',
       caption = 'Fonte: Dados do SIM-SUS') #+ facet_wrap(~nom_mun)


# Números Relativos (100mil/hab)

ggplot(dado, aes(x = ano, y = tx_morte))+
  geom_line(color = 'Red')+
  geom_point(color = 'Red')+
  geom_hline(aes(yintercept = 31.1, linetype = 'Média Nacional (100mil/hab)'), color = 'blue')+
  geom_hline(aes(yintercept = 25.1, linetype = 'Média Hist. do Estado (100mil/hab)'),
             color = 'green')+
  scale_linetype_manual(name = '', values = c('dashed', 'dashed'),
                        guide = guide_legend(override.aes = list(color = c("blue", "green"))))+
  scale_x_continuous(breaks = seq(2000,2019, by = 1))+
  scale_y_continuous(breaks = seq(0,100, by = 5))+
  #guides(guide_legend(order = 3))+
  #scale_color_manual(values = c("Blue", "gray20"))+
  theme_light()+
  labs(title = 'Armas de Fogo (100mil/hab) -- Ceará', subtitle = 'cid10 (X85:Y09 e; Y35:Y36)', 
       y = 'Mortes', x = 'Anos', color = '',
       caption = 'Fonte: Dados do SIM-SUS') #+ facet_wrap(~nom_mun)


### população anual

ggplot(dado, aes(x = ano, y = popT))+
  geom_col(color = 'white', fill = 'red')+
  labs(title = '', subtitle = '', 
       y = 'População', x = 'Anos', color = '',
       caption = 'Fonte: Dados do IBGE')+
  theme_bw()
