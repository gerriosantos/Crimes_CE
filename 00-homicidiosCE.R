
rm(list = ls())
pacman::p_load('microdatasus', 'tidyverse', 'geobr', 'ribge')
#dado <- microdatasus::fetch_datasus(year_start = 1996, month_start = 01, year_end = 2020,
 #                                   month_end = 12, uf = 'CE', information_system = 'SIM-DO')

#saveRDS(dado, 'dado.RDS')
#dado <- readRDS('dado.RDS')


# Package ribge do ipea.
#complete_data <- do.call(rbind, lapply(pop_serie, populacao_municipios))

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


#pop2020 <- populacao_municipios(2020) %>% filter(uf == 'CE') %>% 
 # select(cod_mun = cod_munic6, pop = populacao)

#nome_mun <- geobr::read_municipality(code_muni = 'CE', year = 2019) %>% 
#mutate(cod_mun = substr(code_muni,1,6), cod_mun = as.numeric(cod_mun)) %>% 
#select(cod_mun, nom_mun = name_muni) %>% left_join(pop2020, by = 'cod_mun')


  
#filter(CODMUNRES == 230440|CODMUNRES == 231290|CODMUNRES == 230730) %>% 
#' Após 2005 o código é o de 6 digito, antes é o de 7 digitos.
#' cid10 de CVLI ----
#' X85 a Y09 e Y35 a Y36
#' 

dado <- readRDS('dado.RDS') %>% 
  mutate(cid10 = substr(CAUSABAS,1,3),
         ano = substr(DTOBITO,5,8),
         ano = as.numeric(ano),
         mes = substr(DTOBITO,3,4),
         CODMUNRES = substr(CODMUNRES,1,6),
         CODMUNRES = as.numeric(as.character(CODMUNRES)),
         id = paste(ano, CODMUNRES, sep = '-')) %>% 
  filter((ano >= 2000) & (cid10 == 'X85'| cid10 == 'X86' | cid10 == 'X87' | cid10 == 'X88' |
                            cid10 == 'X89' | cid10 == 'X90' | cid10 == 'X91' | cid10 == 'X92' | cid10 == 'X93'|
                            cid10 == 'X94' | cid10 == 'X95' | cid10 == 'X96' | cid10 == 'X97'|cid10 == 'X98' | cid10 == 'X99' |
                            cid10 == 'Y00' | cid10 == 'Y01' | cid10 == 'Y02' | cid10 == 'Y03' |
                            cid10 == 'Y04' | cid10 == 'Y05' | cid10 == 'Y06' | cid10 == 'Y07' | cid10 == 'Y08' |
                            cid10 == 'Y09' | cid10 == 'Y35' | cid10 == 'Y36')) %>%
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
  labs(title = 'Homicídios -- Ceará', subtitle = 'CID10 (X85:Y09 e; Y35:Y36)', 
       y = 'Mortes', x = 'Anos', color = '',
       caption = 'Fonte: Dados do SIM-SUS') #+ facet_wrap(~nom_mun)


# Números Relativos (100mil/hab)

  ggplot(dado, aes(x = ano, y = tx_morte))+
  geom_line(color = 'Red')+
  geom_point(color = 'Red')+
  geom_hline(aes(yintercept = 31.1, linetype = 'Média Nacional (100mil/hab)'), color = 'blue')+
  scale_linetype_manual(name = '', values = 'dashed',
                        guide = guide_legend(override.aes = list(color = c("blue"))))+
  scale_x_continuous(breaks = seq(2000,2019, by = 1))+
  scale_y_continuous(breaks = seq(0,100, by = 5))+
  #guides(guide_legend(order = 3))+
  #scale_color_manual(values = c("Blue", "gray20"))+
  theme_light()+
  labs(title = 'Homicídios (100mil/hab) -- Ceará', subtitle = 'cid10 (X85:Y09 e; Y35:Y36)', 
       y = 'Mortes', x = 'Anos', color = '',
       caption = 'Fonte: Dados do SIM-SUS')   #+ facet_wrap(~nom_mun)





  