

# Librerias ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(openxlsx)


# Cargar base de datos ----------------------------------------------------

base <-
  readxl::read_xlsx(path = '00 insumos/IDV_ecuador.xlsx', sheet = '01_Idv_sin_noboa')

# cambiar columnas a una sola fila

base <- 
  
  base %>% gather(data = ., key = 'candidato', value = 'idv',
                  -encuesta_raw, -Encuestador, -ini_enc, -fin_enc, -Fecha, -diseno_enc)

# convertir en numero el valor que esta en character

base <- 
  
  base %>% mutate(idv=as.numeric(idv))

base <- 
  
  # agregar los datos para obtener los indefinidos
  
  base %>% filter(str_detect(candidato, pattern = 'Blanc|Nulo|NSNR')) %>% 
  
  group_by(Encuestador, Fecha) %>% 
  
  summarise(indefinidos=sum(idv, na.rm = T)) %>% 
  
  ungroup() %>% 
  
  # juntar con la base original
  
  left_join(x = base, y = .)

# crear variable de votos validos

base <- 
  
  base %>% 
  
  mutate(idv_val=idv/(1-indefinidos), 
         idv_val=case_when(candidato %>% str_detect('Blanc|Nulo|NSNR') ~ NA_real_, 
                           T ~ idv_val)) %>%

# crear una variable candidato solo con importantes 
  
  mutate(candidato2=case_when(candidato %>% str_detect('Arauz|Lasso|Perez') ~ candidato,
                              candidato %>% str_detect('Blanc|Nulo|NSNR') ~ 'Indefinidos',
                              T ~ 'Otros')) %>% 
  
# variable de promedio de intencion de voto
  
  group_by(candidato2) %>% 
  
  mutate(idv_val_promedio=mean(idv_val, na.rm = T)) 


  

# Grafico -----------------------------------------------------------------
# Establecer el orden para el gráfico

base %>% 
  
  filter(!str_detect(candidato2, pattern = 'Indefi'), 
         !is.na(idv_val)) %>% 
  
  mutate(candidato2=factor(candidato2, levels = c('Arauz','Lasso', 'Perez', 'Otros'))) %>% 
  
  ggplot(data = ., aes(x=Fecha, y=idv_val, col=Encuestador)) + geom_point() + 
  
  facet_grid(.~candidato2)
  
  
  
  geom_bar(data = base %>% 
              
              filter(!str_detect(candidato2, pattern = 'Indefi')) %>% 
              
              mutate(candidato2=factor(candidato2, levels = c('Arauz','Lasso', 'Perez', 'Otros'))), 
            aes(y=idv_val_promedio, col=candidato2)) +








