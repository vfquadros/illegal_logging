library(readxl)
library(data.table)
library(sf)
library(tmap)
library(geobr)

directory.vqr <- "C:/Users/vfqdr/OneDrive - Insper - Institudo de Ensino e Pesquisa/amz_access"
remove_acentos <- function(texto) {
  # Substituir os caracteres acentuados pelos equivalentes sem acento
  texto <- gsub("[âàãáâ]", "A", texto, ignore.case = TRUE)
  texto <- gsub("[èéêë]", "E", texto, ignore.case = TRUE)
  texto <- gsub("[íìîï]", "I", texto, ignore.case = TRUE)
  texto <- gsub("[õóòôö]", "O", texto, ignore.case = TRUE)
  texto <- gsub("[üûúù]", "U", texto, ignore.case = TRUE)
  texto <- gsub("[ç]", "C", texto, ignore.case = TRUE)
  return(texto)
}

IBGE <- read_excel(file.path(directory.vqr, "02_data_in/IBGE/AR_BR_RG_UF_MES_MIC_MUN_2018.xls"))

IBGE <- IBGE[,c("NM_MUN_2018", "NM_UF_SIGLA", "CD_GCMUN")] # exemplo para subsetar colunas
names(IBGE) <- c("nome_municipio", "uf", "code")

IBGE <- as.data.frame(IBGE)
colunas_texto <- sapply(IBGE, is.character)  # Identificar colunas de texto
IBGE[, colunas_texto] <- lapply(IBGE[, colunas_texto], remove_acentos)


IBGE<-rbind(IBGE,list("EMBU","SP",3515004))
IBGE<-rbind(IBGE,list("PASSA-VINTE","MG",3147808))
IBGE<-rbind(IBGE,list("SANTANA DO LIVRAMENTO","RS",4317103))
IBGE<-rbind(IBGE,list("SAO LUIS DO PARAITINGA","SP",3550001))
IBGE<-rbind(IBGE,list("BRASOPOLIS","MG",3108909))
IBGE<-rbind(IBGE,list("MOJI MIRIM","SP",3530805))
IBGE<-rbind(IBGE,list("BIRITIBA-MIRIM","SP",3506607))
IBGE<-rbind(IBGE,list("TRAJANO DE MORAIS","RJ",3305901))
IBGE<-rbind(IBGE,list("PARATI","RJ",3303807))
IBGE<-rbind(IBGE,list("PICARRAS","SC",4212809))
IBGE<-rbind(IBGE,list("POXOREO","MT",5107008))
IBGE<-rbind(IBGE,list("SANTA TERESINHA","BA",2928505))
IBGE<-rbind(IBGE,list("ASSU","RN",2400208))
IBGE<-rbind(IBGE,list("SAO LUIZ DO ANUAA","RR",1400605))
IBGE<-rbind(IBGE,list("SANTA ISABEL DO PARA","PA",1506500))
IBGE<-rbind(IBGE,list("SERIDO","PB",2515401))
IBGE<-rbind(IBGE,list("BELEM DE SAO FRANCISCO","PE",2601607))
IBGE<-rbind(IBGE,list("NOVA PETROLANDIA","PE",2611002))
IBGE<-rbind(IBGE,list("IGUARACI","PE",2606903))
IBGE<-rbind(IBGE,list("CAMPO DE SANTANA","PB",2516409))
IBGE<-rbind(IBGE,list("PIACAS","PA",1505650))
IBGE<-rbind(IBGE,list("PRESIDENTE JUSCELINO","RN",2410306))
IBGE<-rbind(IBGE,list("PINGO-D'AGUA","MG",3150539))
IBGE<-rbind(IBGE,list("OLHO-D'AGUA DO BORGES","RN",2408409))
IBGE<-rbind(IBGE,list("MUQUEM DE SAO FRANCISCO","BA",2922250))
IBGE<-rbind(IBGE,list("SAO VALERIO DA NATIVIDADE","TO",1720499))
IBGE<-rbind(IBGE,list("COUTO DE MAGALHAES","TO",1706001))
IBGE<-rbind(IBGE,list("ELDORADO DOS CARAJAS","PA",1502954))
IBGE<-rbind(IBGE,list("PAU D ARCO","TO",1716307))
IBGE<-rbind(IBGE,list("LAGOA DO ITAENGA","PE",2608503))
IBGE<-rbind(IBGE,list("ITAPAGE","CE",2306306))
IBGE<-rbind(IBGE,list("SANTAREM","PB",2513653)) #checar
IBGE<-rbind(IBGE,list("SAO DOMINGOS DE POMBAL","PB",2513968)) #wikipedia
IBGE<-rbind(IBGE,list("FLORINIA","SP",3516101))
IBGE<-rbind(IBGE,list("PRESIDENTE CASTELO BRANCO","SC",4213906))
IBGE<-rbind(IBGE,list("PASSOS DE TORRES","SC",4212270))
IBGE<-rbind(IBGE,list("COLINAS DE GOIAIS","GO",5205521))
 
write.csv(IBGE, file = file.path(directory.vqr, "02_data_in/IBGE/ibge_limpo.csv"))


