library(readxl)
library(data.table)
library(sf)
library(tmap)
library(geobr)

directory.vqr <- "C:/Users/vfqdr/OneDrive - Insper - Institudo de Ensino e Pesquisa/amz_access"

# Baixando arquivos ----
estados <- c("RO", "AC", "AM", "RR", "PA", "AP", "TO", 
             "MA", "PI", "CE", "RN", "PB", "PE", "AL", 
             "SE", "BA", "MG", "ES", "RJ", "SP", "PR", 
             "SC", "RS", "MS", "MT", "GO", "DF")

anos2 <- c("2007", "2008", "2009", "2010", "2011", "2012",
          "2013", "2014", "2015", "2016", "2017", "2018",
          "2019", "2020", "2021", "2022", "2023")

anos_minas <- c("2009", "2010", "2011", "2012", "2013", "2014", 
                "2015", "2016", "2017", "2018","2019", "2020", 
                "2021", "2022", "2023")

# rodando o limpa_ibge --------------------------
source(file = file.path(directory.vqr, "01_code/limpa_ibge.R"))

diretorio_destino <- file.path(directory.vqr, "02_data_in/Ibama/dof")
# Loop para baixar os arquivos ----
for (estado in estados) {
  if (estado == "MG"){
    anos <- anos_minas}
  for (ano in anos) {
    url <- sprintf("http://dadosabertos.ibama.gov.br/dados/DOF/%s/transporte/%s.csv", estado, ano)
    nome_arquivo <- sprintf("dof_%s_%s.csv", tolower(estado), ano)
    caminho_arquivo <- file.path(diretorio_destino, nome_arquivo)
    download.file(url, destfile = caminho_arquivo, mode = "wb")
    
    cat("Baixado:", caminho_arquivo, "\n")
  
  }
anos <- anos2 
}

# Limpando arquivos ---- 

# Preparando nomes novos
nomes_novo <- c("nome_remetente", "cpf_cnpj_remetente", "uf_origem",
                "municipio_origem", "ctf_remetente", "tipo_origem",
                "nome_patio_origem", "numero_autex", "numero_aut_original",
                "tipo_autex", "org_emissor_autex", "validade_autex",
                "numero_di","org_emissor_di", "validade_di", "porto_entrada", "pais_origem", "numero_autesp",
                "org_emissor_autesp", "validade_autesp", "lat_origem", "long_origem",
                "uf_destino", "municipio_destino", "nome_destinatario", "cpf_cnpj_destinatario",
                "ctf_destinatario", "nome_patio_destinatario", "lat_destino", "long_destino",
                "porto_saida", "municipio_porto", "uf_porto", "pais_destino", "data_emissao",
                "ano", "validade_inicial", "validade_final", "ultima_transacao", "data_ult_trans",
                "numero_oferta", "num_serie_dof", "cod_controle_dof", "rota", "produto", 
                "nome_cientifico", "nome_popular", "unidade", "volume", "valor", "ultima_atualizacao")

# Essa parte checa se os nomes batem com os da base original
caminho <- file.path(directory.vqr, "02_data_in/Ibama/dof/dof_ac_2007.csv")
arq <- fread(caminho, dec = ",")

nomes_velho <- names(arq)

check_names <- cbind(nomes_novo, nomes_velho)
View(check_names)

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

IBGE <- fread(file.path(directory.vqr, "02_data_in/IBGE/ibge_limpo.csv"), dec=",", drop = 1)

A = matrix(
  
  rep(0, length(estados)*length(anos)),
  nrow = length(anos),
  ncol = length(estados),
)

A <- as.data.frame(A)

names(A) <- estados
rownames(A) <- anos

na_origem <- A
na_destino <- A
obs <- A

for (estado in estados) {
  if (estado == "MG"){
    anos <- anos_minas}
  for (ano in anos) {
    # carregando arquivos
    caminho <- sprintf(file.path(directory.vqr, "02_data_in/Ibama/dof/dof_%s_%s.csv"), tolower(estado), ano)
    caminho2 <- sprintf(file.path(directory.vqr, "02_data_in/Ibama/dof/_cleanData/dof_%s_%s.csv"), tolower(estado), ano)
    arq <- fread(caminho, dec = ",")
    
    # trocando nomes (#usar a funçcao set names do data.table)
    names(arq) <- nomes_novo
    
    #tirando acentos
    arq <- as.data.frame(arq)
    colunas_texto <- sapply(arq, is.character)  # Identificar colunas de texto
    arq[, colunas_texto] <- lapply(arq[, colunas_texto], toupper)
      
    arq[, colunas_texto] <- lapply(arq[, colunas_texto], remove_acentos)
    
    arq <- merge(arq, IBGE, by.x = c("municipio_origem", "uf_origem") , 
                 by.y = c("nome_municipio", "uf") , all.x = T)
    
    arq <- merge(arq, IBGE, by.x = c("municipio_destino", "uf_destino") , 
                 by.y = c("nome_municipio", "uf") , all.x = T, 
                 suffixes = c("_origem", "_destino"))
    

    #escrevendo csv
    write.table(arq, file = caminho2, row.names = FALSE,sep = ";")
    
    cat("Limpo:", estado, ano, "\n")
    
    na_origem[as.numeric(ano)-2006,estado] <- sum(is.na(arq$code_origem))
    na_destino[as.numeric(ano)-2006,estado] <- sum(is.na(arq$code_destino))
    obs[as.numeric(ano)-2006,estado] <- length(arq$municipio_destino)
  }
anos <- anos2
}


write.csv(obs, file = file.path(directory.vqr, "02_data_in/observacoes.csv"))
write.csv(na_origem, file = file.path(directory.vqr, "02_data_in/na_origem.csv"))
write.csv(na_destino, file = file.path(directory.vqr, "02_data_in/na_destino.csv"))

# Testando lat/long ----

sc <- fread( file.path(directory.vqr, "02_data_in/Ibama/dof/_cleanData/dof_sc_2019.csv"))
ba <- fread("C:/Users/vfqdr/OneDrive - Insper - Institudo de Ensino e Pesquisa/amz_access/02_data_in/Ibama/dof/_cleanData/dof_ba_2011.csv")

sc_geo <- st_as_sf(sc, coords = c("long_origem", "lat_origem"))
ba_geo <- st_as_sf(ba, coords = c("long_origem", "lat_origem"))

sc_geo <- sc_geo[sc_geo$code_origem==4214409,]
ba_geo <- ba_geo[ba_geo$code_origem==2918407,]

plot(sc_geo$geometry)

estados <- read_country(year=2019,showProgress = FALSE)
tm_shape(estados) + tm_borders(alpha=0.3) + tm_shape(sc_geo) + tm_dots("red") + tm_shape(ba_geo)+ tm_dots("blue")

