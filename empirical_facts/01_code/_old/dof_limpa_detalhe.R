library(data.table)
library(tidyverse)
library(dplyr)

directory <- getwd()
destiny <- file.path(directory, "02_data_in/Ibama/dof/dof_detalhe")

# Baixando arquivos ----
estados <- c("RO", "AC", "AM", "RR", "PA", "AP", "TO", 
             "MA", "PI", "CE", "RN", "PB", "PE", "AL", 
             "SE", "BA", "MG", "ES", "RJ", "SP", "PR", 
             "SC", "RS", "MS", "MT", "GO", "DF")

anos <- c("2007", "2008", "2009", "2010", "2011", "2012",
          "2013", "2014", "2015", "2016", "2017", "2018",
          "2019", "2020", "2021", "2022", "2023")

# Loop para baixar os arquivos ----
for (estado in estados) {
  for (ano in anos) {
    url <- sprintf("http://dadosabertos.ibama.gov.br/dados/DOF/%s/detalhe/%s.csv", estado, ano)
    nome_arquivo <- sprintf("dof_detalhe_%s_%s.csv", tolower(estado), ano)
    caminho_arquivo <- file.path(destiny, nome_arquivo)
    download.file(url, destfile = caminho_arquivo, mode = "wb")
    
    cat("Baixado:", caminho_arquivo, "\n")
}}

# Preparando nomes novos
nomes_novo <- c("num_serie_dof","num_trecho", "municipio_origem", "municipio_destino",
                "tipo_transporte", "placa", "uf", "ultima_atualizacao")

# Essa parte checa se os nomes batem com os da base original
caminho <- file.path(directory, "02_data_in/Ibama/dof/dof_detalhe_ac_2007.csv")
arq <- fread(caminho, dec = ",")

nomes_velho <- names(arq)

check_names <- cbind(nomes_novo, nomes_velho)
#View(check_names)

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

for (estado in estados) {
  #Para minas, os dados estao disponiveis a partir de 2009
  for (ano in anos) {
    # carregando arquivos
    caminho <- sprintf(file.path(directory, "02_data_in/Ibama/dof/dof_detalhe_%s_%s.csv"), tolower(estado), ano)
    caminho2 <- sprintf(file.path(directory, "02_data_in/Ibama/dof/_cleanData/dof_detalhe/dof_detalhe_%s_%s.csv"), tolower(estado), ano)
    arq <- fread(caminho, dec = ",")
    
    # trocando nomes (#usar a funçcao set names do data.table)
    names(arq) <- nomes_novo
    
    #tirando acentos
    arq <- as.data.frame(arq)
    colunas_texto <- sapply(arq, is.character)  # Identificar colunas de texto
    arq[, colunas_texto] <- lapply(arq[, colunas_texto], toupper)
    
    arq[, colunas_texto] <- lapply(arq[, colunas_texto], remove_acentos)
    
    # lembrar de criar base para checar NAs em cada uf ano 
    
    #escrevendo csv
    write.table(arq, file = caminho2, row.names = FALSE, quote = FALSE)
    
    cat("Limpo:", estado, ano,"\n")
  }
}

# Investigando um padrao estranho

arq <- fread(file.path(directory, "02_data_in/Ibama/dof/_cleanData/dof_pa_2022.csv"), keepLeadingZeros = TRUE)
arq2 <- fread(file.path(directory, "02_data_in/Ibama/dof/_cleanData/dof_detalhe/dof_detalhe_pa_2022.csv"), keepLeadingZeros = TRUE)

#essa parte cria colunas que contam quais tipos de transporte estão presentes em quais bases
#note que o tamanho desse objeto é o número de dofs únicos emitidos. 
arq2 %>% 
  as_tibble() %>% 
  mutate(value = 1) %>% 
  group_by(tipo_transporte, num_serie_dof) %>% 
  summarise(value = sum(value)) %>% 
  pivot_wider(names_from = tipo_transporte, values_from = value) %>% 
  mutate(across(!num_serie_dof, ~ ifelse(is.na(.), 0, .)))
  # summarise(across(!num_serie_dof, ~ sum(.)))

# Esse é o número de dofs únicas no dof original.
# Note que esse número é menor que o tamanho do que o número de 
# dofs únicos na base dof_detalhe!!!! Conclusão: tem dofs faltantes na base original.

length(unique(arq$num_serie_dof))

# Total por tipo_transporte
arq2 %>% 
  as_tibble() %>% 
  group_by(tipo_transporte) %>% 
  count()

# Total por tipo_transporte dos transportes que não tem dof na base principal
teste2 <- arq2 %>% 
  as_tibble() %>% 
  anti_join(arq
              , by = "num_serie_dof")

#A distribuição dos dados faltantes parece estar viesada.

# Alguns gráficos para agora ----
estados <- c("AC", "AM", "RO", "PA")
anos <- c(2017,2018,2019,2020,2022)

for (estado in estados){
  base_def <- list()
  vol <- list()
  prec <- list()
  for (ano in anos){
    caminho <- sprintf(file.path(directory, "02_data_in/Ibama/dof/_cleanData/toras/toras_%s.csv"), ano)
    arq <- fread(caminho, dec = ",")

    base_def <- subset(arq, uf==estado)
    vol_ <- base_def[, .(total_volume = sum(as.numeric(volume))), by = .(ano, tipo_transporte)]
    prec_ <- base_def[valor >0 , .(preco = mean(sum(as.numeric(valor))/sum(as.numeric(volume)))), by = .(ano, tipo_transporte)]
    
    vol <- rbind(vol, vol_)
    prec <- rbind(prec, prec_)
  }
  
  
  ggplot(vol, aes(x = ano, y = total_volume, color = tipo_transporte)) +
    geom_line(size = 1) +
    scale_colour_manual(values=c('#811622','#E69F00', "#999999", "#199997", "#E99997")) +
    scale_x_continuous(name = "Ano", breaks = c(seq(2017, 2022, by = 1))) +
    scale_y_continuous(name = "Volume Total (m3)") +
    theme(panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_line(linetype = 3, color = "grey"),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom",
          legend.key = element_blank(),
          legend.title = element_blank(),
          legend.text=element_text(size=12)
    )
  
  ggsave(plot = last_plot(), 
         filename = sprintf(file.path(directory, "04_outputs/modals/volumeXmodalxano_%s.png"), tolower(estado)), 
         width = 10, height = 7)
  
    ggplot(prec, aes(x = ano, y = preco, color = tipo_transporte)) +
    geom_line(size = 1) +
    scale_colour_manual(values=c('#811622','#E69F00', "#999999", "#199997", "#E99997")) +
    scale_x_continuous(name = "Ano", breaks = c(seq(2017, 2022, by = 1))) +
    scale_y_continuous(name = "Preço médio (m3)") +
    theme(panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_line(linetype = 3, color = "grey"),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom",
          legend.key = element_blank(),
          legend.title = element_blank(),
          legend.text=element_text(size=12)
    )
  
  ggsave(plot = last_plot(), 
         filename = sprintf(file.path(directory, "04_outputs/modals/precoXmodalxano_%s.png"), tolower(estado)), 
         width = 10, height = 7)
}
