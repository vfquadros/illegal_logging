library(data.table)
library(dplyr)
library(tidyverse)

directory.vqr <- "C:/Users/vfqdr/OneDrive - Insper - Institudo de Ensino e Pesquisa/amz_access"

# Baixando arquivos ----
estados <- c("RO", "RR", "AM", "AP", 
             "TO", "MA", "PA", "AC", "MT") 
estados <- tolower(estados)

anos <- 2007:2022


# Liste os arquivos no diretório
arquivos <- list.files(file.path(directory.vqr, "02_data_in/Ibama/dof/_cleanData"))

# empilha anos ----
for (ano in anos){
  dados_def <- list()
  for(estado in estados){
  for (arquivo in arquivos) {
    # Extrai o nome do estado do arquivo (assumindo que o nome do estado está no nome do arquivo)
      check <- grepl(paste("_",ano, sep =""), basename(arquivo))
      check2 <- grepl(paste("_",estado, sep =""), basename(arquivo))
    
    # Leia o arquivo e faça o que for necessário com os dados
    if (check == TRUE & check2 == TRUE){
      dados <- fread(file.path(directory.vqr, "02_data_in/Ibama/dof/_cleanData", arquivo))  # Ou use a função apropriada para ler seus dados
      dados <- dados[produto == "TORA" &
                     (ultima_transacao == "RECEBIDO" | ultima_transacao == "FORCADA ENTREGA" )]
      dados <- lapply(dados, as.character)
      dados_def[[paste0(arquivo)]] <- dados  
      cat(arquivo, "\n")
    }
  }
}
  
  dados_def <- bind_rows(dados_def, .id = "arquivo")
  
  dir.final <- sprintf(file.path(directory.vqr, "02_data_in/Ibama/dof/_cleanData/toras/toras_amz_%s.csv"), ano)
  write.table(dados_def, dir.final, row.names = F)
  print("--------------")
}

