library(data.table)
library(dplyr)

directory <- getwd()

#estados <- c("RO", "AC", "AM", "RR", "PA", "AP", "TO", 
#             "MA", "PI", "CE", "RN", "PB", "PE", "AL", 
#             "SE", "BA", "MG", "ES", "RJ", "SP", "PR", 
#             "SC", "RS", "MS", "MT", "GO", "DF")

estados <- c("PA")
anos <- 2007:2022
arquivos <- list.files(file.path(directory, "02_data_in/Ibama/dof/_cleanData/dof_detalhe"))

A <- data.frame(ano = 2007:2022, prop_merges = rep(0,length(2007:2022)))

for (ano in anos){
  dados_def <- list()
  for (arquivo in arquivos) {
    # Extrai o nome do estado do arquivo (assumindo que o nome do estado está no nome do arquivo)
    check <- grepl(paste("_",ano, sep =""), basename(arquivo))
    
    # Leia o arquivo e faça o que for necessário com os dados
    if (check == TRUE){
      dados <- fread(file.path(directory, "02_data_in/Ibama/dof/_cleanData/dof_detalhe", arquivo), dec = ",")  # Ou use a função apropriada para ler seus dados
      dados$num_serie_dof <- as.character(dados$num_serie_dof)
      dados <- dados[,municipio_origem := NULL]
      dados <- dados[,municipio_destino := NULL]
      dados <- dados[,ultima_atualizacao := NULL]
      
      dados_def[[paste0(arquivo)]] <- dados  
      cat(arquivo, "\n")
    }
  }
  transporte <- fread(sprintf(file.path(directory, "02_data_in/Ibama/dof/_cleanData/toras/toras_amz_%s.csv"), ano))
  dados_def <- bind_rows(dados_def, .id = "arquivo")
  
  final <- merge(transporte, dados_def, by = "num_serie_dof", all.x = T)
  
  A$prop_merges[A$ano == ano] <- (length(final$tipo_transporte)-sum(is.na(final$tipo_transporte)))/length(final$tipo_transporte)
  write.table(final, sprintf(file.path(directory, "02_data_in/Ibama/dof/_cleanData/toras/toras_amz_%s.csv"), ano))              
}  

A

anos <- c(2017,2018,2019,2020,2022)

vol <- list()
prec <- list()
  for (ano in anos){
    caminho <- sprintf(file.path(directory, "02_data_in/Ibama/dof/_cleanData/toras/toras_%s.csv"), ano)
    arq <- fread(caminho, dec = ",")
    
    arq %>% 
      as_tibble() %>% 
      group_by(tipo_transporte) %>% 
      count()
    
    vol_ <- arq[, .(total_volume = sum(as.numeric(volume))), by = .(tipo_transporte)]
    prec_ <- arq[valor >0 , .(preco = mean(sum(as.numeric(valor))/sum(as.numeric(volume)))), by = .(tipo_transporte)]
    
    ggplot(vol, aes(x = tipo_transporte, y = total_volume, color = tipo_transporte)) +
      geom_line(size = 1) +
      scale_colour_manual(values=c("RODOVIARIO" = '#811622', "FLUVIAL" = '#E69F00')) +
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
    
    ggplot(prec, aes(x = tipo_transporte, y = preco, color = tipo_transporte)) +
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
  
  
  