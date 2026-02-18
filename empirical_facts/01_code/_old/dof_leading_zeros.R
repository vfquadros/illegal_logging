library(data.table)

directory <- getwd()
options(scipen=999)

estados <- c("RO", "AC", "AM", "RR", "PA", "AP", "TO", 
             "MA", "PI", "CE", "RN", "PB", "PE", "AL", 
             "SE", "BA", "MG", "ES", "RJ", "SP", "PR", 
             "SC", "RS", "MS", "MT", "GO", "DF")

anos <- 2007:2023

for(estado in estados){
  for(ano in anos){
    teste <- fread(sprintf(file.path(getwd(), "02_data_in/Ibama/dof/_cleanData/dof_%s_%s.csv"), tolower(estado), ano))
    teste$num_serie_dof <- as.character(teste$num_serie_dof)
    quart <- quantile(nchar(teste$num_serie_dof))[5]
    #teste$num_serie_dof <- sprintf(paste("%", quart, "s", sep=''),
    #       teste$num_serie_dof)
    teste$num_serie_dof <- formatC(teste$num_serie_dof, width = quart, format = "d", flag = "0")
    cat(estado, ano, "travou o número?",length(unique(nchar(teste$num_serie_dof))) == 1, "\n")
    cat(estado, ano, "esse número é o certo?", unique(nchar(teste$num_serie_dof[1])) == quart, "\n")
    write.table(teste, file = sprintf(file.path(directory, "02_data_in/Ibama/dof/_cleanData/dof_%s_%s"),tolower(estado), ano), row.names = FALSE)
  }
}
