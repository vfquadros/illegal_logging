library(readxl)
library(data.table)
library(sf)
library(tmap)
library(geobr)

directory.vqr <- "C:/Users/vfqdr/OneDrive - Insper - Institudo de Ensino e Pesquisa/amz_access"

estados <- c("SC")
anos <- c("2007", "2008", "2009", "2010", "2011", "2012",
          "2013", "2014", "2015", "2016", "2017", "2018",
          "2019", "2020", "2021", "2022", "2023")
for(estado in estados){
  for(ano in anos){
    arquivo <-fread(sprintf(file.path(directory.vqr, "02_data_in/Ibama/dof/_cleanData/dof_%s_%s.csv"), tolower(estado), ano), drop=1)
    a <- arquivo[is.na(arquivo$code_origem)]
    b<- arquivo[is.na(arquivo$code_destino)]
    vec <- append(unique(b$municipio_destino), unique(a$municipio_origem))
    cat(ano,":", unique(vec), "\n")
  }
}
