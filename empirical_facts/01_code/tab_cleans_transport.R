################################################################################
# Project: Illegal Logging                                                     #
# Author: Victor Quadros                                                       #
# Content:                                                                     # 
################################################################################

rm(list = ls())
gc()


library(data.table)
library(stringr)
library(readr)
library(dplyr)
library(tidyr)

# Base folder
base_dir <- "empirical_facts/02_inputs/tabular/dof"

# Target names (exactly as provided)
target_names <- c(
  "nome_remetente", "cpf_cnpj_remetente", "uf_origem",
  "municipio_origem", "ctf_remetente", "tipo_origem",
  "org_homologador", "data_homologacao", "nome_patio_origem", "numero_autex", "numero_aut_original",
  "tipo_autex", "org_emissor_autex", "validade_autex",
  "numero_di","org_emissor_di", "validade_di", "porto_entrada", "pais_origem", "numero_autesp",
  "org_emissor_autesp", "validade_autesp", "lat_origem", "long_origem",
  "uf_destino", "municipio_destino", "nome_destinatario", "cpf_cnpj_destinatario",
  "ctf_destinatario", "nome_patio_destinatario", "lat_destino", "long_destino",
  "porto_saida", "municipio_porto", "uf_porto", "pais_destino", "data_emissao",
  "ano", "validade_inicial", "validade_final", "ultima_transacao", "data_ult_trans",
  "numero_oferta", "num_serie_dof", "cod_controle_dof", "rota", "produto", "cod_rastreio",
  "nome_cientifico", "nome_popular", "unidade", "volume", "valor", "valor2", 
  "sistema_originario", "ultima_atualizacao")

# List files
files <- list.files(
  path = base_dir,
  pattern = "^DOFTransportes_ano_[0-9]{4}_uf_[A-Za-z]{2}\\.csv$",
  full.names = TRUE
)

read_one <- function(f) {
  yr <- str_match(basename(f), "^DOFTransportes_ano_([0-9]{4})_uf_([A-Za-z]{2})\\.csv$")[,2]
  if (is.na(yr)) stop("Could not parse year from filename: ", f)
  yr <- as.integer(yr)
  dt <- fread(f) %>%
    rename_with(~ target_names) %>%
    filter(produto == "Tora",
           ultima_transacao == "Recebido") %>% 
    mutate(year = yr)
  dt[, year := yr]
  dt
}

dt_all <- rbindlist(lapply(files, read_one), use.names = TRUE, fill = FALSE)

save(dt_all, file = "empirical_facts/03_data_outputs/tab/dof.RData")

df_all <- dt_all %>% filter(year == 2010) 

all_munis <- sort(unique(c(df_all$municipio_origem, df_all$municipio_destino)))

trade_shares <-   df_all %>% 
  group_by(year, municipio_origem, municipio_destino) %>%
  summarise(volume = sum(volume, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  complete(
    municipio_origem  = all_munis,
    municipio_destino = all_munis,
    fill = list(volume = 0)
  ) %>%
  ungroup()

trade_shares

