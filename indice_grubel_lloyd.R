# 0) Configurações iniciais -----------------------------------------------

# 0.1) Limpando RStudio
rm(list = ls())
cat("\14")

# 0.2) Removendo notação científica
options(scipen = 999)

# 0.3) Importando pacotes necessários
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)

# 0.5) Definindo função para ler base de dados e realizar tratamentos
read_data <- function(df, n_produto) {
  
  # Renomeando e selecionando colunas
  df_filtered <- 
    df %>% 
    dplyr::rename(importador = ReporterName, exportador = PartnerName, ano = Year, 
                  trade = TradeValue.in.1000.USD, produto = ProductCode) %>% 
    dplyr::select(ano, importador, exportador, produto, trade) %>% 
    dplyr::mutate(ano = as.Date(paste0(ano, "-01-01"))) %>% 
    dplyr::filter(trade > 0) %>% 
    dplyr::filter(produto == n_produto)
  
  # Retornando base de dados
  return(df_filtered)
  
}

# 0.5) Definindo função para calcular o índice Grubel-Lloyd, a fim de ser aplicado
# os dados do Brasil e demais países
calc_grubel_lloyd <- function(df, estrangeiro) {
  
  # Filtrando as exportações do Brasil para o estrangeiro
  exports <- 
    df %>%
    dplyr::filter(exportador == "Brazil", importador == estrangeiro) %>%
    dplyr::rename(exports = trade) %>%
    dplyr::select(ano, exports)
  
  # Filtrando as importações do Brasil do estrangeiro
  imports <- 
    df %>%
    dplyr::filter(importador == "Brazil", exportador == estrangeiro) %>%
    dplyr::rename(imports = trade) %>%
    dplyr::select(ano, imports)
  
  # Unindo os dataframes e importações por ano e substituindo NAs por 0
  trade_data <- 
    imports %>%
    dplyr::full_join(exports, by = "ano") %>%
    dplyr::mutate(imports = replace_na(imports, 0),
                  exports = replace_na(exports, 0))
  
  # Calculando o índice Grubel-Lloyd
  trade_data <- 
    trade_data %>%
    dplyr::mutate(grubel_lloyd = 1 - abs(exports - imports) / (exports + imports))
  
  # Retornando base de dados
  return(trade_data)
  
}

# 0.6) Definindo função para plotagem de gráficos
plot_graph <- function(df, titulo) {
  
    ggplotly(
      ggplot(df, aes(x = ano, y = grubel_lloyd)) +
        geom_line(size = 1.2, color = "black") + 
        labs(title = titulo, 
             x = "Ano", 
             y = "Índice Grubel-Lloyd") +                
        theme_minimal() +                                
        theme(
          plot.title = element_text(size = 16, hjust = 0.5),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12))
    )
    
  }

# 0.7) Lendo bases de dados
indbrachi <- read.csv("inputs/indbrachi.csv")
indbraeua <- read.csv("inputs/indbraeua.csv")
indbraarg <- read.csv("inputs/indbraarg.csv")
analiseprodutos <- read.csv("inputs/analiseprodutos.csv")

# 1) Brasil x China -------------------------------------------------------

# 1.1) Café

# 1.1.1) Importando base de dados e tratando informações
bra_chi_cafe <- read_data(df = indbrachi,
                          n_produto = 901)

# 1.1.2) Calculando o índice Grubel-Lloyd
bra_chi_cafe_ind <- calc_grubel_lloyd(df = bra_chi_cafe, 
                                      estrangeiro = "China")

# 1.1.3) Análise gráfica
# Índice igual a zero em todos os anos da amostra.

# 1.2) Fertilizantes

# 1.2.1) Importando base de dados e tratando informações
bra_chi_fertilizantes <- read_data(df = indbrachi,
                                   n_produto = 3105)

# 1.2.2) Calculando o índice Grubel-Lloyd
bra_chi_fertilizantes_ind <- calc_grubel_lloyd(df = bra_chi_fertilizantes, 
                                      estrangeiro = "China")

# 1.2.3) Análise gráfica
plot_graph(df = bra_chi_fertilizantes_ind,
           titulo = "Brasil x China: Índice Grubel-Lloyd ao longo dos anos (Fertilizantes)")

# 1.3) Açúcar

# 1.3.1) Importando base de dados e tratando informações
bra_chi_acucar <- read_data(df = indbrachi,
                            n_produto = 1701)

# 1.3.2) Calculando o índice Grubel-Lloyd
bra_chi_acucar_ind <- calc_grubel_lloyd(df = bra_chi_acucar, 
                                               estrangeiro = "China")

# 1.3.3) Análise gráfica
plot_graph(df = bra_chi_acucar_ind,
           titulo = "Brasil x China: Índice Grubel-Lloyd ao longo dos anos (Açúcar)")

# 2) Brasil x EUA -------------------------------------------------------

# 2.1) Café

# 2.1.1) Importando base de dados e tratando informações
bra_eua_cafe <- read_data(df = indbraeua,
                          n_produto = 901)

# 2.1.2) Calculando o índice Grubel-Lloyd
bra_eua_cafe_ind <- calc_grubel_lloyd(df = bra_eua_cafe, 
                                      estrangeiro = "United States")

# 2.1.3) Análise gráfica
plot_graph(df = bra_eua_cafe_ind,
           titulo = "Brasil x EUA: Índice Grubel-Lloyd ao longo dos anos (Café)")

# 2.2) Fertilizantes

# 2.2.1) Importando base de dados e tratando informações
bra_eua_fertilizantes <- read_data(df = indbraeua,
                                   n_produto = 3105)

# 2.2.2) Calculando o índice Grubel-Lloyd
bra_eua_fertilizantes_ind <- calc_grubel_lloyd(df = bra_eua_fertilizantes, 
                                               estrangeiro = "United States")

# 2.2.3) Análise gráfica
plot_graph(df = bra_eua_fertilizantes_ind,
           titulo = "Brasil x EUA: Índice Grubel-Lloyd ao longo dos anos (Fertilizantes)")

# 2.3) Açúcar

# 2.3.1) Importando base de dados e tratando informações
bra_eua_acucar <- read_data(df = indbraeua,
                            n_produto = 1701)

# 2.3.2) Calculando o índice Grubel-Lloyd
bra_eua_acucar_ind <- calc_grubel_lloyd(df = bra_eua_acucar, 
                                        estrangeiro = "United States")

# 2.3.3) Análise gráfica
plot_graph(df = bra_eua_acucar_ind,
           titulo = "Brasil x EUA: Índice Grubel-Lloyd ao longo dos anos (Açúcar)")

# 3) Brasil x Argentina --------------------------------------------------

# 3.1) Café

# 3.1.1) Importando base de dados e tratando informações
bra_arg_cafe <- read_data(df = indbraarg,
                          n_produto = 901)

# 3.1.2) Calculando o índice Grubel-Lloyd
bra_arg_cafe_ind <- calc_grubel_lloyd(df = bra_arg_cafe, 
                                      estrangeiro = "Argentina")

# 3.1.3) Análise gráfica
plot_graph(df = bra_arg_cafe_ind,
           titulo = "Brasil x Argentina: Índice Grubel-Lloyd ao longo dos anos (Café)")

# 3.2) Fertilizantes

# 3.2.1) Importando base de dados e tratando informações
bra_arg_fertilizantes <- read_data(df = indbraarg,
                                   n_produto = 3105)

# 3.2.2) Calculando o índice Grubel-Lloyd
bra_arg_fertilizantes_ind <- calc_grubel_lloyd(df = bra_arg_fertilizantes, 
                                               estrangeiro = "Argentina")

# 3.2.3) Análise gráfica
plot_graph(df = bra_arg_fertilizantes_ind,
           titulo = "Brasil x Argentina: Índice Grubel-Lloyd ao longo dos anos (Fertilizantes)")

# 3.3) Açúcar

# 3.3.1) Importando base de dados e tratando informações
bra_arg_acucar <- read_data(df = indbraarg,
                            n_produto = 1701)

# 3.3.2) Calculando o índice Grubel-Lloyd
bra_arg_acucar_ind <- calc_grubel_lloyd(df = bra_arg_acucar, 
                                        estrangeiro = "Argentina")

# 3.3.3) Análise gráfica
plot_graph(df = bra_arg_acucar_ind,
           titulo = "Brasil x Argentina: Índice Grubel-Lloyd ao longo dos anos (Açúcar)")

# 4) Análise entre níveis de agregação ------------------------------------

# Realizou-se uma query coletando as informações sobre Óleos de petróleo e óleos 
# obtidos de minerais betuminosos, exceto brutos entre Brasil e EUA:
# 2710 -- Óleos de petróleo e óleos obtidos de minerais betuminosos, exceto brutos; preparações não especificadas ou incluídas em outro lugar, contendo em peso 70% ou mais de óleos de petróleo ou de óleos obtidos de minerais betuminosos, sendo esses óleos os constituintes básicos
# 271011 -- Óleos de petróleo leves e preparações
# 271019 -- Óleos de petróleo e óleos obtidos de minerais betuminosos (exceto brutos) e preparações não especificadas/incluídas em outro lugar, contendo em peso 70% ou mais de óleos de petróleo ou de óleos obtidos de minerais betuminosos, sendo esses óleos os constituintes básicos
# 271091 -- Óleos de resíduos contendo bifenilos policlorados (PCBs)/terfenilos policlorados (PCTs)/bifenilos polibromados (PBBs)
# 271099 -- Óleos de resíduos, exceto aqueles que contêm bifenilos policlorados (PCBs)/terfenilos policlorados (PCTs)/bifenilos polibromados (PBBs)

# 4.1) Código 2710

# 4.1.1) Importando base de dados e tratando informações
df_2710 <- read_data(df = analiseprodutos,
                     n_produto = 2710)

# 4.1.2) Calculando o índice Grubel-Lloyd (0901)
df_2710_ind <- calc_grubel_lloyd(df = df_2710,
                                 estrangeiro = "United States")

# 4.1.3) Realizando tratamento final
df_2710_ind <- df_2710_ind %>% dplyr::select(ano, indice_2710 = grubel_lloyd)

# 4.2) Código 271011

# 4.2.1) Importando base de dados e tratando informações
df_271011 <- read_data(df = analiseprodutos,
                       n_produto = 271011)

# 4.2.2) Calculando o índice Grubel-Lloyd
df_271011_ind <- calc_grubel_lloyd(df = df_271011,
                                   estrangeiro = "United States")

# 4.2.3) Realizando tratamento final
df_271011_ind <- df_271011_ind %>% dplyr::select(ano, indice_271011 = grubel_lloyd)

# 4.3) Código 271019

# 4.3.1) Importando base de dados e tratando informações
df_271019 <- read_data(df = analiseprodutos,
                       n_produto = 271019)

# 4.3.2) Calculando o índice Grubel-Lloyd
df_271019_ind <- calc_grubel_lloyd(df = df_271019,
                                   estrangeiro = "United States")

# 4.3.3) Realizando tratamento final
df_271019_ind <- df_271019_ind %>% dplyr::select(ano, indice_271019 = grubel_lloyd)

# 4.4) Código 271091

# 4.4.1) Importando base de dados e tratando informações
df_271091 <- read_data(df = analiseprodutos,
                       n_produto = 271091)

# 4.4.2) Calculando o índice Grubel-Lloyd
df_271091_ind <- calc_grubel_lloyd(df = df_271091,
                                   estrangeiro = "United States")

# 4.4.3) Realizando tratamento final
df_271091_ind <- df_271091_ind %>% dplyr::select(ano, indice_271091 = grubel_lloyd)

# 4.5) Código 271099

# 4.5.1) Importando base de dados e tratando informações
df_271099 <- read_data(df = analiseprodutos,
                       n_produto = 271099)

# 4.5.2) Calculando o índice Grubel-Lloyd
df_271099_ind <- calc_grubel_lloyd(df = df_271099,
                                   estrangeiro = "United States")

# 4.5.3) Realizando tratamento final
df_271099_ind <- df_271099_ind %>% dplyr::select(ano, indice_271099 = grubel_lloyd)

# 4.6) Unindo todos os dataframes
df_analise_final <- 
  df_2710_ind %>%
  dplyr::left_join(df_271011_ind, by = "ano") %>%
  dplyr::left_join(df_271019_ind, by = "ano") %>%
  dplyr::left_join(df_271091_ind, by = "ano") %>%
  dplyr::left_join(df_271099_ind, by = "ano")

# 4.7) Análise gráfica
ggplotly(
  ggplot(df_analise_final, aes(x = ano)) +
    geom_line(aes(y = indice_2710, color = "2710"), size = 1.2) + 
    geom_line(aes(y = indice_271011, color = "271011"), size = 1.2) + 
    geom_line(aes(y = indice_271019, color = "271019"), size = 1.2) +
    labs(title = "Gráfico - Índice Grubel-Lloyd entre 2013 a 2023 entre Brasil e EUA para derivados do petróleo.", 
         x = "Ano", 
         y = "Índice Grubel-Lloyd",
         color = "Produto") +                
    theme_minimal() +                                
    theme(
      plot.title = element_text(size = 16, hjust = 0.5),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12))
)
