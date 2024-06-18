# Script em R para o trabalho de Econometria I - Michel Csillag Finger e Theodoro Nunes Figueiredo Mota
# Parte do código é derivada do código INPUR_R_TS_ALUNO_5ER.R, em que o próprio
# INEP/Daeb orienta sobre a leitura dos resultados do SAEB 2019


# Preliminares -------------------------------------------------------
# Pacotes
library(data.table)
library(dplyr)
library(stargazer)
library(ggplot2)

# Funções

# Função que troca todos os "." e "*" para NAs
trocar_para_na <- function(x) {
  x <- na_if(x, ".")
  x <- na_if(x, "*")
  return(x)
}

# Funções para transformarem categóricas de números
transf_em_num_5 <- function(coluna) {
  recode(coluna, 
         "A" = 1, 
         "B" = 2, 
         "C" = 3, 
         "D" = 4, 
         "E" = 5,
         "F" = 0)
}

transf_em_num_3 <- function(coluna) {
  recode(coluna,
         "A" = 0,
         "B" = 1,
         "C" = 2)
}

transf_em_num_2 <- function(coluna) {
  recode(coluna,
         "A" = 0,
         "B" = 1)
}


# Diretório
# Escolha aqui o diretório em que consta o arquivo TS_ALUNO_5EF.csv para uso
setwd('~/Estudos/FGV-EESP/2o ano/Econometria I/Trabalho/')

# Alocação de Memória
memory.limit(24576)


# Manipulação da Base  -------------------------------------------------------
# Importando a Base
dados_original <- data.table::fread(input='TS_ALUNO_5EF.csv',integer64='character')

# Formatando e escolhendo colunas

cols_transf_5 <- c("TX_RESP_Q002", "TX_RESP_Q004", "TX_RESP_Q005")
cols_transf_3 <- c("TX_RESP_Q006A", "TX_RESP_Q006B", "TX_RESP_Q006C", "TX_RESP_Q006D")
cols_transf_2 <- c("TX_RESP_Q003A", "TX_RESP_Q003B", "TX_RESP_Q003C", "TX_RESP_Q003D", "TX_RESP_Q003E")

dados <- dados_original %>%
  mutate_if(is.character, trocar_para_na) %>% 
  na.omit() |>
  mutate(
    Capital = ifelse(ID_AREA == 1, 1, 0), # Formatando Dummies para que elas assumam somente valores 0 e 1
    Urbano = ifelse(ID_LOCALIZACAO == 1, 1, 0),
    LM_PT = ifelse(TX_RESP_Q001 == "A", 1, 0), # Dummy caso português seja a língua materna
    across(all_of(cols_transf_2), transf_em_num_2),
    across(all_of(cols_transf_3), transf_em_num_3),
    across(all_of(cols_transf_5), transf_em_num_5)
  ) %>%
  select(ID_REGIAO, ID_UF, Capital, IN_PUBLICA, Urbano, IN_PREENCHIMENTO_LP, IN_PREENCHIMENTO_MT, 
         IN_PRESENCA_LP, IN_PRESENCA_MT, IN_PROFICIENCIA_LP, IN_PROFICIENCIA_MT,IN_AMOSTRA, PESO_ALUNO_LP,
         PROFICIENCIA_LP, PROFICIENCIA_LP_SAEB, PESO_ALUNO_MT, PROFICIENCIA_MT, PROFICIENCIA_MT_SAEB, 
         IN_PREENCHIMENTO_QUESTIONARIO, LM_PT, TX_RESP_Q002, TX_RESP_Q003A, TX_RESP_Q003B, TX_RESP_Q003C,
         TX_RESP_Q003D, TX_RESP_Q004, TX_RESP_Q005, TX_RESP_Q006A, TX_RESP_Q006B, TX_RESP_Q006C, TX_RESP_Q006D) %>%
  rename(
    "TX_RESP_Q002" = "Raça",
    "TX_RESP_Q003A" = "Pres_Mae",
    "TX_RESP_Q003B" = "Pres_Pai",
    "TX_RESP_Q003C" = "Pres_Irmao",
    "TX_RESP_Q003D" = "Pres_Avo",
    "TX_RESP_Q004" = "Esc_Mae",
    "TX_RESP_Q005" = "Esc_Pai",
    "TX_RESP_Q006A" = "Conversa_Esc",
    "TX_RESP_Q006B" = "Incentivo_Estudo",
    "TX_RESP_Q006C" = "Incentivo_LC",
    "TX_RESP_Q006D" = "Incentivo_Presenca")
# Escolha das colunas para manipulação










# Variáveis Descritivas  -------------------------------------------------------
summary(dados)
stargazer(dados) # pensar melhor nas variáveis descritivas

# conta total de NAs
sum(is.na(dados))




# quantidade de mães e quantidade de pais
dados |> count(Pres_Mae)
dados |> count(Pres_Pai)







# O Modelo  -------------------------------------------------------

# bptest




# Gráficos  -------------------------------------------------------
# Variáveis descritivas
# Raça
ggplot(dados, aes(Raça)) +
  geom_bar()


ggplot(dados, aes(Pres_Mae)) +
  geom_bar()

# Nota x Educação Materna

# Nota x Educação Paterna