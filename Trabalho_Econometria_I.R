# Script em R para o trabalho de Econometria I - Michel Csillag Finger e Theodoro Nunes Figueiredo Mota
# Parte do código é derivada do código INPUR_R_TS_ALUNO_5ER.R, em que o próprio 
# INEP/Daeb orienta sobre a leitura dos resultados do SAEB 2019


# Preliminares -------------------------------------------------------
# Pacotes
library(data.table)
library(dplyr)
library(tidyr)
library(stargazer)
library(ggplot2)
library(lmtest)
library(gridExtra)
library(corrplot)

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
    ES_Mae = ifelse(TX_RESP_Q004 == "E", 1, 0), # Dummy Ensino Superior Mãe
    ES_Pai = ifelse(TX_RESP_Q005 == "E", 1, 0), # Dummy ES Pai
    Conversa_Esc_Alto = ifelse(TX_RESP_Q006A == "C", 1, 0),
    Incentivo_Estudo_Alto = ifelse(TX_RESP_Q006B == "C", 1, 0),
    Incentivo_LC_Alto = ifelse(TX_RESP_Q006C == "C", 1, 0),
    Incentivo_Presenca_Alto = ifelse(TX_RESP_Q006D == "C", 1, 0),
    across(all_of(cols_transf_2), transf_em_num_2),
    across(all_of(cols_transf_3), transf_em_num_3),
    across(all_of(cols_transf_5), transf_em_num_5)
  ) %>%
  select(ID_UF, Capital, IN_PUBLICA, Urbano, IN_PREENCHIMENTO_LP, IN_PREENCHIMENTO_MT, 
         PROFICIENCIA_LP_SAEB, PROFICIENCIA_MT_SAEB, LM_PT, TX_RESP_Q002, TX_RESP_Q003A,
         TX_RESP_Q003B, TX_RESP_Q004, TX_RESP_Q005, TX_RESP_Q006A, TX_RESP_Q006B, 
         TX_RESP_Q006C, TX_RESP_Q006D, ES_Mae, ES_Pai, Conversa_Esc_Alto, 
         Incentivo_Estudo_Alto, Incentivo_LC_Alto, Incentivo_Presenca_Alto) %>%
  rename(
    Raça = TX_RESP_Q002,
    Pres_Mae = TX_RESP_Q003A,
    Pres_Pai = TX_RESP_Q003B,
    Esc_Mae = TX_RESP_Q004,
    Esc_Pai = TX_RESP_Q005,
    Conversa_Esc = TX_RESP_Q006A,
    Incentivo_Estudo = TX_RESP_Q006B,
    Incentivo_LC = TX_RESP_Q006C,
    Incentivo_Presenca = TX_RESP_Q006D)
# Escolha das colunas para manipulação




# Variáveis Descritivas  -------------------------------------------------------
summary(dados) # descrever direto no texto ao invés de fazer tabela, acho; se precisar, faço tabela
stargazer(dados, title="Descritivas das Variáveis Utilizadas no Modelo", 
          label = "descritivas") # tiramos as variáveis categóricas no latex


# Estado com maior média de notas
estado_maior_media_notas <- dados |>
  group_by(ID_UF) |>
  summarise(Nota_LP = mean(PROFICIENCIA_LP_SAEB), 
            Nota_MT = mean(PROFICIENCIA_MT_SAEB))
# Estado Maior Média LP e MT: 35 = São Paulo → será usado como base de comparação

stargazer(estado_maior_media_notas, summary = FALSE, 
          title = "Média de Notas nas Provas por UF", label = "NotasUF")
# Essa tabela ainda foi manipulada manualmente também


# Modelos  -------------------------------------------------------

# Modelo 1: Matemática e Escolaridade da Mãe

modelo1 <- lm(PROFICIENCIA_MT_SAEB ~ ES_Mae + IN_PUBLICA + Urbano + Capital + 
                LM_PT + relevel(as.factor(Raça), ref = "1") + relevel(as.factor(ID_UF), ref = "35") + 
                Conversa_Esc_Alto + Incentivo_Estudo_Alto + Incentivo_LC_Alto + Incentivo_Presenca_Alto, 
              data=dados, subset=(IN_PREENCHIMENTO_MT == 1 & Pres_Mae == 1)) # controles influência direta pais
bptest(modelo1) # há heteroscedasticidade
r_m1 <- coeftest(modelo1, type="HC0")
summary(modelo1)

# Modelo 2: Matemática e Escolaridade do Pai

modelo2 <- lm(PROFICIENCIA_MT_SAEB ~ ES_Pai + IN_PUBLICA + Urbano + Capital + 
                LM_PT + relevel(as.factor(Raça), ref = "1") + relevel(as.factor(ID_UF), ref = "35") + 
                Conversa_Esc_Alto + Incentivo_Estudo_Alto + Incentivo_LC_Alto + Incentivo_Presenca_Alto, 
              data=dados, subset=(IN_PREENCHIMENTO_MT == 1 & Pres_Pai == 1)) # controles influência direta pais
bptest(modelo2) # há heteroscedasticidade
r_m2 <- coeftest(modelo2, type="HC0")
summary(modelo2)


# Modelo 3: LP e Escolaridade do Mae

modelo3 <- lm(PROFICIENCIA_LP_SAEB ~ ES_Mae + IN_PUBLICA + Urbano + Capital + 
                LM_PT + relevel(as.factor(Raça), ref = "1") + relevel(as.factor(ID_UF), ref = "35") + 
                Conversa_Esc_Alto + Incentivo_Estudo_Alto + Incentivo_LC_Alto + Incentivo_Presenca_Alto, 
              data=dados, subset=(IN_PREENCHIMENTO_LP == 1 & Pres_Mae == 1)) # controles influência direta pais
bptest(modelo3) # há heteroscedasticidade
r_m3 <- coeftest(modelo3, type="HC0")
summary(modelo3)


# Modelo 4: LP e Escolaridade do Pai

modelo4 <- lm(PROFICIENCIA_LP_SAEB ~ ES_Pai + IN_PUBLICA + Urbano + Capital + 
                LM_PT + relevel(as.factor(Raça), ref = "1") + relevel(as.factor(ID_UF), ref = "35") + 
                Conversa_Esc_Alto + Incentivo_Estudo_Alto + Incentivo_LC_Alto + Incentivo_Presenca_Alto, 
              data=dados, subset=(IN_PREENCHIMENTO_LP == 1 & Pres_Pai == 1)) # controles influência direta pais
bptest(modelo4) # há heteroscedasticidade
r_m4 <- coeftest(modelo4, type="HC0")
summary(modelo4)


stargazer(r_m1, r_m2, r_m3, r_m4, 
          title = "Resultados das Regressões", label = "resultados",
          column.labels = c("MT e Mãe", "MT e Pai", "LP e Mãe", "LP e Pai"),
          no.space=TRUE, 
          covariate.labels = c("ES Mãe", "ES Pai", "Escola Pública", "Cidade Urbana", "Capital",
                             "Português Língua Materna", "Não quis declarar", "Preta", "Parda", 
                             "Amarela", "Indígena", "Rondônia", "Acre", "Amazonas", "Roraima", "Pará", 
                             "Amapá", "Tocantins", "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte",
                             "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia", "Minas Gerais",
                             "Espírito Santo", "Rio de Janeiro", "Paraná", "Santa Catarina", 
                             "Rio Grande do Sul", "Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal",
                             "Alta Conversa Escola", "Alto Incentivo Estudo", "Alto Incentivo LC", "Alto Incentivo Presença",
                             "Contante"),
          order = c("ES Mae", "ES Pai", "Alta Conversa Escola", "Alto Incentivo Estudo",
                   "Alto Incentivo LC", "Alto Incentivo Presença"))
# Essa tabela ainda foi posteriormente manipulada no Latex
# As medidas de ajuste foram inseridas na tabela manualmente após o seguinte comando:
stargazer(modelo1, modelo2, modelo3, modelo4)


# Gráficos  -------------------------------------------------------
# Variáveis descritivas
# Raça
ggplot(dados, aes(Raça)) +
  geom_bar() +
  theme_minimal()

# Matriz de Correlação
graf_corr <- corPlot(subset(dados, select = c(Capital, IN_PUBLICA, Urbano,
         PROFICIENCIA_LP_SAEB, PROFICIENCIA_MT_SAEB, LM_PT,
         Pres_Mae, Pres_Pai, Esc_Mae, Esc_Pai, Conversa_Esc_Alto,
         Incentivo_Estudo_Alto, Incentivo_LC_Alto, Incentivo_Presenca_Alto)), 
        cex=1, labels = c('Capital', "Pública", "Urbano",
                          "Nota LP", "Nota MT", "LM_PT",
                          "Pres_Mae", "Pres_Pai", "Esc_Mae", "Esc_Pai", "Conv.Esc.",
                          "IEst. Alto", "ILC Alto", "IP Alto"))

ggsave("grafico_correlação.pdf", graf_corr, device = "pdf")

# Criação data frame auxiliar 
count_data <- dados %>%
  summarise(
    Pres_Mae_0 = sum(Pres_Mae == 0),
    Pres_Mae_1 = sum(Pres_Mae == 1),
    Pres_Pai_0 = sum(Pres_Pai == 0),
    Pres_Pai_1 = sum(Pres_Pai == 1)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Count")

# Cria coluna para indicar a variável original e o valor (0 ou 1)
count_data <- count_data %>%
  mutate(
    VariableType = case_when(
      grepl("Pres_Mae", Variable) ~ "Pres_Mae",
      grepl("Pres_Pai", Variable) ~ "Pres_Pai"
    ),
    Value = case_when(
      grepl("_0$", Variable) ~ "0",
      grepl("_1$", Variable) ~ "1"
    )
  )

dev.off()

# Gráfico de Barras
pres_mae_pai <- ggplot(count_data, aes(x = Value, y = Count, fill = VariableType)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Pres_Mae" = "red", "Pres_Pai" = "blue")) +
  labs(x = "Residência em Casa (0 = Não, 1 = Sim)", # Normalmente, quem mora na sua casa?
       y = "Quantidade") + # Título: Comparação de Frequência de Residência dos Pais
  theme_minimal()

ggsave(filename = "graf_barras_mae_pai", plot = pres_mae_pai, device = "pdf")


# Criação data frame auxiliar 
count_data2 <- dados %>%
  summarise(
    ES_Mae_0 = sum(ES_Mae == 0),
    ES_Mae_1 = sum(ES_Mae == 1),
    ES_Pai_0 = sum(ES_Pai == 0),
    ES_Pai_1 = sum(ES_Pai == 1)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Count")

# Cria coluna para indicar a variável original e o valor (0 ou 1)
count_data2 <- count_data2 %>%
  mutate(
    VariableType = case_when(
      grepl("ES_Mae", Variable) ~ "ES_Mae",
      grepl("ES_Pai", Variable) ~ "ES_Pai"
    ),
    Value = case_when(
      grepl("_0$", Variable) ~ "0",
      grepl("_1$", Variable) ~ "1"
    )
  )

# Gráfico de Barras
ES_mae_pai <- ggplot(count_data2, aes(x = Value, y = Count, fill = VariableType)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("ES_Mae" = "coral", "ES_Pai" = "darkturquoise")) +
  labs(x = "Ensino Superior (0 = Não, 1 = Sim)",
       y = "Quantidade") +
  theme_minimal()

ggsave(filename = "graf_barras_ES_mae_pai", plot = ES_mae_pai, device = "pdf")


#Gráfico de distribuição das notas
dist_notas_lp<- ggplot(data = dados, aes(x = PROFICIENCIA_LP_SAEB)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(x = "Valores", y = "Densidade", title = "Distribuição das notas de língua portuguesa")
dist_notas_lp

dist_notas_mt<- ggplot(data = dados, aes(x = PROFICIENCIA_MT_SAEB)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(x = "Valores", y = "Densidade", title = "Distribuição das notas de matemática")
dist_notas_mt