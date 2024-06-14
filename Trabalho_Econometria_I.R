# Script em R para o trabalho de Econometria I - Michel Csillag Finger e Theodoro Nunes Figueiredo Motta
# Parte do código é derivada do código INPUR_R_TS_ALUNO_5ER.R, em que o próprio 
# INEP/Daeb orienta sobre a leitura dos resultados do SAEB 2019


# Preliminares -------------------------------------------------------
# Pacotes
library(data.table)
library(dplyr)
library(ggplot2)

# Funções

# Diretório
# Escolha aqui o diretório em que consta o arquivo TS_ALUNO_5EF.csv para uso
setwd('~/Estudos/FGV-EESP/2o ano/Econometria I/Trabalho/')

# Alocação de Memória
memory.limit(24576)


# Manipulação da Base  -------------------------------------------------------
# Importando a Base
dados_original <- data.table::fread(input='TS_ALUNO_5EF.csv',integer64='character')

# Formatando e escolhendo colunas

dados <- dados_original %>%
  mutate(
    Capital = ifelse(ID_AREA == 1, 1, 0), # Formatando Dummies para que elas assumam somente valores 0 e 1
    Urbano = ifelse(ID_LOCALIZACAO == 1, 1, 0),
    LM_PT = ifelse(TX_RESP_Q001 == "A", 1, 0), # Dummy caso português seja a língua materna
    Raça = forcats::fct_recode(TX_RESP_Q002, # Re-estruturando variáveis categóricas
      "0" = "A", # Branca
      "1" = "B", # Preta
      "2" = "C", # Parda
      "3" = "D", # Amarela
      "4" = "E", # Indígena
      "5" = "F", # Não quis declarar/sem informação
      "5" = "*", # Não quis declarar/sem informação
      "5" = "."),# Não quis declarar/sem informação
    Esc_Mae = forcats::fct_recode(TX_RESP_Q004,
      "0" = "A", # Sem 5o ano completo
      "1" = "B", # EF completo, até 5o ano
      "2" = "C", # EF completo
      "3" = "D", # EM completo
      "4" = "E", # Ensino Superior completo
      "5" = "F", # Não soube/sem informação
      "5" = "*", # Não soube/sem informação
      "5" = "."),# Não soube/sem informação
    Esc_Pai = forcats::fct_recode(TX_RESP_Q005,
      "0" = "A", # Sem 5o ano completo
      "1" = "B", # EF completo, até 5o ano
      "2" = "C", # EF completo
      "3" = "D", # EM completo
      "4" = "E", # Ensino Superior completo
      "5" = "F", # Não soube/sem informação
      "5" = "*", # Não soube/sem informação
      "5" = "."),# Não soube/sem informação
  ) %>%
  mutate(
    Raça = as.numeric(Raça),
    Esc_Mae = as.numeric(Esc_Mae), # Transformando os fatores acima em números
    Esc_Pai = as.numeric(Esc_Pai)) %>%
  select(ID_REGIAO, ID_UF, Capital, IN_PUBLICA, Urbano, IN_SITUACAO_CENSO, IN_PREENCHIMENTO_LP, 
         IN_PREENCHIMENTO_MT, IN_PRESENCA_LP, IN_PRESENCA_MT, IN_PROFICIENCIA_LP, IN_PROFICIENCIA_MT,IN_AMOSTRA, 
         PESO_ALUNO_LP, PROFICIENCIA_LP, ERRO_PADRAO_LP, PROFICIENCIA_LP_SAEB, ERRO_PADRAO_LP_SAEB, PESO_ALUNO_MT,
         PROFICIENCIA_MT, ERRO_PADRAO_MT, PROFICIENCIA_MT_SAEB, ERRO_PADRAO_MT_SAEB, IN_PREENCHIMENTO_QUESTIONARIO, 
         LM_PT, Raça, TX_RESP_Q003A, TX_RESP_Q003B, TX_RESP_Q003C,TX_RESP_Q003D, TX_RESP_Q003E,
         Esc_Mae, Esc_Pai, TX_RESP_Q006A, TX_RESP_Q006B, TX_RESP_Q006C, TX_RESP_Q006D)
  # Escolha das colunas para manipulação


# Variáveis Descritivas  -------------------------------------------------------







# O Modelo  -------------------------------------------------------

# bptest




# Gráficos  -------------------------------------------------------

# Nota x Educação Materna

# Nota x Educação Paterna



# Tabelinha das colunas  -------------------------------------------------------

# ID_AREA -> Dummy: Capital (1), Interior (0)
# IN_PUBLICA -> Dummy: Escola Pública (1), Privada (0)
# ID_LOCALIZACAO -> Dummy: Urbano (1), Rural (0)
# IN_SITUACAO_SENSO -> Dummy de Consistência de SAEB-Censo Educação: (1) é consistente
# Dummys de preenchimento/presença de partes da prova:
# IN_PREENCHIMENTO_LP, IN_PREENCHIMENTO_MT, IN_PRESENCA_LP, IN_PRESENCA_MT
# Série de categóricas sobre a prova
# IN_PROFICIENCIA_LP, IN_PROFICIENCIA_MT: cálculo proficiência das matérias (3 questões respondidas)
# IN_AMOSTRA
# Notas, Peso, Erro Padrão e do SAEB em LP e MT

# TX_RESP_Q001 = Língua que fala em casa: a - Português; b - Espanhol; c - Outra;
# TX_RESP_Q002 = Cor/Raça: a - Branca; b - Preta; c - Parda; d - Amarela; e - Indígena; f - não quero declarar;
# TX_RESP_Q003a = Mãe/Madrasta mora em casa: a - Não; b - Sim;
# TX_RESP_Q003b = Pai/Padrasto mora em casa: a - Não; b - Sim;
# TX_RESP_Q003c = Irmãos/Irmãs mora em casa: a - Não; b - Sim;
# TX_RESP_Q003d = Avô/Avó mora em casa: a - Não; b - Sim;
# TX_RESP_Q003e = Outros mora em casa: a - Não; b - Sim;

# TX_RESP_Q004 = Maior Escolaridade Mãe/Mulher que cuida: 
#                 a - Não completou 5o ano; b - Ensino Fundamental, até 5o ano; c - EF completo; 
#                 d - EM completo; e - Ensino Superior Completo; f - Não sei;

# TX_RESP_Q005 = Maior Escolaridade Pai/Homem que cuida: 
#                 a - Não completou 5o ano; b - Ensino Fundamental, até 5o ano; c - EF completo; 
#                 d - EM completo; e - Ensino Superior Completo; f - Não sei;


# TX_RESP_Q006 = Frequência responsáveis costumam perguntar sobre:
# TX_RESP_Q006a = Conversar com você sobre o que acontece na escola: a - (quase) Nunca; b - De vez em quando; c - (quase) Sempre
# TX_RESP_Q006b = Incentivar você a estudar: a - (quase) Nunca; b - De vez em quando; c - (quase) Sempre
# TX_RESP_Q006c = Incentivar você a fazer a tarefa de casa: a - (quase) Nunca; b - De vez em quando; c - (quase) Sempre
# TX_RESP_Q006d = Incentivar você a comparecer às aulas: a - (quase) Nunca; b - De vez em quando; c - (quase) Sempre
# TX_RESP_Q006e = Ir às reuniões de pais na escola: a - (quase) Nunca; b - De vez em quando; c - (quase) Sempre

# TX_RESP_Q007 = Frequência pai pagam para limpeza/faxina: a - (quase) Nunca; b - Ocasionalmente; c - (quase) Sempre
# TX_RESP_Q008a = Na região que você mora tem Rua Pavimentada: a - Não; b - Sim;
# TX_RESP_Q008b = Na região que você mora tem Água Tratada na Rua: a - Não; b - Sim;
# TX_RESP_Q008c = Na região que você mora tem Iluminação na Rua: a - Não; b - Sim;

# TX_RESP_Q009 = Dos itens abaixo, quantos existem na sua casa?
# 9a - Geladeira; b - Tablet; c - Computador/Notebook; d - Quartos para dormir; e - Televisão; f - Banheiro; g - Carro

# TX_RESP_Q010 = Na sua casa tem:
# a - TV a cabo; b - Wi-Fi; c - Um quarto só seu; d - Mesa para estudar; e - Garagem; 
# f - Microondas; g - Aspirador de pó; h - Máquina de lavar roupa; i - Freezer;

# TX_RESP_Q011 = Quanto tempo demora para chegar à escola: a - <30 min; b - 30 min - 1h; c - > 1h
# TX_RESP_Q012 = Como você vai para a escola
# TX_RESP_Q013 = Idade entrou na escola
# TX_RESP_Q014 = Que tipo escola estudou?
# TX_RESP_Q015 = Já foi reprovado? 
# TX_RESP_Q016 = Já abandonou escola no meio do ano?
# TX_RESP_Q017 - a, b, c, d, e
# TX_RESP_Q018 - a, b, c