#Instalando pacotes
install.packages("readr")
install.packages("dplyr") 
install.packages("ggplot2")

#Carregando as bibliotecas necessarias
library(readr)
library(dplyr)
library(ggplot2)

#Definindo o diretório
setwd("/home/liil-jao/Downloads/Trabalho_bioinfo/pan_lung/")#Coloque seu diretorio aqui

#Carregar dados dos paciente e das amostras
dados_pacientes <- read_tsv("nsclc_tcga_broad_2016/data_clinical_patient.txt", skip = 4) #skip é para pular linhas que não queromos analisar(Não faz parte da amostra)
dados_amostras <- read_tsv("nsclc_tcga_broad_2016/data_clinical_sample.txt", skip = 4)

#ver a estrutura dos dados
glimpse(dados_pacientes)
glimpse(dados_amostras)

#PARTE EXPLORATORIA DOS DADOS 

#Verificando a quantidade de pacientes que temos na amostra
n_distinct(dados_pacientes$PATIENT_ID)

#Tabela de distribuição por sexo
table(dados_pacientes$SEX)

#Verificando valores unicos de cancer
unique(dados_amostras$CANCER_TYPE_DETAILED)

#JUNÇÃO DOS DADOS 

#Juntar dados do paciente com a amostra
dados_completos <- left_join(dados_pacientes, dados_amostras, by = "PATIENT_ID") 
glimpse(dados_completos)

#Verificando se a mesma quantidade de linhas
nrow(dados_amostras) == nrow(dados_completos)

#Verificando se a duplicação de paciente
dados_completos %>% 
  count(PATIENT_ID)%>%
  filter(n>1)

#Verificar se algum paciente ficou sem match
paciente_sem_match <- anti_join(dados_amostras, dados_pacientes, by = "PATIENT_ID")
nrow(paciente_sem_match)

#Analise exploratoria

#Comparação dos dois tipos de cancer
estatistica_cancer <- dados_completos %>% 
  group_by(CANCER_TYPE_DETAILED) %>%
  summarise(
    n=n(),
    media_idade = mean(AGE_AT_SURGERY, na.rm =TRUE),
    media_tmb = mean(TMB_NONSYNONYMOUS, na.rm = TRUE)

  )

estatistica_cancer

#Grafico comparativo de TMB
ggplot(dados_completos, aes(x = CANCER_TYPE_DETAILED, y = TMB_NONSYNONYMOUS, fill = CANCER_TYPE_DETAILED)) +
  geom_boxplot()+
  labs(title = "Comparação do Tumor Mutational Burden",
       x = "Tipo de Câncer",
       y = "TMB Nonsynonymous")+
  theme_minimal()

#Gráfico de TMB por tipo  de câncer e sexo
ggplot(dados_completos, aes(x = CANCER_TYPE_DETAILED, y = TMB_NONSYNONYMOUS, fill = SEX)) + 
  geom_boxplot() +
  labs(title = "TMB por tipo de câncer e sexo",
       x = "Tipo de câncer", 
       y = "TMB")

#Tabela de tabagismo por tipo de câncer
tabagismo_tabela <- table(dados_completos$SMOKING_HISTORY, dados_completos$CANCER_TYPE_DETAILED)
print(tabagismo_tabela)

#Gãfico de tabagismo
ggplot(dados_completos, aes(x = CANCER_TYPE_DETAILED, fill = SMOKING_HISTORY)) +
  geom_bar(position = "fill") + 
  labs(title = "Distribuição do Histótrico de Tabagimo",
       x = "Tipo de Câncer",
       y = "Proporção")

#Análise de Sobrevida
summary(dados_completos$OS_MONTHS)

#Gráfico de sobrevida
ggplot(dados_completos, aes (x = CANCER_TYPE_DETAILED, y = OS_MONTHS, fill = CANCER_TYPE_DETAILED)) +
  geom_boxplot() +
  labs(title = "Sobrevida Global por Tipo de Câncer", 
       x = "Tipo de Câncer",
       y = "Sobrevida")

# Estatísticas descritivas completas
estatisticas_completas <- dados_completos %>%
  group_by(CANCER_TYPE_DETAILED) %>%
  summarise(
    n = n(),
    media_tmb = mean(TMB_NONSYNONYMOUS, na.rm = TRUE),
    media_idade_cirurgia = mean(AGE_AT_SURGERY, na.rm = TRUE),
    media_pack_years = mean(SMOKING_PACK_YEARS, na.rm = TRUE),
    sobrevida_media = mean(OS_MONTHS, na.rm = TRUE)
  )
print(estatisticas_completas)
