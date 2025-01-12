#A ANALISE FOI FEITA PARA OS ANOS DE 2014 A 2024. ALGUNS VALORES PODEM PARECER ABSURDOS, MAS LEMBREMOS QUE SÃO ACUMULADOS.

#------------------------------------------------------------------------------------------------------------------
#PACOTES
install.packages("readxl")    # Para ler arquivos Excel
install.packages("tidyverse") # Para manipular e visualizar os dados
install.packages("rmarkdown") # Para criar o relatório
install.packages("dplyr") #para manipular e transformar dados

#------------------------------------------------------------------------------------------------------------------
#carregando os pacotes
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)

#------------------------------------------------------------------------------------------------------------------
#leitura dos dados - COLOCAR O CAMINHO
dados <- read_excel("C:/Users/matta/OneDrive/Desktop/AED_R/indicadores_fluxo_es_2014-2023/indicadores_trajetoria_educacao_superior_2014_2023.xlsx")
dados <- dados[-1,] #removendo a primeira linha

#observando as duas ultimas linhas (ultima linha /rodapé/ não tem dados)
ultima_linha <- tail(dados, 2)
View(ultima_linha)

dados <- dados[-nrow(dados),] #removendo a ultima linha

View(dados) #vendo a tabela inteira


# Removendo as colunas desnecessárias para nossa análise
dadosLimpos <- dados |> select(-c(1, 4, 5, 9, 10, 12, 13, 14, 16, 19, 26, 27, 28, 29, 30, 31))
View(dadosLimpos)

#CRIANDO A COLUNA DE MODALIDADE DE ENSINO:
dadosLimpos <- dadosLimpos %>%
  mutate(
    `Modalidade de Ensino` = recode(`Modalidade de Ensino`, 
                                    `1` = "Presencial",
                                    `2` = "Curso a distância")
  )


dadosLimpos <- dadosLimpos %>%
  mutate(
    `Código da Região Geográfica do Curso` = recode(`Código da Região Geográfica do Curso`, 
                                    `1` = "Região Norte",
                                    `2` = "Região Nordeste",
                                    `3` = "Região Sudeste",
                                    `4` = "Região Sul",
                                    `5` = "Região Centro-Oeste",
    )
  )


#renomeando o nome da coluna:
dadosLimpos <- dadosLimpos %>%
  rename(`Região do Curso` = `Código da Região Geográfica do Curso`)


#INSERINDO A COLUNA COM NOME DA UF
dadosLimpos <- dadosLimpos %>%
  mutate(
    `Código da Unidade Federativa do Curso` = recode(`Código da Unidade Federativa do Curso`, 
                     '11' = "Rondônia (RO)",
                     '12' = "Acre (AC)",
                     '13' = "Amazonas (AM)",
                     '14' = "Roraima (RR)",
                     '15' = "Pará (PA)",
                     '16' = "Amapá (AP)",
                     '17' = "Tocantis (TO)",
                     '21' = "Maranhão (MA)",
                     '22' = "Piauí (PI)",
                     '23' = "Ceará (CE)",
                     '24' = "Rio Grande do Norte (RN)",
                     '25' = "Paraíba (PB)",
                     '26' = "Pernambuco (PE)",
                     '27' = "Alagoas (AL)",
                     '28' = "Sergipe (SE)",
                     '29' = "Bahia (BA)",
                     '31' = "Minas Gerais (MG)",
                     '32' = "Espírito Santo (ES)",
                     '33' = "Rio de Janeiro (RJ)",
                     '35' = "São Paulo (SP)",
                     '41' = "Paraná (PR)",
                     '42' = "Santa Catarina (SC)",
                     '43' = "Rio Grande do Sul (RS)",
                     '50' = "Mato Grosso do Sul (MS)",
                     '51' = "Mato Grosso (MT)",
                     '52' = "Goiás (GO)",
                     '53' = "Distrito Federal (DF)")
  )


#renomeando o nome da coluna:
dadosLimpos <- dadosLimpos %>%
  rename(`Unidade Federativa do Curso` = `Código da Unidade Federativa do Curso`)


dadosLimpos <- dadosLimpos %>%
  mutate(
    `Categoria Administrativa` = recode(`Categoria Administrativa`, 
                      '1' = "Pública Federal",
                      '2' = "Pública Estadual",
                      '3' = "Pública Municipal",
                      '4' = "Privada com fins lucrativos",
                      '5' = "Privada sem fins lucrativos",
                      '7' = "Especial"))
View(dadosLimpos)


##            ANÁLISES UNIVARIADAS

#------------------------------------------------------------------------------------------------------------------
#vendo os tipos de variáveis na tabela
glimpse(dadosLimpos)
#double e caracteres

#tipo de cada coluna
tipos <- sapply(dadosLimpos, class)
print(tipos)


######################################################################################################################
#criando boxplots para observar se existem outlier's
numeric_columns <- dadosLimpos[sapply(dadosLimpos, is.numeric)]

for (col in names(numeric_columns)) {
  valores <- numeric_columns[[col]]  #extraindo os valores da coluna
  
  boxplot(valores,
          main = paste("Boxplot -", col),
          col = "lightblue",
          ylab = "Valores",
          xlab = col,
          ylim = range(valores, na.rm = TRUE))  #ajusta o eixo y aos valores
}



#####################################################################################################################
#CALCULANDO MÉTRICAS PARA AS COLUNAS NUMÉRICAS
#função para calcular a moda
calcula_moda <- function(x) {
  x <- na.omit(x)  #removendo valores NA
  moda <- as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
  return(moda)
}

#calculando maximo, minimo, soma, média, mediana e moda para cada coluna numérica
resultados <- dadosLimpos %>%
  summarise(across(where(is.numeric), 
                   list(
                     maximo = ~max(.x, na.rm = TRUE),
                     minimo = ~min(.x, na.rm = TRUE),
                     soma = ~sum(.x, na.rm = TRUE),
                     media = ~mean(.x, na.rm = TRUE),
                     mediana = ~median(.x, na.rm = TRUE),
                     moda = ~calcula_moda(.x)
                   )))

View(resultados)
resultados_limpos <- resultados |> select(-c(1:18,21, 27, 33, 31)) #REMOVENDO COLUNAS DESNECESSÁRIAS

#nomes 1 coluna
nomes <- colnames(resultados_limpos)
nomes
#valores 2 coluna
valores <- format(unlist(resultados_limpos[1, ], use.names = FALSE), scientific = FALSE) #formatando em duas colunas
valores
#métricas
metricas <- data.frame(Metrica = nomes, Valor = valores)
View(metricas) #métricas para cada coluna numerica

#RESUMINDO CADA COLUNA DO DATAFRAME
#fazendo o resumo de cada variável para todas as colunas:
summary(dadosLimpos)



##            ANÁLISES BIVARIADAS

######################################################################################################################
#IDENTIFICAÇÃO DA MAIOR QUANTIDADE DE DESISTÊNCIAS
#maior numero de desistencia
maior_desistencia <- dadosLimpos$`Quantidade de Desistência no Curso no ano de referência` |>
  max(na.rm = TRUE)

maior_desistencia #6230 - Educação Física

#IDENTIFICANDO O(S) CURSO(S) COM MAIOR QUANTIDADE DE DESISTENCIAS
#curso com maior quantidade de desistencia
curso_maiorDesistencias <- dadosLimpos |>
  filter(dadosLimpos$`Quantidade de Desistência no Curso no ano de referência` == 6230)

View(curso_maiorDesistencias)
#Educação Física


######################################################################################################################
cursos_maioresDesistencias <- dadosLimpos %>%
  group_by(`Nome do Curso de Graduação`) %>%
  summarise(
  prazo_medio_acompanhamento_universidade = round(mean(`Prazo de Acompanhamento do Curso em anos`, na.rm = TRUE), digits = 2),
  total_desistencias = sum(`Quantidade de Desistência no Curso no ano de referência`, na.rm = TRUE),
  total_permanencia = sum(`Quantidade de Permanência no Curso no ano de referência`, na.rm = TRUE),
  total_ingressantes = sum(`Quantidade de Ingressantes no Curso`, na.rm = TRUE),
  percent_desistencia = round((total_desistencias / total_ingressantes)*100),
  percent_permanencias = round((total_permanencia / total_ingressantes)*100)) %>%
  arrange(desc(total_desistencias)) #ordenando pela coluna de desistencias

#RANK DOS CURSOS COM MAIOR TAXA DE DESISTENCIA (PROPORÇÃO)
#cursos com maior proporção de desistencia
cursos_maior_percent_Desistencias <- cursos_maioresDesistencias %>%
  arrange(desc(percent_desistencia))

View(cursos_maior_percent_Desistencias)
#CIÊNCIAS FARMACÊUTICAS (10%)
#todo o top 10 possui 10%, não vale a pena fazer o gráfico

#MENOR VALOR DE DESISTENCIAS
#menor numero de desistencia
menor_desistencia <- dadosLimpos$`Quantidade de Desistência no Curso no ano de referência` |>
  min(na.rm = TRUE)

menor_desistencia 
#0


######################################################################################################################
#RANK DOS CURSOS COM MENOR QUANTIDADE DE DESISTENCIA (VALORES ABSOLUTOS)
# Curso com menor quantidade de desistência (0) - agrupamento das ocorrencias de cursos sem desistencias 

curso_menorDesistencias <- dadosLimpos %>%
  filter(`Quantidade de Desistência no Curso no ano de referência` == 0) %>%
  group_by(`Nome do Curso de Graduação`) %>%
  summarise(frequencia_curso = n()) %>% #'n()' conta o número de linhas em cada grupo
  arrange(desc(frequencia_curso))

# Exibir o resultado
View(curso_menorDesistencias)
#O CURSO DE ADM É O QUE POSSUI A MENOR QUANTIDADE DE DESISTENCIAS NO BRASIL (7.176 cursos ADM com 0 desistências)


######################################################################################################################
#RESUMO POR CURSOS
#informações sobre os cursos (prazo de acompanhamento, desistencias e permanencias)
curso_resumo <- dadosLimpos %>%
  group_by(`Nome do Curso de Graduação`) %>%
  summarize(
    prazo_medio_acompanhamento_curso = round(mean(`Prazo de Acompanhamento do Curso em anos`, na.rm = TRUE), digits = 2),
    total_desistencias = sum(`Quantidade de Desistência no Curso no ano de referência`, na.rm = TRUE),
    total_permanencia = sum(`Quantidade de Permanência no Curso no ano de referência`, na.rm = TRUE), 
    total_ingressantes = sum(`Quantidade de Ingressantes no Curso`, na.rm = TRUE),
    percent_desistencia = round((total_desistencias / total_ingressantes)*100, digits = 2),
    percent_permanencias = round((total_permanencia / total_ingressantes)*100, digits = 2)
  ) %>%
  arrange(desc(total_desistencias)) #ordenando pela coluna de desistencias

curso_resumo$total_desistencias <- format(curso_resumo$total_desistencias, big.mark = ".", decimal.mark = ",")
curso_resumo$total_permanencia <- format(curso_resumo$total_permanencia, big.mark = ".", decimal.mark = ",")
curso_resumo$total_ingressantes <- format(curso_resumo$total_ingressantes, big.mark = ".", decimal.mark = ",")


View(curso_resumo)
#Adm tbm possui a maior taxa de desistencia


#----------------------------------------------------------------------------------------------------------------
#RANK DOS CURSOS PELO VALOR ABSOLUTO
#ordenando pelos cursos com maior quantidade (números absolutos) de permanencia 
maior_permanencia <- curso_resumo |> 
  arrange(desc(total_permanencia))

View(maior_permanencia)
#Direito possui a maior permanencia

#criando o top 10 maior qtde de desistencia por curso
top10_curso_maior_permanencia <- maior_permanencia %>% slice(1:10)
View(top10_curso_maior_permanencia)

#Plotando o grafico de rank
ggplot(top10_curso_maior_permanencia, aes(x = reorder(`Nome do Curso de Graduação`, -desc(total_permanencia)), y = total_permanencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = total_permanencia), hjust = -0.1, size = 3) +
  coord_flip() +  # Inverte os eixos para facilitar a leitura
  labs(
    title = "Top 10 Maior Quantidade de Permanência por Faculdade",
    x = "Curso",
    y = "Permanências"
  ) +
  theme_minimal()


#----------------------------------------------------------------------------------------------------------------
#ordenando pelos cursos com menor quantidade (números absolutos) de permanencia 
menor_permanencia <- curso_resumo |> 
  arrange(desc(total_desistencias))
View(menor_permanencia)
#ADMINISTRAÇÃO (200.666)

#criando o top 10 maior qtde de desistencia por curso
top10_curso_maior_desistencia <- menor_permanencia %>% slice(1:10)
View(top10_curso_maior_desistencia)

#Plotando o grafico de rank
ggplot(top10_curso_maior_desistencia, aes(x = reorder(`Nome do Curso de Graduação`, -desc(total_desistencias)), y = total_desistencias)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = total_desistencias), hjust = -0.1, size = 3) +
  coord_flip() +  # Inverte os eixos para facilitar a leitura
  labs(
    title = "Top 10 Maior Quantidade de Desistência por Faculdade",
    x = "Curso",
    y = "Desistências"
  ) +
  theme_minimal()


#----------------------------------------------------------------------------------------------------------------
#RANK DOS CURSOS PELA TAXA
#ordenando pelos cursos com menor proporção de permanencia 
menor_percent_permanencia <- curso_resumo |> 
  arrange((percent_permanencias))

View(menor_percent_permanencia)
#FORMAÇÃO INTERDISCIPLINAR posssui a menor permanencia (0%)
#o top10 tem 0%, não vale a pena


#----------------------------------------------------------------------------------------------------------------
#ordenando pelos cursos com maior proporção de permanencia 
maior_percent_permanencia <- curso_resumo |> 
  arrange(desc(percent_permanencias))

View(maior_percent_permanencia)
#DECORAÇÃO possui a maior taxa de permanencia (77.14%)

#criando o top 10 maior % permanencia por curso
top10_curso_maior_percent_permanencia <- maior_percent_permanencia %>% slice(1:10)
View(top10_curso_maior_percent_permanencia)

#Plotando o grafico de rank
ggplot(top10_curso_maior_percent_permanencia, aes(x = reorder(`Nome do Curso de Graduação`, -desc(percent_permanencias)), y = percent_permanencias)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = percent_permanencias), hjust = -0.1, size = 3) +
  coord_flip() +  # Inverte os eixos para facilitar a leitura
  labs(
    title = "Top 10 Maior Taxa Permanência por Faculdade",
    x = "Curso",
    y = "Taxa Permanência (%)"
  ) +
  theme_minimal()


######################################################################################################################
#RESUMO PARA AS UNIVERSIDADES
#informações sobre as universidades (prazo de acompanhamento, desistencias e permanencias)
faculdade_resumo <- dadosLimpos %>%
  group_by(`Nome da Instituição`) %>%
  summarize(
    prazo_medio_acompanhamento_universidade = round(mean(`Prazo de Acompanhamento do Curso em anos`, na.rm = TRUE), digits = 2),
    total_desistencias = sum(`Quantidade de Desistência no Curso no ano de referência`, na.rm = TRUE),
    total_permanencia = sum(`Quantidade de Permanência no Curso no ano de referência`, na.rm = TRUE),
    total_ingressantes = sum(`Quantidade de Ingressantes no Curso`, na.rm = TRUE),
    percent_desistencia = round((total_desistencias / total_ingressantes)*100),
    percent_permanencias = round((total_permanencia / total_ingressantes)*100)
  ) %>%
  arrange(desc(total_desistencias)) #ordenando pela coluna de desistencias

faculdade_resumo$total_desistencias <- format(faculdade_resumo$total_desistencias, big.mark = ".", decimal.mark = ",")
faculdade_resumo$total_permanencia <- format(faculdade_resumo$total_permanencia, big.mark = ".", decimal.mark = ",")
faculdade_resumo$total_ingressantes <- format(faculdade_resumo$total_ingressantes, big.mark = ".", decimal.mark = ",")

View(faculdade_resumo)


#----------------------------------------------------------------------------------------------------------------
#RANKEAMENTO DAS UNIVERSIDADES PELAS TAXAS
#rank de universidades com menor taxa de permanencia 
faculdade_menor_percent_permanencia <- faculdade_resumo %>%
  arrange((percent_permanencias))

View(faculdade_menor_percent_permanencia)
#Faculdade Presidente Antônio Carlos de São Lourenço (0%)
#não é uma análise que valha a pena mostrar em gráfico (top 10 com 0%)

#----------------------------------------------------------------------------------------------------------------
#rank de universidades com maior taxa de permanencia 
faculdade_maior_percent_permanencia <- faculdade_resumo %>%
  arrange(desc(percent_permanencias))

View(faculdade_maior_percent_permanencia)
#FACULDADES INTEGRADAS DE TAQUARA (75%)

#criando o top 10 da faculdade
top10_faculdade_taxa_permanencia <- faculdade_maior_percent_permanencia %>% slice(1:10)
View(top10_faculdade_taxa_permanencia)

#Plotando o grafico de rank
ggplot(top10_faculdade_taxa_permanencia, aes(x = reorder(`Nome da Instituição`, -desc(percent_permanencias)), y = percent_permanencias)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = percent_permanencias), hjust = -0.1, size = 3) +
  coord_flip() +  # Inverte os eixos para facilitar a leitura
  labs(
    title = "Top 10 Maior Taxa Permanência por Faculdade",
    x = "Faculdade",
    y = "Taxa Permanência (%)"
  ) +
  theme_minimal()



#----------------------------------------------------------------------------------------------------------------
#RANKEAMENTO DAS UNIVERSIDADES PELOS VALORES ABSOLUTOS
#rank de universidades com menor taxa de permanencia 
faculdade_menor_permanencia <- faculdade_resumo %>%
  arrange((total_permanencia))

View(faculdade_menor_permanencia)
#Faculdade Presidente Antônio Carlos de São Lourenço (0 permanencias)

#criando o top 10 da faculdade
top10_faculdade_menor_permanencia <- faculdade_menor_permanencia %>% slice(1:10)
View(top10_faculdade_menor_permanencia)

#Plotando o grafico de rank
ggplot(top10_faculdade_menor_permanencia, aes(x = reorder(`Nome da Instituição`, desc(total_permanencia)), y = total_permanencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = total_permanencia), hjust = -0.1, size = 3) +
  coord_flip() +  # Inverte os eixos para facilitar a leitura
  labs(
    title = "Top 10 Menor Permanência por Faculdade",
    x = "Faculdade",
    y = "Permanências"
  ) +
  theme_minimal()


#----------------------------------------------------------------------------------------------------------------
#rank de universidades com maior taxa de permanencia 
faculdade_maior_permanencia <- faculdade_resumo %>%
  arrange(desc(total_permanencia))

View(faculdade_maior_permanencia)
#Universidade Paulista (280.403)

#criando o top 10 da faculdade
top10_faculdade_maior_permanencia <- faculdade_maior_permanencia %>% slice(1:10)
View(top10_faculdade_maior_permanencia)

#Plotando o grafico de rank
ggplot(top10_faculdade_maior_permanencia, aes(x = reorder(`Nome da Instituição`, -desc(total_permanencia)), y = total_permanencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = total_permanencia), hjust = -0.1, size = 3) +
  coord_flip() +  # Inverte os eixos para facilitar a leitura
  labs(
    title = "Top 10 Maior Permanência por Faculdade",
    x = "Faculdade",
    y = "Permanências"
  ) +
  theme_minimal()


##########################################################################################################################
#FAZENDO O RESUMO POR UF
uf_resumo <- dadosLimpos %>%
  filter(!is.na(`Unidade Federativa do Curso`)) %>%
  group_by(`Unidade Federativa do Curso`) %>%
  summarize(
    prazo_medio_acompanhamento_universidade = round(mean(`Prazo de Acompanhamento do Curso em anos`, na.rm = TRUE), digits = 2),
    total_desistencias = sum(`Quantidade de Desistência no Curso no ano de referência`, na.rm = TRUE),
    total_permanencia = sum(`Quantidade de Permanência no Curso no ano de referência`, na.rm = TRUE),
    total_ingressantes = sum(`Quantidade de Ingressantes no Curso`, na.rm = TRUE),
    percent_desistencia = round((total_desistencias / total_ingressantes)*100),
    percent_permanencias = round((total_permanencia / total_ingressantes)*100)
  ) %>%
  arrange(desc(total_desistencias)) #ordenando pela coluna de desistencias

#formatando os valores de milhares para inserir os separadores (.)
uf_resumo$total_desistencias <- format(uf_resumo$total_desistencias, big.mark = ".", decimal.mark = ",")
uf_resumo$total_permanencia <- format(uf_resumo$total_permanencia, big.mark = ".", decimal.mark = ",")
uf_resumo$total_ingressantes <- format(uf_resumo$total_ingressantes, big.mark = ".", decimal.mark = ",")


View(uf_resumo)


#----------------------------------------------------------------------------------------------------------------
#TAXAS DE PERMANENCIA E DE DESISTENCIA POR UF
#rank de UFs com maior taxa de desistencia 
uf_maior_taxa_desistencia <- uf_resumo %>%
  arrange(desc(percent_desistencia))

View(uf_maior_taxa_desistencia)
#São Paulo (SP) (6%)
#top 10 tem 6%

#----------------------------------------------------------------------------------------------------------------
#rank de regiões com maior taxa de permanencia 
uf_maior_taxa_permanencia <- uf_resumo %>%
  arrange(desc(percent_permanencias))

View(uf_maior_taxa_permanencia)
#Maranhão (MA) (37%)

#criando o top 10 da região
top10_uf_taxa_permanencia <- uf_maior_taxa_permanencia %>% slice(1:10)
View(top10_uf_taxa_permanencia)

#Plotando o grafico de rank
ggplot(top10_uf_taxa_permanencia, aes(x = reorder(`Unidade Federativa do Curso`, -desc(percent_permanencias)), y = percent_permanencias)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = percent_permanencias), hjust = -0.1, size = 3) +
  coord_flip() +  # Inverte os eixos para facilitar a leitura
  labs(
    title = "Top 10 Maior Taxa Permanência por Região",
    x = "UF",
    y = "Taxa Permanência (%)"
  ) +
  theme_minimal()



#----------------------------------------------------------------------------------------------------------------
#VALORES ABSOLUTOS DE DESISTENCIA E PERMANENCIA POR UF
#rank de UFs com maior valor de desistencia 
uf_maior_desistencia <- uf_resumo %>%
  arrange(desc(total_desistencias))

View(uf_maior_desistencia)
#São Paulo (SP) (431.985)

#criando o top 10 da região
top10_uf_desistencia <- uf_maior_desistencia %>% slice(1:10)
View(top10_uf_desistencia)

#Plotando o grafico de rank
ggplot(top10_uf_desistencia, aes(x = reorder(`Unidade Federativa do Curso`, -desc(total_desistencias)), y = total_desistencias)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = total_desistencias), hjust = -0.1, size = 3) +
  coord_flip() +  # Inverte os eixos para facilitar a leitura
  labs(
    title = "Top 10 Desistências por UF",
    x = "UF",
    y = "Desistências"
  ) +
  theme_minimal()


#----------------------------------------------------------------------------------------------------------------
#rank de regiões com maior valor de permanencia 
uf_maior_permanencia <- uf_resumo %>%
  arrange(desc(total_permanencia))

View(uf_maior_permanencia)
#São Paulo (SP) (1.855.644)


#criando o top 10 da região
top10_uf <- uf_maior_permanencia %>% slice(1:10)
View(top10_uf)


ggplot(top10_uf, aes(x = reorder(`Unidade Federativa do Curso`, -desc(total_permanencia)), y = total_permanencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = total_permanencia), hjust = -0.1, size = 3) +
  coord_flip() +  # Inverte os eixos para facilitar a leitura
  labs(
    title = "Top 10 Permanências por UF",
    x = "UF",
    y = "Permanências"
  ) +
  theme_minimal()

#######################################################################################################################
#RESUMO PELO TIPO DE ADMINISTRAÇÃO:
adm_resumo <- dadosLimpos %>%
  group_by(`Categoria Administrativa`) %>%
  summarize(
    prazo_medio_acompanhamento_curso = round(mean(`Prazo de Acompanhamento do Curso em anos`, na.rm = TRUE), digits = 2),
    total_desistencias = sum(`Quantidade de Desistência no Curso no ano de referência`, na.rm = TRUE),
    total_permanencia = sum(`Quantidade de Permanência no Curso no ano de referência`, na.rm = TRUE),
    total_ingressantes = sum(`Quantidade de Ingressantes no Curso`, na.rm = TRUE),
    percent_desistencia = round((total_desistencias / total_ingressantes)*100, digits = 2),
    percent_permanencias = round((total_permanencia / total_ingressantes)*100, digits = 2)
  ) %>%
  arrange(desc(total_desistencias)) #ordenando pela coluna de desistencias

#formatando os valores de milhares para inserir os separadores (.)
adm_resumo$total_desistencias <- format(adm_resumo$total_desistencias, big.mark = ".", decimal.mark = ",")
adm_resumo$total_permanencia <- format(adm_resumo$total_permanencia, big.mark = ".", decimal.mark = ",")
adm_resumo$total_ingressantes <- format(adm_resumo$total_ingressantes, big.mark = ".", decimal.mark = ",")


View(adm_resumo)
#----------------------------------------------------------------------------------------------------------------
#rank maior permanencia absoluta por ADM
adm_maior_permanencia <- adm_resumo %>%
  arrange(desc(total_permanencia))

View(adm_maior_permanencia)
#Privada com fins lucrativos (4.596.778)

#criando o rank da adm permanencia
rank_adm <- adm_maior_permanencia %>% slice(1:10)
View(rank_adm)


ggplot(rank_adm, aes(x = reorder(`Categoria Administrativa`, -desc(total_permanencia)), y = total_permanencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = total_permanencia), hjust = -0.1, size = 3) +
  coord_flip() +  # Inverte os eixos para facilitar a leitura
  labs(
    title = "Total de Permanência por Tipo Administrativo",
    x = "Tipo Administrativo",
    y = "Permanências"
  ) +
  theme_minimal()

#----------------------------------------------------------------------------------------------------------------
#rank maior desistencia absoluta por ADM
adm_maior_desistencia <- adm_resumo %>%
  arrange(desc(total_desistencias))

View(adm_maior_desistencia)
#Privada com fins lucrativos (748.804)

#criando o rank da adm permanencia
rank_adm_menor <- adm_maior_desistencia %>% slice(1:10)
View(rank_adm_menor)


ggplot(rank_adm_menor, aes(x = reorder(`Categoria Administrativa`, -desc(total_desistencias)), y = total_desistencias)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = total_desistencias), hjust = -0.1, size = 3) +
  coord_flip() +  # Inverte os eixos para facilitar a leitura
  labs(
    title = "Total de Desistência por Tipo Administrativo",
    x = "Tipo Administrativo",
    y = "Desistências"
  ) +
  theme_minimal()


#----------------------------------------------------------------------------------------------------------------
#rank maior permanencia taxa por ADM
adm_maior_taxa_permanencia <- adm_resumo %>%
  arrange(desc(percent_permanencias))

View(adm_maior_taxa_permanencia)
#Publica Federal (36.6)

#criando o rank da adm pela taxa
rank_adm_taxa <- adm_maior_taxa_permanencia %>% slice(1:10)
View(rank_adm_taxa)


ggplot(rank_adm_taxa, aes(x = reorder(`Categoria Administrativa`, -desc(percent_permanencias)), y = percent_permanencias)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = percent_permanencias), hjust = -0.1, size = 3) +
  coord_flip() +  # Inverte os eixos para facilitar a leitura
  labs(
    title = "Taxas de Permanência por Tipo Administrativo",
    x = "Tipo Administrativo",
    y = "Taxa Permanência (%)"
  ) +
  theme_minimal()


#######################################################################################################################

#RESUMO PELA MODALIDADE DE ENSINO
modalidade_resumo <- dadosLimpos %>%
  group_by(`Modalidade de Ensino`) %>%
  summarize(
    prazo_medio_acompanhamento_curso = round(mean(`Prazo de Acompanhamento do Curso em anos`, na.rm = TRUE), digits = 2),
    total_desistencias = sum(`Quantidade de Desistência no Curso no ano de referência`, na.rm = TRUE),
    total_permanencia = sum(`Quantidade de Permanência no Curso no ano de referência`, na.rm = TRUE),
    total_ingressantes = sum(`Quantidade de Ingressantes no Curso`, na.rm = TRUE),
    percent_desistencia = round((total_desistencias / total_ingressantes)*100, digits = 2),
    percent_permanencias = round((total_permanencia / total_ingressantes)*100, digits = 2)
  ) %>%
  arrange(desc(total_desistencias)) #ordenando pela coluna de desistencias

#formatando os valores de milhares para inserir os separadores (.)
modalidade_resumo$total_desistencias <- format(modalidade_resumo$total_desistencias, big.mark = ".", decimal.mark = ",")
modalidade_resumo$total_permanencia <- format(modalidade_resumo$total_permanencia, big.mark = ".", decimal.mark = ",")
modalidade_resumo$total_ingressantes <- format(modalidade_resumo$total_ingressantes, big.mark = ".", decimal.mark = ",")


View(modalidade_resumo)
#----------------------------------------------------------------------------------------------------------------
#permanencias por modalidade (valor absoluto)
modalidade_permanencias <- modalidade_resumo %>%
  arrange(desc(total_permanencia))

ggplot(modalidade_permanencias, aes(x = reorder(`Modalidade de Ensino`, -desc(total_permanencia)), y = total_permanencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = total_permanencia), hjust = -0.1, size = 3) +
  coord_flip() +  # Inverte os eixos para facilitar a leitura
  labs(
    title = "Permanências por Modalidade",
    x = "Modalidade",
    y = "Permanências"
  ) +
  theme_minimal()

#----------------------------------------------------------------------------------------------------------------
#permanencias por modalidade (taxa)
modalidade_permanencias_taxa <- modalidade_resumo %>%
  arrange(desc(percent_permanencias))

ggplot(modalidade_permanencias_taxa, aes(x = reorder(`Modalidade de Ensino`, -desc(percent_permanencias)), y = percent_permanencias)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = percent_permanencias), hjust = -0.1, size = 3) +
  coord_flip() +  # Inverte os eixos para facilitar a leitura
  labs(
    title = "Taxa de Permanência por Modalidade",
    x = "Modalidade",
    y = "Taxa Permanência (%)"
  ) +
  theme_minimal()

#----------------------------------------------------------------------------------------------------------------
#desistencias por modalidade (valor absoluto)
modalidade_desistencias <- modalidade_resumo %>%
  arrange(desc(total_desistencias))

ggplot(modalidade_desistencias, aes(x = reorder(`Modalidade de Ensino`, -desc(total_desistencias)), y = total_desistencias)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = total_desistencias), hjust = -0.1, size = 3) +
  coord_flip() +  # Inverte os eixos para facilitar a leitura
  labs(
    title = "Desistências por Modalidade",
    x = "Modalidade",
    y = "Desistências"
  ) +
  theme_minimal()

#----------------------------------------------------------------------------------------------------------------
#desistencias por modalidade (taxa)
modalidade_desistencias_taxa <- modalidade_resumo %>%
  arrange(desc(percent_desistencia))

ggplot(modalidade_desistencias_taxa, aes(x = reorder(`Modalidade de Ensino`, -desc(percent_desistencia)), y = percent_desistencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = percent_desistencia), hjust = -0.1, size = 3) +
  coord_flip() +  # Inverte os eixos para facilitar a leitura
  labs(
    title = "Taxa de Desistência por Modalidade",
    x = "Modalidade",
    y = "Taxa Desistência (%)"
  ) +
  theme_minimal()




#######################################################################################################################
#Correlações:

#FAZENDO CORRELAÇÃO ENTRE PERMANENCIA E TIPO DE INSTITUIÇÃO (PRIVADA/PUBLICA)
#Distribuição de Permanências X Tipo instituição
ggplot(dadosLimpos, aes(x = `Categoria Administrativa`, y = `Quantidade de Permanência no Curso no ano de referência`)) +
  geom_boxplot() +
  labs(title = "Distribuição de Permanências X Tipo instituição")

#Distribuição de Desistências X Tipo instituição
ggplot(dadosLimpos, aes(x = `Categoria Administrativa`, y = `Quantidade de Desistência no Curso no ano de referência`)) +
  geom_boxplot() +
  labs(title = "Distribuição de Desistências X Tipo instituição")

#----------------------------------------------------------------------------------------------------------------
#Distribuição de Permanências X Modalidade de Ensino
ggplot(dadosLimpos, aes(x = `Modalidade de Ensino`, y = `Quantidade de Permanência no Curso no ano de referência`)) +
  geom_boxplot() +
  labs(title = "Distribuição de Permanências X Modalidade de Ensino")

#Distribuição de Desistências X Modalidade de Ensino
ggplot(dadosLimpos, aes(x = `Modalidade de Ensino`, y = `Quantidade de Desistência no Curso no ano de referência`)) +
  geom_boxplot() +
  labs(title = "Distribuição de Desistências X Modalidade de Ensino")

#----------------------------------------------------------------------------------------------------------------
#FAZENDO CORRELAÇÃO ENTRE DESISTENCIA, PERMANENCIA E TIPOS DE INSTITUIÇÕES
ggplot(dadosLimpos, aes(x = `Quantidade de Desistência no Curso no ano de referência`, y = `Quantidade de Permanência no Curso no ano de referência`, color = `Categoria Administrativa`)) +
  geom_point(size = 3) +  
  geom_smooth(method = "lm", se = FALSE, color = "black") + #adicionando a linha de regressão linear
  labs(
    title = "Gráfico de Dispersão por Tipo de Instituição",
    x = "Desistências",
    y = "Permanências",
    color = "Tipo de Instituição"
  ) +
  theme_minimal() +
  facet_wrap(~ `Categoria Administrativa`, ncol = 2)  


#----------------------------------------------------------------------------------------------------------------
#FAZENDO CORRELAÇÃO ENTRE DESISTENCIA, PERMANENCIA E MODALIDADE
ggplot(dadosLimpos, aes(x = `Quantidade de Desistência no Curso no ano de referência`, y = `Quantidade de Permanência no Curso no ano de referência`, color = `Modalidade de Ensino`)) +
  geom_point(size = 3) +  
  geom_smooth(method = "lm", se = FALSE, color = "black") + #adicionando a linha de regressão linear
  labs(
    title = "Gráfico de Dispersão pela Modalidade do Curso",
    x = "Desistências",
    y = "Permanências",
    color = "Modalidade de Ensino"
  ) +
  theme_minimal() +
  facet_wrap(~ `Modalidade de Ensino`, ncol = 2)  


#----------------------------------------------------------------------------------------------------------------
#FAZENDO CORRELAÇÃO ENTRE DESISTENCIA, PERMANENCIA E REGIÃO
ggplot(dadosLimpos, aes(x = `Quantidade de Desistência no Curso no ano de referência`, y = `Quantidade de Permanência no Curso no ano de referência`, color = `Região do Curso`)) +
  geom_point(size = 3) +  
  geom_smooth(method = "lm", se = FALSE, color = "black") + #adicionando a linha de regressão linear
  labs(
    title = "Gráfico de Dispersão pela Região do Curso",
    x = "Desistências",
    y = "Permanências",
    color = "Região"
  ) +
  theme_minimal() +
  facet_wrap(~ `Região do Curso`, ncol = 2)  

#RESULTOU EM MUITOS NA's - PROVAVELMENTE DEVIDO AOS CURSOS REMOTOS

#retirando os NA's e refazendo
dados_sem_na <- dadosLimpos %>%
  filter(!is.na(`Região do Curso`))

ggplot(dados_sem_na, aes(x = `Quantidade de Desistência no Curso no ano de referência`, y = `Quantidade de Permanência no Curso no ano de referência`, color = `Região do Curso`)) +
  geom_point(size = 3) +  
  geom_smooth(method = "lm", se = FALSE, color = "black") + #adicionando a linha de regressão linear
  labs(
    title = "Gráfico de Dispersão pela Região do Curso",
    x = "Desistências",
    y = "Permanências",
    color = "Região"
  ) +
  theme_minimal() +
  facet_wrap(~ `Região do Curso`, ncol = 2)  


#FAZENDO CORRELAÇÃO ENTRE DESISTENCIA, PERMANENCIA E TIPO DE ADM
ggplot(dadosLimpos, aes(x = `Quantidade de Desistência no Curso no ano de referência`, y = `Quantidade de Permanência no Curso no ano de referência`, color = `Categoria Administrativa`)) +
  geom_point(size = 3) +  
  geom_smooth(method = "lm", se = FALSE, color = "black") + #adicionando a linha de regressão linear
  labs(
    title = "Gráfico de Dispersão pela Categoria Administrativa",
    x = "Desistências",
    y = "Permanências",
    color = "Categoria Administrativa"
  ) +
  theme_minimal() +
  facet_wrap(~ `Categoria Administrativa`, ncol = 2)


