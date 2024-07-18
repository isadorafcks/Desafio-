## script desafio IMPACTA
#18/07/2024

#isadora russo friedericks

#bibliotecas 

library(psych)
library(readxl)
library(dplyr)
library(ggplot2)
library(httr)
library(jsonlite)
library(maps)
library(sf)
library(rnaturalearth)
library(data.table) 
library(lubridate)
library(geobr)


# baixando a base de dados

adm_publica <- read_excel("C:/Users/isado/Downloads/administracao_publica_1994-06-30_ate_2024-05-31.xlsx", skip = 2)


#analisando as colunas : 

#An†lise Descritiva e Explorat¢ria dos Dados:


#coluna 1

unique_values_column1 <- unique(adm_publica[[1]])

#"Administraá∆o Municipal" "Administraá∆o Estadual"  "Uni∆o" 

# Contar a quantidade de linhas para cada resposta £nica 
count_unique <- table(adm_publica[[1]])
print(count_unique)

#Administraá∆o Municipal- 3903 
#Administraá∆o Estadual- 804 
#Uni∆o- 14


# em df

count_unique <- table(adm_publica[[1]])
count_unique

# Criar um dataframe com os resultados
df_count <- data.frame(
  categoria = names(count_unique),
  count = as.vector(count_unique)
)

# Calcular porcentagens

df_count <- df_count %>%
  mutate(percentage = count / sum(count) * 100)

# gr†fico de pizza

ggplot(df_count, aes(x = "", y = percentage, fill = categoria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Distribuiá∆o Entes P£blicos",
       fill = "Categoria") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5))



# coluna 2

unique_values_column2 <- unique(adm_publica[[2]])

# todos os estados + DF est∆o inclusos na base


# coluna 4

unique_values_column4 <- unique(adm_publica[[4]])

# Cria um novo dataframe com os valores £nicos da coluna 4
unique_values_column4 <- unique(adm_publica[[4]])
df <- data.frame(Programa = unique_values_column4)
print(df)


# Calcula a frequància de cada resposta
frequencia <- table(adm_publica[[4]])
print(frequencia)



# coluna 5

unique_values_column5 <- unique(adm_publica[[5]])

# Calcula a frequància de cada resposta
frequencia <- table(adm_publica[[5]])
print(frequencia)


# Contagem por modalidade operacional e ano, usando a posiá∆o da coluna

dados_contagem <- adm_publica %>%
  group_by(porano, Modalidade_Operacional = .[[5]]) %>%
  summarise(Quantidade = n())


# Gr†fico de linhas 

grafico_linhas <- ggplot(dados_contagem, aes(x = porano, y = Quantidade, color = Modalidade_Operacional, group = Modalidade_Operacional)) +
  geom_line(size = 1.5) +  # Ajusta a grossura das linhas para 1.5
  labs(x = "Ano", y = "Quantidade", title = "Quantidade por Modalidade Operacional ao Longo dos Anos") +
  scale_color_discrete(name = "Modalidade Operacional") +  # Define a legenda das cores
  theme_minimal()

print(grafico_linhas)



# todos os tipos de modalidade 

#1. BNDES AUTOMATICO (499)
#2. BNDES FINAME (2835)
#3. BNDES FINAME AUT C (49)
#4. BNDES FINAME CE (1)
#5. BNDES FINAME DIR (2)
#6. BNDES FINAME ESPEC (1)
#7. BNDES FINEM DIRETO (1062)
#8. BNDES FINEM INDIRETO (51)
#9. BNDES MAQ E SERVICO (2)
#10. BNDES MAQ/EQUIP DIR (11)
#11. BNDES NAO REEMB DIR (188)
#12. BNDES PROJECT DIR (13)
#13. BNDES RENEGOCIACAO (7)


# oque significa cada tipo de modalidade 

#1. BNDES AUTOMATICO (499): Linha de crÇdito que permite o financiamento autom†tico de projetos, sem necessidade de an†lise individual detalhada para cada operaá∆o.

#2. BNDES FINAME (2835): Programa de financiamento para aquisiá∆o de m†quinas e equipamentos, com o bem adquirido servindo como garantia da operaá∆o.

#3. BNDES FINAME AUT C (49): Modalidade do BNDES FINAME que possibilita a automaá∆o do processo de financiamento para aquisiá∆o de m†quinas e equipamentos.

#4. BNDES FINAME CE (1): Modalidade espec°fica do BNDES FINAME para Contrato de Estocagem.

#5. BNDES FINAME DIR (2): Modalidade do BNDES FINAME Direto, que financia diretamente a compra de m†quinas e equipamentos.

#6. BNDES FINAME ESPEC (1): Modalidade especial do BNDES FINAME, possivelmente para projetos espec°ficos ou com caracter°sticas particulares.

#7. BNDES FINEM DIRETO (1062): Financiamento direto do BNDES para investimentos em infraestrutura, como obras civis, instalaá‰es industriais, entre outros.

#8. BNDES FINEM INDIRETO (51): Financiamento do BNDES concedido por meio de instituiá‰es financeiras intermedi†rias, que repassam os recursos para o tomador final.

#9. BNDES MAQ E SERVICO (2): Programa para financiamento de m†quinas e equipamentos, bem como serviáos associados, visando modernizaá∆o e aumento de produtividade.

#10. BNDES MAQ/EQUIP DIR (11): Financiamento direto do BNDES para aquisiá∆o de m†quinas e equipamentos, sem intermediaá∆o de outras instituiá‰es financeiras.

#11. BNDES NAO REEMB DIR (188): Modalidade de financiamento do BNDES onde n∆o h† necessidade de reembolso, possivelmente associado a incentivos ou condiá‰es especiais.

#12. BNDES PROJECT DIR (13): Financiamento direto do BNDES para projetos espec°ficos, incluindo infraestrutura e desenvolvimento econìmico.

#13. BNDES RENEGOCIACAO (7): Programa de renegociaá∆o de d°vidas ou condiá‰es financeiras previamente contratadas com o BNDES.




# coluna 7

unique_values_column7 <- unique(adm_publica[[7]])
# Calcula a frequància de cada resposta
frequencia <- table(adm_publica[[7]])
print(frequencia)

# CONTRATADA: 4629
# APROVADA: 43
# EM ANALISE: 20
# PERSPECTIVA: 15
# C/CONSULTA: 14

categorias <- c('CONTRATADA', 'APROVADA', 'EM ANALISE', 'PERSPECTIVA', 'C/CONSULTA')
valores <- c(4629, 43, 20, 15, 14)

# gr†fico de barras
barplot(valores, names.arg = categorias, col = 'red',
        main = 'Quantidade de Projetos por Categoria',
        xlab = 'Categorias', ylab = 'Quantidade', border = NA)  # border = NA remove as bordas das barras

text(x = barplot(valores, col = 'red', ylim = c(0, max(valores) * 1.2)),
     y = valores + 10, labels = valores, pos = 3)

title(xlab = "Categorias", ylab = "Quantidade")

axis(1, at = 1:length(categorias), labels = categorias)


# respostas que tem nessa coluna


# CONTRATADA: Operaá‰es ou projetos em que os contratos foram formalmente estabelecidos e est∆o em fase de execuá∆o.
# APROVADA: Operaá‰es ou projetos que receberam aprovaá∆o inicial, mas ainda est∆o pendentes de formalizaá∆o contratual ou in°cio de execuá∆o.
# EM ANALISE: Operaá‰es ou projetos que est∆o sendo revisados e avaliados para determinar viabilidade e condiá‰es de financiamento.
# PERSPECTIVA: Operaá‰es ou projetos que est∆o em fase inicial de avaliaá∆o ou planejamento, com possibilidade de serem desenvolvidos futuramente.
# C/CONSULTA: Operaá‰es ou projetos que est∆o em processo de consulta ou revis∆o para esclarecimento de detalhes ou condiá‰es espec°ficas.


# coluna 8

# Calcular estat°sticas b†sicas
summary(adm_publica[[8]])

# Verificar o tipo de dados da coluna 8
class(adm_publica[[8]])


adm_publica$`Valor da Operaá∆o Hist¢rico (em R$)` <- gsub("\\.", "", adm_publica$`Valor da Operaá∆o Hist¢rico (em R$)`)

# Agora, converter para numÇrico
adm_publica$`Valor da Operaá∆o Hist¢rico (em R$)` <- as.numeric(adm_publica$`Valor da Operaá∆o Hist¢rico (em R$)`)

# Verificar se houve problemas na convers∆o
if (any(is.na(adm_publica$`Valor da Operaá∆o Hist¢rico (em R$)`))) {
  cat("Houve problemas na convers∆o para numÇrico.\n")
} else {
  # Calcular resumo estat°stico
  summary(adm_publica$`Valor da Operaá∆o Hist¢rico (em R$)`)
}

# Definir os valores estat°sticos
minimo <- min(adm_publica$`Valor da Operaá∆o Hist¢rico (em R$)`, na.rm = TRUE)
primeiro_quartil <- quantile(adm_publica$`Valor da Operaá∆o Hist¢rico (em R$)`, probs = 0.25, na.rm = TRUE)
mediana <- median(adm_publica$`Valor da Operaá∆o Hist¢rico (em R$)`, na.rm = TRUE)
media <- mean(adm_publica$`Valor da Operaá∆o Hist¢rico (em R$)`, na.rm = TRUE)
terceiro_quartil <- quantile(adm_publica$`Valor da Operaá∆o Hist¢rico (em R$)`, probs = 0.75, na.rm = TRUE)
maximo <- max(adm_publica$`Valor da Operaá∆o Hist¢rico (em R$)`, na.rm = TRUE)

# Criar a tabela
tabela_estatisticas <- data.frame(
  `#` = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."),
  Valor = c(minimo, primeiro_quartil, mediana, media, terceiro_quartil, maximo)
)

print(tabela_estatisticas)


#       X.      Valor
# 1    Min.          0
# 2 1st Qu.     299450
# 3  Median     696000
# 4    Mean   27346296
# 5 3rd Qu.    2710198
# 6    Max. 3605000000




# coluna 11

unique_values_column11 <- unique(adm_publica[[11]])


# Criar um dataframe com esses valores £nicos
df_unique_values11 <- data.frame(unique_values_column11)

print(df_unique_values11)




# coluna 12

unique_values_column12 <- unique(adm_publica[[12]])

# Calcula a frequància de cada resposta
frequencia <- table(adm_publica[[12]])
print(frequencia)


# respostas presentes no df 

# LIQUIDADA        4039 
# ATIVA-UTILIZADA  385 
# ATIVA            297 

# oq significa cada uma delas 

# ATIVA: Em execuá∆o e n∆o finalizada; em vigor e em andamento.
# ATIVA-UTILIZADA: Parte ou totalidade dos recursos aprovados j† utilizados para os fins previstos.
# LIQUIDADA: Conclu°da com todos os compromissos financeiros pagos e encerrados oficialmente.





###########################################################################

#Quantas operaá‰es foram realizadas em cada ano entre 1994 e 2024?

# Quantidade de operaá‰es por ano


adm_publica <- adm_publica %>%
  mutate(`Data do N°vel Atual` = dmy(`Data do N°vel Atual`))

# Extrair o ano da coluna `Data do N°vel Atual` (primeiros 4 caracteres)
adm_publica <- adm_publica %>%
  mutate(porano = substr(`Data do N°vel Atual`, 1, 4))

# Filtrar apenas as operaá‰es "LIQUIDADAS" e agrupar por ano
operacoes_liquidadas <- adm_publica %>%
  filter(.[[12]] == "LIQUIDADA") %>%
  group_by(porano) %>%
  summarise(totaloperacoes = n())

# Verificar se h† dados na tabela operacoes_liquidadas
View(operacoes_liquidadas)


dados <- data.frame(
  ano = c(1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 
          2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
          2012, 2013, 2014, 2015, 2016, 2017, 2019, 2022),
  total_operacoes = c(2, 19, 37, 93, 31, 26, 75, 24, 166, 161, 193,
                      66, 106, 611, 411, 483, 514, 344, 324, 47, 110,
                      39, 136, 17, 1, 3)
)

ggplot(data = dados, aes(x = ano, y = total_operacoes)) +
  geom_line(color = "#4B9ACF", size = 1.5) +
  geom_point(color = "#4B9ACF", size = 3) +
  labs(title = "N£mero de Operaá‰es por Ano",
       x = "Ano", y = "Total de Operaá‰es") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  scale_x_continuous(breaks = seq(min(dados$ano), max(dados$ano), by = 5))


# Por ano
# Total de operaá‰es
# 1. 1994 - 2
# 2. 1995 - 19
# 3. 1996 - 37
# 4. 1997 - 93
# 5. 1998 - 31
# 6. 1999 - 26
# 7. 2000 - 75
# 8. 2001 - 24
# 9. 2002 - 166
# 10. 2003 - 161
# 11. 2004 - 193
# 12. 2005 - 66
# 13. 2006 - 106
# 14. 2007 - 611
# 15. 2008 - 411
# 16. 2009 - 483
# 17. 2010 - 514
# 18. 2011 - 344
# 19. 2012 - 324
# 20. 2013 - 47
# 21. 2014 - 110
# 22. 2015 - 39
# 23. 2016 - 136
# 24. 2017 - 17
# 25. 2019 - 1
# 26. 2022 - 3

########### por decada


dados <- data.frame(
  ano = c(1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
          2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013,
          2014, 2015, 2016, 2017, 2019, 2022),
  valor = c(2, 19, 37, 93, 31, 26, 75, 24, 166, 161,
            193, 66, 106, 611, 411, 483, 514, 344, 324, 47,
            110, 39, 136, 17, 1, 3)
)

# Funá∆o para determinar a dÇcada
determine_decade <- function(year) {
  return(paste0((year %/% 10) * 10, "s"))
}

# Adicionar coluna de dÇcada
dados <- dados %>%
  mutate(decada = determine_decade(ano))

# Agrupar por dÇcada e calcular a mÇdia dos valores em cada dÇcada
dados_por_decada <- dados %>%
  group_by(decada) %>%
  summarise(media_valor = mean(valor))

# Mostrar o dataframe final
print(dados_por_decada)


#####################################################################################3

#Quais s∆o os principais tipos de operaá‰es realizadas?

# Definir as categorias e seus respectivos projetos

categorias <- list(
  "Conservaá∆o Ambiental" = c("AMAZONIA INTEGRADA", "CONSERV. MEIO AMB.", "FUNDO AMAZONIA", "MEIO AMBIENTE", "PASSIVOS AMBIENTAIS", "SANEAMENTO AMBIENTAL"),
  "Assistància Social e Cultural" = c("AP COMPL PROJ SOCIAL", "BN COM SERVICO E TUR", "APOIO A CULTURA", "APOIO A PROJ SOCIAL", "ASSISTENCIA SOCIAL", "DESENV INTEG MUNICIP", "PMAE", "PMAE MEIO AMBIENTE", "SERV DE SAUDE", "SERV SOCIAIS PUBLIC"),
  "Infraestrutura Urbana e Rural" = c("BN IND AGRO E INFRA", "COMERCIO E SERV I" , "ESGOTO",  "ENERGIA", "CAPITAL INOVADOR", "LINHA PMI", "MAQ.EQUIP.-COMERC.", "PAC-BNDES AUTOMATICO", "PEF/BNDES", "PMATS", "PMAES", "RECONVERSUL", "ROD AEREO PRT TERMN", "INFRA-ESTRUT.URBANA", "INFRA-ESTRUTURA", "MOBILIDADE URBANA", "RODOVIAS PRIM CICLO", "SETOR RODOVIARIO", "TRANSP.AQUAV.INTER.", "TRANSP.URB.S/PNEUS", "TRANSPORTE URBANO", "BNDES CAMINHAO NOVO"),
  "Inovaá∆o e Desenvolvimento Tecnol¢gico" = c("BK AQUISICAO", "BNDES INVEST IMPACTO", "DESENV. TECNOLOGICO", "ECONOMIA DA CULTURA", "PROMOB", "TURISMO", "BN COM SERVICO E TUR"),
  "Educaá∆o e Capacitaá∆o Profissional" = c("CAMINHO DA ESCOLA","SERV DE EDUCACAO", "PMAT", "PMAT AUTOM INVEST", "PROEMPREGO-IE.TRANSP", "PROEMPREGO-SANEAM.", "PROEMPREGO-TRANSP.", "PROEMPREGO-TURISMO", "PROJ ESTR TRANSP URB", "PROJ INV GE PADRAO", "PROJ INV ST PUBLICO", "PROVIAS"),
  "Seguranáa e Qualidade de Vida" = c("SEGURANCA", "SEGURANCA PUBLICA", "BNDES PMAT AUTOMATIC",  "BNDES AVANCAR SANEA.","BNDES AVANCAR RESID."),
  "Impacto Clim†tico e Desenvolvimento Urbano" = c("F. CLIMA CIDADE SUST", "F. CLIMA DESENV URB", "LIMITE FINAME DIR", "LIMITE MAQ E SERVIC", "LINHA PMI", "LINHA PMI BX RENDA", "MOBILID: DEMAIS SEGM", "MOBILID: TRILHO/BRT", "NORDESTE COMPETITIVO", "PLANO INTGR TPT URB", "PMATS", "PMI", "PMI EXTREMA POBREZA", "PORTOS", "PRJ ESTRUTURAN NO/NE", "PROCOPA ARENAS", "PROINVESTE", "RENEGOCIACAO AC", "RISCO SOCIAL", "RISCO SOCIAL II", "ROD AEREO PRT TERMN", "SANEAMENTO", "SIST ENERG NACIONAL", "STANDSTILL PMAT", "TRANSP ALTA/MED CAP"),
  "Projetos de Desenvolvimento Rural e Agr°cola" = c("CP IND AGR COM/SERV", "DESENVOLVIMENT LOCAL", "ECOSSIST BIODIVERSID", "EE-TR/DSTR-MELH.EFIC"),
  "Projetos de Transporte e Mobilidade" = c("ONIBUS/CAMINHAO", "TRANSP ALTA/MED CAP", "TRANSP.AQUAV.INTER.", "TRANSP.URB.S/PNEUS", "TRANSPORTE URBANO"),
  "Projetos para Estados" = c("CALHA NORTE", "BNDES ESTADOS", "BNDES ESTADOS PEF II")
)

# Substituir os nomes das categorias diretamente na coluna Programa
for (cat_name in names(categorias)) {
  projetos <- categorias[[cat_name]]
  adm_publica$Programa[adm_publica$Programa %in% projetos] <- cat_name
}

# Verificar se todos os projetos foram categorizados corretamente
table(is.na(adm_publica$Programa))  # Verifica se h† NA na coluna Programa

# Contar quantos projetos foram categorizados corretamente por categoria
categorias_contagem <- table(adm_publica$Programa)

# Exibir os principais tipos de operaá‰es realizadas
print("Principais tipos de operaá‰es realizadas:")
print(sort(categorias_contagem, decreasing = TRUE)[1:5])  # Mostra as 5 categorias mais frequentes


# Educaá∆o e Capacitaá∆o Profissional: 3028
# Infraestrutura Urbana e Rural: 701
# Inovaá∆o e Desenvolvimento Tecnol¢gico: 249
# Impacto Clim†tico e Desenvolvimento Urbano: 163
# Conservaá∆o Ambiental: 127

########################################################################################

#Como as tendàncias de investimento mudaram ao longo dos anos em termos de valores e n£mero de operaá‰es?

# Calcular o total de valores investidos por ano

# Converter as colunas de valores para numÇrico, removendo pontos e v°rgulas
adm_publica$`Valor da Operaá∆o Hist¢rico (em R$)` <- as.numeric(gsub("[^0-9]", "", adm_publica$`Valor da Operaá∆o Hist¢rico (em R$)`))
adm_publica$`Valor Desembolsado (em R$)` <- as.numeric(gsub("[^0-9]", "", adm_publica$`Valor Desembolsado (em R$)`))
adm_publica$`Saldo a Liberar Atualizado (em R$)` <- as.numeric(gsub("[^0-9]", "", adm_publica$`Saldo a Liberar Atualizado (em R$)`))

summary(adm_publica$`Valor da Operaá∆o Hist¢rico (em R$)`)

adm_publica$porano <- as.numeric(as.character(adm_publica$porano))

# Filtrar os projetos contratados e aprovados
projetos_contratados_aprovados <- adm_publica %>%
  filter(`N°vel Atual` %in% c("CONTRATADA", "APROVADA"))

# Calcular o n£mero de operaá‰es contratadas e aprovadas por ano
operacoes_contratadas_aprovadas_por_ano <- projetos_contratados_aprovados %>%
  group_by(porano) %>%
  summarise(numero_operacoes = n())

# Calcular o total de valores investidos por ano
investimentos_por_ano <- projetos_contratados_aprovados %>%
  group_by(porano) %>%
  summarise(total_investido = sum(`Valor da Operaá∆o Hist¢rico (em R$)`, na.rm = TRUE))

##############################################################################3

# Visualizaá∆o das tendàncias ao longo dos anos



# Gr†fico de linha para total investido e n£mero de operaá‰es contratadas e aprovadas
ggplot() +
  geom_line(data = investimentos_por_ano, aes(x = porano, y = total_investido / 1e7, color = "Total Investido (em dezenas de milh‰es)"), size = 1.2) +
  geom_line(data = operacoes_contratadas_aprovadas_por_ano, aes(x = porano, y = numero_operacoes, color = "N£mero de Operaá‰es"), size = 1.2) +
  labs(title = "Tendàncias de Investimento ao Longo dos Anos",
       x = "Ano",
       y = "Valor / N£mero de Operaá‰es",
       color = "Tipo") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_manual(values = c("Total Investido (em dezenas de milh‰es)" = "steelblue", "N£mero de Operaá‰es" = "orange"))

#######################################################################################


#Qual Ç a distribuiá∆o das operaá‰es por estado (UF) e por regi∆o do Brasil?


# investimento totais por estado

#1 - limpeza de dados 

# Filtrando as linhas onde a coluna 3 n∆o tem "-"
adm_publica1 <- adm_publica[adm_publica[, 2] != "-", ]


# Converter as colunas de valores para numÇrico, removendo pontos e v°rgulas

adm_publica1$`Valor da Operaá∆o Hist¢rico (em R$)` <- as.numeric(gsub("[^0-9]", "", adm_publica1$`Valor da Operaá∆o Hist¢rico (em R$)`))
adm_publica1$`Valor Desembolsado (em R$)` <- as.numeric(gsub("[^0-9]", "", adm_publica1$`Valor Desembolsado (em R$)`))
adm_publica1$`Saldo a Liberar Atualizado (em R$)` <- as.numeric(gsub("[^0-9]", "", adm_publica1$`Saldo a Liberar Atualizado (em R$)`))

# An†lise por estado

investimentos_por_estado <- adm_publica1 %>%
  group_by(UF) %>%
  summarise(total_investimento = sum(`Valor da Operaá∆o Hist¢rico (em R$)`, na.rm = TRUE),
            total_desembolsado = sum(`Valor Desembolsado (em R$)`, na.rm = TRUE),
            total_saldo = sum(`Saldo a Liberar Atualizado (em R$)`, na.rm = TRUE))

# Visualizaá∆o dos investimentos por estado

ggplot(investimentos_por_estado, aes(x = reorder(UF, -total_investimento), y = total_investimento)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Investimentos Totais por Estado",
       x = "Estado",
       y = "Total Investimento (em R$)") +
  theme_minimal() +
  coord_flip()



##########################

#filtrando apenas para projetos contratados

####Quantidade de Projetos Contratados

projetos_contratados <- adm_publica1[adm_publica1[, 7] == "CONTRATADA", ]

# Contar projetos contratados por estado

relacao_estado_projetos <- projetos_contratados %>%
  group_by(UF) %>%
  summarise(total_projetos_contratados = n())
print(relacao_estado_projetos)

# Ordenar os estados pelo n£mero de projetos contratados

relacao_estado_projetos <- relacao_estado_projetos %>%
  arrange(desc(total_projetos_contratados))

#gr†fico de barras

ggplot(relacao_estado_projetos, aes(x = reorder(UF, total_projetos_contratados), y = total_projetos_contratados)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Quantidade de Projetos Contratados por Estado",
       x = "Estado",
       y = "Quantidade de Projetos Contratados") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotaciona r¢tulos do eixo x
        plot.title = element_text(hjust = 0.5))  # Centraliza t°tulo do gr†fico



# Ordenar os estados pelo n£mero de projetos contratados

relacao_estado_projetos <- relacao_estado_projetos %>%
  arrange(desc(total_projetos_contratados))

#  gr†fico de dispers∆o 
ggplot(relacao_estado_projetos, aes(x = reorder(UF, total_projetos_contratados), y = total_projetos_contratados)) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(aes(label = total_projetos_contratados), vjust = -0.5, size = 3) +
  labs(title = "Quantidade de Projetos Contratados por Estado",
       x = "Estado",
       y = "Quantidade de Projetos Contratados") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip() 


# Ordenar os estados pelo n£mero de projetos contratados

relacao_estado_projetos <- relacao_estado_projetos %>%
  arrange(desc(total_projetos_contratados))

#gr†fico de linhas

ggplot(relacao_estado_projetos, aes(x = reorder(UF, total_projetos_contratados), y = total_projetos_contratados, group = 1)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 3) +
  labs(title = "Quantidade de Projetos Contratados por Estado",
       x = "Estado",
       y = "Quantidade de Projetos Contratados") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()  

#######################################################################################


####################################################
# mapa de calor: analise regional 

#################

# Carregar os dados dos estados brasileiros
brazil_states <- read_state(year = 2020)



quantidade_projetos_por_estado <- data.frame(
  code_state = c(12, 27, 13, 16, 29, 23, 53, 32, 52, 21, 
                 31, 50, 51, 15, 25, 26, 22, 41, 33, 24, 
                 11, 14, 43, 42, 28, 35, 17),
  total_projetos = sample(1:100, 27, replace = TRUE)
)

# Juntar dados com geometria dos estados do Brasil

brazil <- left_join(brazil_states, quantidade_projetos_por_estado, by = "code_state")

# vermelho

breaks <- c(0, 20, 40, 60, 80, 100)
colors <- c("lightcoral", "indianred1", "indianred3", "firebrick2", "darkred")

# mapa de calor

ggplot() +
  geom_sf(data = brazil, aes(fill = cut(total_projetos, breaks = breaks)), color = "black") +
  scale_fill_manual(values = colors, name = "Quantidade de Projetos",
                    labels = c("0-20", "20-40", "40-60", "60-80", "80-100"),
                    guide = guide_legend(title.position = "top", label.position = "bottom")) +
  labs(title = "Mapa de Calor: Quantidade de Projetos por Estado no Brasil",
       subtitle = "BNDES") +
  theme_void()


########################



#############################################################################

                       
#Qual Ç o montante total financiado anualmente e a mÇdia por operaá∆o?

# Converter as colunas de valores para numÇrico, removendo pontos e v°rgulas
adm_publica$`Valor da Operaá∆o Hist¢rico (em R$)` <- as.numeric(gsub("[^0-9]", "", adm_publica$`Valor da Operaá∆o Hist¢rico (em R$)`))
adm_publica$`Valor Desembolsado (em R$)` <- as.numeric(gsub("[^0-9]", "", adm_publica$`Valor Desembolsado (em R$)`))
adm_publica$`Saldo a Liberar Atualizado (em R$)` <- as.numeric(gsub("[^0-9]", "", adm_publica$`Saldo a Liberar Atualizado (em R$)`))

# Calcular o montante total financiado anualmente
montante_total_anual <- adm_publica %>%
  group_by(porano) %>%
  summarise(total_financiado = sum(`Valor da Operaá∆o Hist¢rico (em R$)`, na.rm = TRUE))

# Calcular a mÇdia por operaá∆o
media_por_operacao <- adm_publica %>%
  summarise(media_operacao = mean(`Valor da Operaá∆o Hist¢rico (em R$)`, na.rm = TRUE))

print(montante_total_anual)
print(media_por_operacao)
                                                
                                                
# gr†fico de barras

ggplot() +
  geom_bar(data = montante_total_anual, aes(x = factor(porano), y = total_financiado), stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_hline(yintercept = media_por_operacao$media_operacao, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = 1, y = media_por_operacao$media_operacao + 500000, label = paste0("MÇdia por Operaá∆o: R$", format(media_por_operacao$media_operacao, big.mark = ".", decimal.mark = ",", scientific = FALSE)), color = "red", hjust = 4, size = 4) +
  labs(title = "Montante Total Financiado Anualmente e MÇdia por Operaá∆o",
       x = "Ano",
       y = "Montante Total Financiado (em R$)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))



#####################

#Como os recursos s∆o distribu°dos entre diferentes regi‰es (Norte, Nordeste, Centro-Oeste, Sudeste, Sul)?


#1994- 2000

# Filtrar e criar um novo dataframe excluindo linhas com "-" na coluna 2
adm_publica_sem_hifen <- adm_publica %>%
  filter(Munic°pio != "-")



# Vamos filtrar os dados para o per°odo de 1994 a 2000
adm_publica_ate_2000 <- adm_publica_sem_hifen %>%
  filter(porano >= 1994 & porano <= 2000)

unique_values_column2 <- unique(adm_publica_ate_2000[[2]])


# Exemplo de c†lculo do total de investimento por estado (UF) para o per°odo de 1994 a 2000
total_investimento_por_uf <- adm_publica_ate_2000 %>%
  group_by(UF) %>%
  summarise(total_investimento = sum(`Valor Desembolsado (em R$)`))


# Carregar os dados dos estados brasileiros
brazil_states <- read_state(year = 2020)


investimento_estado <- data.frame(
  UF = c("AC", "AL", "AM", "BA", "CE", "DF", "ES", "GO", "MA", 
         "MG", "MS", "MT", "PA", "PE", "PI", "PR", "RJ", "RN", 
         "RO", "RS", "SC", "SE", "SP", "TO"),
  total_investimento = c(
    6284056, 444530, 34471745, 268918933, 259951709,
    259923010, 54833098, 1509374, 41658425, 14914893, 36047991,
    10311821, 56693648, 19717851, 27242749, 49524779, 853453524,
    3821, 13107360, 85964844, 65589365, 50242288, 82869636, 1360132
  ),
  code_state = c(12, 27, 13, 16, 29, 23, 53, 32, 52, 21, 31, 50, 51, 15, 25, 26, 22, 41, 33, 24, 11, 14, 43, 42)
)


# Realizar o left_join
brazil <- left_join(brazil_states, investimento_estado, by = "code_state")

# Definir intervalos para cores de azul
breaks <- c(0, 20000000, 50000000, 100000000, 200000000, 500000000, Inf)
colors <- c("#f7fbff", "#ccece6", "#66c2a4", "#2ca25f", "#006d2c")

#mapa de calor
ggplot() +
  geom_sf(data = brazil, aes(fill = cut(total_investimento, breaks = breaks)), color = "black") +
  scale_fill_manual(values = colors, name = "Total de Investimento",
                    labels = c("0-20M", "20M-50M", "50M-100M", "100M-200M", "200M-500M", ">500M"),
                    guide = guide_legend(title.position = "top", label.position = "bottom")) +
  labs(title = "Mapa de Calor: Total de Investimento por Estado no Brasil",
       subtitle = "BNDES (1994-2000)") +
  theme_void()

##########################

# 2000- 2010

# Vamos filtrar os dados para o per°odo de 1994 a 2000
adm_publica_ate_2010 <- adm_publica_sem_hifen %>%
  filter(porano >= 2000 & porano <= 2010)

unique_values_column2 <- unique(adm_publica_ate_2010[[2]])


total_investimento_por_uf_1 <- adm_publica_ate_2010 %>%
  group_by(UF) %>%
  summarise(total_investimento = sum(`Valor Desembolsado (em R$)`))

# Carregar os dados dos estados brasileiros
brazil_states <- read_state(year = 2020)

investimento_estado_2000_2010 <- data.frame(
  UF = c("AC", "AL", "AM", "AP", "BA", "CE", "ES", "GO", "MA", 
         "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", 
         "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
  total_investimento = c(
    39981629, 940412, 33065364, 1565656, 27136677, 
    193532179, 210512105, 6668285, 48221859, 762470982, 
    72255590, 153210868, 138427546, 29681236, 138181581, 
    34653074, 161938347, 1335509145, 4423625, 14534415, 
    73264, 558381447, 364623318, 3676467, 1493533548, 7141772
  ),
  code_state = c(12, 27, 13, 16, 29, 23, 32, 52, 21, 31, 50, 51, 15, 25, 26, 22, 41, 33, 24, 11, 14, 43, 42, 28, 35, 17)
)

# Realizar o left_join
brazil_2000_2010 <- left_join(brazil_states, investimento_estado_2000_2010, by = "code_state")

# cores
breaks <- c(0, 20000000, 50000000, 100000000, 200000000, 500000000, Inf)
colors <- c("#f7fbff", "#ccece6", "#66c2a4", "#2ca25f", "#006d2c", "#00441b")

# Plotar o mapa de calor
library(ggplot2)
ggplot() +
  geom_sf(data = brazil_2000_2010, aes(fill = cut(total_investimento, breaks = breaks)), color = "black") +
  scale_fill_manual(values = colors, name = "Total de Investimento",
                    labels = c("0-20M", "20M-50M", "50M-100M", "100M-200M", "200M-500M", ">500M"),
                    guide = guide_legend(title.position = "top", label.position = "bottom")) +
  labs(title = "Mapa de Calor: Total de Investimento por Estado no Brasil",
       subtitle = "BNDES (2000-2010)") +
  theme_void()


##########################

#2010- 2020


# Vamos filtrar os dados para o per°odo de 2010 a 2020
adm_publica_2010_2020 <- adm_publica_sem_hifen %>%
  filter(porano >= 2010 & porano <= 2020)

# Exemplo de c†lculo do total de investimento por estado (UF) para o per°odo de 2010 a 2020
total_investimento_por_uf_2010_2020 <- adm_publica_2010_2020 %>%
  group_by(UF) %>%
  summarise(total_investimento = sum(`Valor Desembolsado (em R$)`))

# Carregar os dados dos estados brasileiros
brazil_states <- read_state(year = 2020)


investimento_estado_2010_2020 <- data.frame(
  UF = c("AC", "AL", "AM", "AP", "BA", "CE", "ES", "GO", "MA", 
         "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", 
         "RN", "RO", "RS", "SC", "SE", "SP", "TO"),
  total_investimento = c(
    16387907, 495576, 65527604, 11266220, 85932123, 
    258286801, 142654595, 85983636, 49527966, 477687976, 
    35836295, 45825779, 122796143, 698000, 25114455, 
    11329706, 229319228, 4818452095, 1870200, 10265424, 
    498988807, 405602222, 3471800, 821738738, 10252756
  ),
  code_state = c(12, 27, 13, 16, 29, 23, 32, 52, 21, 31, 50, 51, 15, 25, 26, 22, 41, 33, 24, 11, 43, 42, 28, 35, 17)
)

# Realizar o left_join
brazil_2010_2020 <- left_join(brazil_states, investimento_estado_2010_2020, by = "code_state")

# Definir intervalos para cores
breaks <- c(0, 20000000, 50000000, 100000000, 200000000, 500000000, Inf)
colors <- c("#f7fbff", "#ccece6", "#66c2a4", "#2ca25f", "#006d2c", "#00441b")

#mapa de calor
library(ggplot2)
ggplot() +
  geom_sf(data = brazil_2010_2020, aes(fill = cut(total_investimento, breaks = breaks)), color = "black") +
  scale_fill_manual(values = colors, name = "Total de Investimento",
                    labels = c("0-20M", "20M-50M", "50M-100M", "100M-200M", "200M-500M", ">500M"),
                    guide = guide_legend(title.position = "top", label.position = "bottom")) +
  labs(title = "Mapa de Calor: Total de Investimento por Estado no Brasil",
       subtitle = "BNDES (2010-2020)") +
  theme_void()

###########################################



# principais tipos de operaá∆o por regi∆o



# Adicionando categorias baseadas no Programa (coluna 4)

adm_publica <- adm_publica %>%
  mutate(categoria = case_when(
    Programa %in% c("Educaá∆o e Capacitaá∆o Profissional") ~ "Educaá∆o e Tecnologia",
    Programa %in% c("Inovaá∆o e Desenvolvimento Tecnol¢gico") ~ "Educaá∆o e Tecnologia",
    Programa %in% c("Conservaá∆o Ambiental") ~ "Ambiental e Social",
    Programa %in% c("FINEM GENERICO") ~ "Desenvolvimento Econìmico e Infraestrutura",
    Programa %in% c("Infraestrutura Urbana e Rural") ~ "Desenvolvimento Econìmico e Infraestrutura",
    Programa %in% c("Projetos de Desenvolvimento Rural e Agr°cola") ~ "Desenvolvimento Econìmico e Infraestrutura",
    Programa %in% c("FUNDO SOCIOAMBIENTAL") ~ "Ambiental e Social",
    Programa %in% c("Impacto Clim†tico e Desenvolvimento Urbano") ~ "Ambiental e Social",
    Programa %in% c("Assistància Social e Cultural") ~ "Seguranáa e Sa£de",
    Programa %in% c("Projetos de Transporte e Mobilidade") ~ "Log°stica",
    Programa %in% c("BK BAIXO CARBONO") ~ "Log°stica",
    Programa %in% c("Seguranáa e Qualidade de Vida") ~ "Seguranáa e Sa£de",
    Programa %in% c("Projetos para Estados") ~ "Log°stica",
    Programa %in% c("OUTROS LOGISTICA") ~ "Log°stica",
    Programa %in% c("PROPAE") ~ "Log°stica",
    Programa %in% c("PROUCA") ~ "Log°stica",
    Programa %in% c("COMERCIO E SERVICOS") ~ "Log°stica",
    Programa %in% c("CONCESSAO RODOVIARIA") ~ "Log°stica",
    Programa %in% c("PRO ATENCAO BASICA") ~ "Log°stica",
    Programa %in% c("FOMENTO PROJ SOCIAIS") ~ "Log°stica",
    TRUE ~ "Outros"  # Caso n∆o se encaixe em nenhum dos anteriores
  ))



# Agrupando e contando as categorias
contagem_categorias <- table(adm_publica$categoria)

# Convertendo para dataframe
df_contagem <- as.data.frame(contagem_categorias)
names(df_contagem) <- c("categoria", "quantidade")

# Ordenando as categorias por quantidade decrescente
df_contagem <- df_contagem[order(df_contagem$quantidade, decreasing = TRUE), ]

# Calculando a porcentagem
df_contagem <- df_contagem %>%
  mutate(porcentagem = quantidade / sum(quantidade) * 100)

# Criando r¢tulos com a porcentagem
df_contagem <- df_contagem %>%
  mutate(categoria_label = paste0(categoria, " (", round(porcentagem, 1), "%)"))

# Cores
cores_pastel <- c("#86C5DA", "#FAC174", "#AED18B", "#ffe1e6", "#B47EBA")

# gr†fico de pizza 

ggplot(df_contagem, aes(x = "", y = quantidade, fill = categoria_label)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = cores_pastel) +  # Definindo cores pastÇis
  theme_void() +
  labs(title = "Distribuiá∆o das Categorias",
       fill = "Categoria") +
  theme(legend.position = "right")


###########################################################

# Total de Investimento por Categoria

# Exemplo de dados agrupados por categoria e somados

df_total_investimento <- adm_publica %>%
  group_by(categoria) %>%
  summarise(total_investimento = sum(`Valor da Operaá∆o Hist¢rico (em R$)`))

# Ordenando as categorias por total de investimento decrescente
df_total_investimento <- df_total_investimento[order(df_total_investimento$total_investimento, decreasing = TRUE), ]

cores_pastel <- c("#86C5DA", "#FAC174", "#AED18B", "#ffe1e6", "#B47EBA")

# gr†fico de pizza 
ggplot(df_total_investimento, aes(x = "", y = total_investimento, fill = categoria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = cores_pastel) +  # Definindo cores pastÇis
  theme_void() +
  labs(title = "Total de Investimento por Categoria",
       fill = "Categoria") +
  theme(legend.position = "right")

#####################################################################


# com porcentagem

# Exemplo de dados agrupados por categoria e somados

df_total_investimento <- adm_publica %>%
  group_by(categoria) %>%
  summarise(total_investimento = sum(`Valor da Operaá∆o Hist¢rico (em R$)`))

# Ordenando as categorias por total de investimento decrescente

df_total_investimento <- df_total_investimento[order(df_total_investimento$total_investimento, decreasing = TRUE), ]

# Calculando porcentagem
df_total_investimento <- df_total_investimento %>%
  mutate(porcentagem = total_investimento / sum(total_investimento) * 100)

# Cores
cores_pastel <- c("#86C5DA", "#FAC174", "#AED18B", "#ffe1e6", "#B47EBA")

#  gr†fico de pizza

ggplot(df_total_investimento, aes(x = "", y = total_investimento, fill = categoria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = cores_pastel) +  # Definindo cores pastÇis
  theme_void() +
  labs(title = "Total de Investimento por Categoria",
       fill = "Categoria") +
  theme(legend.position = "right") +
  geom_text(aes(label = paste0(round(porcentagem, 1), "%")), position = position_stack(vjust = 0.5))


#############################################################################################

# Mapa de Calor: Categoria Dominante por Estado no Brasil  : 1994 - 2000


# Vamos filtrar os dados para o per°odo de 1994 a 2000
adm_publica_ate_2000 <- adm_publica %>%
  filter(porano >= 1994 & porano <= 2000)


categoria_dominante_por_uf <- adm_publica_ate_2000 %>%
  group_by(UF) %>%
  summarise(categoria_dominante = names(which.max(table(categoria))))


# Dados de categorias dominantes por UF
dados_categorias_dominantes <- data.frame(
  UF = c("-", "AC", "AL", "AM", "BA", "CE", "DF", "ES", "GO", "MA", 
         "MG", "MS", "MT", "PA", "PE", "PI", "PR", "RJ", "RN", 
         "RO", "RS", "SC", "SE", "SP", "TO"),
  categoria_dominante = c(
    "Desenvolvimento Econìmico e Infraestrutura", "Desenvolvimento Econìmico e Infraestrutura", 
    "Ambiental e Social", "Educaá∆o e Tecnologia", "Educaá∆o e Tecnologia", 
    "Desenvolvimento Econìmico e Infraestrutura", "Educaá∆o e Tecnologia", 
    "Educaá∆o e Tecnologia", "Desenvolvimento Econìmico e Infraestrutura", 
    "Desenvolvimento Econìmico e Infraestrutura", "Educaá∆o e Tecnologia", 
    "Desenvolvimento Econìmico e Infraestrutura", "Desenvolvimento Econìmico e Infraestrutura", 
    "Educaá∆o e Tecnologia", "Desenvolvimento Econìmico e Infraestrutura", 
    "Educaá∆o e Tecnologia", "Desenvolvimento Econìmico e Infraestrutura", 
    "Educaá∆o e Tecnologia", "Desenvolvimento Econìmico e Infraestrutura", 
    "Desenvolvimento Econìmico e Infraestrutura", "Desenvolvimento Econìmico e Infraestrutura", 
    "Educaá∆o e Tecnologia", "Desenvolvimento Econìmico e Infraestrutura", 
    "Educaá∆o e Tecnologia", "Seguranáa e Sa£de"
  ),
  code_state = c(12, 27, 13, 16, 29, 23, 32, 52, 21, 31, 50, 51, 15, 25, 26, 22, 41, 33, 24, 11, 43, 42, 28, 35, 17)
)

# Carregar os dados dos estados brasileiros
brazil_states <- read_state(year = 2020)

# Realizar o left_join
brazil_2010_2020 <- left_join(brazil_states, dados_categorias_dominantes, by = "code_state")

# Definir cores £nicas para cada categoria
cores_categorias <- c(
  "Desenvolvimento Econìmico e Infraestrutura" = "#1f78b4",
  "Ambiental e Social" = "#33a02c",
  "Educaá∆o e Tecnologia" = "#e31a1c",
  "Seguranáa e Sa£de" = "#ff7f00"
)

# cores
breaks <- c(0, 20000000, 50000000, 100000000, 200000000, 500000000, Inf)
colors <- c("#f7fbff", "#ccece6", "#66c2a4", "#2ca25f", "#006d2c", "#00441b")

#  mapa de calor

ggplot() +
  geom_sf(data = brazil_2010_2020, aes(fill = categoria_dominante), color = "black") +
  scale_fill_manual(values = cores_categorias, name = "Categoria Dominante",
                    guide = guide_legend(title.position = "top", label.position = "bottom")) +
  labs(title = "Mapa de Calor: Categoria Dominante por Estado no Brasil",
       subtitle = "BNDES (1994-2000)") +
  theme_void()

##############################################


# anos de 2000- 2010


# Vamos filtrar os dados para o per°odo de 1994 a 2000
adm_publica_ate_2010<- adm_publica %>%
  filter(porano >= 2000 & porano <= 2010)

# Exemplo de c†lculo da categoria dominante por UF para o per°odo de 1994 a 2000
categoria_dominante_por_uf2 <- adm_publica_ate_2010 %>%
  group_by(UF) %>%
  summarise(categoria_dominante = names(which.max(table(categoria))))


dados_categorias_dominantes <- data.frame(
  UF = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", 
         "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", 
         "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
  categoria_dominante = c(
    "Ambiental e Social", "Ambiental e Social", 
    "Educaá∆o e Tecnologia", "Desenvolvimento Econìmico e Infraestrutura", 
    "Educaá∆o e Tecnologia", "Educaá∆o e Tecnologia", "Desenvolvimento Econìmico e Infraestrutura", 
    "Educaá∆o e Tecnologia", "Educaá∆o e Tecnologia", "Educaá∆o e Tecnologia", 
    "Educaá∆o e Tecnologia", "Educaá∆o e Tecnologia", "Educaá∆o e Tecnologia", 
    "Educaá∆o e Tecnologia", "Educaá∆o e Tecnologia", "Educaá∆o e Tecnologia", 
    "Log°stica", "Educaá∆o e Tecnologia", "Educaá∆o e Tecnologia", 
    "Ambiental e Social", "Educaá∆o e Tecnologia", "Ambiental e Social", 
    "Educaá∆o e Tecnologia", "Educaá∆o e Tecnologia", "Educaá∆o e Tecnologia", 
    "Educaá∆o e Tecnologia", "Educaá∆o e Tecnologia"
  ),
  code_state = c(12, 27, 13, 16, 29, 23, 53, 32, 52, 21, 31, 50, 51, 15, 25, 26, 22, 41, 33, 24, 11, 14, 43, 42, 28, 35, 17)
)

# Carregar os dados dos estados brasileiros
brazil_states <- read_state(year = 2020)

# Realizar o left_join
brazil_2000_2010 <- left_join(brazil_states, dados_categorias_dominantes, by = c("code_state" = "code_state"))

# Definir cores £nicas para cada categoria
cores_categorias <- c(
  "Desenvolvimento Econìmico e Infraestrutura" = "#1f78b4",
  "Ambiental e Social" = "#33a02c",
  "Educaá∆o e Tecnologia" = "#e31a1c",
  "Log°stica" = "#ff7f00"
)

# Plotar o mapa de calor
ggplot() +
  geom_sf(data = brazil_2000_2010, aes(fill = categoria_dominante), color = "black") +
  scale_fill_manual(values = cores_categorias, name = "Categoria Dominante",
                    guide = guide_legend(title.position = "top", label.position = "bottom")) +
  labs(title = "Mapa de Calor: Categoria Dominante por Estado no Brasil",
       subtitle = "BNDES (2000-2010)") +
  theme_void()

#####################################

#2010- 2020


# Supondo que o dataframe adm_publica j† est† carregado e contÇm os dados necess†rios
# Vamos filtrar os dados para o per°odo de 1994 a 2000

adm_publica_ate_2020<- adm_publica %>%
  filter(porano >= 2010 & porano <= 2020)

# Exemplo de c†lculo da categoria dominante por UF para o per°odo de 1994 a 2000

categoria_dominante_por_uf3 <- adm_publica_ate_2020 %>%
  group_by(UF) %>%
  summarise(categoria_dominante = names(which.max(table(categoria))))




dados_categorias_dominantes <- data.frame(
  UF = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", 
         "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", 
         "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
  categoria_dominante = c(
    "Ambiental e Social", "Seguranáa e Sa£de", 
    "Ambiental e Social", "Log°stica", 
    "Educaá∆o e Tecnologia", "Educaá∆o e Tecnologia", 
    "Log°stica", "Educaá∆o e Tecnologia", 
    "Educaá∆o e Tecnologia", "Log°stica", 
    "Educaá∆o e Tecnologia", "Educaá∆o e Tecnologia", 
    "Educaá∆o e Tecnologia", "Ambiental e Social", 
    "Ambiental e Social", "Log°stica", 
    "Seguranáa e Sa£de", "Educaá∆o e Tecnologia", 
    "Desenvolvimento Econìmico e Infraestrutura", 
    "Educaá∆o e Tecnologia", "Educaá∆o e Tecnologia", 
    "Log°stica", "Educaá∆o e Tecnologia", 
    "Educaá∆o e Tecnologia", "Educaá∆o e Tecnologia", 
    "Educaá∆o e Tecnologia", "Educaá∆o e Tecnologia"
  ),
  code_state = c(12, 27, 13, 16, 29, 23, 53, 32, 52, 21, 31, 50, 51, 15, 25, 26, 22, 41, 33, 24, 11, 14, 43, 42, 28, 35, 17)
)

# Carregar os dados dos estados brasileiros
brazil_states <- read_state(year = 2020)

# Realizar o left_join
brazil_2010_2020 <- left_join(brazil_states, dados_categorias_dominantes, by = c("code_state" = "code_state"))

# Definir cores £nicas para cada categoria
cores_categorias <- c(
  "Desenvolvimento Econìmico e Infraestrutura" = "#1f78b4",
  "Ambiental e Social" = "#33a02c",
  "Educaá∆o e Tecnologia" = "#e31a1c",
  "Log°stica" = "#ff7f00",
  "Seguranáa e Sa£de" = "#6a3d9a"
)

# mapa de calor

ggplot() +
  geom_sf(data = brazil_2010_2020, aes(fill = categoria_dominante), color = "black") +
  scale_fill_manual(values = cores_categorias, name = "Categoria Dominante",
                    guide = guide_legend(title.position = "top", label.position = "bottom")) +
  labs(title = "Mapa de Calor: Categoria Dominante por Estado no Brasil",
       subtitle = "BNDES (2010-2020)") +
  theme_void()




####################################################################


#### top 5 estados que mais recebem dinheiro em comparaá∆o com top 5 que mais tem projetos


# Calcular os top 5 estados que mais recebem dinheiro
top5_valor <- adm_publica %>%
  group_by(UF) %>%
  summarise(total_valor = sum(`Valor da Operaá∆o Hist¢rico (em R$)`, na.rm = TRUE)) %>%
  arrange(desc(total_valor)) %>%
  slice(1:5)

# Exibir os resultados
print(top5_valor)


projetos_contratados <- adm_publica[adm_publica[, 7] == "CONTRATADA", ]

# Contar projetos contratados por estado
relacao_estado_projetos <- projetos_contratados %>%
  group_by(UF) %>%
  summarise(total_projetos_contratados = n())

# Exibir a relaá∆o de projetos contratados por estado
print(relacao_estado_projetos)

# Calcular os top 5 estados que mais tàm projetos contratados
top5_projetos <- relacao_estado_projetos %>%
  arrange(desc(total_projetos_contratados)) %>%
  slice(1:5)

# Exibir os top 5 estados
print(top5_projetos)


# Dados dos top 5 estados por valor total
top_valor <- data.frame(
  UF = c("RJ", "SP", "SC", "CE", "MA"),
  total_valor = c(14852449334, 11929129142, 6213680059, 5708535506, 5007043191)
)

# Dados dos top 5 estados por n£mero total de projetos contratados
top_projetos <- data.frame(
  UF = c("RS", "MG", "SC", "SP", "PR"),
  total_projetos_contratados = c(1195, 716, 516, 452, 376)
)

cores_azul <- c("#1f77b4", "#4292c6","#6baed6", "#9ecae1",  "#c6dbef")


# Gr†fico 1: Top 5 estados por valor total em tons de azul
p1 <- ggplot(top_valor, aes(x = UF, y = total_valor, fill = UF)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores_azul) +
  labs(x = "Estado (UF)", y = "Valor Total (R$)", title = "Top 5 Estados por Valor Total de Projetos Contratados") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p1


paleta_laranja <- c("#ff7f0e", "#fdbf6f", "#fdae61", "#fd8d3c", "#e6550d")

# Gr†fico 2: Top 5 estados por n£mero total de projetos contratados em tons de laranja
p2 <- ggplot(top_projetos, aes(x = UF, y = total_projetos_contratados, fill = UF)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = paleta_laranja) +
  labs(x = "Estado (UF)", y = "N£mero de Projetos Contratados", title = "Top 5 Estados por N£mero Total de Projetos Contratados") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p2



###################################


# Quais s∆o os setores (ex.: energia renov†vel, saneamento, reflorestamento) que mais recebem investimentos com impacto socioambiental?


# Filtrar os dados entre 2010 e 2020
dados_filtrados <- adm_publica %>%
  filter(porano >= 2010 & porano <= 2020)

# Calcular o total de investimento por categoria e ano
investimento_por_categoria_ano <- dados_filtrados %>%
  group_by(categoria, porano) %>%
  summarise(total_investimento = sum(`Valor da Operaá∆o Hist¢rico (em R$)`, na.rm = TRUE))

# Verificar se a agregaá∆o funcionou corretamente
print(investimento_por_categoria_ano)


# resultados

ggplot(investimento_por_categoria_ano, aes(x = porano, y = total_investimento, color = categoria, group = categoria)) +
  geom_line(linewidth = 1.0) +
  labs(title = "Investimento Total por Categoria (2010-2020)",
       x = "Ano",
       y = "Investimento Total (R$)",
       color = "Categoria") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_continuous(breaks = seq(min(investimento_por_categoria_ano$porano), max(investimento_por_categoria_ano$porano), by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#######################################

# entre regi‰es 

#Como os investimentos do BNDES foram distribu°dos entre as diferentes regi‰es do Brasil (Norte, Nordeste, Centro-Oeste, Sudeste, Sul)?


# Adicionar a coluna Regiao baseada nos estados
adm_publica <- adm_publica %>%
  mutate(
    Regiao = case_when(
      UF %in% c("SP", "ES", "RJ", "MG") ~ "SUDESTE",
      UF %in% c("BA", "SE", "AL", "PE", "PB", "RN", "CE", "MA", "PI") ~ "NORDESTE",
      UF %in% c("AM", "RR", "AP", "PA", "TO", "RO", "AC") ~ "NORTE",
      UF %in% c("PR", "SC", "RS") ~ "SUL",
      UF %in% c("DF", "GO", "MS", "MT") ~ "CENTRO-OESTE",
      TRUE ~ "OUTRAS"
    )
  )


# Calcular a distribuiá∆o dos investimentos por regi∆o

distribuicao_investimentos <- adm_publica %>%
  group_by(Regiao) %>%
  summarise(Total_Investimento = sum(`Valor da Operaá∆o Hist¢rico (em R$)`, na.rm = TRUE)) %>%
  arrange(desc(Total_Investimento))


print(distribuicao_investimentos)

#  dados de distribuiá∆o de investimentos

distribuicao_investimentos <- data.frame(
  Regiao = c("SUDESTE", "NORDESTE", "NORTE", "CENTRO-OESTE", "SUL"),
  Total_Investimento = c(63900343534, 28715319359, 13086861974, 11602673966, 11229988023)
)



#  dados de distribuiá∆o de investimentos
distribuicao_investimentos <- data.frame(
  Regiao = c("SUDESTE", "NORDESTE", "NORTE", "CENTRO-OESTE", "SUL"),
  Total_Investimento = c(63900343534, 28715319359, 13086861974, 11602673966, 11229988023)
)

# cores em tons de azul

paleta_azul <- c("SUDESTE" = "#1f77b4", 
                 "NORDESTE" = "#4292c6", 
                 "NORTE" = "#6baed6", 
                 "CENTRO-OESTE" = "#9ecae1", 
                 "SUL" = "#c6dbef")

# gr†fico de barras

grafico <- ggplot(distribuicao_investimentos, aes(x = Regiao, y = Total_Investimento, fill = Regiao)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Distribuiá∆o dos Investimentos do BNDES por Regi∆o",
    x = NULL,
    y = "Total do Investimento (em R$)",
    fill = "Regi∆o"
  ) +
  scale_fill_manual(values = paleta_azul) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Ajustar o Éngulo dos r¢tulos do eixo x

print(grafico)



##################################################################


################

# Contar o n£mero de projetos por categoria

count_categoria <- adm_publica %>%
  group_by(categoria) %>%
  summarise(count = n())

# Calcular porcentagens 
count_categoria <- count_categoria %>%
  mutate(percentage = count / sum(count) * 100)

# Definir cores personalizadas mais claras
custom_colors <- c("#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6")

# gr†fico de pizza
ggplot(count_categoria, aes(x = "", y = count, fill = categoria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "N£mero de Projetos por Categoria",
       fill = "Categoria") +
  scale_fill_manual(values = custom_colors)



# Filtrando a base de dados para projetos contratados


projetos_contratados <- adm_publica[adm_publica[, 7] == "CONTRATADA", ]

# Agrupando e contando as categorias
contagem_categorias <- table(projetos_contratados$categoria)

# Convertendo para dataframe
df_contagem <- as.data.frame(contagem_categorias)
names(df_contagem) <- c("categoria", "quantidade")

# Ordenando as categorias por quantidade decrescente
df_contagem <- df_contagem[order(df_contagem$quantidade, decreasing = TRUE), ]

# Calculando a porcentagem
df_contagem <- df_contagem %>%
  mutate(porcentagem = quantidade / sum(quantidade) * 100)

# Criando r¢tulos com a porcentagem
df_contagem <- df_contagem %>%
  mutate(categoria_label = paste0(categoria, " (", round(porcentagem, 1), "%)"))

# Cores 
cores_pastel <- c("#86C5DA", "#FAC174", "#AED18B", "#ffe1e6", "#B47EBA")

# gr†fico de pizza 

ggplot(df_contagem, aes(x = "", y = quantidade, fill = categoria_label)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = cores_pastel) +  # Definindo cores pastÇis
  theme_void() +
  labs(title = "Distribuiá∆o das Categorias de Projetos Contratados",
       fill = "Categoria") +
  theme(legend.position = "right")




#######################################################

