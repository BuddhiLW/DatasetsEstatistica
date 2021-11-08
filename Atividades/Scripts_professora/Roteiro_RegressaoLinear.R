
######################## ANÁLISE DE REGRESSÃO LINEAR #########################

# Instalação dos pacotes necessarios para este tópico
packages <- c("ggcorrplot","plotly", "ggplot2", "caret", "leaps", "MASS")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

# Carregamento dos pacotes que serão usados
library("ggcorrplot") #Visualization of a Correlation Matrix using 'ggplot2'
library("plotly")     #Create Interactive Web Graphics via 'plotly.js'
library("ggplot2")    #Create Elegant Data Visualisations Using the Grammar of Graphics
library("caret")      #Classification and Regression Training (roteiro crimes de odio)
library("leaps")      #Regression Subset Selection (roteiro crimes de odio)
library("MASS")       #Support Functions and Datasets for Venables and Ripley's MASS (roteiro crimes de odio)


###############################################################################
######################## ROTEIRO: HORAS DE TREINAMENTO ########################
###############################################################################
# Exercício Análise de Regressão simples:
# Para uma amostra de oito operadores de máquina, foram coletados o número de 
# horas de treinamento (x) e o tempo necessário para completar o trabalho (y). 
# Os dados coletados estão abaixo:
###############################################################################

# Passo 01: Criação dos vetores x e y, construindo o conjunto de dados a partir
# da digitação dos valores:
y <- c(5.2,5.1,4.9,4.6,4.7,4.8,4.6,4.9)
x <- c(13,15,18,20,19,17,21,16)

# Passo 02: A análise será iniciada com o cálculo de algumas medidas-resumo 
# utilizando o comando summary(). 
summary(x) 
summary(y)

# Passo 03: O cálculo do Coeficiente de Correlação é obtido pela função cor()
cor(x,y)

# Passo 04: Para analisarmos os dados de forma visual, podemos construir gráficos.
# Para nomear o eixo x usa-se o comando xlab="Nome do eixo x", para nomear o eixo 
# y, coloca-se ylab="Nome do eixo y" e para dar título coloca-se main="Nome do 
# título". Esses comandos devem ser inseridos junto ao plot, separados por vírgula:
plot(x,y)
plot(x,y,xlab="Horas de Treinamento", ylab="Tempo da Tarefa",main="Diagrama de Dispersão")

# Passo 05: O modelo de regressão linear é calculado com o comando lm(). 
# Armazenaremos os resultados do modelo no objeto "modreg_trein".
modreg_trein <- lm(y ~ x)
summary(modreg_trein)

# Passo 06: Para traçar a reta de regressão juntamente com o gráfico de 
# dispersão deve-se usar o comando abline()
abline(modreg_trein)

# Passo 07: Após a obtenção do modelo final, precisamos verificar se as
# hipóteses do modelo são satisfeitas.

# Normalidade de resíduos: pelo teste de Shapiro-Wilk, com o comando shapiro.test()
shapiro.test(modreg_trein$residuals)
# Se p-value > 0.5, não rejeitamos H0 => H0: resíduo distribuidos normalmente.

# Aleatoriedade dos dados (analisado graficamente)
plot(modreg_trein$fitted.values,modreg_trein$residuals)

# Passo 08:  Interpretação dos resultados (feito a partir dos seus conhecimentos sobre o tema)

# Remover todas as variáveis criadas no R antes de iniciar nosso próximo
# exercício.  
rm(list=ls())



###############################################################################
###################### ROTEIRO: ACABAMENTO DA SUPERFICIE ######################
###############################################################################
## Exercício Análise de Regressão Multipla (Ex 12.13 Montgomery & Runger):
##
## Um engenheiro mecânico está investigando o acabamento na superfície de 
## partes metálicas, produzidas em um torno mecânico, e sua relação com 
## a velocidade (em revoluções por minuto) do torno. 
###############################################################################

# Passo 01: Importação do arquivo de dados no R. 

# ATENCAO: Definir o diretório de trabalho antes de ler o arquivo
# pode ser feito pelos menus da aplicação ou pelo comando setwd("<path>")

# O conjunto de dados será armazenado em um objeto chamado "dados_torno". 
# Comando "header=TRUE" indica que o conjunto de dados tem cabeçalho.
dadtorno <- read.table("Exemplo02_Torno.txt", header = TRUE)

# Para verificar se os dados foram importados de forma correta, pode-se exibir 
# as primeiras linhas do objeto "dados_torno" com o comando head()
head(dadtorno)
names(dadtorno)

# Passo 02: Vamos indicar que a variável Tipo_Ferram é uma variável 
# qualitativa com o comando factor()
dadtorno$TipoFerram_q <- factor(dadtorno$Tipo_Ferram) 
head(dadtorno)

# Passo 03: A análise será iniciada com o cálculo de algumas medidas-resumo 
# utilizando o comando summary(). 
summary(dadtorno$Acab_Superf) 
summary(dadtorno$RPM)
summary(dadtorno$TipoFerram_q)
# ou
summary(dadtorno)

# Passo 04: O cálculo do Coeficiente de Correlação é obtido pela função cor()
cor(dadtorno[,2:3])

# Passo 05: Para analisarmos os dados de forma visual, podemos construir gráficos.
# Há diversas funções no R para construção de gráficos. Abaixo serão apresentadas
# duas opções.

# Opção 01 (construção de gráfico iterativo com o comando plot_ly)
plot_ly(dadtorno, x = ~RPM, y = ~Acab_Superf, z=~TipoFerram_q)

# Opção 02 (construção de gráfico com o comando ggplot)
ggplot(dadtorno, aes(y = Acab_Superf, x = RPM)) +
  geom_boxplot()  +
  geom_jitter(alpha = 0.35, width = 0.1, size=3) +
  facet_wrap(~TipoFerram_q)

# Passo 06: O modelo de regressão linear é calculado com o comando lm(). 
# Armazenaremos os resultados do modelo no objeto "modreg_acab".
modreg_acab <- lm(Acab_Superf ~ RPM + TipoFerram_q + RPM*TipoFerram_q, data = dadtorno)
summary(modreg_acab)

# Passo 07: Pelo resultado do modelo, observamos que não há interação entre as 
# três variáveis. O modelo final é obtido eliminando os termos não significantes 
# do modelo, um a um.
modreg_acab <- lm(Acab_Superf ~ RPM + TipoFerram_q + RPM*TipoFerram_q, data = dadtorno)
summary(modreg_acab)

# Resultado: Modelo final obtido, após a eliminação das variáveis não 
# significativas, uma a uma.
modreg_acab2 <- lm(Acab_Superf ~ RPM + TipoFerram_q, data = dadtorno)
summary(modreg_acab2)

# Passo 08 Após a obtenção do modelo final, precisamos verificar se as
# hipóteses do modelo são satisfeitas.

# Normalidade de resíduos: pelo teste de Shapiro-Wilk, com o comando shapiro.test()
shapiro.test(modreg_acab2$residuals) 

# Aleatoriedade dos dados (analisado graficamente)
plot(modreg_acab2$fitted.values,modreg_acab2$residuals)

# Passo 09:  Interpretação dos resultados (feito a partir dos seus conhecimentos sobre o tema)

# Remover todas as variáveis criadas no R antes de iniciar nosso próximo
# exercício.  
rm(list=ls())



###############################################################################
######################## ROTEIRO: CRIMES DE ÓDIO ##############################
###############################################################################
## Variáveis:
# X1: Estado (não entrará no modelo);
# X2: Mediana Renda familiar (2016);
# X3: % população desempregada (2016);
# X4: % população áreas metropolitanas (2015);
# X5: % população >= 25 anos com diploma ensino médio (2015);
# X6: % imigrantes;
# X7: % população brancos que vivem na pobreza (2015)
# X8: % população não-brancos (2015);
# X9: % votantes em Trump em 2016;
# X10: Média anual de crimes de ódio/100.000 hab (2010-2015);
# X11: Crimes de ódio/100.000 hab (2016) (VARIÁVEL RESPOSTA)
###############################################################################

# Passo 01: Importação do arquivo de dados no R. 

# ATENCAO: Definir o diretório de trabalho antes de ler o arquivo
# pode ser feito pelos menus da aplicação ou pelo comando setwd("<path>")

# O conjunto de dados será armazenado em um objeto chamado "dados_torno". 
# Comando "header=TRUE" indica que o conjunto de dados tem cabeçalho.
dadcrimes <- read.table("crimes_odio.txt", header = TRUE)

# Para verificar se os dados foram importados de forma correta, pode-se exibir 
# as primeiras linhas do objeto "dados_torno" com o comando head()
head(dadcrimes)
names(dadcrimes)

# Não há variáveis qualitativas, apenas quantitativas

# Passo 02: A análise será iniciada com o cálculo de algumas medidas-resumo 
# utilizando o comando summary(). 
summary(dadcrimes)
summary(dadcrimes[,2:11]) 

# Passo 03: O cálculo do Coeficiente de Correlação é obtido pela função cor()
corr <- cor(dadcrimes[,2:11])
corr

# Construção do gráfico de correlação
ggcorrplot(corr, method = "circle", lab = TRUE)
# ou
ggcorrplot(corr, lab = TRUE)

# Passo 04: O modelo de regressão linear é calculado com o comando lm(). 
# Armazenaremos os resultados do modelo no objeto "modreg_acab".
xnam <- paste("X", 2:10, sep="")
vexplicativas <- as.formula(paste("X11 ~ ", paste(xnam, collapse= "+")))
modcrimes <- lm(vexplicativas, data = dadcrimes)
summary(modcrimes)

# Passo 05: Pelo resultado do modelo, observamos que há algumas variáveis não
# significantes no modelo inicial. Podemos eliminar os termos não significantes 
# manualmente, um a um. Outra opção é usar o comando "Stepwise" para esta
# seleção.
# No Stepwise, o comando "trace = TRUE" apresenta todos os passos de  
# eliminação de variáveis enquanto "trace=FALSE" não apresenta os passos desta
# eliminação.
modcrimes_step <- stepAIC(modcrimes, direction = "both", trace = TRUE)
summary(modcrimes_step)

# Passo 06: Após a obtenção do modelo final, precisamos verificar se as
# hipóteses do modelo são satisfeitas.

# Normalidade de resíduos: pelo teste de Shapiro-Wilk, com o comando shapiro.test()
shapiro.test(modcrimes_step$residuals) 

# Aleatoriedade dos dados (analisado graficamente)
plot(modcrimes_step$fitted.values,modcrimes_step$residuals)

# Passo 07:  Interpretação dos resultados (feito a partir dos seus conhecimentos sobre o tema)

###############################################################################
# Gráfico Crimes de ódio vs variáveis significantes
# Variáveis:
# X2: Mediana Renda familiar (2016);
# X5: % população >= 25 anos com diploma ensino médio (2015);
# X9: % votantes em Trump em 2016;
# X10: Média anual de crimes de ódio/100.000 hab (2010-2015);
# X11: Crimes de ódio/100.000 hab (2016) (VARIÁVEL RESPOSTA)
###############################################################################

# Construção de gráfico iterativo com o comando plot_ly
plot_ly(dadcrimes, x = ~X2, y = ~X11, z=~X5, color=~X9)

###############################################################################
# Complemento - Análise do efeito da observação discrepante no modelo:
newdadcrimes <- dadcrimes[-9,]
newdadcrimes

xnam <- paste("X", 2:10, sep="")
vexplicativas <- as.formula(paste("X11 ~ ", paste(xnam, collapse= "+")))
modcrimes_new <- lm(vexplicativas, data = newdadcrimes)
summary(modcrimes_new)
modcrimes_stepnew <- stepAIC(modcrimes_new, direction = "both", trace = TRUE)
summary(modcrimes_stepnew)
# X5: % população >= 25 anos com diploma ensino médio (2015);
# X7: % população brancos que vivem na pobreza (2015)
# X9: % votantes em Trump em 2016;
# X10: Média anual de crimes de ódio/100.000 hab (2010-2015);
# X11: Crimes de ódio/100.000 hab (2016) (VARIÁVEL RESPOSTA)

# Construção de gráfico iterativo com o comando plot_ly
plot_ly(newdadcrimes, x = ~X7, y = ~X11, z=~X5, color=~X9)

# Comparação entre as estimativas de ambos os modelos.
plot(modcrimes$fitted.values[-9], modcrimes_new$fitted.values,pch=16)
abline(0,1)

# Remover todas as variáveis criadas no R antes de iniciar nosso próximo
# exercício.  
rm(list=ls())
