

##### Lê os dados do arquivo da RAIS de 2015 para o Amazonas

library(readr)

AM2015 <- NULL

AM2015 <- read_delim("C:/Base_Rais/2015/AM2015.txt", 
                     ";", escape_double = FALSE, trim_ws = TRUE)


### Reduz para o número de atributos que serão avaliados

AM2015Redu <- NULL

AM2015Redu <- AM2015[  AM2015$'Causa Afastamento 1' != 99
                       | AM2015$'Causa Afastamento 2' != 99
                       | AM2015$'Causa Afastamento 3' != 99
                       | AM2015$'Motivo Desligamento' != 0
                       ,c( 04, 05, 06, 07, 08, 13, 17, 18, 31, 38, 39 )
                       ]

## Cria dataframe

df <- as.data.frame.data.frame(AM2015Redu)

## verifica a estrutura atual

str(df)

# cria os campos tipo factor

#sexo

df$sexo <- NA

df$sexo <- cut(as.numeric(df$`Sexo Trabalhador`), breaks = c(0,1,2), labels=c("masc.","fem."))

df$`Sexo Trabalhador` <- NULL

#Raça e Cor

df$raca <- NA

df$raca <- cut(as.numeric(df$`Raca Cor`), breaks = c(0,1,2, 4, 6, 8, 9), labels=c("Indígena","Branca","Preta","Amarela","Parda","Não Ident."))

df$`Raca Cor` <- NULL

# Faixa Etária

df$Idade <- NA

df$Idade <- cut(as.numeric(df$`Faixa Etaria`), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8), labels=c("10 a 14","15 a 17", "18 a 24", "25 a 29", "30 a 39","40 a 49", "50 a 64" ,"65 ou +" ))

df$`Faixa Etaria` <- NULL

# Escolaridade

df$Escolar <- NA

df$Escolar<- cut(as.numeric(df$`Escolaridade apos 2005`), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ), labels=c("Analfabeto", "Até 5ª Inc", "5ª Fund.", "6ª a 9ª Fund.", "Fund. Compl", "Médio Incompl.", "Médio Compl", "Super.Incompl.","Super. Compl", "Mestrado" , "Doutorado"))

df$`Escolaridade apos 2005` <- NULL


# Numero Funcionarios

df$NumFunc <- NA

df$NumFunc<- cut(as.numeric(df$`Tamanho Estabelecimento`), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ), labels=c("Até 4", "5 a 9", "10 a 19", "20 a 49", "50 a 99", "100 a 249", "250 a 499", "500 a 999", "1000 ou +"))

df$`Tamanho Estabelecimento`<- NULL


# Desligamento 

df$MotivDesliga <- NA

df$MotivDesliga<- cut((df$`Motivo Desligamento`), breaks = c(0, 10, 11, 12, 20, 21, 22, 30, 31, 32 , 33, 40, 50, 60, 62, 63, 64, 70, 71, 72, 73, 74, 75, 76, 78,79,  80)
                      , labels=c("Dem.com JC", "Dem.sem JC", "Term. Contr.", "Desl.com JC", "Desl.sem JC"
                                 , "Poss out Car", "Trans c/ Onus","Trans s/ Onus", "Read/Redis", "Cessão"
                                 , "Mud.Regime","Reforma","Falecimento", "Falec. Ac Trab", "Falec. Ac Tip"
                                 , "Falec. D Prof.", "Apos. Ts Cres", "Apos. Ts Sres", "Apos. Id Cres"
                                 , "Apos. in Acid", "Apos. in Doen","Apos. Compuls", "Apos. in Outr"
                                 , "Apos. Id Sres", "Apos. Esp Cre", "Apos. Esp Sre"))

df$`Motivo Desligamento`<- NULL


## tira os afastamentos 2 e 3 temporariamente

df$`Causa Afastamento 2`<- NULL
df$`Causa Afastamento 3`<- NULL

# tira o tempo de emprego temporariamente

df$TempoEmpr <- NA

df$TempoEmpr<- cut(as.numeric(df$`Faixa Tempo Emprego`), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8), labels=c("01", "02", "03", "04", "05", "06", "07", "08"))

df$`Faixa Tempo Emprego` <- NULL

# ajusta o afastamento

df$Afastamento <- NA

df$Afastamento<- cut(df$`Causa Afastamento 1`, breaks = c(0, 10, 20,30, 40, 50, 60, 70, 99)
                     , labels=c("Acid.Trb.Tip.", "Acid.Trb.Traj.","Doenc.Relac.Trab", "Doenc.Não.Rel.Trab","Licença Mater.", "Serviço Milit.", "Licença Sem Venc.", "Sem Afasta." ))

df$`Causa Afastamento 1`<- NULL

# CBO - nível 3

df$CBO<-NA

#x<- df$`CBO Ocupacao 2002`


x <- substr(as.character(df$`CBO Ocupacao 2002`),1,3)

df$CBO<-as.ordered(x)

df$CBO<-as.factor(x)
#

# df$CBO<-as.numeric(x)


df$`CBO Ocupacao 2002` <- NULL

## Verifica a nova estrutura com os campos tipo factor

str(df)

## sumário das informações

summary(df)

############## inicia o trabalho com os afastamentos  ############


AM2015Afast<- AM2015[  AM2015$'Causa Afastamento 1' != 99
              ,c( 04, 08, 13, 17, 18, 31, 38, 39 )
            ]



df <- as.data.frame.data.frame(AM2015Afast)

# cria os campos tipo factor

#sexo

df$sexo <- NA

df$sexo <- cut(as.numeric(df$`Sexo Trabalhador`), breaks = c(0,1,2), labels=c("masc.","fem."))

df$`Sexo Trabalhador` <- NULL

#Raça e Cor

df$raca <- NA

df$raca <- cut(as.numeric(df$`Raca Cor`), breaks = c(0,1,2, 4, 6, 8, 9), labels=c("Indígena","Branca","Preta","Amarela","Parda","Não Ident."))

df$`Raca Cor` <- NULL

# Faixa Etária

df$Idade <- NA

df$Idade <- cut(as.numeric(df$`Faixa Etaria`), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8), labels=c("10 a 14","15 a 17", "18 a 24", "25 a 29", "30 a 39","40 a 49", "50 a 64" ,"65 ou +" ))

df$`Faixa Etaria` <- NULL

# Escolaridade

df$Escolar <- NA

df$Escolar<- cut(as.numeric(df$`Escolaridade apos 2005`), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ), labels=c("Analfabeto", "Até 5ª Inc", "5ª Fund.", "6ª a 9ª Fund.", "Fund. Compl", "Médio Incompl.", "Médio Compl", "Super.Incompl.","Super. Compl", "Mestrado" , "Doutorado"))

df$`Escolaridade apos 2005` <- NULL


# Numero Funcionarios

df$NumFunc <- NA

df$NumFunc<- cut(as.numeric(df$`Tamanho Estabelecimento`), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ), labels=c("Até 4", "5 a 9", "10 a 19", "20 a 49", "50 a 99", "100 a 249", "250 a 499", "500 a 999", "1000 ou +"))

df$`Tamanho Estabelecimento`<- NULL



## tira os afastamentos 2 e 3 temporariamente

df$`Causa Afastamento 2`<- NULL
df$`Causa Afastamento 3`<- NULL

# tira o tempo de emprego temporariamente

df$TempoEmpr <- NA

df$TempoEmpr<- cut(as.numeric(df$`Faixa Tempo Emprego`), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8), labels=c("01", "02", "03", "04", "05", "06", "07", "08"))

df$`Faixa Tempo Emprego` <- NULL

# ajusta o afastamento

df$Afastamento <- NA

df$Afastamento<- cut(df$`Causa Afastamento 1`, breaks = c(0, 10, 20,30, 40, 50, 60, 70, 99)
, labels=c("Acid.Trb.Tip.", "Acid.Trb.Traj.","Doenc.Relac.Trab", "Doenc.Não.Rel.Trab","Licença Mater.", "Serviço Milit.", "Licença Sem Venc.", "Sem Afasta." ))

df$`Causa Afastamento 1`<- NULL

# CBO - nível 3

df$CBO<-NA

#x<- df$`CBO Ocupacao 2002`

x <- substr(as.character(df$`CBO Ocupacao 2002`),1,3)

df$CBO<-as.ordered(x)

df$CBO<-as.factor(x)
#

df$`CBO Ocupacao 2002` <- NULL


## Exibe summario


str(df)

## sumário das informações

summary(df)

AM2015Afast2 <- AM2015[AM2015$'Causa Afastamento 2' != 99
                       , c(04, 05, 06, 07, 08, 13, 17, 18, 31, 38, 39 )]

# 13.458 observaÃ§Ãµes

## pega os registros de terceiro afastamento

AM2015Afast3 <- AM2015[AM2015$'Causa Afastamento 3' != 99
                       , c(04, 05, 06, 07, 08, 13, 17, 18, 31, 38, 39 )]

# 7.299 observaÃ§Ãµes

############## inicia o trabalho com os DESLIGAMENTOS  ############

AM2015Desliga <- AM2015[  AM2015$'Motivo Desligamento' != 0
                          ,c( 07, 08, 13, 17, 18, 31, 38, 39 )
                          ]

df <- as.data.frame.data.frame(AM2015Desliga)



#############################################

# cria os campos tipo factor

#sexo

df$sexo <- NA

df$sexo <- cut(as.numeric(df$`Sexo Trabalhador`), breaks = c(0,1,2), labels=c("masc.","fem."))

df$`Sexo Trabalhador` <- NULL

#Raça e Cor

df$raca <- NA

df$raca <- cut(as.numeric(df$`Raca Cor`), breaks = c(0,1,2, 4, 6, 8, 9), labels=c("Indígena","Branca","Preta","Amarela","Parda","Não Ident."))

df$`Raca Cor` <- NULL

# Faixa Etária

df$Idade <- NA

df$Idade <- cut(as.numeric(df$`Faixa Etaria`), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8), labels=c("10 a 14","15 a 17", "18 a 24", "25 a 29", "30 a 39","40 a 49", "50 a 64" ,"65 ou +" ))

df$`Faixa Etaria` <- NULL

# Escolaridade

df$Escolar <- NA

df$Escolar<- cut(as.numeric(df$`Escolaridade apos 2005`), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ), labels=c("Analfabeto", "Até 5ª Inc", "5ª Fund.", "6ª a 9ª Fund.", "Fund. Compl", "Médio Incompl.", "Médio Compl", "Super.Incompl.","Super. Compl", "Mestrado" , "Doutorado"))

df$`Escolaridade apos 2005` <- NULL


# Numero Funcionarios

df$NumFunc <- NA

df$NumFunc<- cut(as.numeric(df$`Tamanho Estabelecimento`), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ), labels=c("Até 4", "5 a 9", "10 a 19", "20 a 49", "50 a 99", "100 a 249", "250 a 499", "500 a 999", "1000 ou +"))

df$`Tamanho Estabelecimento`<- NULL



# tira o tempo de emprego temporariamente

df$TempoEmpr <- NA

df$TempoEmpr<- cut(as.numeric(df$`Faixa Tempo Emprego`), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8), labels=c("01", "02", "03", "04", "05", "06", "07", "08"))

df$`Faixa Tempo Emprego` <- NULL

# Desligamento 

df$MotivDesliga <- NA

df$MotivDesliga<- cut((df$`Motivo Desligamento`), breaks = c(0, 10, 11, 12, 20, 21, 22, 30, 31, 32 , 33, 40, 50, 60, 62, 63, 64, 70, 71, 72, 73, 74, 75, 76, 78,79,  80)
                      , labels=c("Dem.com JC", "Dem.sem JC", "Term. Contr.", "Desl.com JC", "Desl.sem JC"
                                 , "Poss out Car", "Trans c/ Onus","Trans s/ Onus", "Read/Redis", "Cessão"
                                 , "Mud.Regime","Reforma","Falecimento", "Falec. Ac Trab", "Falec. Ac Tip"
                                 , "Falec. D Prof.", "Apos. Ts Cres", "Apos. Ts Sres", "Apos. Id Cres"
                                 , "Apos. in Acid", "Apos. in Doen","Apos. Compuls", "Apos. in Outr"
                                 , "Apos. Id Sres", "Apos. Esp Cre", "Apos. Esp Sre"))

df$`Motivo Desligamento`<- NULL

# CBO - nível 3

df$CBO<-NA

#x<- df$`CBO Ocupacao 2002`

x <- substr(as.character(df$`CBO Ocupacao 2002`),1,3)

df$CBO<-as.ordered(x)

df$CBO<-as.factor(x)
#

df$`CBO Ocupacao 2002` <- NULL


## Exibe summario


str(df)

## sumário das informações

summary(df)


##### plota grafico #####

library(arules)
#library(arulesViz)
library(ggplot2)

dfmatrix <- as.vector.data.frame(df$Afastamento)

#### Gráficos de Barra
dd <- as.array(df$Idade)
plot(dd)

dd <- as.array(df$sexo)
plot(dd)

dd <- as.array(df$raca)
plot(dd)

dd <- as.array(df$Escolar)
plot(dd)

dd <- as.array(df$NumFunc)
plot(dd)

dd <- as.array(df$MotivDesliga)
plot(dd)

dd <- as.array(df$Afastamento)
plot(dd)

dd <- as.array(df$CBO)
plot(dd)

##### Gráficos de LInha ( 2 variáveis) ####

plot(df$Idade~df$sexo)

plot(df$sexo, df$Idade., "b")

plot.design(df)

str(df)

#library(dplyr)

#AM2015tot <- bind_rows (AM2015Redu, AM2015Afast2, AM2015Afast3)

# 353.530 observaÃ§Ãµes


### grava arquivo csv modificado

write.csv(df, file="C:/Mestrado_CEFET/AM2015Redu.csv")

###### carrega libraries #######
library(arules)

library(arulesViz)

##### APRIORI ######

str(df)
summary(df)


rules.all <- apriori(df)

rules <- apriori(df, parameter = list(supp = 0.1, conf = 0.5, target = "rules"))

summary(rules)

inspect(rules)

inspect(rules.all)

summary(rules.all)

quality(rules) <- round(quality(rules), digit = 3)

roc <- sort(rules, by="lift")

inspect(roc)

plot(rules, method="graph", control=list(type="items"))

#############################################################################3

#library(arulesViz)

#transactionInfo(df)

#rules <- apriori(df,
#                 + parameter = list(minlen=2, supp=0.005, conf=0.8),
##                 + appearance = list(rhs=c("Survived=No", "Survived=Yes"),
##                 + default="lhs"),
#                 + control = list(verbose=F))
#rules.sorted <- sort(rules, by="lift")
#inspect(rules.sorted)


####  ECLAT ####

#itemsets <- eclat(df, parameter = list(supp = 0.1, minlen = 2))

#inspect(itemsets)

#rules <- ruleInduction(itemsets, df , confidence = 0.5)

##############  WECLAT  ##########

#s <- weclat(df, parameter = list(support = 0.1),
#            control = list(verbose = TRUE))
#inspect(sort(s))

## create rules using weighted support (satisfying a minimum 
## weighted confidence of 90%).
#r <- ruleInduction(s, confidence = .5)

#inspect(r)


#df <- as.data.frame.data.frame(AM2015Redu)


## Exibe summario
str(df)


####################################################################
#lhs               rhs         support   confidence lift
#1 {}             => {Age=Adult} 0.9504771 0.9504771  1.0000000
#2 {Class=2nd}    => {Age=Adult} 0.1185825 0.9157895  0.9635051
#3 {Class=1st}    => {Age=Adult} 0.1449341 0.9815385  1.0326798
#4 {Sex=Female}   => {Age=Adult} 0.1930940 0.9042553  0.9513700
#5 {Class=3rd}    => {Age=Adult} 0.2848705 0.8881020  0.9343750
#6 {Survived=Yes} => {Age=Adult} 0.2971377 0.9198312  0.9677574
#7 {Class=Crew}   => {Sex=Male}  0.3916402 0.9740113  1.2384742
######################################################################
