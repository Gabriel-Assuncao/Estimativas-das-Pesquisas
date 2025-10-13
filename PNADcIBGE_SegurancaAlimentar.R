##########################################################################

# Limpando arquivos armazenados na memória
rm(list=ls(all=TRUE))

# Definindo limite de memória para compilação do programa
aviso <- getOption("warn")
options(warn=-1)
memory.limit(size=50000)
options(warn=aviso)
rm(aviso)

# Definindo tempo de espera para obtenção de resposta do servidor
aviso <- getOption("warn")
options(warn=-1)
options(timeout=600)
options(warn=aviso)
rm(aviso)

# Definindo opção de codificação dos caracteres e linguagem
aviso <- getOption("warn")
options(warn=-1)
options(encoding="latin1")
options(warn=aviso)
rm(aviso)

# Definindo opção de exibição de números sem representação em exponencial
aviso <- getOption("warn")
options(warn=-1)
options(scipen=999)
options(warn=aviso)
rm(aviso)

# Definindo opção de repositório para instalação dos pacotes necessários
aviso <- getOption("warn")
options(warn=-1)
options(repos=structure(c(CRAN="https://cran.r-project.org/")))
options(warn=aviso)
rm(aviso)

# Definindo diretório de trabalho
caminho <- getwd()
setwd(dir=caminho)

# Carregando pacotes necessários para obtenção da estimativa desejada
if("PNADcIBGE" %in% rownames(installed.packages())==FALSE)
{
  install.packages(pkgs="PNADcIBGE", dependencies=TRUE)
}
library(package="PNADcIBGE", verbose=TRUE)
if("survey" %in% rownames(installed.packages())==FALSE)
{
  install.packages(pkgs="survey", dependencies=TRUE)
}
library(package="survey", verbose=TRUE)

# Obtendo microdados do período de referência para cálculo da estimativa
variaveis_selecionadas <- c("V1022","V2005","V2009",sprintf("S170%02d", seq(1:14)),"SD17001")
pnadc_anual_trimestre <- PNADcIBGE::get_pnadc(year=2023, topic=4, vars=variaveis_selecionadas)

# Criando variáveis auxiliares para cálculo da estimativa
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, ID_DOMICILIO=paste0(UPA,V1008,V1014))
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, Pais=as.factor("Brasil"))
pnadc_anual_trimestre$variables$Pais <- factor(x=pnadc_anual_trimestre$variables$Pais, levels=c("Brasil"))
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, GR=as.factor(ifelse(substr(UPA, start=1, stop=1)=="1","Norte",ifelse(substr(UPA, start=1, stop=1)=="2","Nordeste",ifelse(substr(UPA, start=1, stop=1)=="3","Sudeste",ifelse(substr(UPA, start=1, stop=1)=="4","Sul",ifelse(substr(UPA, start=1, stop=1)=="5","Centro-Oeste",NA)))))))
pnadc_anual_trimestre$variables$GR <- factor(x=pnadc_anual_trimestre$variables$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))

# Criando variável derivada de situação de segurança alimentar do domicílio caso seja necessário
domicilios_com_menores <- unique(x=pnadc_anual_trimestre$variables$ID_DOMICILIO[pnadc_anual_trimestre$variables$V2009<18])
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, NS17001=as.numeric(ifelse(ID_DOMICILIO%in%domicilios_com_menores,ifelse(is.na(S17001),0,ifelse(S17001=="Sim",1,0))+ifelse(is.na(S17002),0,ifelse(S17002=="Sim",1,0))+ifelse(is.na(S17003),0,ifelse(S17003=="Sim",1,0))+ifelse(is.na(S17004),0,ifelse(S17004=="Sim",1,0))+ifelse(is.na(S17005),0,ifelse(S17005=="Sim",1,0))+ifelse(is.na(S17006),0,ifelse(S17006=="Sim",1,0))+ifelse(is.na(S17007),0,ifelse(S17007=="Sim",1,0))+ifelse(is.na(S17008),0,ifelse(S17008=="Sim",1,0))+ifelse(is.na(S17009),0,ifelse(S17009=="Sim",1,0))+ifelse(is.na(S17010),0,ifelse(S17010=="Sim",1,0))+ifelse(is.na(S17011),0,ifelse(S17011=="Sim",1,0))+ifelse(is.na(S17012),0,ifelse(S17012=="Sim",1,0))+ifelse(is.na(S17013),0,ifelse(S17013=="Sim",1,0))+ifelse(is.na(S17014),0,ifelse(S17014=="Sim",1,0)),
                                                                                                        ifelse(is.na(S17001),0,ifelse(S17001=="Sim",1,0))+ifelse(is.na(S17002),0,ifelse(S17002=="Sim",1,0))+ifelse(is.na(S17003),0,ifelse(S17003=="Sim",1,0))+ifelse(is.na(S17004),0,ifelse(S17004=="Sim",1,0))+ifelse(is.na(S17005),0,ifelse(S17005=="Sim",1,0))+ifelse(is.na(S17006),0,ifelse(S17006=="Sim",1,0))+ifelse(is.na(S17007),0,ifelse(S17007=="Sim",1,0))+ifelse(is.na(S17008),0,ifelse(S17008=="Sim",1,0)))))
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, MS17001=as.factor(ifelse(S17001=="Não" & S17002=="Não" & S17003=="Não" & S17004=="Não","Segurança alimentar",
                                                                                                       ifelse(ID_DOMICILIO%in%domicilios_com_menores,ifelse(NS17001>=1 & NS17001<=5,"Insegurança alimentar leve",ifelse(NS17001>=6 & NS17001<=9,"Insegurança alimentar moderada",ifelse(NS17001>=10 & NS17001<=14,"Insegurança alimentar grave",NA))),
                                                                                                              ifelse(NS17001>=1 & NS17001<=3,"Insegurança alimentar leve",ifelse(NS17001>=4 & NS17001<=5,"Insegurança alimentar moderada",ifelse(NS17001>=6 & NS17001<=8,"Insegurança alimentar grave",NA)))))))
pnadc_anual_trimestre$variables$MS17001 <- factor(x=pnadc_anual_trimestre$variables$MS17001, levels=c("Segurança alimentar","Insegurança alimentar leve","Insegurança alimentar moderada","Insegurança alimentar grave"))
rm(domicilios_com_menores)
if(sum(is.na(pnadc_anual_trimestre$variables$SD17001)) == nrow(pnadc_anual_trimestre$variables))
{
  pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, SD17001=as.factor(ifelse(is.na(MS17001),NA,as.character(MS17001))))
  pnadc_anual_trimestre$variables$SD17001 <- factor(x=pnadc_anual_trimestre$variables$SD17001, levels=c("Segurança alimentar","Insegurança alimentar leve","Insegurança alimentar moderada","Insegurança alimentar grave"))
}

# Calculando total e proporção de domicílios de acordo com segurança alimentar (SIDRA - Tabela 9552)
print(x=domicilios_total_seguranca_alimentar <- survey::svybys(formula=~SD17001, bys=~Pais+GR+V1022, design=subset(pnadc_anual_trimestre, V2005=="Pessoa responsável pelo domicílio"), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
print(x=domicilios_proporcao_seguranca_alimentar <- survey::svybys(formula=~SD17001, bys=~Pais+GR+V1022, design=subset(pnadc_anual_trimestre, V2005=="Pessoa responsável pelo domicílio"), FUN=svymean, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

# Calculando total e proporção de moradores de acordo com segurança alimentar (SIDRA - Tabela 9552)
print(x=moradores_total_seguranca_alimentar <- survey::svybys(formula=~SD17001, bys=~Pais+GR+V1022, design=pnadc_anual_trimestre, FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
print(x=moradores_proporcao_seguranca_alimentar <- survey::svybys(formula=~SD17001, bys=~Pais+GR+V1022, design=pnadc_anual_trimestre, FUN=svymean, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

##########################################################################
