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
if("PNSIBGE" %in% rownames(installed.packages())==FALSE)
{
  install.packages(pkgs="PNSIBGE", dependencies=TRUE)
}
library(package="PNSIBGE", verbose=TRUE)
if("survey" %in% rownames(installed.packages())==FALSE)
{
  install.packages(pkgs="survey", dependencies=TRUE)
}
library(package="survey", verbose=TRUE)

# Obtendo microdados do período de referência para cálculo da estimativa
pns2019_moradorselecionado <- PNSIBGE::get_pns(year=2019, selected=TRUE)

# Criando variáveis auxiliares para cálculo da estimativa
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, Pais=as.factor("Brasil"))
pns2019_moradorselecionado$variables$Pais <- factor(pns2019_moradorselecionado$variables$Pais, levels=c("Brasil"))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, GR=as.factor(ifelse(substr(x=UPA_PNS,start=1,stop=1)=="1","Norte",ifelse(substr(x=UPA_PNS,start=1,stop=1)=="2","Nordeste",ifelse(substr(x=UPA_PNS,start=1,stop=1)=="3","Sudeste",ifelse(substr(x=UPA_PNS,start=1,stop=1)=="4","Sul",ifelse(substr(x=UPA_PNS,start=1,stop=1)=="5","Centro-oeste",NA)))))))
pns2019_moradorselecionado$variables$GR <- factor(pns2019_moradorselecionado$variables$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-oeste"))

# Criando variáveis para identificação de câncer exceto câncer de pele não melanônico
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, MQ12102=ifelse(is.na(Q12102),"Não aplicável",as.character(Q12102)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, MQ12103=ifelse(is.na(Q12103),"Não aplicável",as.character(Q12103)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, MQ12104=ifelse(is.na(Q12104),"Não aplicável",as.character(Q12104)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, MQ12105=ifelse(is.na(Q12105),"Não aplicável",as.character(Q12105)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, MQ12106=ifelse(is.na(Q12106),"Não aplicável",as.character(Q12106)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, MQ12107=ifelse(is.na(Q12107),"Não aplicável",as.character(Q12107)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, MQ12108=ifelse(is.na(Q12108),"Não aplicável",as.character(Q12108)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, MQ12109=ifelse(is.na(Q12109),"Não aplicável",as.character(Q12109)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, MQ121010=ifelse(is.na(Q121010),"Não aplicável",as.character(Q121010)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, MQ121011=ifelse(is.na(Q121011),"Não aplicável",as.character(Q121011)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, MQ121012=ifelse(is.na(Q121012),"Não aplicável",as.character(Q121012)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, MQ121013=ifelse(is.na(Q121013),"Não aplicável",as.character(Q121013)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, MQ121014=ifelse(is.na(Q121014),"Não aplicável",as.character(Q121014)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, MQ121015=ifelse(is.na(Q121015),"Não aplicável",as.character(Q121015)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, MQ121016=ifelse(is.na(Q121016),"Não aplicável",as.character(Q121016)))

# Calculando estimativa de total e proporcao de diagnóstico de câncer exceto câncer de pele não melanônico (SIDRA - Tabela 7964)
print(x=total_cancer <- survey::svybys(formula=~as.integer(MQ12102=="Não" | MQ12103=="Sim" | (MQ12103%in%c("Não","Não sei") & (MQ12104=="Sim" | MQ12105=="Sim" | MQ12106=="Sim" | MQ12107=="Sim" | MQ12108=="Sim" | MQ12109=="Sim" | MQ121010=="Sim" | MQ121011=="Sim" | MQ121012=="Sim" | MQ121013=="Sim" | MQ121014=="Sim" | MQ121015=="Sim" | MQ121016=="Sim"))), bys=~Pais+GR, design=subset(pns2019_moradorselecionado, C008>=18 & C008<999), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
print(x=proporcao_cancer <- survey::svybys(formula=~as.integer(MQ12102=="Não" | MQ12103=="Sim" | (MQ12103%in%c("Não","Não sei") & (MQ12104=="Sim" | MQ12105=="Sim" | MQ12106=="Sim" | MQ12107=="Sim" | MQ12108=="Sim" | MQ12109=="Sim" | MQ121010=="Sim" | MQ121011=="Sim" | MQ121012=="Sim" | MQ121013=="Sim" | MQ121014=="Sim" | MQ121015=="Sim" | MQ121016=="Sim"))), bys=~Pais+GR, design=subset(pns2019_moradorselecionado, C008>=18 & C008<999), FUN=svymean, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

##########################################################################