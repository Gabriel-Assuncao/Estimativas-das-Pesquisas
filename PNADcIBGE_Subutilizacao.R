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
if("utils" %in% rownames(installed.packages())==FALSE)
{
  install.packages(pkgs="utils", dependencies=TRUE)
}
library(package="utils", verbose=TRUE)

# Obtendo microdados do período de referência para cálculo da estimativa
ano <- 2022
pnadc_trimestral <- PNADcIBGE::get_pnadc(year=ano, quarter=2)

# Criando variáveis de trabalho com as adaptações necessárias
pnadc_trimestral$variables <- transform(pnadc_trimestral$variables, MVD4001=as.factor(ifelse(is.na(VD4001),"Pessoas não consideradas",as.character(VD4001)))) 
pnadc_trimestral$variables <- transform(pnadc_trimestral$variables, MVD4002=as.factor(ifelse(is.na(VD4002),"Pessoas não consideradas",as.character(VD4002)))) 
pnadc_trimestral$variables <- transform(pnadc_trimestral$variables, MVD4003=as.factor(ifelse(is.na(VD4003),"Pessoas não consideradas",as.character(VD4003))))  
{
  if(ano < 2015)
  {
    pnadc_trimestral$variables <- transform(pnadc_trimestral$variables, MVD4004=as.factor(ifelse(is.na(VD4004),"Pessoas não consideradas",as.character(VD4004))))
  }
  else if(ano == 2015)
  {
    pnadc_trimestral$variables <- transform(pnadc_trimestral$variables, MVD4004=as.factor(ifelse(Trimestre>=1 & Trimestre<=3,ifelse(is.na(VD4004),"Pessoas não consideradas",as.character(VD4004)),ifelse(Trimestre==4,ifelse(is.na(VD4004A),"Pessoas não consideradas",as.character(VD4004A)),NA))))
  }
  else
  {
    pnadc_trimestral$variables <- transform(pnadc_trimestral$variables, MVD4004A=as.factor(ifelse(is.na(VD4004A),"Pessoas não consideradas",as.character(VD4004A))))
  }
}

# Calculando as estimativas da taxa de subutilização total e por sexo (SIDRA - Tabela 6396)
{
  if(ano <= 2015)
  {
    taxa_subutilizacao <- survey::svyratio(numerator=~((MVD4002=="Pessoas desocupadas") | (MVD4003=="Pessoas fora da força de trabalho e na força de trabalho potencial") | (MVD4004=="Pessoas subocupadas")), denominator=~((MVD4001=="Pessoas na força de trabalho") | (MVD4003=="Pessoas fora da força de trabalho e na força de trabalho potencial")), design=subset(pnadc_trimestral, V2009>=14 & V2009<999), na.rm=FALSE)
    taxa_subutilizacao_sexo <- survey::svyby(formula=~((MVD4002=="Pessoas desocupadas") | (MVD4003=="Pessoas fora da força de trabalho e na força de trabalho potencial") | (MVD4004=="Pessoas subocupadas")), denominator=~((MVD4001=="Pessoas na força de trabalho") | (MVD4003=="Pessoas fora da força de trabalho e na força de trabalho potencial")), by=~V2007, design=subset(pnadc_trimestral, V2009>=14 & V2009<999), FUN=svyratio, vartype=c("se","cv"), keep.names=FALSE, na.rm=FALSE)
  }
  else
  {
    taxa_subutilizacao <- survey::svyratio(numerator=~((MVD4002=="Pessoas desocupadas") | (MVD4003=="Pessoas fora da força de trabalho e na força de trabalho potencial") | (MVD4004A=="Pessoas subocupadas")), denominator=~((MVD4001=="Pessoas na força de trabalho") | (MVD4003=="Pessoas fora da força de trabalho e na força de trabalho potencial")), design=subset(pnadc_trimestral, V2009>=14 & V2009<999), na.rm=FALSE)
    taxa_subutilizacao_sexo <- survey::svyby(formula=~((MVD4002=="Pessoas desocupadas") | (MVD4003=="Pessoas fora da força de trabalho e na força de trabalho potencial") | (MVD4004A=="Pessoas subocupadas")), denominator=~((MVD4001=="Pessoas na força de trabalho") | (MVD4003=="Pessoas fora da força de trabalho e na força de trabalho potencial")), by=~V2007, design=subset(pnadc_trimestral, V2009>=14 & V2009<999), FUN=svyratio, vartype=c("se","cv"), keep.names=FALSE, na.rm=FALSE)
  }
}
print(x=taxa_subutilizacao)
cv(object=taxa_subutilizacao)
print(x=taxa_subutilizacao_sexo)

# Calculando a estimativa da taxa de subutilização por estrato geográfico (Painel PNAD Contínua)
pnadc_trimestral$variables <- transform(pnadc_trimestral$variables, Estrato_Geografico=as.factor(substr(Estrato, start=1, stop=4)))
utils::download.file(url="https://painel.ibge.gov.br/saibamais/files/Municipios_por_Estratos.csv", destfile=paste0(tempdir(),"/Municipios_por_Estratos.csv"), mode="wb")
municipios_estratos <- utils::read.csv(file=paste0(tempdir(),"/Municipios_por_Estratos.csv"), header=TRUE, sep=";", dec=",", fileEncoding="UTF-8")
names(municipios_estratos) <- c("Nome_Estrato_Geografico","Estrato_Geografico","Nome_Municipio","Municipio")
pnadc_trimestral$variables <- merge(x=pnadc_trimestral$variables, y=unique(municipios_estratos[,c("Nome_Estrato_Geografico","Estrato_Geografico")]), by.x="Estrato_Geografico", by.y="Estrato_Geografico", all.x=TRUE, all.y=FALSE)
{
  if(ano <= 2015)
  {
    taxa_subutilizacao_estrato_geografico <- survey::svyby(formula=~((MVD4002=="Pessoas desocupadas") | (MVD4003=="Pessoas fora da força de trabalho e na força de trabalho potencial") | (MVD4004=="Pessoas subocupadas")), denominator=~((MVD4001=="Pessoas na força de trabalho") | (MVD4003=="Pessoas fora da força de trabalho e na força de trabalho potencial")), by=~Nome_Estrato_Geografico, design=subset(pnadc_trimestral, V2009>=14 & V2009<999), FUN=svyratio, vartype=c("se","cv"), keep.names=FALSE, na.rm=FALSE)
  }
  else
  {
    taxa_subutilizacao_estrato_geografico <- survey::svyby(formula=~((MVD4002=="Pessoas desocupadas") | (MVD4003=="Pessoas fora da força de trabalho e na força de trabalho potencial") | (MVD4004A=="Pessoas subocupadas")), denominator=~((MVD4001=="Pessoas na força de trabalho") | (MVD4003=="Pessoas fora da força de trabalho e na força de trabalho potencial")), by=~Nome_Estrato_Geografico, design=subset(pnadc_trimestral, V2009>=14 & V2009<999), FUN=svyratio, vartype=c("se","cv"), keep.names=FALSE, na.rm=FALSE)
  }
}
print(x=taxa_subutilizacao_estrato_geografico)

##########################################################################