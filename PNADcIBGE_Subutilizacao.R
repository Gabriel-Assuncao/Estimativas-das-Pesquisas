# Limpando dados armazenados na memoria de programa
rm(list=ls(all=TRUE))

# Aumentando memoria atribuida para execucao do programa
memory.limit(size=30000)

# Carregando pacotes necessarios para calculo das estimativas
library("PNADcIBGE")
library("survey")

# Obtendo microdados do periodo de referencia para calculo das estimativas
ano <- 2022
pnadc_trimestral <- get_pnadc(year=ano, quarter=2)

# Realizando a criacao das variaveis de trabalho com as adaptacoes necessarias
pnadc_trimestral$variables <- transform(pnadc_trimestral$variables, MVD4001 = as.factor(ifelse(is.na(VD4001),"Pessoas não consideradas",as.character(VD4001)))) 
pnadc_trimestral$variables <- transform(pnadc_trimestral$variables, MVD4002 = as.factor(ifelse(is.na(VD4002),"Pessoas não consideradas",as.character(VD4002)))) 
pnadc_trimestral$variables <- transform(pnadc_trimestral$variables, MVD4003 = as.factor(ifelse(is.na(VD4003),"Pessoas não consideradas",as.character(VD4003))))  
{
  if(ano < 2015)
  {
    pnadc_trimestral$variables <- transform(pnadc_trimestral$variables, MVD4004 = as.factor(ifelse(is.na(VD4004),"Pessoas não consideradas",as.character(VD4004))))
  }
  else if(ano == 2015)
  {
    pnadc_trimestral$variables <- transform(pnadc_trimestral$variables, MVD4004 = as.factor(ifelse(Trimestre>=1 & Trimestre<=3,ifelse(is.na(VD4004),"Pessoas não consideradas",as.character(VD4004)),ifelse(Trimestre==4,ifelse(is.na(VD4004A),"Pessoas não consideradas",as.character(VD4004A)),NA))))
  }
  else
  {
    pnadc_trimestral$variables <- transform(pnadc_trimestral$variables, MVD4004A = as.factor(ifelse(is.na(VD4004A),"Pessoas não consideradas",as.character(VD4004A))))
  }
}

# Calculando as estimativas da taxa de subutilizacao total e por sexo
{
  if(ano <= 2015)
  {
    print(x=txsubutil <- svyratio(numerator=~((MVD4002 == "Pessoas desocupadas") | (MVD4003 == "Pessoas fora da força de trabalho e na força de trabalho potencial") | (MVD4004 == "Pessoas subocupadas")), denominator=~((MVD4001 == "Pessoas na força de trabalho") | (MVD4003 == "Pessoas fora da força de trabalho e na força de trabalho potencial")), design=pnadc_trimestral, na.rm=FALSE))
    cv(object=txsubutil)
    print(x=txsubutil_sexo <- svyby(formula=~((MVD4002 == "Pessoas desocupadas") | (MVD4003 == "Pessoas fora da força de trabalho e na força de trabalho potencial") | (MVD4004 == "Pessoas subocupadas")), denominator=~((MVD4001 == "Pessoas na força de trabalho") | (MVD4003 == "Pessoas fora da força de trabalho e na força de trabalho potencial")), by=~V2007, design=pnadc_trimestral, FUN=svyratio, vartype=c("se","cv"), keep.names=FALSE, na.rm=FALSE))
  }
  else
  {
    print(x=txsubutil <- svyratio(numerator=~((MVD4002 == "Pessoas desocupadas") | (MVD4003 == "Pessoas fora da força de trabalho e na força de trabalho potencial") | (MVD4004A == "Pessoas subocupadas")), denominator=~((MVD4001 == "Pessoas na força de trabalho") | (MVD4003 == "Pessoas fora da força de trabalho e na força de trabalho potencial")), design=pnadc_trimestral, na.rm=FALSE))
    cv(object=txsubutil)
    print(x=txsubutil_sexo <- svyby(formula=~((MVD4002 == "Pessoas desocupadas") | (MVD4003 == "Pessoas fora da força de trabalho e na força de trabalho potencial") | (MVD4004A == "Pessoas subocupadas")), denominator=~((MVD4001 == "Pessoas na força de trabalho") | (MVD4003 == "Pessoas fora da força de trabalho e na força de trabalho potencial")), by=~V2007, design=pnadc_trimestral, FUN=svyratio, vartype=c("se","cv"), keep.names=FALSE, na.rm=FALSE))
  }
}