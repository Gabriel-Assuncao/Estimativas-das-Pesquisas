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
ano <- 2023
{
  if(ano == 2019)
  {
    pnadc_anual_turismo <- PNADcIBGE::get_pnadc(year=ano, topic=3)
  }
  else
  {
    pnadc_anual_turismo <- PNADcIBGE::get_pnadc(year=ano, interview=2)
  }
}

# Criando variáveis auxiliares para cálculo da estimativa
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, Pais=as.factor("Brasil"))
pnadc_anual_turismo$variables$Pais <- factor(x=pnadc_anual_turismo$variables$Pais, levels=c("Brasil"))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, GR=as.factor(ifelse(substr(UPA, start=1, stop=1)=="1","Norte",ifelse(substr(UPA, start=1, stop=1)=="2","Nordeste",ifelse(substr(UPA, start=1, stop=1)=="3","Sudeste",ifelse(substr(UPA, start=1, stop=1)=="4","Sul",ifelse(substr(UPA, start=1, stop=1)=="5","Centro-Oeste",NA)))))))
pnadc_anual_turismo$variables$GR <- factor(x=pnadc_anual_turismo$variables$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))

# Calculando estimativa de domicílios por ocorrência de viagem (SIDRA - Tabela 7277)
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, domicilios=as.factor(ifelse(V2005=="Pessoa responsável pelo domicílio","Total","Não aplicável")))
pnadc_anual_turismo$variables$domicilios <- factor(x=pnadc_anual_turismo$variables$domicilios, levels=c("Total","Não aplicável"))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, ocorrencia_viagens=as.factor(ifelse(V2005=="Pessoa responsável pelo domicílio" & S08001=="Sim","Houve viagem",ifelse(V2005=="Pessoa responsável pelo domicílio" & S08001=="Não","Não houve viagem",NA))))
pnadc_anual_turismo$variables$ocorrencia_viagens <- factor(x=pnadc_anual_turismo$variables$ocorrencia_viagens, levels=c("Houve viagem","Não houve viagem"))
print(x=domicilios_viagens <- survey::svybys(formula=~as.integer(domicilios=="Total")+ocorrencia_viagens, bys=~Pais+GR+UF, design=subset(pnadc_anual_turismo, V2005=="Pessoa responsável pelo domicílio"), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

# Calculando estimativa de viagens realizadas pelos moradores dos domicílios (SIDRA - Tabela 7203)
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, viagens=as.integer(ifelse(S08002>=3,3,S08002)))
print(x=viagens_realizadas <- survey::svybys(formula=~viagens, bys=~Pais+GR+UF, design=subset(pnadc_anual_turismo, V2005=="Pessoa responsável pelo domicílio" & S08001=="Sim"), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

# Calculando estimativa de viagens nacionais realizadas pelos moradores dos domicílios (SIDRA - Tabela 7203)
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08005=as.factor(ifelse(is.na(S08005),"Não aplicável",as.character(S08005))))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, NS08005=as.integer(ifelse(MS08005=="Nacional",1,0)))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08018=as.factor(ifelse(is.na(S08018),"Não aplicável",as.character(S08018))))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, NS08018=as.integer(ifelse(MS08018=="Nacional",1,0)))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08031=as.factor(ifelse(is.na(S08031),"Não aplicável",as.character(S08031))))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, NS08031=as.integer(ifelse(MS08031=="Nacional",1,0)))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, nacionais=as.integer(NS08005+NS08018+NS08031))
print(x=viagens_nacionais <- survey::svybys(formula=~nacionais, bys=~Pais+GR+UF, design=subset(pnadc_anual_turismo, V2005=="Pessoa responsável pelo domicílio" & S08001=="Sim"), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

# Calculando estimativa de viagens com pernoite realizadas pelos moradores dos domicílios (SIDRA - Tabela 7203)
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08009=as.factor(ifelse(is.na(S08009),"Não aplicável",as.character(S08009))))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, NS08009=as.integer(ifelse(MS08009=="Sim",1,0)))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08022=as.factor(ifelse(is.na(S08022),"Não aplicável",as.character(S08022))))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, NS08022=as.integer(ifelse(MS08022=="Sim",1,0)))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08035=as.factor(ifelse(is.na(S08035),"Não aplicável",as.character(S08035))))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, NS08035=as.integer(ifelse(MS08035=="Sim",1,0)))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, pernoites=as.integer(NS08009+NS08022+NS08035))
print(x=viagens_pernoites <- survey::svybys(formula=~pernoites, bys=~Pais+GR+UF, design=subset(pnadc_anual_turismo, V2005=="Pessoa responsável pelo domicílio" & S08001=="Sim"), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

# Calculando estimativa de viagens nacionais com pernoite realizadas pelos moradores dos domicílios (SIDRA - Tabela 7203)
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08005=as.factor(ifelse(is.na(S08005),"Não aplicável",as.character(S08005))))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08009=as.factor(ifelse(is.na(S08009),"Não aplicável",as.character(S08009))))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, NS08005_NS08009=as.integer(ifelse(MS08005=="Nacional" & MS08009=="Sim",1,0)))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08018=as.factor(ifelse(is.na(S08018),"Não aplicável",as.character(S08018))))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08022=as.factor(ifelse(is.na(S08022),"Não aplicável",as.character(S08022))))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, NS08018_NS08022=as.integer(ifelse(MS08018=="Nacional" & MS08022=="Sim",1,0)))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08031=as.factor(ifelse(is.na(S08031),"Não aplicável",as.character(S08031))))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08035=as.factor(ifelse(is.na(S08035),"Não aplicável",as.character(S08035))))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, NS08031_NS08035=as.integer(ifelse(MS08031=="Nacional" & MS08035=="Sim",1,0)))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, nacionais_pernoites=as.integer(NS08005_NS08009+NS08018_NS08022+NS08031_NS08035))
print(x=viagens_nacionais_pernoites <- survey::svybys(formula=~nacionais_pernoites, bys=~Pais+GR+UF, design=subset(pnadc_anual_turismo, V2005=="Pessoa responsável pelo domicílio" & S08001=="Sim"), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

# Calculando estimativa do gasto total de viagens nacionais com pernoite realizadas pelos moradores dos domicílios (SIDRA - Tabela 8470)
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, gasto_total=as.numeric(ifelse(!is.na(S08005) & !is.na(S08009) & !is.na(S08017A7) & S08005=="Nacional" & S08009=="Sim" & S08017A7=="Sim",S08017A72*CO2,0)+ifelse(!is.na(S08018) & !is.na(S08022) & !is.na(S08030A7) & S08018=="Nacional" & S08022=="Sim" & S08030A7=="Sim",S08030A72*CO2,0)+ifelse(!is.na(S08031) & !is.na(S08035) & !is.na(S08043A7) & S08031=="Nacional" & S08035=="Sim" & S08043A7=="Sim",S08043A72*CO2,0)))
print(x=viagens_gasto_total <- survey::svybys(formula=~gasto_total, bys=~Pais+GR+UF, design=subset(pnadc_anual_turismo, V2005=="Pessoa responsável pelo domicílio" & S08001=="Sim"), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

##########################################################################