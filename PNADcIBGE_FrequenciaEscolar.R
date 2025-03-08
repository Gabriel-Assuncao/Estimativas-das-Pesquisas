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
ano <- 2019
pnadc_anual_trimestre <- PNADcIBGE::get_pnadc(year=ano, topic=2)

# Criando variáveis auxiliares para cálculo da estimativa
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, Pais=as.factor("Brasil"))
pnadc_anual_trimestre$variables$Pais <- factor(x=pnadc_anual_trimestre$variables$Pais, levels=c("Brasil"))
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, GR=as.factor(ifelse(substr(UPA, start=1, stop=1)=="1","Norte",ifelse(substr(UPA, start=1, stop=1)=="2","Nordeste",ifelse(substr(UPA, start=1, stop=1)=="3","Sudeste",ifelse(substr(UPA, start=1, stop=1)=="4","Sul",ifelse(substr(UPA, start=1, stop=1)=="5","Centro-Oeste",NA)))))))
pnadc_anual_trimestre$variables$GR <- factor(x=pnadc_anual_trimestre$variables$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))

# Criando variáveis de educação com as adaptações necessárias
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, MV3002=as.factor(ifelse(is.na(V3002),"Não aplicável",as.character(V3002))))
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, MV3003A=as.factor(ifelse(is.na(V3003A),"Não aplicável",as.character(V3003A))))
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, MVD3004=as.factor(ifelse(is.na(VD3004),"Não aplicável",as.character(VD3004))))
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, frequencia_escolar_6a14anos=ifelse(V2009>=6 & V2009<=14 & ((MV3003A=="Regular do ensino fundamental" | MV3003A=="Educação de jovens e adultos (EJA) do ensino fundamental" | MV3003A=="Regular do ensino médio" | MV3003A=="Educação de jovens e adultos (EJA) do ensino médio" | MV3003A=="Superior - graduação" | MV3003A=="Especialização de nível superior" | MV3003A=="Mestrado" | MV3003A=="Doutorado") | (MV3002=="Não" & (MVD3004=="Fundamental completo ou equivalente" | MVD3004=="Médio incompleto ou equivalente" | MVD3004=="Médio completo ou equivalente" | MVD3004=="Superior incompleto ou equivalente" | MVD3004=="Superior completo"))),"Frequente",ifelse(V2009>=6 & V2009<=14,"Infrequente",NA)))
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, frequencia_escolar_15a17anos=ifelse(V2009>=15 & V2009<=17 & ((MV3003A=="Regular do ensino médio" | MV3003A=="Educação de jovens e adultos (EJA) do ensino médio" | MV3003A=="Superior - graduação" | MV3003A=="Especialização de nível superior" | MV3003A=="Mestrado" | MV3003A=="Doutorado") | (MV3002=="Não" & (MVD3004=="Médio completo ou equivalente" | MVD3004=="Superior incompleto ou equivalente" | MVD3004=="Superior completo"))),"Frequente",ifelse(V2009>=15 & V2009<=17,"Infrequente",NA)))
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, frequencia_escolar_18a24anos=ifelse(V2009>=18 & V2009<=24 & ((MV3003A=="Superior - graduação" | MV3003A=="Especialização de nível superior" | MV3003A=="Mestrado" | MV3003A=="Doutorado") | (MV3002=="Não" & (MVD3004=="Superior completo"))),"Frequente",ifelse(V2009>=18 & V2009<=24,"Infrequente",NA)))

# Calculando a estimativa da taxa ajustada de frequência escolar líquida (SIDRA - Tabela 7141)
print(x=taxa_frequencia_escolar_6a14anos <- survey::svybys(formula=~(V2009>=6 & V2009<=14 & frequencia_escolar_6a14anos=="Frequente"), denominator=~(V2009>=6 & V2009<=14), bys=~Pais+GR, design=pnadc_anual_trimestre, FUN=svyratio, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
print(x=taxa_frequencia_escolar_15a17anos <- survey::svybys(formula=~(V2009>=15 & V2009<=17 & frequencia_escolar_15a17anos=="Frequente"), denominator=~(V2009>=15 & V2009<=17), bys=~Pais+GR, design=pnadc_anual_trimestre, FUN=svyratio, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
print(x=taxa_frequencia_escolar_18a24anos <- survey::svybys(formula=~(V2009>=18 & V2009<=24 & frequencia_escolar_18a24anos=="Frequente"), denominator=~(V2009>=18 & V2009<=24), bys=~Pais+GR, design=pnadc_anual_trimestre, FUN=svyratio, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

##########################################################################