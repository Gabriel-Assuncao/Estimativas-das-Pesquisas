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
pnadc_trimestral <- PNADcIBGE::get_pnadc(year=2022, quarter=2)

# Criando variável com indicação de informalidade na ocupação
pnadc_trimestral$variables <- transform(pnadc_trimestral$variables, informalidade=as.factor(ifelse(is.na(VD4009),NA,ifelse(VD4009=="Empregado no setor privado sem carteira de trabalho assinada"|VD4009=="Trabalhador doméstico sem carteira de trabalho assinada"|(VD4009=="Empregador"&V4019=="Não")|(VD4009=="Conta-própria"&V4019=="Não")|VD4009=="Trabalhador familiar auxiliar","Pessoas na informalidade","Pessoas na formalidade"))))

# Calculando estimativa do total de pessoas na informalidade (SIDRA - Tabela 8517)
print(x=pessoas_informalidade <- survey::svytotal(x=~as.integer(informalidade=="Pessoas na informalidade"), design=subset(pnadc_trimestral, V2009>=14 & V2009<999), na.rm=TRUE))
cv(object=pessoas_informalidade)

# Calculando estimativa da taxa de informalidade (SIDRA - Tabela 8529)
print(x=taxa_informalidade <- survey::svyratio(numerator=~(informalidade=="Pessoas na informalidade"), denominator=~(VD4002=="Pessoas ocupadas"), design=subset(pnadc_trimestral, V2009>=14 & V2009<999), na.rm=TRUE))
cv(object=taxa_informalidade)

# Calculando estimativas do total de pessoas na informalidade e da taxa de informalidade por estrato geográfico (Painel PNAD Contínua)
pnadc_trimestral$variables <- transform(pnadc_trimestral$variables, Estrato_Geografico=as.factor(substr(Estrato, start=1, stop=4)))
utils::download.file(url="https://painel.ibge.gov.br/saibamais/files/Municipios_por_Estratos.csv", destfile=paste0(tempdir(),"/Municipios_por_Estratos.csv"), mode="wb")
municipios_estratos <- utils::read.csv(file=paste0(tempdir(),"/Municipios_por_Estratos.csv"), header=TRUE, sep=";", dec=",", fileEncoding="UTF-8")
names(municipios_estratos) <- c("Nome_Estrato_Geografico","Estrato_Geografico","Nome_Municipio","Municipio")
pnadc_trimestral$variables <- merge(x=pnadc_trimestral$variables, y=unique(municipios_estratos[,c("Nome_Estrato_Geografico","Estrato_Geografico")]), by.x="Estrato_Geografico", by.y="Estrato_Geografico", all.x=TRUE, all.y=FALSE)
print(x=pessoas_informalidade_estrato_geografico <- survey::svyby(formula=~as.integer(informalidade=="Pessoas na informalidade"), by=~Nome_Estrato_Geografico, design=subset(pnadc_trimestral, V2009>=14 & V2009<999), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
print(x=taxa_informalidade_estrato_geografico <- survey::svyby(formula=~(informalidade=="Pessoas na informalidade"), denominator=~(VD4002=="Pessoas ocupadas"), by=~Nome_Estrato_Geografico, design=subset(pnadc_trimestral, V2009>=14 & V2009<999), FUN=svyratio, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

##########################################################################