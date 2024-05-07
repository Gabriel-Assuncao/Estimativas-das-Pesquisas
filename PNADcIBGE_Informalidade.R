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
pnadc_trimestral <- PNADcIBGE::get_pnadc(year=2022, quarter=2)
pnadc_trimestral$variables <- transform(pnadc_trimestral$variables, informalidade=as.factor(ifelse(is.na(VD4009),NA,ifelse(VD4009=="Empregado no setor privado sem carteira de trabalho assinada"|VD4009=="Trabalhador doméstico sem carteira de trabalho assinada"|(VD4009=="Empregador"&V4019=="Não")|(VD4009=="Conta-própria"&V4019=="Não")|VD4009=="Trabalhador familiar auxiliar","Pessoas na informalidade","Pessoas na formalidade"))))

# Calculando estimativa do total de pessoas na informalidade (SIDRA - Tabela 4093)
print(x=pessoas_informalidade <- survey::svytotal(x=~as.integer(informalidade=="Pessoas na informalidade"), design=subset(pnadc_trimestral, V2009>=14 & V2009<999), na.rm=TRUE))
cv(object=pessoas_informalidade)

# Calculando estimativa da taxa de informalidade (SIDRA - Tabela 8529)
print(x=taxa_informalidade <- survey::svyratio(numerator=~(informalidade=="Pessoas na informalidade"), denominator=~(VD4002=="Pessoas ocupadas"), design=subset(pnadc_trimestral, V2009>=14 & V2009<999), na.rm=TRUE))
cv(object=taxa_informalidade)

##########################################################################