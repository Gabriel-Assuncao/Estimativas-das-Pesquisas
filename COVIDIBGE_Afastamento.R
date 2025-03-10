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
if("COVIDIBGE" %in% rownames(installed.packages())==FALSE)
{
  install.packages(pkgs="COVIDIBGE", dependencies=TRUE)
}
library(package="COVIDIBGE", verbose=TRUE)
if("survey" %in% rownames(installed.packages())==FALSE)
{
  install.packages(pkgs="survey", dependencies=TRUE)
}
library(package="survey", verbose=TRUE)

# Obtendo microdados do período de referência para cálculo da estimativa
pnad_covid <- COVIDIBGE::get_covid(year=2020, month=11)

# Criando variáveis relacionadas ao afatamento das pessoas durante quarentena
pnad_covid$variables <- transform(pnad_covid$variables, MC001=as.factor(ifelse(is.na(C001),"Não aplicável",as.character(C001))))
pnad_covid$variables <- transform(pnad_covid$variables, MC002=as.factor(ifelse(is.na(C002),"Não aplicável",as.character(C002))))
pnad_covid$variables <- transform(pnad_covid$variables, MC003=as.factor(ifelse(is.na(C003),"Não aplicável",as.character(C003))))
pnad_covid$variables <- transform(pnad_covid$variables, MC004=as.factor(ifelse(is.na(C004),"Não aplicável",as.character(C004))))
pnad_covid$variables <- transform(pnad_covid$variables, MC007=as.factor(ifelse(is.na(C007),"Não aplicável",as.character(C007))))

# Calculando estimativa da taxa de afastamento das pessoas durante quarentena
print(x=taxa_afastamento_quarentena <- survey::svyby(formula=~(A002>=14 & MC003=="Estava em quarentena, isolamento, distanciamento social ou férias coletivas" & MC007%in%c("Trabalhador doméstico (empregado doméstico, cuidados, babá)","Militar do exercito, marinha ou aeronáutica","Policial militar ou bombeiro militar","Empregado do setor privado","Empregado do setor público (inclusive empresas de economia mista)","Empregador","Conta própria","Trabalhador familiar não remunerado em ajuda a membro do domicílio ou parente")), denominator=~(A002>=14 & MC007%in%c("Trabalhador doméstico (empregado doméstico, cuidados, babá)","Militar do exercito, marinha ou aeronáutica","Policial militar ou bombeiro militar","Empregado do setor privado","Empregado do setor público (inclusive empresas de economia mista)","Empregador","Conta própria","Trabalhador familiar não remunerado em ajuda a membro do domicílio ou parente")), by=~UF, design=subset(pnad_covid, A002>=14 & A002<999), FUN=svyratio, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

##########################################################################