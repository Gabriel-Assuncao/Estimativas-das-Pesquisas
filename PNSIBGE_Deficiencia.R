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
pns2019 <- PNSIBGE::get_pns(year=2019)

# Criando variáveis auxiliares para cálculo da estimativa
pns2019$variables <- transform(pns2019$variables, Pais=as.factor("Brasil"))
pns2019$variables$Pais <- factor(pns2019$variables$Pais, levels=c("Brasil"))
pns2019$variables <- transform(pns2019$variables, GR=as.factor(ifelse(substr(x=UPA_PNS,start=1,stop=1)=="1","Norte",ifelse(substr(x=UPA_PNS,start=1,stop=1)=="2","Nordeste",ifelse(substr(x=UPA_PNS,start=1,stop=1)=="3","Sudeste",ifelse(substr(x=UPA_PNS,start=1,stop=1)=="4","Sul",ifelse(substr(x=UPA_PNS,start=1,stop=1)=="5","Centro-oeste",NA)))))))
pns2019$variables$GR <- factor(pns2019$variables$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-oeste"))

# Criando variável para identificação de deficiência
pns2019$variables <- transform(pns2019$variables, Deficiencia=as.factor(ifelse((G046=="Sim, muita dificuldade" | G046=="Sim, não consegue de modo algum" | G047=="Sim, muita dificuldade" | G047=="Sim, não consegue de modo algum" | G057=="Sim, muita dificuldade" | G057=="Sim, não consegue de modo algum" | G058=="Sim, muita dificuldade" | G058=="Sim, não consegue de modo algum" | G070=="Sim, muita dificuldade" | G070=="Sim, não consegue de modo algum" | G071=="Sim, muita dificuldade" | G071=="Sim, não consegue de modo algum" | G079=="Sim, muita dificuldade" | G079=="Sim, não consegue de modo algum" | G080=="Sim, muita dificuldade" | G080=="Sim, não consegue de modo algum" | G081=="Sim, muita dificuldade" | G081=="Sim, não consegue de modo algum" | G082=="Sim, muita dificuldade" | G082=="Sim, não consegue de modo algum" | G083=="Sim, muita dificuldade" | G083=="Sim, não consegue de modo algum"),"Sim",NA)))
pns2019$variables <- transform(pns2019$variables, Deficiencia=as.factor(ifelse(C008>=2 & is.na(Deficiencia),"Não",ifelse(C008>=2 & Deficiencia=="Sim","Sim",NA))))
pns2019$variables$Deficiencia <- factor(x=pns2019$variables$Deficiencia, levels=c("Sim","Não"))

# Calculando estimativas de total e proporção de deficiência com aberturas (SIDRA - Tabela 6701 e 8196)
print(x=total_deficiencia <- survey::svybys(formula=~as.integer(Deficiencia=="Sim"), bys=~Pais+GR+C006+C009, design=pns2019, FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
print(x=proporcao_deficiencia <- survey::svybys(formula=~as.integer(Deficiencia=="Sim"), bys=~Pais+GR+C006+C009, design=pns2019, FUN=svymean, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

##########################################################################