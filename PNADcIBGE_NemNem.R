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
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, Grupo_Idade=as.factor(ifelse(V2009>=15 & V2009<=17,"15 a 17 anos",ifelse(V2009>=18 & V2009<=24,"18 a 24 anos",ifelse(V2009>=25 & V2009<=29,"25 a 29 anos",NA)))))
pnadc_anual_trimestre$variables$Grupo_Idade <- factor(x=pnadc_anual_trimestre$variables$Grupo_Idade, levels=c("15 a 17 anos","18 a 24 anos","25 a 29 anos"))
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, Sexo=as.factor(ifelse(V2007=="Homem","Homens",ifelse(V2007=="Mulher","Mulheres",NA))))
pnadc_anual_trimestre$variables$Sexo <- factor(x=pnadc_anual_trimestre$variables$Sexo, levels=c("Homens","Mulheres"))
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, Cor_Raca=as.factor(ifelse(V2010=="Branca","Branca",ifelse(V2010=="Preta" | V2010=="Parda","Preta ou parda",NA))))
pnadc_anual_trimestre$variables$Cor_Raca <- factor(x=pnadc_anual_trimestre$variables$Cor_Raca, levels=c("Branca","Preta ou parda"))

# Criando variáveis de educação com as adaptações necessárias
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, MV3002=as.factor(ifelse(is.na(V3002),"Não aplicável",as.character(V3002))))
{
  if(ano < 2019)
  {
    pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, MV3019=as.factor(ifelse(is.na(V3019),"Não aplicável",as.character(V3019))))
  }
  else
  {
    pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, MV3019A=as.factor(ifelse(is.na(V3019A),"Não aplicável",as.character(V3019A))))
  }
}
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, MV3024=as.factor(ifelse(is.na(V3024),"Não aplicável",as.character(V3024))))
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, MV3025=as.factor(ifelse(is.na(V3025),"Não aplicável",as.character(V3025))))
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, MV3026=as.factor(ifelse(is.na(V3026),"Não aplicável",as.character(V3026))))

# Criando variável de condição de ocupação e estudo
{
  if(ano < 2019)
  {
    pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, condicao_ocupacao_estudo=as.factor(ifelse((VD4001=="Pessoas na força de trabalho" & VD4002=="Pessoas ocupadas") & (MV3002=="Sim" | MV3019=="Sim" | MV3024=="Sim" | MV3025=="Sim" | MV3026=="Sim"),"Ocupadas e frequentando escola, cursos pré-vestibular, técnico de nível médio, normal (magistério) ou qualificação profissional",
                                                                                            ifelse((VD4001=="Pessoas na força de trabalho" & VD4002=="Pessoas ocupadas") & (MV3002!="Sim" & MV3019!="Sim" & MV3024!="Sim" & MV3025!="Sim" & MV3026!="Sim"),"Ocupadas e não frequentando escola, nem cursos pré-vestibular, técnico de nível médio, normal (magistério) ou qualificação profissional",
                                                                                                   ifelse(((VD4001=="Pessoas na força de trabalho" & VD4002=="Pessoas desocupadas") | VD4001=="Pessoas fora da força de trabalho") & (MV3002=="Sim" | MV3019=="Sim" | MV3024=="Sim" | MV3025=="Sim" | MV3026=="Sim"),"Não ocupadas e frequentando escola, cursos pré-vestibular, técnico de nível médio, normal (magistério) ou qualificação profissional",
                                                                                                          ifelse(((VD4001=="Pessoas na força de trabalho" & VD4002=="Pessoas desocupadas") | VD4001=="Pessoas fora da força de trabalho") & (MV3002!="Sim" & MV3019!="Sim" & MV3024!="Sim" & MV3025!="Sim" & MV3026!="Sim"),"Não ocupadas e não frequentando escola, nem cursos pré-vestibular, técnico de nível médio, normal (magistério) ou qualificação profissional",NA))))))
  }
  else
  {
    pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, condicao_ocupacao_estudo=as.factor(ifelse((VD4001=="Pessoas na força de trabalho" & VD4002=="Pessoas ocupadas") & (MV3002=="Sim" | (MV3019A=="Sim, curso técnico de nível médio" | MV3019A=="Sim, curso normal (magistério)") | MV3024=="Sim" | MV3025=="Sim" | MV3026=="Sim"),"Ocupadas e frequentando escola, cursos pré-vestibular, técnico de nível médio, normal (magistério) ou qualificação profissional",
                                                                                            ifelse((VD4001=="Pessoas na força de trabalho" & VD4002=="Pessoas ocupadas") & (MV3002!="Sim" & (MV3019A!="Sim, curso técnico de nível médio" & MV3019A!="Sim, curso normal (magistério)") & MV3024!="Sim" & MV3025!="Sim" & MV3026!="Sim"),"Ocupadas e não frequentando escola, nem cursos pré-vestibular, técnico de nível médio, normal (magistério) ou qualificação profissional",
                                                                                                   ifelse(((VD4001=="Pessoas na força de trabalho" & VD4002=="Pessoas desocupadas") | VD4001=="Pessoas fora da força de trabalho") & (MV3002=="Sim" | (MV3019A=="Sim, curso técnico de nível médio" | MV3019A=="Sim, curso normal (magistério)") | MV3024=="Sim" | MV3025=="Sim" | MV3026=="Sim"),"Não ocupadas e frequentando escola, cursos pré-vestibular, técnico de nível médio, normal (magistério) ou qualificação profissional",
                                                                                                          ifelse(((VD4001=="Pessoas na força de trabalho" & VD4002=="Pessoas desocupadas") | VD4001=="Pessoas fora da força de trabalho") & (MV3002!="Sim" & (MV3019A!="Sim, curso técnico de nível médio" & MV3019A!="Sim, curso normal (magistério)") & MV3024!="Sim" & MV3025!="Sim" & MV3026!="Sim"),"Não ocupadas e não frequentando escola, nem cursos pré-vestibular, técnico de nível médio, normal (magistério) ou qualificação profissional",NA))))))
  }
}
pnadc_anual_trimestre$variables$condicao_ocupacao_estudo <- factor(x=pnadc_anual_trimestre$variables$condicao_ocupacao_estudo, levels=c("Ocupadas e frequentando escola, cursos pré-vestibular, técnico de nível médio, normal (magistério) ou qualificação profissional","Ocupadas e não frequentando escola, nem cursos pré-vestibular, técnico de nível médio, normal (magistério) ou qualificação profissional","Não ocupadas e frequentando escola, cursos pré-vestibular, técnico de nível médio, normal (magistério) ou qualificação profissional","Não ocupadas e não frequentando escola, nem cursos pré-vestibular, técnico de nível médio, normal (magistério) ou qualificação profissional"))

# Calculando a estimativa de jovens que nem estudam e nem trabalham (SIDRA - Tabelas 7162, 7163 e 7164 / Tabelas 7205, 7206 e 7207)
print(x=total_nemnem <- survey::svybys(formula=~as.integer(condicao_ocupacao_estudo=="Não ocupadas e não frequentando escola, nem cursos pré-vestibular, técnico de nível médio, normal (magistério) ou qualificação profissional"), bys=~Pais+GR+Grupo_Idade+Sexo+Cor_Raca, design=subset(pnadc_anual_trimestre, V2009>=15 & V2009<=29), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
print(x=proporcao_nemnem <- survey::svybys(formula=~as.integer(condicao_ocupacao_estudo=="Não ocupadas e não frequentando escola, nem cursos pré-vestibular, técnico de nível médio, normal (magistério) ou qualificação profissional"), bys=~Pais+GR+Grupo_Idade+Sexo+Cor_Raca, design=subset(pnadc_anual_trimestre, V2009>=15 & V2009<=29), FUN=svymean, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

##########################################################################