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

# Criando variáveis de tempo relacionadas à atividade física e lazer
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, NP04001=ifelse(is.na(P04001) | P04001==9,0,as.integer(P04001)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, NP04101=ifelse(is.na(P04101) | P04101==99,0,as.integer(P04101)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, NP04102=ifelse(is.na(P04102) | P04102==99,0,as.integer(P04102)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, tempo_deslocamento_trabalho=NP04001*(NP04101*60+NP04102))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, NP035=ifelse(is.na(P035) | P035==9,0,as.integer(P035)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, NP03701=ifelse(is.na(P03701) | P03701==99,0,as.integer(P03701)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, NP03702=ifelse(is.na(P03702) | P03702==99,0,as.integer(P03702)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, tempo_lazer_semana=ifelse(is.na(P036),0,ifelse(P036=="Caminhada (não vale para o trabalho)" | P036=="Caminhada em esteira" | P036=="Hidroginástica" | P036=="Ginástica / localizada/pilates/alongamento/ioga" | P036=="Natação" | P036=="Artes marciais e luta" | P036=="Bicicleta/bicicleta ergométrica" | P036=="Voleibol" | P036=="Dança (com o objetivo de praticar atividade física)" | P036=="Outro" | P036=="Ignorado",NP035*(NP03701*60+NP03702),ifelse(P036=="Corrida/cooper" | P036=="Corrida em esteira" | P036=="Musculação" | P036=="Ginástica aeróbica/spinning/step/jump" | P036=="Futebol" | P036=="Basquetebol" | P036=="Tênis",2*NP035*(NP03701*60+NP03702),0))))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, NP03904=ifelse(is.na(P03904) | P03904==9,0,as.integer(P03904)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, NP03905=ifelse(is.na(P03905) | P03905==99,0,as.integer(P03905)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, NP03906=ifelse(is.na(P03906) | P03906==99,0,as.integer(P03906)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, tempo_trabalho=NP03904*(NP03905*60+NP03906))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, NP042=ifelse(is.na(P042) | P042==9,0,as.integer(P042)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, NP04301=ifelse(is.na(P04301) | P04301==99,0,as.integer(P04301)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, NP04302=ifelse(is.na(P04302) | P04302==99,0,as.integer(P04302)))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, tempo_deslocamento_atividade_habitual=NP042*(NP04301*60+NP04302))

# Calculando estimativa de pessoas de 18 anos ou mais insuficientemente ativos (SIDRA - Tabela 7740)
print(x=total_insuficientemente_ativo <- survey::svybys(formula=~as.integer(((tempo_deslocamento_trabalho)+(tempo_lazer_semana)+(tempo_trabalho)+(tempo_deslocamento_atividade_habitual))<150), bys=~Pais+GR, design=subset(pns2019_moradorselecionado, C008>=18 & C008<999), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
print(x=proporcao_insuficientemente_ativo <- survey::svybys(formula=~as.integer(((tempo_deslocamento_trabalho)+(tempo_lazer_semana)+(tempo_trabalho)+(tempo_deslocamento_atividade_habitual))<150), bys=~Pais+GR, design=subset(pns2019_moradorselecionado, C008>=18 & C008<999), FUN=svymean, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

# Calculando estimativa de pessoas de 18 anos ou mais que praticam o nível recomendado de atividade física no lazer (SIDRA - Tabela 4250)
print(x=total_atividade_fisica_lazer <- survey::svybys(formula=~as.integer(tempo_lazer_semana>=150), bys=~Pais+GR, design=subset(pns2019_moradorselecionado, C008>=18 & C008<999), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
print(x=proporcao_atividade_fisica_lazer <- survey::svybys(formula=~as.integer(tempo_lazer_semana>=150), bys=~Pais+GR, design=subset(pns2019_moradorselecionado, C008>=18 & C008<999), FUN=svymean, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

##########################################################################