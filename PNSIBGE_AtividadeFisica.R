# Limpando dados armazenados na memoria de programa
rm(list=ls(all=TRUE))

# Aumentando memoria atribuida para execucao do programa
memory.limit(size=30000)

# Carregando pacotes necessarios para calculo das estimativas
library("PNSIBGE")
library("survey")

# Obtendo microdados e criando variaveis para calculo das estimativas
pns2019_moradorselecionado <- get_pns(year=2019, selected=TRUE)
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, Brasil=as.factor("Brasil"))
pns2019_moradorselecionado$variables <- transform(pns2019_moradorselecionado$variables, GR=as.factor(ifelse(substr(x=UPA_PNS,start=1,stop=1)=="1","Norte",ifelse(substr(x=UPA_PNS,start=1,stop=1)=="2","Nordeste",ifelse(substr(x=UPA_PNS,start=1,stop=1)=="3","Sudeste",ifelse(substr(x=UPA_PNS,start=1,stop=1)=="4","Sul",ifelse(substr(x=UPA_PNS,start=1,stop=1)=="5","Centro-oeste",NA)))))))
pns2019_moradorselecionado$variables$GR <- factor(pns2019_moradorselecionado$variables$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-oeste"))
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

# Calculando estimativa de pessoas de 15 anos ou mais insuficientemente ativos (SIDRA: 7740)
print(x=insuficientemente_ativo_total <- svybys(formula=~as.integer(((tempo_deslocamento_trabalho)+(tempo_lazer_semana)+(tempo_trabalho)+(tempo_deslocamento_atividade_habitual))<150), bys=~Brasil+GR, design=subset(pns2019_moradorselecionado, C008>=18 & C008<999), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
print(x=insuficientemente_ativo_percentual <- svybys(formula=~as.integer(((tempo_deslocamento_trabalho)+(tempo_lazer_semana)+(tempo_trabalho)+(tempo_deslocamento_atividade_habitual))<150), bys=~Brasil+GR, design=subset(pns2019_moradorselecionado, C008>=18 & C008<999), FUN=svymean, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

# Calculando estimativa de pessoas de 15 anos ou mais que praticam o nivel recomendado de atividade fisica no lazer (SIDRA: 4250)
print(x=atividade_fisica_lazer_total <- svybys(formula=~as.integer(tempo_lazer_semana>=150), bys=~Brasil+GR, design=subset(pns2019_moradorselecionado, C008>=18 & C008<999), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
print(x=atividade_fisica_lazer_percentual <- svybys(formula=~as.integer(tempo_lazer_semana>=150), bys=~Brasil+GR, design=subset(pns2019_moradorselecionado, C008>=18 & C008<999), FUN=svymean, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))