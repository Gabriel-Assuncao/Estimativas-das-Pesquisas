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

# Calculando estimativa de diagnostico de cancer exceto cancer de pele nao melanonico
print(x=total_cancer <- svybys(formula=~as.integer(Q12102=="Não" | Q12103=="Sim" | (Q12103%in%c("Não","Não sei") & (Q12104=="Sim" | Q12105=="Sim" | Q12106=="Sim" | Q12107=="Sim" | Q12108=="Sim" | Q12109=="Sim" | Q121010=="Sim" | Q121011=="Sim" | Q121012=="Sim" | Q121013=="Sim" | Q121014=="Sim" | Q121015=="Sim" | Q121016=="Sim"))), bys=~Brasil+GR, design=subset(pns2019_moradorselecionado, C008>=18), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))