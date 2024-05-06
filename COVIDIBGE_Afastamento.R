# Limpando dados armazenados na memoria de programa
rm(list=ls(all=TRUE))

# Aumentando memoria atribuida para execucao do programa
memory.limit(size=30000)

# Carregando pacotes necessarios para calculo das estimativas
library("COVIDIBGE")
library("survey")

# Obtendo microdados e criando variaveis para calculo das estimativas
pnad_covid <- get_covid(year=2020, month=11)
pnad_covid$variables <- transform(pnad_covid$variables, MC001=as.factor(ifelse(is.na(C001),"Não aplicável",as.character(C001))))
pnad_covid$variables <- transform(pnad_covid$variables, MC002=as.factor(ifelse(is.na(C002),"Não aplicável",as.character(C002))))
pnad_covid$variables <- transform(pnad_covid$variables, MC003=as.factor(ifelse(is.na(C003),"Não aplicável",as.character(C003))))
pnad_covid$variables <- transform(pnad_covid$variables, MC004=as.factor(ifelse(is.na(C004),"Não aplicável",as.character(C004))))
pnad_covid$variables <- transform(pnad_covid$variables, MC007=as.factor(ifelse(is.na(C007),"Não aplicável",as.character(C007))))

# Calculando estimativa da taxa de afastamento das pessoas por quarentena
print(x=taxa_afst_dist <- svyby(formula=~(A002>=14 & MC003=="Estava em quarentena, isolamento, distanciamento social ou férias coletivas" & MC007%in%c("Trabalhador doméstico (empregado doméstico, cuidados, babá)","Militar do exercito, marinha ou aeronáutica","Policial militar ou bombeiro militar","Empregado do setor privado","Empregado do setor público (inclusive empresas de economia mista)","Empregador","Conta própria","Trabalhador familiar não remunerado em ajuda a membro do domicílio ou parente")), denominator=~(A002>=14 & MC007%in%c("Trabalhador doméstico (empregado doméstico, cuidados, babá)","Militar do exercito, marinha ou aeronáutica","Policial militar ou bombeiro militar","Empregado do setor privado","Empregado do setor público (inclusive empresas de economia mista)","Empregador","Conta própria","Trabalhador familiar não remunerado em ajuda a membro do domicílio ou parente")), by=~UF, design=pnad_covid, FUN=svyratio, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))