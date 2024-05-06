# Limpando dados armazenados na memoria de programa
rm(list=ls(all=TRUE))

# Aumentando memoria atribuida para execucao do programa
memory.limit(size=30000)

# Carregando pacotes necessarios para calculo das estimativas
library("PNADcIBGE")
library("survey")

# Obtendo microdados do periodo de referencia para calculo das estimativas
pnadc_trimestral <- get_pnadc(year=2022, quarter=2)
pnadc_trimestral$variables <- transform(pnadc_trimestral$variables, informalidade=as.factor(ifelse(is.na(VD4009),NA,ifelse(VD4009=="Empregado no setor privado sem carteira de trabalho assinada"|VD4009=="Trabalhador doméstico sem carteira de trabalho assinada"|(VD4009=="Empregador"&V4019=="Não")|(VD4009=="Conta-própria"&V4019=="Não")|VD4009=="Trabalhador familiar auxiliar","Pessoas na informalidade","Pessoas na formalidade"))))

# Calculando estimativa da taxa de informalidade
print(x=taxa_informalidade <- svyratio(numerator=~(informalidade=="Pessoas na informalidade"), denominator=~(VD4002=="Pessoas ocupadas"), design=pnadc_trimestral, na.rm=TRUE))
cv(object=taxa_informalidade)

# Calculando estimativa do total de pessoas na informalidade
print(x=pessoas_informalidade <- svytotal(x=~as.integer(informalidade=="Pessoas na informalidade"), design=pnadc_trimestral, na.rm=TRUE))
cv(object=pessoas_informalidade)