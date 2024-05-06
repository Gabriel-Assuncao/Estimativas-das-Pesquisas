# Limpando dados armazenados na memoria de programa
rm(list=ls(all=TRUE))

# Aumentando memoria atribuida para execucao do programa
memory.limit(size=30000)

# Carregando pacotes necessarios para calculo das estimativas
library("PNADcIBGE")
library("survey")

# Obtendo microdados do periodo de referencia para calculo das estimativas
ano <- 2019
pnadc_anual_trimestre <- get_pnadc(year=ano, topic=2)

# Realizando a criacao das variaveis de educacao com as adaptacoes necessarias
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, MV3003A=as.factor(ifelse(is.na(V3003A),"Não aplicável",as.character(V3003A))))
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, MVD3004=as.factor(ifelse(is.na(VD3004),"Não aplicável",as.character(VD3004))))
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, frequencia_escolar=ifelse(MV3003A=="Regular do ensino fundamental" | MVD3004=="Fundamental completo ou equivalente" | MVD3004=="Médio incompleto ou equivalente" | MVD3004=="Médio completo ou equivalente" | MVD3004=="Superior incompleto ou equivalente" | MVD3004=="Superior completo","Frequente","Infrequente"))

# Calculando a estimativa da taxa ajustada de frequencia escolar liquida
svybys(formula=~as.integer(frequencia_escolar=="Frequente"), bys=~Pais+GR, design=subset(pnadc2019, V2009>=6 & V2009<=14), FUN=svymean, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE)