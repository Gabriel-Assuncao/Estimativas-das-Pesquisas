# Limpando dados armazenados na memoria de programa
rm(list=ls(all=TRUE))

# Aumentando memoria atribuida para execucao do programa
memory.limit(size=30000)

# Carregando pacotes necessarios para calculo das estimativas
library("PNADcIBGE")
library("survey")

# Obtendo microdados do periodo de referencia para calculo das estimativas
variaveis_selecionadas <- c("V1022","V2005",sprintf("S170%02d", seq(1:14)),"SD17001")
pnadc_anual_trimestre <- get_pnadc(year=2023, topic=4, vars=variaveis_selecionadas)

# Criando variaveis auxiliares para calculo das estimativas
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, Pais=as.factor("Brasil"))
pnadc_anual_trimestre$variables$Pais <- factor(x=pnadc_anual_trimestre$variables$Pais, levels=c("Brasil"))
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, GR=as.factor(ifelse(substr(UPA, start=1, stop=1)=="1","Norte",ifelse(substr(UPA, start=1, stop=1)=="2","Nordeste",ifelse(substr(UPA, start=1, stop=1)=="3","Sudeste",ifelse(substr(UPA, start=1, stop=1)=="4","Sul",ifelse(substr(UPA, start=1, stop=1)=="5","Centro-Oeste",NA)))))))
pnadc_anual_trimestre$variables$GR <- factor(x=pnadc_anual_trimestre$variables$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))

# Calculando proporcao de domicilios de acordo com seguranca alimentar
print(x=proporcao_seguranca_alimentar <- svybys(formula=~SD17001, bys=~Pais+GR+V1022, design=subset(pnadc_anual_trimestre, V2005=="Pessoa responsável pelo domicílio"), FUN=svymean, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

# Calculando total de domicilios de acordo com seguranca alimentar
print(x=total_seguranca_alimentar <- svybys(formula=~SD17001, bys=~Pais+GR+V1022, design=subset(pnadc_anual_trimestre, V2005=="Pessoa responsável pelo domicílio"), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))