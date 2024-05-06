# Limpando dados armazenados na memoria de programa
rm(list=ls(all=TRUE))

# Aumentando memoria atribuida para execucao do programa
memory.limit(size=30000)

# Carregando pacotes necessarios para calculo das estimativas
library("PNSIBGE")
library("survey")

# Obtendo microdados e criando variaveis para calculo das estimativas
pns2019 <- get_pns(year=2019)
pns2019$variables <- transform(pns2019$variables, Total=as.factor("Total"))
pns2019$variables <- transform(pns2019$variables, Deficiencia=as.factor(ifelse((G046=="Sim, muita dificuldade" | G046=="Sim, não consegue de modo algum" | G047=="Sim, muita dificuldade" | G047=="Sim, não consegue de modo algum" | G057=="Sim, muita dificuldade" | G057=="Sim, não consegue de modo algum" | G058=="Sim, muita dificuldade" | G058=="Sim, não consegue de modo algum" | G070=="Sim, muita dificuldade" | G070=="Sim, não consegue de modo algum" | G071=="Sim, muita dificuldade" | G071=="Sim, não consegue de modo algum" | G079=="Sim, muita dificuldade" | G079=="Sim, não consegue de modo algum" | G080=="Sim, muita dificuldade" | G080=="Sim, não consegue de modo algum" | G081=="Sim, muita dificuldade" | G081=="Sim, não consegue de modo algum" | G082=="Sim, muita dificuldade" | G082=="Sim, não consegue de modo algum" | G083=="Sim, muita dificuldade" | G083=="Sim, não consegue de modo algum"),"Sim",NA)))
pns2019$variables <- transform(pns2019$variables, Deficiencia=as.factor(ifelse(C008>=2 & is.na(Deficiencia),"Não",ifelse(C008>=2 & Deficiencia=="Sim","Sim",NA))))
pns2019$variables$Deficiencia <- factor(x=pns2019$variables$Deficiencia, levels=c("Sim","Não"))

# Calculando estimativas de total e proporcao de deficiencia por sexo (SIDRA: 6701) e cor ou raca (SIDRA: 8196)
print(x=est_def_tot <- svybys(formula=~as.integer(Deficiencia=="Sim"), bys=~Total+C006+C009, design=pns2019, FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
print(x=est_def_prp <- svybys(formula=~as.integer(Deficiencia=="Sim"), bys=~Total+C006+C009, design=pns2019, FUN=svymean, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))