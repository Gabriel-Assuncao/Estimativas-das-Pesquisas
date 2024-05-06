# Limpando dados armazenados na memoria de programa
rm(list=ls(all=TRUE))

# Aumentando memoria atribuida para execucao do programa
memory.limit(size=30000)

# Carregando pacotes necessarios para calculo das estimativas
library("PNADcIBGE")
library("survey")

# Obtendo microdados do periodo de referencia para calculo das estimativas
ano <- 2018
pnadc_anual_trimestre <- get_pnadc(year=ano, topic=2)

# Realizando a criacao das variaveis de abertura para calculo das estimativas
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, brasil=as.factor("Brasil"))
pnadc_anual_trimestre$variables$brasil <- factor(x=pnadc_anual_trimestre$variables$brasil, levels=c("Brasil"))
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, grande_regiao=as.factor(ifelse(substr(UPA, start=1, stop=1)=="1","Norte",ifelse(substr(UPA, start=1, stop=1)=="2","Nordeste",ifelse(substr(UPA, start=1, stop=1)=="3","Sudeste",ifelse(substr(UPA, start=1, stop=1)=="4","Sul",ifelse(substr(UPA, start=1, stop=1)=="5","Centro-Oeste",NA)))))))
pnadc_anual_trimestre$variables$grande_regiao <- factor(x=pnadc_anual_trimestre$variables$grande_regiao, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, grupo_idade=as.factor(ifelse(V2009>=15 & V2009<=17,"15 a 17 anos",ifelse(V2009>=18 & V2009<=24,"18 a 24 anos",ifelse(V2009>=25 & V2009<=29,"25 a 29 anos",NA)))))
pnadc_anual_trimestre$variables$grupo_idade <- factor(x=pnadc_anual_trimestre$variables$grupo_idade, levels=c("15 a 17 anos","18 a 24 anos","25 a 29 anos"))
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, sexo=as.factor(ifelse(V2007=="Homem","Homens",ifelse(V2007=="Mulher","Mulheres",NA))))
pnadc_anual_trimestre$variables$sexo <- factor(x=pnadc_anual_trimestre$variables$sexo, levels=c("Homens","Mulheres"))
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, cor_raca=as.factor(ifelse(V2010=="Branca","Branca",ifelse(V2010=="Preta" | V2010=="Parda","Preta ou parda",NA))))
pnadc_anual_trimestre$variables$cor_raca <- factor(x=pnadc_anual_trimestre$variables$cor_raca, levels=c("Branca","Preta ou parda"))

# Realizando a criacao das variaveis de educacao com as adaptacoes necessarias
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

# Realizando a criacao da variavel de condicao de ocupacao e estudo
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

# Calculando as estimativas de jovens que nem estudam e nem trabalham
print(x=total_nemnem <- svybys(formula=~as.integer(condicao_ocupacao_estudo=="Não ocupadas e não frequentando escola, nem cursos pré-vestibular, técnico de nível médio, normal (magistério) ou qualificação profissional"), bys=~brasil+grande_regiao+grupo_idade+sexo+cor_raca, design=subset(pnadc_anual_trimestre, V2009>=15 & V2009<=29), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
print(x=proporcao_nemnem <- svybys(formula=~as.integer(condicao_ocupacao_estudo=="Não ocupadas e não frequentando escola, nem cursos pré-vestibular, técnico de nível médio, normal (magistério) ou qualificação profissional"), bys=~brasil+grande_regiao+grupo_idade+sexo+cor_raca, design=subset(pnadc_anual_trimestre, V2009>=15 & V2009<=29), FUN=svymean, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))