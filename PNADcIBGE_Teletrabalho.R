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
pnadc_anual_trimestre <- PNADcIBGE::get_pnadc(year=2022, topic=4)

# Criando variável referente a lista de ocupações aptas ao teletrabalho
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, ocupacoes_teletrabalho=ifelse(V4010%in%c("0000","0110","0210","0411","0412","0511","0512","1111","1112","1113","1114","1120","1211","1212","1213","1219","1221","1222","1223","1311","1312","1321","1322","1323","1324","1330","1341","1342","1343","1344","1345","1346","1349","1411","1412","1420","1431","1439","2111","2112","2113","2114","2120","2131","2132","2133","2141","2142","2143","2144","2145","2146","2149","2151","2152","2153","2161","2162","2163","2164","2165","2166","2211","2212","2221","2222","2230","2240","2250","2261","2262","2263","2264","2265","2266","2267","2269","2310","2320","2330","2341","2342","2351","2352","2353","2354","2355","2356","2359","2411","2412","2413","2421","2422","2423","2424","2431","2432","2433","2434","2511","2512","2513","2514","2519","2521","2522","2523","2529","2611","2612","2619","2621","2622","2631","2632","2633","2634","2635","2636","2641","2642","2643","2651","2652","2653","2654","2655","2656","2659","3111","3112","3113","3114","3115","3116","3117","3118","3119","3121","3122","3123","3131","3132","3133","3134","3135","3139","3141","3142","3143","3151","3152","3153","3154","3155","3211","3212","3213","3214","3221","3222","3230","3240","3251","3252","3253","3254","3255","3256","3257","3258","3259","3311","3312","3313","3314","3315","3321","3322","3323","3324","3331","3332","3333","3334","3339","3341","3342","3343","3344","3351","3352","3353","3354","3355","3359","3411","3412","3413","3421","3422","3423","3431","3432","3433","3434","3435","3511","3512","3513","3514","3521","3522","4110","4120","4131","4132","4211","4212","4213","4214","4221","4222","4223","4224","4225","4226","4227","4229","4311","4312","4313","4321","4322","4323","4411","4412","4413","4414","4415","4416","4419","5113","5120","5141","5142","5151","5152","5153","5161","5163","5165","5168","5169","5211","5212","5221","5222","5223","5230","5241","5242","5243","5244","5246","5249","5312","5412","5414","6111","6112","6114","6121","6122","6123","6129","6130","6210","6221","7111","7115","7125","7231","7311","7313","7319","7321","7322","7412","7422","7512","7515","7522","7531","7543","8132","8141","8219","9121","9122","9411","9520","9999"),"Sim","Não"))

# Calculando estimativa do total de pessoas ocupadas (SIDRA - Tabela 9471)
print(x=pessoas_ocupadas <- survey::svytotal(x=~as.integer(V4001=="Sim" | V4002=="Sim" | V4003=="Sim" | V4004=="Sim"), design=subset(pnadc_anual_trimestre, V2009>=14 & V2009<999), na.rm=TRUE))
cv(object=pessoas_ocupadas)

# Calculando estimativa do total de pessoas que realizou trabalho remoto (SIDRA - Tabela 9471)
print(x=pessoas_trabalho_remoto <- survey::svytotal(x=~as.integer(((V4012%in%c("Trabalhador doméstico","Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar","Empregado do setor privado","Empregado do setor público (inclusive empresas de economia mista)") | V40121%in%c("Em ajuda a empregado","Em ajuda a trabalhador doméstico")) & ((S14001=="No domicílio de residência" | S14002=="Sim") | S14006=="Não")) | ((V4012%in%c("Empregador","Conta própria") | V40121=="Em ajuda a conta própria ou empregador") & ((S14001=="No domicílio de residência" & V4020=="Em loja, escritório, galpão, etc.") | (S14001=="Em loja, escritório, galpão etc. (do negócio/empresa onde trabalha ou de cliente)" & S14002=="Sim") | (S14001!="No domicílio de residência" & S14001!="Em loja, escritório, galpão etc. (do negócio/empresa onde trabalha ou de cliente)" & V4020=="Em loja, escritório, galpão, etc." & S14002=="Sim") | S14005=="Sim"))), design=subset(pnadc_anual_trimestre, V2009>=14 & V2009<999 & (V4001=="Sim" | V4002=="Sim" | V4003=="Sim" | V4004=="Sim")), na.rm=TRUE))
cv(object=pessoas_trabalho_remoto)

# Calculando estimativa do total de pessoas que realizou teletrabalho (SIDRA - Tabela 9471)
print(x=pessoas_teletrabalho <- survey::svytotal(x=~as.integer(ocupacoes_teletrabalho=="Sim" & (((V4012%in%c("Trabalhador doméstico","Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar","Empregado do setor privado","Empregado do setor público (inclusive empresas de economia mista)") | V40121%in%c("Em ajuda a empregado","Em ajuda a trabalhador doméstico")) & S14004=="Sim") | ((V4012%in%c("Empregador","Conta própria") | V40121=="Em ajuda a conta própria ou empregador") & S14004=="Sim" & (S14001=="Em loja, escritório, galpão etc. (do negócio/empresa onde trabalha ou de cliente)" | (S14001!="Em loja, escritório, galpão etc. (do negócio/empresa onde trabalha ou de cliente)" & V4020=="Em loja, escritório, galpão, etc.")))) | (ocupacoes_teletrabalho=="Sim" & S14008=="Sim")), design=subset(pnadc_anual_trimestre, V2009>=14 & V2009<999 & (V4001=="Sim" | V4002=="Sim" | V4003=="Sim" | V4004=="Sim")), na.rm=TRUE))
cv(object=pessoas_teletrabalho)

# Criando variável derivada de trabalhador plataformizado no trabalho principal caso seja necessário
pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, MS14001=as.factor(ifelse(S140091=="Sim" | S140092=="Sim" | (S140093=="Sim" & V4013%in%c("49030","49040","52020","53002")) | (S140093=="Sim" & V4013%in%c("48020","48030","48041","48042","48050","48060","48071","48072","48073","48074","48075","48076","48077","48078","48079","48080","48090","48100","56011","56012","56020") & (VD4008=="Empregador" | VD4008=="Conta-própria" | (V4012=="Trabalhador familiar não remunerado" & V40121=="Em ajuda a conta própria ou empregador"))) | S140094=="Sim","Sim",
                                                                                                       ifelse(S140091=="Não" & S140092=="Não" & (S140093=="Não" | (S140093=="Sim" & V4013%in%c("48020","48030","48041","48042","48050","48060","48071","48072","48073","48074","48075","48076","48077","48078","48079","48080","48090","48100","56011","56012","56020") & !(VD4008=="Empregador" | VD4008=="Conta-própria" | (V4012=="Trabalhador familiar não remunerado" & V40121=="Em ajuda a conta própria ou empregador")))) | S140094=="Não","Não",NA))))
pnadc_anual_trimestre$variables$MS14001 <- factor(x=pnadc_anual_trimestre$variables$MS14001, levels=c("Sim","Não"))
if(sum(is.na(pnadc_anual_trimestre$variables$SD14001)) == nrow(pnadc_anual_trimestre$variables))
{
  pnadc_anual_trimestre$variables <- transform(pnadc_anual_trimestre$variables, SD14001=as.factor(ifelse(is.na(MS14001),NA,as.character(MS14001))))
  pnadc_anual_trimestre$variables$SD14001 <- factor(x=pnadc_anual_trimestre$variables$SD14001, levels=c("Sim","Não"))
}

# Calculando estimativa do total de pessoas que realizou trabalho por meio de plataforma digital (SIDRA - Tabela 9432)
print(x=trabalho_plataforma_digital <- survey::svytotal(x=~as.integer(!is.na(SD14001))+SD14001, design=subset(pnadc_anual_trimestre, V2009>=14 & V2009<999), na.rm=TRUE))
cv(object=trabalho_plataforma_digital)

##########################################################################
