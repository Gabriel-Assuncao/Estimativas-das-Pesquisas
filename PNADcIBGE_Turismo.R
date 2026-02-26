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
ano <- 2023
{
  if(ano == 2019)
  {
    pnadc_anual_turismo <- PNADcIBGE::get_pnadc(year=ano, topic=3)
  }
  else
  {
    pnadc_anual_turismo <- PNADcIBGE::get_pnadc(year=ano, interview=2)
  }
}

# Criando variáveis auxiliares para cálculo da estimativa
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, Pais=as.factor("Brasil"))
pnadc_anual_turismo$variables$Pais <- factor(x=pnadc_anual_turismo$variables$Pais, levels=c("Brasil"))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, GR=as.factor(ifelse(substr(UPA, start=1, stop=1)=="1","Norte",ifelse(substr(UPA, start=1, stop=1)=="2","Nordeste",ifelse(substr(UPA, start=1, stop=1)=="3","Sudeste",ifelse(substr(UPA, start=1, stop=1)=="4","Sul",ifelse(substr(UPA, start=1, stop=1)=="5","Centro-Oeste",NA)))))))
pnadc_anual_turismo$variables$GR <- factor(x=pnadc_anual_turismo$variables$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))

# Calculando estimativa de domicílios por ocorrência de viagem (SIDRA - Tabela 7277)
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, domicilios=as.factor(ifelse(V2005=="Pessoa responsável pelo domicílio","Total","Não aplicável")))
pnadc_anual_turismo$variables$domicilios <- factor(x=pnadc_anual_turismo$variables$domicilios, levels=c("Total","Não aplicável"))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, ocorrencia_viagens=as.factor(ifelse(V2005=="Pessoa responsável pelo domicílio" & S08001=="Sim","Houve viagem",ifelse(V2005=="Pessoa responsável pelo domicílio" & S08001=="Não","Não houve viagem",NA))))
pnadc_anual_turismo$variables$ocorrencia_viagens <- factor(x=pnadc_anual_turismo$variables$ocorrencia_viagens, levels=c("Houve viagem","Não houve viagem"))
print(x=domicilios_viagens <- survey::svybys(formula=~as.integer(domicilios=="Total")+ocorrencia_viagens, bys=~Pais+GR+UF, design=subset(pnadc_anual_turismo, V2005=="Pessoa responsável pelo domicílio"), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

# Calculando estimativa de viagens realizadas pelos moradores dos domicílios (SIDRA - Tabela 7203)
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, viagens=as.integer(ifelse(S08002>=3,3,S08002)))
print(x=viagens_realizadas <- survey::svybys(formula=~viagens, bys=~Pais+GR+UF, design=subset(pnadc_anual_turismo, V2005=="Pessoa responsável pelo domicílio" & S08001=="Sim"), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

# Calculando estimativa de viagens nacionais realizadas pelos moradores dos domicílios (SIDRA - Tabela 7203)
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08005=as.factor(ifelse(is.na(S08005),"Não aplicável",as.character(S08005))))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, NS08005=as.integer(ifelse(MS08005=="Nacional",1,0)))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08018=as.factor(ifelse(is.na(S08018),"Não aplicável",as.character(S08018))))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, NS08018=as.integer(ifelse(MS08018=="Nacional",1,0)))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08031=as.factor(ifelse(is.na(S08031),"Não aplicável",as.character(S08031))))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, NS08031=as.integer(ifelse(MS08031=="Nacional",1,0)))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, nacionais=as.integer(NS08005+NS08018+NS08031))
print(x=viagens_nacionais <- survey::svybys(formula=~nacionais, bys=~Pais+GR+UF, design=subset(pnadc_anual_turismo, V2005=="Pessoa responsável pelo domicílio" & S08001=="Sim"), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

# Calculando estimativa de viagens com pernoite realizadas pelos moradores dos domicílios (SIDRA - Tabela 7203)
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08009=as.factor(ifelse(is.na(S08009),"Não aplicável",as.character(S08009))))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, NS08009=as.integer(ifelse(MS08009=="Sim",1,0)))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08022=as.factor(ifelse(is.na(S08022),"Não aplicável",as.character(S08022))))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, NS08022=as.integer(ifelse(MS08022=="Sim",1,0)))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08035=as.factor(ifelse(is.na(S08035),"Não aplicável",as.character(S08035))))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, NS08035=as.integer(ifelse(MS08035=="Sim",1,0)))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, pernoites=as.integer(NS08009+NS08022+NS08035))
print(x=viagens_pernoites <- survey::svybys(formula=~pernoites, bys=~Pais+GR+UF, design=subset(pnadc_anual_turismo, V2005=="Pessoa responsável pelo domicílio" & S08001=="Sim"), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

# Calculando estimativa de viagens nacionais com pernoite realizadas pelos moradores dos domicílios (SIDRA - Tabela 7203)
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08005=as.factor(ifelse(is.na(S08005),"Não aplicável",as.character(S08005))))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08009=as.factor(ifelse(is.na(S08009),"Não aplicável",as.character(S08009))))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, NS08005_NS08009=as.integer(ifelse(MS08005=="Nacional" & MS08009=="Sim",1,0)))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08018=as.factor(ifelse(is.na(S08018),"Não aplicável",as.character(S08018))))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08022=as.factor(ifelse(is.na(S08022),"Não aplicável",as.character(S08022))))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, NS08018_NS08022=as.integer(ifelse(MS08018=="Nacional" & MS08022=="Sim",1,0)))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08031=as.factor(ifelse(is.na(S08031),"Não aplicável",as.character(S08031))))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08035=as.factor(ifelse(is.na(S08035),"Não aplicável",as.character(S08035))))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, NS08031_NS08035=as.integer(ifelse(MS08031=="Nacional" & MS08035=="Sim",1,0)))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, nacionais_pernoites=as.integer(NS08005_NS08009+NS08018_NS08022+NS08031_NS08035))
print(x=viagens_nacionais_pernoites <- survey::svybys(formula=~nacionais_pernoites, bys=~Pais+GR+UF, design=subset(pnadc_anual_turismo, V2005=="Pessoa responsável pelo domicílio" & S08001=="Sim"), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

# Calculando estimativa de viagens realizadas pela quantidade de moradores participantes na viagem (SIDRA - Tabela 7237)
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, ignorado_primeira_viagem=ifelse(S0801001=="Ignorado" | S0801002=="Ignorado" | S0801003=="Ignorado" | S0801004=="Ignorado" | S0801005=="Ignorado" | S0801006=="Ignorado" | S0801007=="Ignorado" | S0801008=="Ignorado" | S0801009=="Ignorado" | S0801010=="Ignorado" |
                                                                                                            S0801011=="Ignorado" | S0801012=="Ignorado" | S0801013=="Ignorado" | S0801014=="Ignorado" | S0801015=="Ignorado" | S0801016=="Ignorado" | S0801017=="Ignorado" | S0801018=="Ignorado" | S0801019=="Ignorado" | S0801020=="Ignorado" |
                                                                                                            S0801021=="Ignorado" | S0801022=="Ignorado" | S0801023=="Ignorado" | S0801024=="Ignorado" | S0801025=="Ignorado" | S0801026=="Ignorado" | S0801027=="Ignorado" | S0801028=="Ignorado" | S0801029=="Ignorado" | S0801030=="Ignorado","Sim",NA))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, NS08010=ifelse(is.na(S08005),NA,ifelse(!is.na(ignorado_primeira_viagem) & ignorado_primeira_viagem=="Sim",99,as.numeric(ifelse(is.na(S0801001),0,ifelse(S0801001=="Sim",1,0))+ifelse(is.na(S0801002),0,ifelse(S0801002=="Sim",1,0))+ifelse(is.na(S0801003),0,ifelse(S0801003=="Sim",1,0))+ifelse(is.na(S0801004),0,ifelse(S0801004=="Sim",1,0))+ifelse(is.na(S0801005),0,ifelse(S0801005=="Sim",1,0))+ifelse(is.na(S0801006),0,ifelse(S0801006=="Sim",1,0))+ifelse(is.na(S0801007),0,ifelse(S0801007=="Sim",1,0))+ifelse(is.na(S0801008),0,ifelse(S0801008=="Sim",1,0))+ifelse(is.na(S0801009),0,ifelse(S0801009=="Sim",1,0))+ifelse(is.na(S0801010),0,ifelse(S0801010=="Sim",1,0))+
                                                                                                                                                                           ifelse(is.na(S0801011),0,ifelse(S0801011=="Sim",1,0))+ifelse(is.na(S0801012),0,ifelse(S0801012=="Sim",1,0))+ifelse(is.na(S0801013),0,ifelse(S0801013=="Sim",1,0))+ifelse(is.na(S0801014),0,ifelse(S0801014=="Sim",1,0))+ifelse(is.na(S0801015),0,ifelse(S0801015=="Sim",1,0))+ifelse(is.na(S0801016),0,ifelse(S0801016=="Sim",1,0))+ifelse(is.na(S0801017),0,ifelse(S0801017=="Sim",1,0))+ifelse(is.na(S0801018),0,ifelse(S0801018=="Sim",1,0))+ifelse(is.na(S0801019),0,ifelse(S0801019=="Sim",1,0))+ifelse(is.na(S0801020),0,ifelse(S0801020=="Sim",1,0))+
                                                                                                                                                                           ifelse(is.na(S0801021),0,ifelse(S0801021=="Sim",1,0))+ifelse(is.na(S0801022),0,ifelse(S0801022=="Sim",1,0))+ifelse(is.na(S0801023),0,ifelse(S0801023=="Sim",1,0))+ifelse(is.na(S0801024),0,ifelse(S0801024=="Sim",1,0))+ifelse(is.na(S0801025),0,ifelse(S0801025=="Sim",1,0))+ifelse(is.na(S0801026),0,ifelse(S0801026=="Sim",1,0))+ifelse(is.na(S0801027),0,ifelse(S0801027=="Sim",1,0))+ifelse(is.na(S0801028),0,ifelse(S0801028=="Sim",1,0))+ifelse(is.na(S0801029),0,ifelse(S0801029=="Sim",1,0))+ifelse(is.na(S0801030),0,ifelse(S0801030=="Sim",1,0))))))
if(sum(is.na(pnadc_anual_turismo$variables$SD08001)) == nrow(pnadc_anual_turismo$variables) | is.null(pnadc_anual_turismo$variables$SD08001))
{
  pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, SD08001=as.numeric(ifelse(is.na(NS08010),NA,as.numeric(NS08010))))
}
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08010=as.factor(ifelse(is.na(S08005),NA,ifelse(SD08001==1,"1 morador",ifelse(SD08001>=2 & SD08001<=3,"2 ou 3 moradores",ifelse(SD08001>=4 & SD08001<=5,"4 ou 5 moradores",ifelse(SD08001>=6 & SD08001<99,"6 ou mais moradores","Sem declaração")))))))
pnadc_anual_turismo$variables$MS08010 <- factor(x=pnadc_anual_turismo$variables$MS08010, levels=c("1 morador","2 ou 3 moradores","4 ou 5 moradores","6 ou mais moradores","Sem declaração"))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, ignorado_segunda_viagem=ifelse(S0802301=="Ignorado" | S0802302=="Ignorado" | S0802303=="Ignorado" | S0802304=="Ignorado" | S0802305=="Ignorado" | S0802306=="Ignorado" | S0802307=="Ignorado" | S0802308=="Ignorado" | S0802309=="Ignorado" | S0802310=="Ignorado" |
                                                                                                           S0802311=="Ignorado" | S0802312=="Ignorado" | S0802313=="Ignorado" | S0802314=="Ignorado" | S0802315=="Ignorado" | S0802316=="Ignorado" | S0802317=="Ignorado" | S0802318=="Ignorado" | S0802319=="Ignorado" | S0802320=="Ignorado" |
                                                                                                           S0802321=="Ignorado" | S0802322=="Ignorado" | S0802323=="Ignorado" | S0802324=="Ignorado" | S0802325=="Ignorado" | S0802326=="Ignorado" | S0802327=="Ignorado" | S0802328=="Ignorado" | S0802329=="Ignorado" | S0802330=="Ignorado","Sim",NA))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, NS08023=ifelse(is.na(S08018),NA,ifelse(!is.na(ignorado_segunda_viagem) & ignorado_segunda_viagem=="Sim",99,as.numeric(ifelse(is.na(S0802301),0,ifelse(S0802301=="Sim",1,0))+ifelse(is.na(S0802302),0,ifelse(S0802302=="Sim",1,0))+ifelse(is.na(S0802303),0,ifelse(S0802303=="Sim",1,0))+ifelse(is.na(S0802304),0,ifelse(S0802304=="Sim",1,0))+ifelse(is.na(S0802305),0,ifelse(S0802305=="Sim",1,0))+ifelse(is.na(S0802306),0,ifelse(S0802306=="Sim",1,0))+ifelse(is.na(S0802307),0,ifelse(S0802307=="Sim",1,0))+ifelse(is.na(S0802308),0,ifelse(S0802308=="Sim",1,0))+ifelse(is.na(S0802309),0,ifelse(S0802309=="Sim",1,0))+ifelse(is.na(S0802310),0,ifelse(S0802310=="Sim",1,0))+
                                                                                                                                                                         ifelse(is.na(S0802311),0,ifelse(S0802311=="Sim",1,0))+ifelse(is.na(S0802312),0,ifelse(S0802312=="Sim",1,0))+ifelse(is.na(S0802313),0,ifelse(S0802313=="Sim",1,0))+ifelse(is.na(S0802314),0,ifelse(S0802314=="Sim",1,0))+ifelse(is.na(S0802315),0,ifelse(S0802315=="Sim",1,0))+ifelse(is.na(S0802316),0,ifelse(S0802316=="Sim",1,0))+ifelse(is.na(S0802317),0,ifelse(S0802317=="Sim",1,0))+ifelse(is.na(S0802318),0,ifelse(S0802318=="Sim",1,0))+ifelse(is.na(S0802319),0,ifelse(S0802319=="Sim",1,0))+ifelse(is.na(S0802320),0,ifelse(S0802320=="Sim",1,0))+
                                                                                                                                                                         ifelse(is.na(S0802321),0,ifelse(S0802321=="Sim",1,0))+ifelse(is.na(S0802322),0,ifelse(S0802322=="Sim",1,0))+ifelse(is.na(S0802323),0,ifelse(S0802323=="Sim",1,0))+ifelse(is.na(S0802324),0,ifelse(S0802324=="Sim",1,0))+ifelse(is.na(S0802325),0,ifelse(S0802325=="Sim",1,0))+ifelse(is.na(S0802326),0,ifelse(S0802326=="Sim",1,0))+ifelse(is.na(S0802327),0,ifelse(S0802327=="Sim",1,0))+ifelse(is.na(S0802328),0,ifelse(S0802328=="Sim",1,0))+ifelse(is.na(S0802329),0,ifelse(S0802329=="Sim",1,0))+ifelse(is.na(S0802330),0,ifelse(S0802330=="Sim",1,0))))))
if(sum(is.na(pnadc_anual_turismo$variables$SD08002)) == nrow(pnadc_anual_turismo$variables) | is.null(pnadc_anual_turismo$variables$SD08002))
{
  pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, SD08002=as.numeric(ifelse(is.na(NS08023),NA,as.numeric(NS08023))))
}
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08023=as.factor(ifelse(is.na(S08018),NA,ifelse(SD08002==1,"1 morador",ifelse(SD08002>=2 & SD08002<=3,"2 ou 3 moradores",ifelse(SD08002>=4 & SD08002<=5,"4 ou 5 moradores",ifelse(SD08002>=6 & SD08002<99,"6 ou mais moradores","Sem declaração")))))))
pnadc_anual_turismo$variables$MS08023 <- factor(x=pnadc_anual_turismo$variables$MS08023, levels=c("1 morador","2 ou 3 moradores","4 ou 5 moradores","6 ou mais moradores","Sem declaração"))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, ignorado_terceira_viagem=ifelse(S0803601=="Ignorado" | S0803602=="Ignorado" | S0803603=="Ignorado" | S0803604=="Ignorado" | S0803605=="Ignorado" | S0803606=="Ignorado" | S0803607=="Ignorado" | S0803608=="Ignorado" | S0803609=="Ignorado" | S0803610=="Ignorado" |
                                                                                                            S0803611=="Ignorado" | S0803612=="Ignorado" | S0803613=="Ignorado" | S0803614=="Ignorado" | S0803615=="Ignorado" | S0803616=="Ignorado" | S0803617=="Ignorado" | S0803618=="Ignorado" | S0803619=="Ignorado" | S0803620=="Ignorado" |
                                                                                                            S0803621=="Ignorado" | S0803622=="Ignorado" | S0803623=="Ignorado" | S0803624=="Ignorado" | S0803625=="Ignorado" | S0803626=="Ignorado" | S0803627=="Ignorado" | S0803628=="Ignorado" | S0803629=="Ignorado" | S0803630=="Ignorado","Sim",NA))
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, NS08036=ifelse(is.na(S08031),NA,ifelse(!is.na(ignorado_terceira_viagem) & ignorado_terceira_viagem=="Sim",99,as.numeric(ifelse(is.na(S0803601),0,ifelse(S0803601=="Sim",1,0))+ifelse(is.na(S0803602),0,ifelse(S0803602=="Sim",1,0))+ifelse(is.na(S0803603),0,ifelse(S0803603=="Sim",1,0))+ifelse(is.na(S0803604),0,ifelse(S0803604=="Sim",1,0))+ifelse(is.na(S0803605),0,ifelse(S0803605=="Sim",1,0))+ifelse(is.na(S0803606),0,ifelse(S0803606=="Sim",1,0))+ifelse(is.na(S0803607),0,ifelse(S0803607=="Sim",1,0))+ifelse(is.na(S0803608),0,ifelse(S0803608=="Sim",1,0))+ifelse(is.na(S0803609),0,ifelse(S0803609=="Sim",1,0))+ifelse(is.na(S0803610),0,ifelse(S0803610=="Sim",1,0))+
                                                                                                                                                                           ifelse(is.na(S0803611),0,ifelse(S0803611=="Sim",1,0))+ifelse(is.na(S0803612),0,ifelse(S0803612=="Sim",1,0))+ifelse(is.na(S0803613),0,ifelse(S0803613=="Sim",1,0))+ifelse(is.na(S0803614),0,ifelse(S0803614=="Sim",1,0))+ifelse(is.na(S0803615),0,ifelse(S0803615=="Sim",1,0))+ifelse(is.na(S0803616),0,ifelse(S0803616=="Sim",1,0))+ifelse(is.na(S0803617),0,ifelse(S0803617=="Sim",1,0))+ifelse(is.na(S0803618),0,ifelse(S0803618=="Sim",1,0))+ifelse(is.na(S0803619),0,ifelse(S0803619=="Sim",1,0))+ifelse(is.na(S0803620),0,ifelse(S0803620=="Sim",1,0))+
                                                                                                                                                                           ifelse(is.na(S0803621),0,ifelse(S0803621=="Sim",1,0))+ifelse(is.na(S0803622),0,ifelse(S0803622=="Sim",1,0))+ifelse(is.na(S0803623),0,ifelse(S0803623=="Sim",1,0))+ifelse(is.na(S0803624),0,ifelse(S0803624=="Sim",1,0))+ifelse(is.na(S0803625),0,ifelse(S0803625=="Sim",1,0))+ifelse(is.na(S0803626),0,ifelse(S0803626=="Sim",1,0))+ifelse(is.na(S0803627),0,ifelse(S0803627=="Sim",1,0))+ifelse(is.na(S0803628),0,ifelse(S0803628=="Sim",1,0))+ifelse(is.na(S0803629),0,ifelse(S0803629=="Sim",1,0))+ifelse(is.na(S0803630),0,ifelse(S0803630=="Sim",1,0))))))
if(sum(is.na(pnadc_anual_turismo$variables$SD08003)) == nrow(pnadc_anual_turismo$variables) | is.null(pnadc_anual_turismo$variables$SD08003))
{
  pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, SD08003=as.numeric(ifelse(is.na(NS08036),NA,as.numeric(NS08036))))
}
pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, MS08036=as.factor(ifelse(is.na(S08031),NA,ifelse(SD08003==1,"1 morador",ifelse(SD08003>=2 & SD08003<=3,"2 ou 3 moradores",ifelse(SD08003>=4 & SD08003<=5,"4 ou 5 moradores",ifelse(SD08003>=6 & SD08003<99,"6 ou mais moradores","Sem declaração")))))))
pnadc_anual_turismo$variables$MS08036 <- factor(x=pnadc_anual_turismo$variables$MS08036, levels=c("1 morador","2 ou 3 moradores","4 ou 5 moradores","6 ou mais moradores","Sem declaração"))
print(x=viagens_moradores_participantes <- list(survey::svyby(formula=~as.integer(S08001=="Sim"), by=~MS08010, design=subset(pnadc_anual_turismo, V2005=="Pessoa responsável pelo domicílio" & S08002>=1), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE), survey::svyby(formula=~as.integer(S08001=="Sim"), by=~MS08023, design=subset(pnadc_anual_turismo, V2005=="Pessoa responsável pelo domicílio" & S08002>=2), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE), survey::svyby(formula=~as.integer(S08001=="Sim"), by=~MS08036, design=subset(pnadc_anual_turismo, V2005=="Pessoa responsável pelo domicílio" & S08002>=3), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE)))
data.frame(quantidade_moradores=as.character(viagens_moradores_participantes[[1]]$MS08010), viagens_realizadas=as.numeric(viagens_moradores_participantes[[1]]$`as.integer(S08001 == "Sim")`+viagens_moradores_participantes[[2]]$`as.integer(S08001 == "Sim")`+viagens_moradores_participantes[[3]]$`as.integer(S08001 == "Sim")`))

# Calculando estimativa do gasto total de viagens nacionais com pernoite realizadas pelos moradores dos domicílios (SIDRA - Tabela 8470)
if(ano > 2019)
{
  pnadc_anual_turismo$variables <- transform(pnadc_anual_turismo$variables, gasto_total=as.numeric(ifelse(!is.na(S08005) & !is.na(S08009) & !is.na(S08017A7) & S08005=="Nacional" & S08009=="Sim" & S08017A7=="Sim",S08017A72*CO2,0)+ifelse(!is.na(S08018) & !is.na(S08022) & !is.na(S08030A7) & S08018=="Nacional" & S08022=="Sim" & S08030A7=="Sim",S08030A72*CO2,0)+ifelse(!is.na(S08031) & !is.na(S08035) & !is.na(S08043A7) & S08031=="Nacional" & S08035=="Sim" & S08043A7=="Sim",S08043A72*CO2,0)))
  print(x=viagens_gasto_total <- survey::svybys(formula=~gasto_total, bys=~Pais+GR+UF, design=subset(pnadc_anual_turismo, V2005=="Pessoa responsável pelo domicílio" & S08001=="Sim"), FUN=svytotal, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
}

##########################################################################
