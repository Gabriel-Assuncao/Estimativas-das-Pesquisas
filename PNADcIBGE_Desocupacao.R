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
if("plot.matrix" %in% rownames(installed.packages())==FALSE)
{
  install.packages(pkgs="plot.matrix", dependencies=TRUE)
}
library(package="plot.matrix", verbose=TRUE)

# Definindo intervalo de período dos microdados para cálculo da estimativa
indice_ano <- c(2012:2024)
indice_trimestre <- c(1:4)

# Criando matriz de referência onde estimativa dos períodos será armazenada
taxa_desocupacao <- matrix(data=NA, nrow=length(indice_ano), ncol=length(indice_trimestre))
rownames(taxa_desocupacao) <- indice_ano
colnames(taxa_desocupacao) <- indice_trimestre

# Realizando processo de cálculo da taxa de desocupação para períodos indicados
for(ano in indice_ano)
{
  for(trimestre in indice_trimestre)
  {
    pnadc_trimestral <- PNADcIBGE::get_pnadc(year=ano, quarter=trimestre)
    if(!is.null(pnadc_trimestral))
    {
      taxa_desocupacao[as.character(ano),as.character(trimestre)] <- round(survey::svyratio(numerator=~(VD4002=="Pessoas desocupadas"), denominator=~(VD4001=="Pessoas na força de trabalho"), design=subset(pnadc_trimestral, V2009>=14 & V2009<999), na.rm=TRUE)[[1]][1]*100, digits=1)
    }
    rm(pnadc_trimestral)
    gc(verbose=FALSE, reset=FALSE, full=TRUE)
  }
}

# Observando resultado obtido da estimativa para períodos analisados (SIDRA - Tabela 4099)
print(x=taxa_desocupacao)

# Traçando gráfico da matriz de resultados da taxa de desocupação
par(mar=c(5.1, 4.1, 4.1, 4.1))
plot(x=t(taxa_desocupacao[,c(ncol(taxa_desocupacao):1)]), breaks=5, col=rev(heat.colors(5)), digits=1, fmt.cell="%.1f", fmt.key="%.1f", text.cell=list(cex=1), key=list(side=4, las=1), axis.col=list(side=1, las=2), axis.row=list(side=2, las=1), main="Taxa de Desocupação", xlab="Ano", ylab="Trimestre")

##########################################################################