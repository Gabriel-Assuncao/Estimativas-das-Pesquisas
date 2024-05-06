# Limpando dados armazenados na memoria de programa
rm(list=ls(all=TRUE))

# Aumentando memoria atribuida para execucao do programa
memory.limit(size=30000)

# Carregando pacotes necessarios para calculo das estimativas
library("PNADcIBGE")
library("survey")

# Definindo intervalo de periodo dos microdados para calculo das estimativas
indice_ano <- c(2012:2023)
indice_trimestre <- c(1:4)

# Criando matriz de referencia onde estimativas serao armazenadas
resultado <- matrix(data=NA, nrow=length(indice_ano), ncol=length(indice_trimestre))
rownames(resultado) <- indice_ano
colnames(resultado) <- indice_trimestre

# Realizando processo de calculo da taxa de desocupacao para periodos indicados
for(ano in indice_ano)
{
  for(trimestre in indice_trimestre)
  {
    pnadc_trimestral <- get_pnadc(year=ano, quarter=trimestre)
    if(!is.null(pnadc_trimestral))
    {
      resultado[as.character(ano),as.character(trimestre)] <- round(svyratio(numerator=~(VD4002=="Pessoas desocupadas"), denominator=~(VD4001=="Pessoas na força de trabalho"), design=pnadc_trimestral, na.rm=TRUE)[[1]][1]*100, digits=1)
    }
    rm(pnadc_trimestral)
    gc(verbose=FALSE, reset=FALSE, full=TRUE)
  }
}

# Observando resultado das estimativas obtido para periodos analisados
print(x=resultado)