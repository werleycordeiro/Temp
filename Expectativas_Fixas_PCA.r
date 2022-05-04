#=======================
#
# Expectativas Fixas 
#
#=======================

"""
Explicação para transformar expectativas de final de ano para expectativas de 1 ano a frente.

No dia 'dA1', temos previsões para 'A1-12-31' e 'A2-12-31', sendo que a previsão no dia 'dA1' é para o ano corrente 'A1' e para o ano seguinte 'A2'.
Porém queremos expectativas fixas para 'd252' dias a frente para todo 'd'. Como fazer isso?
Fazemos a interpolação entre  'A1-12-31' e 'A2-12-31' com 252 observações.
Pegar a diferença entre dA1 e 'A1-12-31' (ddA1) e diminuir de 252 para saber qual valor pegar nos resultados da interpolação.

"""


setwd("C:\\Users\\werle\\Dropbox\\DNS_Modelos\\Base_Dados_Expectativas_Desativado")

data = read.csv("Dataset_Expectativas_Anuais.csv", check.names=F, fileEncoding = "UTF-8", sep=",", header=TRUE)
data[,5] = gsub(",",".",data[,5])


var <- unique(data[,1]) # Pegar os nomes das variáveis execeto balança comercial
var1 <- unique(data[,2])[-4]
var <- c(var1,var)

for(j in 1:length(var)){
	
	if( j == 1|| j==2 || j==3){

		temp <- data[ data[,2]==var[j],] # Pegar variável especial	

	}else{

		temp <- data[ data[,1]==var[j],] # Pegar variável 

	}
	

	ii = 0
	data_unica <- unique(temp[,3])
	
	for(i in 1:length(data_unica)){

		temp2 <- temp[temp[,3]==data_unica[i],] # selecionar dados com data única
		dA1_ano <- substr(temp2[1,3],1,4) # pegar ano do 'dA1'
		A1_ano <- dA1_ano # Pegar ano do 'A1'
		A2_ano <- as.character(as.numeric(A1_ano) + 1) # Pegar ano do 'A2'

		if( nrow(temp2)>=2 && min(temp2[,4])!=A1_ano ){
			ii = ii + 1
			new2 <- as.numeric(temp2[which(temp2[,4]==min(temp2[,4])),5])[1]
			
			if(ii==1){

			    	dA1_OneYear <- c(data_unica[i], new2[1])

			    }else{

			    	temp4 <- c(data_unica[i], new2[1])
			    	dA1_OneYear <- rbind( dA1_OneYear,  temp4)

			    }						

		}else{

			if( nrow(temp2)>=2 && any(temp2[,4]==A1_ano) && any(temp2[,4]==A2_ano) ){ # se os dados tiveram no minimo duas linhas e se tem datas sequenciais
				
				ii = ii + 1
				start <- as.Date(temp2[which(temp2[,4]==A1_ano),3], format='%Y-%m-%d')
				end <- as.Date(paste0(A1_ano,"-12-31"), format='%Y-%m-%d')
				ddA1 <- as.numeric(252 - round( (end - start) * 0.690411 ))
				valor_inicio <- as.numeric(temp2[which(temp2[,4]==A1_ano),5]) # Expectativa 1
				valor_final <- as.numeric(temp2[which(temp2[,4]==A2_ano),5]) # Expectativa 2
				valor_inicio <- mean(valor_inicio)				
				valor_final <- mean(valor_final)				
				new <- seq( valor_inicio, valor_final, length.out=252 )

				
				if(ii==1){

			    	dA1_OneYear <- c(data_unica[i], new[ddA1][1])

			    }else{

			    	temp4 <- c(data_unica[i], new[ddA1][1])
			    	dA1_OneYear <- rbind( dA1_OneYear,  temp4)

			    }
			    
			}

		}		

	}


	if(j==1){

		data_OneYear <- cbind( dA1_OneYear, rep(var[j],nrow(dA1_OneYear)) )

	}else{

		dA1_OneYear <- cbind( dA1_OneYear, rep(var[j],nrow(dA1_OneYear)) )
		data_OneYear <- rbind(data_OneYear, dA1_OneYear)		

	}

	pb = txtProgressBar(min = (1/length(var)), max = length(var), style = 3)
    setTxtProgressBar(pb,j)

}

tempo <- seq(as.Date(min(as.Date(data_OneYear[,1]))), as.Date(max(as.Date(data_OneYear[,1]))), "days")
data_final <- data.frame(matrix(NA,length(tempo),length(var)))
colnames(data_final) <- var
rownames(data_final) <- tempo

for( j in 1:length(var) ){

	tmp <- data_OneYear[data_OneYear[,3]==var[j],] # pega a matriz da variável desejada		

		for(i in 1:nrow(tmp)){

			if( any(tmp[i,1]==tempo) ){ # tem alguma correspondência nas datas? deve ter.

				data_final[which(tmp[i,1]==tempo),var[j]] <- tmp[i,2] # pega a data correspondetes numa e colocar noutra.

			}

		}

	pb = txtProgressBar(min = (1/length(var)), max = length(var), style = 3)
    setTxtProgressBar(pb,j)

}


data_final <- apply(data_final,2,as.numeric)
require(xts)
data_final2 <- xts(data_final,order.by=tempo)
data_final2_mon <- data_final2[xts:::endof(data_final2, "months")]

for( i in 1:length(var) ){

	print(var[i])
	tmp <- data_final2_mon[!is.na(data_final2_mon[,i]),i]
	print( length( tmp ) )

}

#
pre_pca <- data_final2[,c("Balança comercial",
	"Conta corrente",
	"Resultado primário",
	"PIB Total",
	"Investimento direto no país",
	"Dívida líquida do setor público",
	"Selic",
	"Câmbio",
	"IPCA")]

pre_pca1 <- na.omit(pre_pca)
pre_pca1 <- pre_pca1[xts:::endof(pre_pca1, "months")]

write.zoo(pre_pca1,file="Variaveis_Exp_1Ano.csv", col.names=T, row.names=T,sep=",")

# Obs.:

ipca <- as.matrix(as.vector(pre_pca1))
row.names(ipca) <- as.character.Date(index(pre_pca1))
write.csv(ipca,file='IPCA_Exp_v3.csv')

#==============================
#
# Principal Component Analysis
#
#==============================


