'ROTINA ELABORADA COMO TESTE DE DATA SCIENTIST PARA A EMPRESA DR. MONITORA'
'DATA: 28/09/2021'
'AUTOR: ELIAS NETO - ITACOATIATA/AM'



'importando todos os dados - CASCO1'

casco1_2019 <- read.csv("C:/Users/CITA/Documents/Dr Monitora/Teste/Autoseg2019B/arq_casco_comp.csv", sep = ";", header = T)
#casco3_2019 <- read.csv("C:/Users/CITA/Documents/Dr Monitora/Teste/Autoseg2019B/arq_casco3_comp.csv", sep = ";", header = T)
#casco4_2019 <- read.csv("C:/Users/CITA/Documents/Dr Monitora/Teste/Autoseg2019B/arq_casco4_comp.csv", sep = ";", header = T)

casco1_2020 <- read.csv("C:/Users/CITA/Documents/Dr Monitora/Teste/Autoseg2020A/arq_casco_comp.csv", sep = ";", header = T)
#casco3_2020 <- read.csv("C:/Users/CITA/Documents/Dr Monitora/Teste/Autoseg2020A/arq_casco3_comp.csv", sep = ";", header = T)
#casco4_2020 <- read.csv("C:/Users/CITA/Documents/Dr Monitora/Teste/Autoseg2020A/arq_casco4_comp.csv", sep = ";", header = T)

#verificando se as planilhas tem as mesmas colunas
names(casco1_2019)
names(casco1_2020)


#vamos juntar as duas planilhas em uma única, assim vamos ter os dados dos dois períodos

casco1 = rbind(casco1_2019,casco1_2020)

#ficando apenas com as colunas de interesse

casco1 = casco1[,c("COD_TARIF", "REGIAO", "COD_MODELO", "ANO_MODELO", "SEXO",
             "IDADE","EXPOSICAO1", "FREQ_SIN1", "INDENIZ1", "ENVIO" )]


#a principal coluna é a frequência de sinistros, caso a linha seja zero, não faz sentido mantê-la

casco1 <- casco1[(casco1$FREQ_SIN1!= 0), ]

class(casco1$ANO_MODELO) #verificando a classe da coluna Ano/modelo

#soma de sinistros por ano_modelo

ano <- aggregate(casco1$FREQ_SIN1,list(Nome=casco1$ANO_MODELO),sum,na.rm=T)  
ano <- ano[order(ano$x, decreasing = T),]  #ordenando pela soma das frequências

#soma de sinistros por modelo

modelo <- aggregate(casco1$FREQ_SIN1,list(CODIGO=casco1$COD_MODELO),sum,na.rm=T)   
modelo <- modelo[order(modelo$CODIGO),] 


"importando a tabela auxiliar de descrição dos modelos"

modelo_desc <- read.csv("C:/Users/CITA/Documents/Dr Monitora/Teste/Autoseg2020A/auto2_vei.csv", sep = ";", header = T)

#vamos unir agora as tabelas modelo e modelo_desc, onde tem as informações das descrições

modelo <- merge(modelo, modelo_desc, by="CODIGO")

modelo <- modelo[order(modelo$x,decreasing = T),]     #ordenando pela soma das frequências

#soma de sinistros por sexo
sexo <- aggregate(casco1$FREQ_SIN1,list(Nome=casco1$SEXO),sum,na.rm=T)   
sexo <- sexo[order(sexo$x,decreasing = T),]     #ordenando pela soma das frequências


#soma de sinistros por faixa etária
idade <- aggregate(casco1$FREQ_SIN1,list(Nome=casco1$IDADE),sum,na.rm=T)   
idade <- idade[order(idade$x,decreasing = T),]     #ordenando pela soma das frequências

#soma de sinistros por ModeloxAno

modelo_ano <- aggregate(casco1$FREQ_SIN1,list(CODIGO=casco1$COD_MODELO,
                                              ano=casco1$ANO_MODELO),sum,na.rm=T)   


modelo_ano <- merge(modelo_ano, modelo_desc, by="CODIGO") #adicionando as descrições dos modelos


modelo_ano <- modelo_ano[order(modelo_ano$x, decreasing = T),] #ordenando pela soma das frequências


#soma de sinistros por Região

regiao <- aggregate(casco1$FREQ_SIN1,list(CODIGO=casco1$REGIAO),sum,na.rm=T)   
regiao <- regiao[order(regiao$CODIGO),]


#importando a tabela de descrição das regiões
regiao_desc <- read.csv("C:/Users/CITA/Documents/Dr Monitora/Teste/Autoseg2020A/auto_reg.csv", sep = ";", header = T)

regiao <- merge(regiao, regiao_desc, by="CODIGO", all = T) #adicionando as descrições das regiões


regiao <- regiao[order(regiao$x, decreasing = T),]


#Exportando as tabelas em excel

write.csv(ano, file = "C:/Users/CITA/Documents/Dr Monitora/Teste/resultados/ano.csv") 
write.csv(modelo, file = "C:/Users/CITA/Documents/Dr Monitora/Teste/resultados/modelo.csv") 
write.csv(modelo_ano, file = "C:/Users/CITA/Documents/Dr Monitora/Teste/resultados/modelo_ano.csv") 
write.csv(sexo, file = "C:/Users/CITA/Documents/Dr Monitora/Teste/resultados/sexo.csv") 
write.csv(idade, file = "C:/Users/CITA/Documents/Dr Monitora/Teste/resultados/idade.csv") 
write.csv(regiao, file = "C:/Users/CITA/Documents/Dr Monitora/Teste/resultados/regiao.csv") 


head(modelo,10)



'AQUI VAMOS TRABALHAR COM CASCO3'

casco3_2019 <- read.csv("C:/Users/CITA/Documents/Dr Monitora/Teste/Autoseg2019B/arq_casco3_comp.csv", sep = ";", header = T)
casco3_2020 <- read.csv("C:/Users/CITA/Documents/Dr Monitora/Teste/Autoseg2020A/arq_casco3_comp.csv", sep = ";", header = T)


#verificando se as planilhas tem as mesmas colunas
names(casco3_2019)
names(casco3_2020)

head(casco3)

#vamos juntar as duas planilhas em uma única, assim vamos ter os dados dos dois períodos

casco3 = rbind(casco3_2019,casco3_2020)

#ficando apenas com as colunas de interesse

casco3 = casco3[,c("CEP", "REGIAO", "FREQ_SIN1", "EXPOSICAO", "INDENIZ1", "PREMIO", "ENVIO")]


#a principal coluna é a frequência de sinistros, caso a linha seja zero, não faz sentido mantê-la

casco3 <- casco3[(casco3$FREQ_SIN1!= 0), ]

#soma de sinistros por cep

cep <- aggregate(casco3$FREQ_SIN1,list(CEP=casco3$CEP),sum,na.rm=T)  

cep <- cep [order(cep$x, decreasing = T),]  #ordenando pela soma das frequências

#Exportando as tabelas em excel

write.csv(cep, file = "C:/Users/CITA/Documents/Dr Monitora/Teste/resultados/cep.csv") 



'AQUI VAMOS TRABALHAR COM CASCO4'

casco4_2019 <- read.csv("C:/Users/CITA/Documents/Dr Monitora/Teste/Autoseg2019B/arq_casco4_comp.csv", sep = ";", header = T)
casco4_2020 <- read.csv("C:/Users/CITA/Documents/Dr Monitora/Teste/Autoseg2020A/arq_casco4_comp.csv", sep = ";", header = T)


#verificando se as planilhas tem as mesmas colunas
names(casco4_2019)
names(casco4_2020)

#vamos juntar as duas planilhas em uma única, assim vamos ter os dados dos dois períodos

casco4 = rbind(casco4_2019,casco4_2020)

head(casco4)

#ficando apenas com as colunas de interesse

names(casco4)

casco4 = casco4[,c("CIDADE", "EXPOSICAO", "FREQ_SIN1", "COD_CIDADE",
                   "ENVIO")]



#a principal coluna é a frequência de sinistros, caso a linha seja zero, não faz sentido mantê-la

casco4 <- casco4[(casco4$FREQ_SIN1!= 0), ]


#soma de sinistros por cidade

cidade <- aggregate(casco4$FREQ_SIN1,list(CIDADE=casco4$CIDADE),sum,na.rm=T)  

cidade <- cidade[order(cidade$x, decreasing = T),]  #ordenando pela soma das frequências

#Exportando as tabelas em excel

write.csv(cidade, file = "C:/Users/CITA/Documents/Dr Monitora/Teste/resultados/cidade.csv") 


#PERIODO
periodo <- aggregate(casco4$FREQ_SIN1,list(PERIODO=casco4$ENVIO),sum,na.rm=T)  
periodo <- periodo[order(periodo$x, decreasing = T),]  #ordenando pela soma das frequências

#Exportando as tabelas em excel

write.csv(periodo, file = "C:/Users/CITA/Documents/Dr Monitora/Teste/resultados/periodo.csv") 



'Análise de cluster -  vamos realizar uma análise de agrupamento para identificar 
quais cidades se parecem entre si - isso pode ajudar na fase de planejamento de expansão dos negócios
Assim, se os grupos estiverem "corretos" dá pra ter uma noção de como as cidades são parecidas'

#importando os dados
dados <- read.csv("C:/Users/CITA/Documents/Dr Monitora/Teste/cidade-exp-freq.csv", sep = ";", header = T)

cidade<-dados$cidades
row.names(dados)<-cidade      #nomeando as linhas com nome das cidades

names(dados)
dados <- dados[,c("sin_roubos","est_segurados")]

#Distância euclidiana entre as variáveis

D1<-dist(dados,method = "euclidean") #Distância euclidiana
agrupamento<-hclust(D1, method = "single") #agrupando - com esse método, menor variação entre os grupos



png(filename = "AGRUPAMENTO.png",width = 1280, height = 1280, pointsize = 8)
plot(agrupamento)
rect.hclust(agrupamento,5,border = "red")
dev.off() 


grupos <- cutree(agrupamento,5)
grupos <- data.frame(grupos)


write.csv(grupos, file = "C:/Users/CITA/Documents/Dr Monitora/Teste/resultados/grupos.csv") 
