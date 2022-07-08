library(arules)
library(arulesViz)
library(readr)

#Creamos la BD transaccional:
trx <- read_delim(file.choose(), 
                  ";", escape_double = FALSE, col_types = cols(`Etiquetas de fila` = col_skip()), 
                  trim_ws = TRUE)

trx2 <-  apply(trx,2,as.logical)
trx2[(is.na(trx2))] <- FALSE
datos <- as(trx2, "transactions")
datos


#Ejercicio 1
summary(datos)
#Obtenemos las variables que tienen mayor significancia en el análisis:
itemFrequencyPlot(datos,topN=35,type='relative')

#Soporte 1000-----------------
rules_1000=apriori(datos, parameter=list(supp=0.05, conf=0.5), maxlen=2)

summary(rules_1000)
inspect(head(sort(rules_1000, by = "lift"),30))

plot(rules_1000, engine="interactive")
subrules <- rules_1000[quality(rules_1000)$lift > 15]
inspect(subrules)
plot(subrules, method="matrix", measure="lift", control=list(reorder="measure"))
#plot(rules, measure=c("support", "lift"), shading="confidence")
#plot(rules, shading="order", control=list(main = "Two-key plot"))
options(digits=2)
inspect(rules[1:5])
plot(subrules, method="grouped")

subrules2 <- head(sort(rules, by="lift"), 10)
plot(subrules2, method="graph")

#Soporte 100-----------------
rules_100=apriori(datos, parameter=list(supp=0.005, conf=0.5), maxlen=2)

summary(rules_100)
inspect(head(sort(rules_100, by = "lift"),30))

plot(rules_100, engine="interactive")
subrules <- rules_1000[quality(rules_100)$lift > 15]
inspect(subrules)
plot(subrules, method="matrix", measure="lift", control=list(reorder="measure"))
#plot(rules, measure=c("support", "lift"), shading="confidence")
#plot(rules, shading="order", control=list(main = "Two-key plot"))
options(digits=2)
inspect(rules[1:5])
plot(subrules, method="grouped")

subrules2 <- head(sort(rules, by="lift"), 10)
plot(subrules2, method="graph")

#Soporte 10-----------------
rules_10=apriori(datos, parameter=list(supp=0.0005, conf=0.5), maxlen=2)

summary(rules_10)
inspect(head(sort(rules_10, by = "lift"),30))

plot(rules_10, engine="interactive")
subrules <- rules_100[quality(rules_100)$lift > 15]
inspect(subrules)
plot(subrules, method="matrix", measure="lift", control=list(reorder="measure"))
#plot(rules, measure=c("support", "lift"), shading="confidence")
#plot(rules, shading="order", control=list(main = "Two-key plot"))
options(digits=2)
inspect(rules[1:5])
plot(subrules, method="grouped")

subrules2 <- head(sort(rules, by="lift"), 10)
plot(subrules2, method="graph")


#Ejercicio 2
#Soporte 20-----------------
rules_20=apriori(datos, parameter=list(supp=0.001, conf=0.5), maxlen=2)

summary(rules_20)
inspect(head(sort(rules_20, by = "confidence"),20))
inspect(head(sort(rules_20, by = "lift"),20))
inspect(head(sort(rules_20, by = "support"),20))

plot(rules_20, engine="interactive")
subrules <- rules_20[quality(rules_20)$lift > 15]
inspect(subrules)
plot(subrules, method="matrix", measure="lift", control=list(reorder="measure"))
#plot(rules, measure=c("support", "lift"), shading="confidence")
#plot(rules, shading="order", control=list(main = "Two-key plot"))
options(digits=2)
inspect(rules[1:5])
plot(subrules, method="grouped")

subrules2 <- head(sort(rules, by="lift"), 10)
plot(subrules2, method="graph")


#Ejercicio 3
rules_20=apriori(datos, parameter=list(supp=0.01, conf=0.5), maxlen=2)

summary(rules_20)
inspect(head(sort(rules_20, by = "confidence"),20))

#Mayonesa con soporte a 20 y 50% de confianza
filtro=arules::subset(rules_20, subset = items %in% "MAYONESA")
inspect(head(filtro, by = "lift", 50))

#Galletas dulces con soporte a 20 y 50% de confianza
filtro=arules::subset(rules_20, subset = items %in% "GALLETAS DULCES")
inspect(head(filtro, by = "support", 50))

#Verduras congeladas con soporte a 20 y 50% de confianza
filtro=arules::subset(rules_20, subset = items %in% "VERDURAS CONGELADAS")
inspect(head(filtro, by = "support", 50))

#Salsas de tomate con soporte a 20 y 50% de confianza
filtro=arules::subset(rules_20, subset = items %in% "SALSAS DE TOMATE")
inspect(head(filtro, by = "support", 50))


#Ejercicio 4
rules_4=apriori(datos, parameter=list(supp=0.0001, conf=0.3), maxlen=2)
inspect(datos[1:10])
#1a transacción
filter=arules::subset(rules_4, subset = lhs %in% c("DURAZNO EN CONSERVA"))
inspect(head(filter, by = "support", 50))
#2a transacción
filter=arules::subset(rules_4, subset = lhs %in% c("ACEITE VEGETAL",     
                                                 "AZUCAR GRANULADA",        
                                                 "JUGOS EN POLVO",     
                                                 "LIQUIDO PARA PISOS",    
                                                 "PANOS Y FIBRAS SINTE"))
inspect(head(filter, by = "lift", 50))
#3a transacción
filter=arules::subset(rules_4, subset = lhs %in% c("GALLETAS DULCES",     
                                                   "JUGOS EN POLVO",        
                                                   "LECHES LIQUIDAS",     
                                                   "OTRAS CECINAS",    
                                                   "PAPEL HIGIENICO",
                                                   "PATE",    
                                                   "QUESOS",
                                                   "TOALLAS DE PAPEL",
                                                   "VERDURAS CONGELADAS"))
inspect(head(filter, by = "lift", 50))
#4a transacción
filter=arules::subset(rules_4, subset = lhs %in% c("ACEITE VEGETAL",     
                                                   "AMPOLLETAS",        
                                                   "ARROZ PREGRANEADO",     
                                                   "ARROZ REGULAR",    
                                                   "ATUN",
                                                   "AZUCAR GRANULADA",    
                                                   "CAFE INSTAN PURO REG",
                                                   "CEREALES",
                                                   "CLOROS LIQUIDOS",        
                                                   "FIDEO CORTO",     
                                                   "FIDEO LARGO",    
                                                   "FOSFOROS",
                                                   "HARINA ENVASADA",    
                                                   "LENTEJAS",
                                                   "LIQUIDO PARA PISOS",
                                                   "MAIZENA",        
                                                   "MARGARINA",     
                                                   "PAPEL ALUSA",    
                                                   "SALSAS DE TOMATE",
                                                   "SOPAS",    
                                                   "TE EN BOLSA",
                                                   "TE EN GRANEL",
                                                   "TOALLAS HIGIENICAS",
                                                   "YOGHURT"))
inspect(head(filter, by = "lift", 50))
#5a transacción
filter=arules::subset(rules_4, subset = lhs %in% c("BEBIDAS GASEOSAS",     
                                                   "CARAMELOS DUROS",        
                                                   "GALLETAS DULCES",     
                                                   "PAPAS FRITAS",    
                                                   "RAMITAS",
                                                   "SIN DESCRIPCION",    
                                                   "SUFLE"))
inspect(head(filter, by = "support", 50))
#6a transacción
filter=arules::subset(rules_4, subset = lhs %in% c("AZUCAR GRANULADA",     
                                                   "BEBIDA LACTEA",        
                                                   "BEBIDAS GASEOSAS",     
                                                   "CARAMELOS BLANDOS",    
                                                   "CERAL AZUCARADO",
                                                   "CHOCOLATES RELLENOS",    
                                                   "GALLETAS DULCES",
                                                   "HELADOS",
                                                   "NECTARES LIQUIDOS",        
                                                   "OTROS CONFITES",     
                                                   "POSTRES REFRIGERADOS",    
                                                   "SIN DESCRIPCION",
                                                   "SURTIDO FRUT CONSER"))
inspect(head(filter, by = "support", 50))
#7a transacción
filter=arules::subset(rules_4, subset = lhs %in% c("BEBIDAS GASEOSAS",     
                                                   "GALLETAS DULCES",        
                                                   "LECHE CONDENSADA",     
                                                   "LECHES LIQUIDAS",    
                                                   "MARGARINA",
                                                   "SOPAS",    
                                                   "VINOS"))
inspect(head(filter, by = "lift", 50))
#8a transacción
filter=arules::subset(rules_4, subset = lhs %in% c("ACEITE VEGETAL",     
                                                   "ARROZ REGULAR",        
                                                   "CLOROS LIQUIDOS",     
                                                   "FIDEO CORTO",    
                                                   "FIDEO LARGO",
                                                   "GALLETAS DULCES",    
                                                   "HAMBURGUESAS",
                                                   "LAVALOZAS",
                                                   "LECHES LIQUIDAS",        
                                                   "LIQUIDO PARA PISOS",     
                                                   "MANJAR",    
                                                   "PASTA DENTAL",
                                                   "SALSAS DE TOMATE",
                                                   "TOALLAS HIGIENICAS",     
                                                   "VERDURAS CONGELADAS",    
                                                   "VIENESAS",
                                                   "YOGHURT"))
inspect(head(filter, by = "lift", 50))
#9a transacción
filter=arules::subset(rules_4, subset = lhs %in% c("ALFAJOR",     
                                                   "AZUCAR GRANULADA",        
                                                   "BEBIDAS GASEOSAS",     
                                                   "CAFE INSTAN PURO REG",    
                                                   "DURAZNO EN CONSERVA",
                                                   "JUREL",    
                                                   "KETCHUP",
                                                   "LEVADURA GRANULADA",
                                                   "MANJAR",        
                                                   "MAYONESA",     
                                                   "TE EN BOLSA",    
                                                   "YERBA MATE GRANEL"))
inspect(head(filter, by = "confidence", 50))
#10a transacción
filter=arules::subset(rules_4, subset = lhs %1in% c("BEBIDAS GASEOSAS",     
                                                   "CARNES CONGELADAS"))
inspect(head(filter, by = "lift", 50))
