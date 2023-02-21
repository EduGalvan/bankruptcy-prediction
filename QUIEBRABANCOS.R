
library(foreign)
quiebra<- as.data.frame(read_sav("QUIEBRA.sav"))
library(MultBiplotR)

# Convierto la quiebra en factor
quiebra$Quiebra=factor(quiebra$Quiebra) 
levels(quiebra$Quiebra)=c("No","Si")
# La primera columna contiene los nombres de los bancos
bancos=quiebra$BANCO
# Obtengo los datos numéricos y le añado los nombres
X=quiebra[,2:10]
rownames(X)=bancos
# Guardo en XU los datos de los que no tienen quiebra
XU=X[61:66,]
# Los elimino para hacer el análisis
X=X[1:60,]
# Selecciono, a parte, el factor de quiebra
Quiebra=quiebra[1:60, 11]


MANOVA = manova(as.matrix(X) ~ Quiebra)
summary(MANOVA)
summary.aov(MANOVA)

library(MASS)
LDA2=lda(X, Quiebra)
LDA2
Prediccion2=predict(LDA2, X)$class
ct2 <- table(Quiebra, Prediccion2)
ct2
ngrupo=table(Quiebra)
ngrupo
propgrupo=100*diag(ct2)/ngrupo
propgrupo
ptoptotal=100*sum(diag(ct2))/sum(ngrupo)
ptoptotal


# Predicción de los desconocidos
predicc=predict(LDA2, XU)$class
names(predicc)=bancos[61:66]
predicc








colnames(bancos)

modelo <- glm(Quiebra ~ R1Liqui1 + R2Liqui2+ R3Liqui3 +R4Autof + R5RenEc + R6RenFin + R7Apal + R8CosVen + R9Cash, data = bancos, family = "binomial")
summary(modelo)




dif_residuos <- modelo$null.deviance - modelo$deviance

# Grados libertad
df <- modelo$df.null - modelo$df.residual

# p-value
p_value <- pchisq(q = dif_residuos,df = df, lower.tail = FALSE)



library(vcd)
predicciones <- ifelse(test = modelo$fitted.values > 0.5, yes = 1, no = 0)
matriz_confusion <- table(modelo$model$Quiebra, predicciones,
                          dnn = c("observaciones", "predicciones"))
matriz_confusion


mosaic(matriz_confusion, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

0.95


