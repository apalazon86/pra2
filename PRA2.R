library(knitr)
library(ggplot2)

wine <- read.csv2("winequality-red.csv", header=TRUE, sep=",")
kable(head(wine))

tipos <- sapply(wine,class)
kable(tipos)

wine$fixed.acidity <- as.numeric( sub(",","\\.", wine$fixed.acidity))
wine$volatile.acidity <- as.numeric( sub(",","\\.", wine$volatile.acidity))
wine$citric.acid <- as.numeric( sub(",","\\.", wine$citric.acid))
wine$residual.sugar <- as.numeric( sub(",","\\.", wine$residual.sugar))
wine$chlorides <- as.numeric( sub(",","\\.", wine$chlorides))
wine$free.sulfur.dioxide <- as.numeric( sub(",","\\.", wine$free.sulfur.dioxide))
wine$total.sulfur.dioxide <- as.numeric( sub(",","\\.", wine$total.sulfur.dioxide))
wine$density <- as.numeric( sub(",","\\.", wine$density))
wine$pH <- as.numeric( sub(",","\\.", wine$pH))
wine$sulphates <- as.numeric( sub(",","\\.", wine$sulphates))
wine$alcohol <- as.numeric( sub(",","\\.", wine$alcohol))

tipos <- sapply(wine,class)
kable(tipos)

kable(summary(wine))

kable(sapply(wine, function(x) sum(is.na(x))))

boxplot(wine$fixed.acidity, main="fixed.acidity")
boxplot.stats(wine$fixed.acidity)$out
boxplot(wine$volatile.acidity, main="volatile.acidity")
boxplot.stats(wine$volatile.acidity)$out
boxplot(wine$citric.acid, main="citric.acid")
boxplot.stats(wine$citric.acid)$out
boxplot(wine$residual.sugar, main="residual.sugar")
boxplot.stats(wine$residual.sugar)$out
boxplot(wine$chlorides, main="chlorides")
boxplot.stats(wine$chlorides)$out
boxplot(wine$free.sulfur.dioxide, main="free.sulfur.dioxide")
boxplot.stats(wine$free.sulfur.dioxide)$out
boxplot(wine$total.sulfur.dioxide, main="total.sulfur.dioxide")
boxplot.stats(wine$total.sulfur.dioxide)$out
boxplot(wine$density, main="density")
boxplot.stats(wine$density)$out
boxplot(wine$pH, main="pH")
boxplot.stats(wine$pH)$out
boxplot(wine$sulphates, main="sulphates")
boxplot.stats(wine$sulphates)$out
boxplot(wine$alcohol, main="alcohol")
boxplot.stats(wine$alcohol)$out

ggplot(wine, aes(x=quality)) + geom_bar()

library(GGally)
ggcorr(wine, label = TRUE)

wine.bad <- wine[wine$quality < 6,c("quality","alcohol","sulphates","volatile.acidity")]
wine.good <- wine[wine$quality > 5,c("quality","alcohol","sulphates","volatile.acidity")]

library(nortest)
alpha = 0.05
col.names = colnames(wine)
for(i in 1:ncol(wine)){
  p_val = lillie.test(wine[,i])$p.value
  #p_val = shapiro.test(wine[,i])$p.value
  if(p_val>alpha){
    cat(col.names[i])
    cat(" sigue una distribución normal\n")
  }
  else{
    cat(col.names[i])
    cat(" NO sigue una distribución normal\n")
    qqnorm(wine[,i], main=col.names[i])
    qqline(wine[,i])
  }
}

var.test(wine.bad$alcohol, wine.good$alcohol)

var.test(wine.bad$sulphates, wine.good$sulphates)

var.test(wine.bad$volatile.acidity, wine.good$volatile.acidity)

model <- lm(quality ~ alcohol + sulphates + volatile.acidity, data=wine)
summary(model)

wine$good<- ifelse(wine$quality >5, 1, 0)
kable(head(wine[, c("quality","good")]))

modelo2 = glm(good ~ alcohol + sulphates + volatile.acidity, data = wine, family=binomial)
summary(modelo2)

new = data.frame(alcohol=12, sulphates=1, volatile.acidity=0.8)
predict(modelo2, new, type="response")

t.test(
  wine.bad$volatile.acidity,
  wine.good$volatile.acidity,
  alternative = "less"
)

t.test(
  wine.bad$alcohol,
  wine.good$alcohol,
  alternative="greater"
)


t.test(
  wine.good$sulphates, 
  mu=0.55,
  alternative = "greater"
)

ggplot(wine, aes(x = alcohol, y = sulphates)) +
    geom_point(aes(color = factor(good)))

ggplot(wine, aes(x = alcohol, y = volatile.acidity)) +
    geom_point(aes(color = factor(good)))

ggplot(wine, aes(x = sulphates, y = volatile.acidity)) +
    geom_point(aes(color = factor(good)))