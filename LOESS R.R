
### Load Packages
require(dplyr)
require(datasets)
require(stats)
require(ggplot2)

















### Split Data into Train/Valid
data(iris)
iris %>% head()
train.split.pct <- 0.7
train.row <- runif(n=nrow(iris))  <  train.split.pct
train <- iris[train.row, ]
valid <- iris[!train.row,]
nrow(train)
nrow(valid)


















### LOESS function
# Categorical Variables
mod <- loess( formula=Petal.Width~.,
              data=train,
              span = 0.3,
              degree = 1)

mod <- loess( formula=Petal.Width~
                Sepal.Width+Sepal.Length+Petal.Width,
              data=train,
              span = 0.3,
              degree = 1)


















# Normalization
mod <- loess( formula=Petal.Width~Sepal.Width+Sepal.Length+Petal.Width,
              data=train,
              span = 0.3,
              degree = 1,
              normalize = TRUE)
?loess

normalize <- function(x){
  x <-x-mean(x)
  x/sd(x)
}

mean(normalize(train$Sepal.Length))
var( normalize(train$Sepal.Length))


N.train <-  train %>% 
  mutate(N.Sepal.Width  = normalize(Sepal.Width),
         N.Petal.Width  = normalize(Petal.Width),
         N.Sepal.Length = normalize(Sepal.Length)
  )

# Correlation
N.train %>% select(-Species) %>% cor() %>% round(2)



















# Large Enough Span
mod <- loess( formula=Petal.Width~Sepal.Width+Sepal.Length+Petal.Width,
              data=train,
              span = 0.05,
              degree = 1,
              normalize = TRUE)



### Find Best Span
# Metric of Interest
calc.RMSE <- function(pred,act){
  sqrt(mean((pred-act)^2)/length(pred))
}
# Hyperparameter Search
results <- matrix(NA,0,4)
colnames(results) <- c("span","degree","train.rmse","valid.rmse")

for(degree in seq(0,2,1)){
  for(span in seq(0.15,1,0.01)){
    mod <- loess( formula=Petal.Width~Sepal.Width+Sepal.Length+Petal.Width,
                  data=train,
                  span = span,
                  degree = degree,
                  normalize = TRUE,
                  control = loess.control(surface = "direct"))
    train.rmse <- calc.RMSE(mod$fitted,train$Petal.Width)
    valid.rmse <- calc.RMSE(predict(mod,newdata=valid) %>% 
                              as.data.frame(),valid$Petal.Width)
    
    results <- rbind(results,c(span,degree,train.rmse,valid.rmse))
  }
}

results <- as.data.frame(results)
best    <- results[which.min(results$valid.rmse),]
 
ggplot(results, aes(span, degree, fill= valid.rmse)) +
  geom_tile()+
  ggtitle(paste("Span:",round(best$span,2),"   Degree:",best$degree)) +
  scale_fill_gradient(low="red",high="white")






















final.mod <- loess( formula=Petal.Width~
                      Sepal.Width+Sepal.Length+Petal.Width,
                    data=train,
                    span = best$span,
                    degree = best$degree,
                    normalize = TRUE,
                    control = loess.control(surface = "direct"))




