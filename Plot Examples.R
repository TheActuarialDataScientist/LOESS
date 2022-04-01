
# OLS - When it makes sense

x <- seq(0,1,.01)
y <- x*3 + rnorm(length(x),0,.5)+3

par(bty="n")
plot(x,y,pch=16,cex=2,col="#77777744",xlab="X Variable",ylab="Target")

mod <- lm(y~x)

abline(mod$coefficients,col="#0076B8",lwd=6)





# OLS - When there is a more complicated relationship

sim_data <- function(seed=1,True_Value=FALSE){
  min <- 0.005
  x <- seq(min,2.5,min)
  set.seed(seed)
  y <- sin(x*15)*(x+.5) + exp(x)*3  + rnorm(length(x),0,1)+3
  y <- ifelse(x>1.7,20- 30*(x-1.7)+ rnorm(length(x),0,1),y)
  y <- ifelse(x>2,11+rnorm(length(x),0,1),y)
  
  if(True_Value){
    y <- sin(x*15)*(x+.5) + exp(x)*3+3
    y <- ifelse(x>1.7,20- 30*(x-1.7),y)
    y <- ifelse(x>2,11,y)
  }
  
  return(cbind(x,y))
}

train <- sim_data(1)
test <- sim_data(123)
x <- train[,1]
y <- train[,2]

y_true <- sim_data(True_Value=TRUE)

par(bty="n")
plot(x,y,pch=16,cex=2,col="#77777744",xlab="X Variable",ylab="Target")

mod <- lm(y~x)

lines(y_true,col="black",lwd=3,type="l",lty=2)
abline(mod$coefficients,col="#0076B8",lwd=6)






# OLS - When there is a more complicated relationship
par(bty="n")
plot(x,y,pch=16,cex=2,col="#77777744",xlab="X Variable",ylab="Target")

mod <- lm(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)+I(x^7)+I(x^8)+
            I(x^9)+I(x^10)+I(x^11)+I(x^12)+I(x^13)+I(x^14)+I(x^15))

lines(y_true,col="black",lwd=3,type="l",lty=2)
lines(x,mod$fitted.values,col="#0076B8",lwd=6,type="l")







# LOESS - When there is a more complicated relationship


x_test <- test[,1]
y_test <- test[,2]

spans <- seq(.02,.2,.01)
res <- matrix(NA,0,2)

for(span in spans){
  mod <- loess(y~x,span = span,degree = 1)
  rmse <- sqrt(mean((predict(mod,x_test)-y_test)^2))
  res <- rbind(res,c(span,rmse))
}

plot(res[,1]*100,res[,2],type="l",lwd=5,col="#777777",xlab="% Span",ylab="Validation Error (RMSE)")
best_span <- res[which.min(as.data.frame(res)[,2])]
abline(v=100*best_span,col="#0076B8",lwd=5)


par(bty="n")
plot(x,y,pch=16,cex=2,col="#77777744",xlab="X Variable",ylab="Target")

mod <- loess(y~x,span = res[i,][1],degree = 1)

lines(y_true,col="black",lwd=3,type="l",lty=2)
lines(x,mod$fitted,col="#0076B8",lwd=6,type="l")





path = "C:\\Users\\Taylor\\Desktop\\R Markdown Tutorials\\LOESS\\Images\\LOESS_on_SIM.gif"
animation::saveGIF(expr = 
                     for (span in seq(0.01,.2,.01)){
                       span=.06
                       mod <- loess(y~x,span = span,degree = 1)
                       title <- paste("Span: ",span*100,"%",sep="")
                       if(abs(span-best_span)<0.0001){
                         for(j in 1:8){
                           par(bty="n")
                           plot(x,y,pch=16,cex=2,col="#77777744",xlab="X Variable",ylab="Target")
                           text(0.3,22,label=title,col="#0076B8",cex = 4)
                           lines(y_true,col="black",lwd=3,type="l",lty=2)
                           lines(x,mod$fitted,col="#0076B8",lwd=6,type="l")
                         }
                       }else{
                         reps <- ifelse(span==0.01 | span==.2,4,1)
                         for(j in 1:reps){
                           par(bty="n")
                           plot(x,y,pch=16,cex=2,col="#77777744",xlab="X Variable",ylab="Target")
                           text(0.3,22,label=title,col="#444444",cex = 4)
                           lines(y_true,col="black",lwd=3,type="l",lty=2)
                           lines(x,mod$fitted,col="#444444",lwd=6,type="l")
                         }
                       }
                       
                     },
                   movie.name = path,
                   interval = .5,
                   output = path,
                   ani.width=1200, 
                   ani.height=800
)







tricubic <- function(x,val,span,return_weights = TRUE,unif=FALSE){
  dist <- abs(x-val)
  
  maxdist <- quantile(dist,probs = span)
  
  heights <- apply(as.data.frame((1-(dist/maxdist)^3)^3),1,max,0)
  
  hist_data <- matrix(NA,0,2)
  for(i in 1:(length(x)-1)){
    hist_data <- rbind(hist_data,c(x[i],heights[i]))
    hist_data <- rbind(hist_data,c(x[i],heights[i+1]))
  }
  
  hist_data <- rbind(c(0,0),hist_data,c(2.5,0))
  
  if(unif){
    minx <- min(x[dist<=maxdist])
    maxx <- max(x[dist<=maxdist])
    hist_data <- rbind(c(0,0),c(minx,0),c(minx,1),c(maxx,1),c(maxx,0),c(2.5,0))
  }
  
  if(return_weights){
    return(heights)
  }else{
    return(hist_data)
  }
  
}



plot(tricubic(x,.5,span=0.2),type="l")






span <- best_span
span <- 0.2



path = "C:\\Users\\Taylor\\Desktop\\R Markdown Tutorials\\LOESS\\Images\\LOESS_on_SIM_best_span"
animation::saveGIF(expr = 
                     
                     for(val in seq(0,2.5,.02)){
                       #val = 1.1
                       mod <- loess(y~x,span = span,degree = 1)
                       weights <- tricubic(x,val,span)
                       plotweights <- tricubic(x,val,span,FALSE)
                       local_mod <- lm(y~x,weights = weights)
                       
                       title <- paste("Span: ",span*100,"%",sep="")
                       
                       par(bty="n")
                       plot(x,y,pch=16,cex=2,col=ifelse(weights>0,"#77777777","#77777715"),xlab="X Variable",ylab="Target",ylim=c(0,23))
                       text(0.3,22,label=title,col="#0076B8",cex = 4)
                       polygon(plotweights[,1],plotweights[,2]*5,col="#44444444")
                       #lines(y_true,col="black",lwd=3,type="l",lty=2)
                       lines(x,mod$fitted,col="#777777",lwd=6,type="l")
                       abline(local_mod$coefficients,col="#0076B8",lwd=3)
                       lines(x=val,y=predict(local_mod,as.data.frame(cbind(x=val))),type="p",cex=3,col="#0076B8",pch=16)
                       
                     },
                   movie.name = paste(path,span,".gif",sep=""),
                   interval = .25,
                   output = path,
                   ani.width=1200, 
                   ani.height=800
)














#kNN
span=0.2
mod <- loess(y~x,span = span,degree = 0)
weights <- tricubic(x,val,span)
plotweights <- tricubic(x,val,span,FALSE,unif = TRUE)
local_mod <- lm(y~1,weights = weights)

title <- paste("Span: ",span*100,"%",sep="")

par(bty="n")
plot(x,y,pch=16,cex=2,col=ifelse(weights>0,"#77777777","#77777715"),xlab="X Variable",ylab="Target",ylim=c(0,23))
text(0.3,22,label=title,col="#0076B8",cex = 4)
polygon(plotweights[,1],plotweights[,2]*5,col="#44444444")
#lines(y_true,col="black",lwd=3,type="l",lty=2)
lines(x,mod$fitted,col="#777777",lwd=6,type="l")
abline(h=local_mod$coefficients,col="#0076B8",lwd=3)
lines(x=val,y=predict(local_mod,as.data.frame(cbind(x=val))),type="p",cex=3,col="#0076B8",pch=16)




mod <- loess(mpg~hp*wt,data=mtcars)

dat <- matrix(NA,0,2)
for(i in seq(50,300,1)){
  for(j in seq(2,5,.1)){
    dat <- rbind(dat,c(i,j))
  }
}

colnames(dat)<- c("hp","wt")
dat <- as.data.frame(dat)
raw_preds <- predict(mod,dat)

all_dat <- cbind(dat,raw_preds)
all_dat <- na.omit(all_dat)


raw_preds <- all_dat[,3]

preds <- (raw_preds-min(raw_preds))/(max(raw_preds)-min(raw_preds))
colors <- colorRampPalette(c("gray","gold","orange","green","darkgreen","#0076B8","darkblue","black"))
col <- colors(101)[round(preds*100+1)]
plot(all_dat$hp,all_dat$wt,col=col,pch=15,cex=3)











# Loading
#data()
cars <- data(mtcars)

head(cars)



setosa_ind <- ifelse(iris$Species=="setosa",1,0)
mod <- loess(setosa_ind~iris$Sepal.Length,span = .5)

plot(iris$Sepal.Length,setosa_ind)
lines(seq(4,8,.1),predict(mod,seq(4,8,.1)))


mod <- loess(setosa_ind~Sepal.Length*Sepal.Width,span = .5,data = cbind(iris,setosa_ind))


newdata <- expand.grid(seq(4.5,8,.05),seq(2,4.5,.05))
colnames(newdata) <- c("Sepal.Length","Sepal.Width")

#newdata <- as.data.frame(cbind(Sepal.Length=c(4,5,6,7),Sepal.Width=c(2,3,4,5)))
  
raw_preds <- predict(mod,newdata)



preds <- (raw_preds-min(raw_preds))/(max(raw_preds)-min(raw_preds))

colors <- colorRampPalette(c("gray","#0076B8"))

col <- colors(101)[round(preds*100+1)]


plot(newdata[,1],newdata[,2],col=col,pch=15)















