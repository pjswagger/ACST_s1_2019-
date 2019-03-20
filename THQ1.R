#Question 1
Price_Bond <- function(C,F,n,i){
  j=seq(0.5,n,0.5)
  sum(C*exp(-i*j))+F*exp(-i[2*n]*n)
}
i <- rep(0.1,2)

#Question 3
a)SingNA <- read.csv("singapore.economy.csv", header = T,)
b)Sing <- na.omit(SingNA)
c)plot(Sing$time, Sing$gdp,xlab = "Time",ylab = "GDP(%)", main = "Singapore GDP growth")
d)m <- aggregate(Sing$gdp, by=list(Sing$period),FUN=mean)
  sd <- aggregate(Sing$gdp, by=list(Sing$period),FUN=sd)
  stat.table <-cbind(m[,2],sd[,2])
  rownames(stat.table) <- c("Period 1","Period 2", "Period 3")
  colnames(stat.table) <- c("Mean","Sd")
e)pairs(~Sing$gdp+Sing$exp+Sing$epg+Sing$hpr+Sing$gdpus+Sing$oil+Sing$crd+Sing$bci,Sing)
f)FittedlmGvE <- lm(Sing$gdp~Sing$exp)
  summary(FittedlmGvE)
  #Comment:The intercept is 1.19832, the coefficient of
  #the exp is 0.19076. The p-value of both are close to zero
  #so both of them are significant. R square is 28.8%, so 
  #it might not suggest a strong relationship between response and predictor
g)FittedlmGvM <- lm(Sing$gdp~Sing$exp+Sing$epg+Sing$hpr+Sing$oil+Sing$gdpus+Sing$crd)
  summary(FittedlmGvM)
  #Comment: the coefficient of the preditors
  #show that exp and epg have a positive effect to the response
  #whereas hpr, gdpus and crd have negative effect.
  #For p-value, intercept, exp,  epg, hpr are significant
  #whereas gdpus and crd are not.
  #R-square is 37.2% indicate that the relationship between 
  #response and predictor is not strong
h)q <- quantile(Sing$gdp,probs = 0.05)
  state <- factor(ifelse(Sing$gdp<q,"crsis","normal"))
  train <- Sing[Sing$time<2008,]
  test <- Sing[Sing$time>=2008,]
  FittedState <- glm(Sing$state~Sing$bci,data=test, family = binomial)
  summary(FittedState)
  #confusion matrix
  prob <- predict(FittedState, data = test, type = 'response')
  contrasts(state)
  pred <- ifelse(prob<0.05, "crisis","normal")
  conf.mat <- table(pred, state);conf.mat