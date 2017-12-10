library(glmnet)
library(reshape2)
library(tidyr)
mapdata <- read_csv("./data/mapdata3.csv")
mapdata$ID=ifelse(mapdata$billtype=="S",mapdata$session*10000+mapdata$billid,mapdata$session*100000+mapdata$billid)
length(unique(mapdata$ID))
mapdata$law=ifelse(mapdata$billoutcome=="Became Law",1,0)
mapdata=mapdata%>%select(bioguideId,session,policy_area,billtype,party,state,ID,law)
#Navie Logit
library(DescTools)
m1=glm(law~factor(session)+billtype+party+party*factor(session)+billtype*factor(session),family = binomial(link='logit'),data=mapdata)
m2=glm(law~factor(session)+billtype+policy_area+state+party+party*factor(session)+billtype*factor(session),family = binomial(link='logit'),data=mapdata)

plot(m1$fitted.values)
summary(m1)
PseudoR2(m1)
PseudoR2(m3)


#Backward Elimation
indictor=model.matrix(~factor(mapdata$policy_area))
dat=cbind(mapdata,indictor[,2:ncol(indictor)])
indictor=model.matrix(~factor(mapdata$state))
dat=cbind(dat,indictor[,2:ncol(indictor)])
indictor=model.matrix(~factor(mapdata$session))
dat=subset(dat,select = -c(session,policy_area,bioguideId,billoutcome,state,ID))
m=glm(law~.,family = binomial(link='logit'),data=dat)
summary(m)
PseudoR2(m)
#result=step(m,direction="backward")
#Backward Elimation did not really work
x=subset(dat,select=-c(law))
x$billtype=ifelse(x$billtype=="S",1,0)
x$party=ifelse(x$party=="Republican",1,0)
for(i in 1:ncol(x))
{
  x[,i]=as.factor(x[,i])
}
glmmod <- glmnet(as.matrix(x), y=as.factor(dat$law), alpha=1, family="binomial")
summary(glmmod)
plot(glmmod,xvar="lambda")

x2 <- model.matrix(law~factor(session)+billtype+party+party*factor(session)+billtype*factor(session),data=mapdata)
x <- model.matrix(law~factor(session)+billtype+policy_area+state+party+party*factor(session)+billtype*factor(session),mapdata)
dim(x)
#split test and train
#set.seed(1)
#test_index=sample(1:nrow(mapdata),1000)
#convert training data to matrix format
#train=x[-test_index,]
#test=x[test_index,]

#convert class to numerical variable
y <- mapdata$law
#perform grid search to find optimal value of lambda
#family= binomial => logistic regression, alpha=1 => lasso
# check docs to explore other type.measure options
cv.out_lasso <- cv.glmnet(x,y,alpha=1,family="binomial")
cv.out_ridge <- cv.glmnet(x,y,alpha=0,family="binomial")
#cv.outp <- cv.glmnet(train,y,alpha=1,family="poisson",type.measure = "mse" )
#plot result
#plot(cv.outp)
plot(cv.out_lasso)
plot(cv.out_ridge)
#best value of lambda
lambda_1se_lasso <- cv.out_lasso$lambda.1se
lambda_1se_ridge<-cv.out_ridge$lambda.1se
#regression coefficients
coef(cv.out_lasso,s=lambda_1se_lasso)
coef(cv.out_ridge,s=lambda_1se_ridge)

#predict class, type=”class”
prob_lasso <-predict(cv.out_lasso,newx = x,s=lambda_1se,type="response")
prob_ridge <-predict(cv.out_ridge,newx = x,s=lambda_1se,type="response")
prob_regular<-fitted.values(m2)
#policy area
a=as.data.frame(cbind(round(prob_ridge,3),mapdata$policy_area))
colnames(a)=c("fitted_prob","policy_area")
ggplot(a,aes(x=policy_area,y=as.numeric(as.character(fitted_prob)),color=policy_area))+geom_point()

#session
b=as.data.frame(cbind(round(prob_regular,4),round(prob_lasso,4),round(prob_ridge,4),mapdata$session,mapdata$policy_area,mapdata$party,mapdata$state))
colnames(b)=c("o","l","r","session","policy_area","party","state")
write_csv(b,"data/fittedprob.csv")
ggplot(b,aes(x=state,y=as.numeric(as.character(fitted_prob)),color=factor(party)))+
  geom_point(alpha=0.5,size=0.5)+facet_grid(session~.)+theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(subset(b,session==113),aes(x=1:nrow(subset(b,session==113)),y=as.numeric(as.character(fitted_prob)),color=factor(party)))+geom_point(alpha=0.5)

