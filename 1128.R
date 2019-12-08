#This project is tring to investigate the relationship
#between the interest rate of each loan and the requested loan amount.
#Fixed effect related to the loan: loan_amnt, term,installment,purpose
#Random effect related to the borrower: grade, emp_length, home_ownership, annual_inc, verfication_status, delinq_2yrs,fico_range_low, inq_last_6mths,open_acc,recoveries(binary),application type

pacman::p_load(tidyverse,dplyr,ggplot2,ggthemes,RColorBrewer,rockchalk,
               car,varhandle,Hmisc,lme4,standardize,glmnet,ggplot2,corrplot,rstanarm,foreign,MASS,Hmisc,reshape2,caret,bayesplot,loo)
#Data acquisation and cleaning
lendingdata <- read.csv("lendingclub_12-15.csv",na.strings = "")
colnames(lendingdata)
subdata <- lendingdata %>% dplyr::select(1,3,6,7,8,9,10,12,13,14,15,16,26,28,33,46)
subdata <- subdata%>%tidyr::drop_na()
colnames(subdata)
summary(subdata)
class(subdata$grade)
class(subdata$home_ownership)
class(subdata$verification_status)
subdata$term <- substr(subdata$term,1,3)
subdata$int_rate <- substr(subdata$int_rate,2,6)
subdata$int_rate <- as.numeric(subdata$int_rate)
#subdata$term <- as.numeric(subdata$term)
subdata$emp_length <- substr(subdata$emp_length,1,2)
levels(subdata$verification_status) <- c(0,1,2) #Not verified = 0, source verfied = 1, verified = 2
subdata[grep(pattern = "<",x = subdata$emp_length),"emp_length"] <- "0"
subdata <- subdata %>% mutate(int_level = ifelse(int_rate<=quantile(int_rate,0.25),"1",
                                          ifelse(int_rate<=quantile(int_rate,0.5),"2",
                                          ifelse(int_rate<=quantile(int_rate,0.75),"3",
                                          ifelse(int_rate<=quantile(int_rate,1),"4")))))
class(subdata$loan_amnt)
subdata <- subdata %>% mutate(loan_amnt_level = ifelse(loan_amnt<=quantile(loan_amnt,0.25),"1",
                                                ifelse(loan_amnt<=quantile(loan_amnt,0.5),"2",
                                                ifelse(loan_amnt<=quantile(loan_amnt,0.75),"3",
                                                ifelse(loan_amnt<=quantile(loan_amnt,1),"4")))))

#term_level <- to.dummy(subdata$term,"term")
#term_level
#EDA
#plot1: interest rate distribution
plot1 <- subdata %>% group_by(int_level) %>% 
  dplyr::summarize(freq = n()) %>% 
  ggplot(aes(x=int_level,y=freq,fill=freq))+
  geom_bar(stat = "identity")+
  xlab("Interest Rate Level")+
  ylab("Frequancy")+
  theme_fivethirtyeight()+
  theme(legend.position = 'none',axis.text.x = element_text(size = 15))+
  geom_text(aes(label = freq),vjust = -0.1, size = 4.5)+
  scale_fill_gradientn(name = '',colors = rev(brewer.pal(10,'Spectral')))+
  ggtitle("Interest Rate Level")
print(plot1)

#plot2: loan amount distribution
plot2 <- subdata %>% group_by(loan_amnt_level) %>% 
  dplyr::summarize(freq = n()) %>% 
  ggplot(aes(x=loan_amnt_level,y=freq,fill=freq))+
  geom_bar(stat = "identity")+
  xlab("Loan Amount Level")+
  ylab("Frequancy")+
  theme_fivethirtyeight()+
  theme(legend.position = 'none',axis.text.x = element_text(size = 15))+
  geom_text(aes(label = freq),vjust = -0.1, size = 4.5)+
  scale_fill_gradientn(name = '',colors = rev(brewer.pal(10,'Spectral')))+
  ggtitle("Loan Amount Level")
print(plot2)

#plot3: potential relationship between interest rate and loan amount
plot3.a <- subdata %>% ggplot(aes(x=loan_amnt_level,y=int_rate))+geom_point(size = 0.3)
plot3
plot3.b <- boxplot(int_rate~loan_amnt_level,data=subdata)
#plot4: grade distribution
plot4 <- subdata %>% group_by(grade) %>% 
  dplyr::summarize(freq = n()) %>% 
  ggplot(aes(x=grade,y=freq,fill=freq))+
  geom_bar(stat = "identity")+
  xlab("Grade")+
  ylab("Frequancy")+
  theme_fivethirtyeight()+
  theme(legend.position = 'none',axis.text.x = element_text(size = 15))+
  geom_text(aes(label = freq),vjust = -0.1, size = 4.5)+
  scale_fill_gradientn(name = '',colors = rev(brewer.pal(10,'Spectral')))+
  ggtitle("Grade")
print(plot4)

#plot5: grade related with interest rates
plot5 <- subdata %>% ggplot(aes(x=int_level,fill=grade))+
  geom_density(alpha = 0.3)+
  theme(legend.position = "none")+
  theme_fivethirtyeight()+
  xlab("Interst rate level")+
  ggtitle("Interest rate by borrower's Grade")+
  facet_grid(grade ~., scales = "free")
plot5

#plot6: employment length
plot6 <- subdata %>% group_by(emp_length) %>% 
  dplyr::summarize(freq = n()) %>% 
  top_n(50) %>% 
  ggplot(aes(reorder(emp_length,freq),y=freq,fill = freq))+
  geom_bar(stat = "identity", position = "dodge")+
  xlab("emp_length")+
  ylab("Frequency")+
  coord_flip()+
  theme_fivethirtyeight()+
  theme(legend.position = 'none',axis.text.x = element_text(size = 15))+
  geom_text(aes(label = freq),vjust = -0.1, size = 4.5)+
  scale_fill_gradientn(name = '',colors = rev(brewer.pal(10,'Spectral')))+
  ggtitle("Employment length")
print(plot6)

#plot7: borrowers home ownership type
plot7 <- subdata %>% group_by(home_ownership) %>% 
  dplyr::summarize(freq = n()) %>% 
  ggplot(aes(reorder(home_ownership,freq),y = freq, fill = freq))+
  geom_bar(stat = "identity", position = "dodge")+
  xlab("home_ownership")+
  ylab("Frequency")+
  #scale_color_fivethirtyeight()+
  theme_fivethirtyeight()+
  theme(legend.position = 'none',axis.text.x = element_text(size = 15))+
  geom_text(aes(label = freq),vjust = -0.1, size = 4.5)+
  scale_fill_gradientn(name = '',colors = rev(brewer.pal(10,'Spectral')))+
  ggtitle("Home Ownership")
print(plot7)
#Interpretation: Most of the borrowers rent a house or have a house mortgage.

#MODELING
#Use training set to do Modeling
train.control <- trainControl(method = "cv",number = 2)
model.1 <- train(int_rate~.,data=subdata,method = "lm",trControl = train.control)


#Model 1: simple linear model without confounding variables (personal information realted with interest rate)
fit1 <- lm(formula = log(int_rate)~log(loan_amnt)+term+installment,data = subdata)
summary(fit1)
plot(fit1, which = 1)
crPlots(fit1)

#confounding variables: grade, emp_length, home_ownership, annual_inc, verfication_status, delinq_2yrs,fico_range_low,open_acc,recoveries(binary),application type
#Model 2: simple liner model with confounding variables (personal information related with interest rate). Does this model perform better?
###weird outliers
fit2 <- lm(formula = log(int_rate)~log(loan_amnt)+term+log(installment)+grade+emp_length+home_ownership+annual_inc+verification_status+delinq_2yrs+fico_range_low+open_acc+recoveries,data=subdata)
summary(fit2)
plot(fit2,which=1)
#Based on the adjusted R square, the model "fit2" performs better.
anova(fit1,fit2)


#Correlation map to check if the confounding variables are correlated with the outcome (interst rate).
cordata <- subdata %>% dplyr::select(4,6,7,9,10,11,13,14,15,16)%>%dplyr::mutate_if(.predicate = is.factor,.funs = as.numeric)
str(cordata)
cordata <- as.numeric(cordata)
cormat <- round(cor(cordata),2)
corplot1 <- corrplot(cormat,type = "upper",order="hclust",tl.cex = 0.5)
#Based on the correlation plot, int_rate and grade, int_rate and fico_range have strong relationship.

#Model3: Lasso regression to choose the confounding varibales
x <- model.matrix(int_rate~.,cordata)
y <- cordata$int_rate
train <- sample(1:nrow(x),nrow(x)*.67)
test <- (-train)
y.test <- y[test]
length(y[train])
length(y.test)
#find the best lambda from out list via cross-validation
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out,label=TRUE)
bestlam <- cv.out$lambda.min
#glmnet()function standardizes the varibales by default
fit3_lasso <- glmnet(x[train,],y[train],alpha = 1,lambda = 0.008)
predict(fit3_lasso,type = "coefficients",s=bestlam)
#make predictions
#fit3_pred <- predict(fit3_lasso,s=bestlam,newx = x[test,])
coef(fit3_lasso)
#The model indicates that the annual_inc, recoveries, fico_range_low have been shriked to around zero
#Thus we are left with grade, verification_status and delinq_2yrs to be the selected confounding varibales.
#confounding variable selection

fittest <- lm(formula = int_rate~loan_amnt+term+installment+grade+verification_status+delinq_2yrs+recoveries,data=subdata)
plot(fittest,which = 1)
outliers_fittest <- which(fittest$residuals<=-5)
subdata[outliers_fittest,]
str(fittest$residuals)
predict(fittest,newdata = subdata[outliers_fittest,])
cor(subdata$int_rate,as.numeric(subdata$grade))
plot(lm(data = subdata,int_rate~grade),which = 1)
plot(lm(data = subdata,int_rate~sub_grade),which = 1)
#switch to mixed effect model

#Model4: multilevel linear regression model with varying intercept
fit4 <- lmer(log(int_rate)~log(loan_amnt)+term+installment+(1|grade)+(1|verification_status)+(1|delinq_2yrs),data=subdata,REML = TRUE)
summary(fit4)
plot(fit4)
qqnorm(resid(fit4))
qqline(resid(fit4))
aic(fit4)
predict(fit4)
mm_plot <- ggplot(subdata, aes(x = loan_amnt, y = int_rate)) +
  facet_wrap(~grade)   # a panel for each grade
  #geom_line(data = cbind(subdata, pred = predict(fit4)), aes(y = pred), size = 1)
mm_plot
#Model5: multilevel linear regression model with varying slopes
fit5 <- lmer(log(int_rate)~log(loan_amnt)+term+installment+(1+log(loan_amnt)|grade)+(1+log(loan_amnt)|verification_status)+(1+log(loan_amnt)|delinq_2yrs),data=subdata)
summary(fit5)
anova(fit4,fit5)
#par(mar = rep(2, 4))
#plot(residuals(fit5))
#Model6: Beyasian multilevel linear regression model
fit6 <- stan_lmer(log(int_rate)~log(loan_amnt)+term+installment+(1|grade)+(1|verification_status)+(1|delinq_2yrs),data=subdata,REML = TRUE)

#Model7: Beyasian multilevel linear regression model
fit7 <- stan_lmer(log(int_rate)~log(loan_amnt)+term+installment+(1+log(loan_amnt)|grade)+(1+log(loan_amnt)|verification_status)+(1+log(loan_amnt)|delinq_2yrs),data=subdata)
anova(fit6,fit7)

#Model8: Categorical regression model
fit8 <- polr(formula=as.factor(int_level)~loan_amnt_level+as.factor(term)+installment,data=subdata,Hess = TRUE)
summary(fit8)
ctable <- coef(summary(fit8))
p <- pnorm(abs(ctable[,"t value"]),lower.tail = FALSE)*2
ctable <- cbind(ctable,"p value" = p)
ci <- confint(fit8)
exp(coef(fit8))
exp(cbind(OR = coef(fit8),ci))
#One unit increase in loan_amnt_level, from 2 to 3, the odds of interest rate of "level 1" applying verus "level 2" applying are 0.45 greater.
#For installment, when the installment amount moves 1 unit, the odds of interest rate level moving from "level 1" to other levels are nearly the same.
summary(update(fit8,method = "probit",Hess = TRUE),digits = 3)
summary(update(fit8,method = "logistic",Hess = TRUE),digits = 3)
summary(update(fit8,method = "cloglog",Hess = TRUE),digits = 3)
#plot the model
plot8 <- update(fit8, Hess = TRUE)
plot8
pr <- profile(fit8)
confint(pr)
plot(pr)
pairs(pr)

#Model Validation
train.control <- trainControl(method = "cv",number = 10)
model.1 <- train(int_rate~.,data=subdata,method = "lm",trControl = train.control)
