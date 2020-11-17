# Compositional-Regression-





```{r}
require("compositions")
autoload("bpy.colors", "sp")
require("lmtest")

x <- read.csv("C:\\Users\\Owner\\Dropbox\\time management\\Second paper_demographics recoded.csv", header = TRUE)
#colnames(x)

xc = acomp(x, c(26, 27, 28, 29, 30, 31))
colnames(xc)

summary(xc)

interaction1 = x[, 2]
autonomy1 = x[, 3]
change1 = x[, 4]
uncertainty1 = x[, 5]
complexity1 = x[, 6]
hierarchical1 = x[, 7]
flexibility1 = x[, 8]
total_ave1 = x[, 9]
total_agg1 = x[, 10]

summary (interaction1)
summary(autonomy1)
summary(change1)
summary(uncertainty1)
summary(complexity1)
summary(hierarchical1)
summary(flexibility1)
summary(total_ave1)
summary(total_agg1)

help("lm")

resC1 = lm(interaction1 ~  ilr(xc))
anova(resC1)



resC2 = lm(autonomy1 ~  ilr(xc))
anova(resC2)


resC3 = lm(change1~  ilr(xc))
anova(resC3)

resC4 = lm(uncertainty1~  ilr(xc))
anova(resC4)

resC5 = lm(complexity1~  ilr(xc))
anova(resC5)

resC6 = lm(hierarchical1~  ilr(xc))
anova(resC6)

resC7 = lm(flexibility1~  ilr(xc))
anova(resC7)

resC8 = lm(total_ave1~  ilr(xc))
anova(resC8)

resC9 = lm(total_agg1~ ilr(xc))
resC9

anova(resC1)
anova(resC2)
anova(resC3)
anova(resC4)
anova(resC5)
anova(resC6)
anova(resC7)
anova(resC8)

anova(resC9)

help("ilrInv")


total_agg0 = coef(resC9)[1]
total_agg0

c(coef(resC9)[3])
c(coef(resC9)[5])
c(coef(resC9)[3],-coef(resC9)[2])
c(coef(resC9)[6],coef(resC9)[5], coef(resC9)[4], coef(resC9)[3],-coef(resC9)[2])
  
gradient = ilrInv(coef(resC9)[-1], orig=xc)
  gradorth = ilrInv(c(coef(resC9)[3],-coef(resC9)[2]))
  gradorth = ilrInv(c(coef(resC9)[6],-coef(resC9)[5], -coef(resC9)[4], coef(resC9)[3],-coef(resC9)[2]))
  
  gradient
    NROW(gradient);NCOL(gradient)

  
  gradorth
  NROW(gradorth);NCOL(gradorth)
  
  
  scalar(gradient, gradorth)

vaux = vcov(resC9)
vartotal_agg0 = vaux[1,1]
vargradient = ilrvar2clr(vaux[-1,-1])

help("plot")
help("pch")
help("par")
c(1,2)
c(3,3,1,1)
#acomp(xc)
mar=c(3,3,3,1,1,1)
length(mar)

seq(from=-1, to=3, by=0.1)

help("straight")
par(mfrow=c(1,2),mar=c(3,3,1,1))
 plot(acomp(xc))
 straight(mean(xc), gradient, lwd=2)
 #straight(intercept, slope, lty=2)
levels = seq(from=-1, to=3, by=1)-mean(total_agg1)
length(levels)
colscale = bpy.colors(length(levels))
for(i in 1:length(levels)){
 straight(mean(xc)+levels[i]/(norm(gradient))^2 *gradient, gradorth, col=colscale[i])
}
plot(acomp(xc), pch=20+total_agg1, add=TRUE)
#bg=colset[total_agg1], 

r90 = sqrt(qchisq(0.90, df=2) )
r99 = sqrt(qchisq(0.99, df=2) )

 plot(gradient, pch=19)
 plot( gradient, add=TRUE)
 ellipses(gradient, vargradient, r=r90)
 ellipses(gradient, vargradient, r=r99)
 ellipses( gradient, vargradient, r=r90, lty=2)
 ellipses( gradient, vargradient, r=r99, lty=2)
 isoProportionLines(by=1)

 
 
 # check coefficient significance of the model
y = balance(xc, expr=~(x[,26]/x[,27])/x[,31])

mydat = data.frame(y,total_agg1)
 colnames(mydat)
rescheck = lm(total_agg1 ~x[,26]*x[,28]*x[,31]+x[,31]*x[,27], mydat)
coeftest(rescheck)

y
```

```{r}
require("compositions")
autoload("bpy.colors", "sp")
require("lmtest")

x <- read.csv("C:\\Users\\Owner\\Dropbox\\time management\\Second paper_demographics recoded.csv", header = TRUE)
#colnames(x)

xc = acomp(x, c(26, 27, 28, 29, 30, 31))
colnames(xc)

summary(xc)

interaction1 = x[, 2]
autonomy1 = x[, 3]
change1 = x[, 4]
uncertainty1 = x[, 5]
complexity1 = x[, 6]
hierarchical1 = x[, 7]
flexibility1 = x[, 8]
total_ave1 = x[, 9]
total_agg1 = x[, 10]

summary (interaction1)
summary(autonomy1)
summary(change1)
summary(uncertainty1)
summary(complexity1)
summary(hierarchical1)
summary(flexibility1)
summary(total_ave1)
summary(total_agg1)

help("lm")

resC1 = lm(ilr(xc) ~  interaction1)
anova(resC1)



resC2 = lm(ilr(xc) ~  autonomy1)
anova(resC2)


resC3 = lm(ilr(xc) ~  change1)
anova(resC3)

resC4 = lm(ilr(xc) ~  uncertainty1)
anova(resC4)

resC5 = lm(ilr(xc) ~  complexity1)
anova(resC5)

resC6 = lm(ilr(xc) ~  hierarchical1)
anova(resC6)

resC7 = lm(ilr(xc) ~  flexibility1)
anova(resC7)

resC8 = lm(ilr(xc) ~  total_ave1)
anova(resC8)

resC9 = lm(ilr(xc) ~  total_agg1)
resC9

anova(resC9)

help("ilrInv")


coef(resC9)

compcoef = ilrInv(coef(resC9), orig = xc)
compcoef
#compcoef[1, ]
#compcoef[2, ]

 intercept = acomp(compcoef[1, ])
 intercept
 
 slope = acomp(compcoef[2, ])
 slope
 
 help("vcov")
 vaux = vcov(resC9)
 
 vaux
 
 help("ilrvar2clr")
 
2*(0:1) +1
 vaux[2 * (0:1) + 1, 2 * (0:1) + 1]
 vaux[2 * (0:1) + 2, 2 * (0:1) + 2]
 
 varintercept = ilrvar2clr(vaux[2 * (0:1) + 1, 2 * (0:1) + 1])
  varintercept
  
 varslope = ilrvar2clr(vaux[2 * (0:1) + 2, 2 * (0:1) + 2])
 varslope
 
 help("plot")
 par(mfrow=c(1,2),mar=c(3,3,1,1))
 plot(acomp(xc), pch=20+total_agg1)
 straight(intercept, slope, lwd=2)
 r = sqrt(qchisq(0.90, df=2) )
 r99 = sqrt(qchisq(0.99, df=2) )
for(i in 0:2){
 pivot = cbind(ilr1=c(1,i,0,0), ilr2=c(0,0,1,i))
 myvar = ilrvar2clr( t(pivot) %*% vaux %*% pivot )
 ellipses(intercept+i*slope, myvar, r=r90, col=colset[i+1])
 ellipses(intercept+i*slope, myvar, r=r99, col=colset[i+1])
}

  plot(compcoef, pch=c("a","b"))
 ellipses(slope, varslope, r=r90)
 ellipses(slope, varslope, r=r99)
 ellipses(intercept, varintercept, r=r90, col=2)
 ellipses(intercept, varintercept, r=r99, col=2)
 isoProportionLines(by=0.5)
 ellipses(effectL, varL, r=r90, lty=2)
 ellipses(effectL, varL, r=r99, lty=2)

```



```{r}
update.packages("rlang")

library("lm")
read.csv2("Milad325_7")
model1 <- lm(formula =Total_agg_ST ~ 1 +	
               
               Age +	Parents_checking +	Parent_concern_need + Sibling +	Car_status + Financial_aids +	Grant	+ Scholarship	+ Loan + Major_of_study +	Time_Academic +	Time_Extracurricular +	Time_work +	Time_social +	Time_family +	 Co_op +	Internship +	GPA +	Ethinicity_1	+  Gender_1 +Father_education_1	+ Mother_education_1	+ Houshold_status_1+ Houshold_living_status_1,
                data    = Milad325_7)

summary(model1)

coef(model1)
  AIC(model1)
```
```{r}
install.packages("caret")


library(caret)
# Simple linear regression model (lm means linear model)
model <- train(mpg ~ wt,
               data = mtcars,
               method = "lm")

# Multiple linear regression model
model <- train(mpg ~ .,
               data = mtcars,
               method = "lm")

# Ridge regression model
model <- train(mpg ~ .,
               data = mtcars,
               method = "ridge") # Try using "lasso"



## 10-fold CV
# possible values: boot", "boot632", "cv", "repeatedcv", "LOOCV", "LGOCV"
fitControl <- trainControl(method = "repeatedcv",   
                           number = 10,     # number of folds
                           repeats = 10)    # repeated ten times

model.cv <- train(mpg ~ .,
               data = mtcars,
               method = "lasso",  # now we're using the lasso method
               trControl = fitControl)  

model.cv   

model.cv <- train(mpg ~ .,
               data = mtcars,
               method = "lasso",
               trControl = fitControl,
               preProcess = c('scale', 'center')) # default: no pre-processing

?train    # if you need more information about the train function
model.cv

# Here I generate a dataframe with a column named lambda with 100 values that goes from 10^10 to 10^-2
lambdaGrid <- expand.grid(lambda = 10^seq(10, -2, length=100))

model.cv <- train(mpg ~ .,
               data = mtcars,
               method = "ridge",
               trControl = fitControl,
               preProcess = c('scale', 'center'),
               tuneGrid = lambdaGrid,   # Test all the lambda values in the lambdaGrid dataframe
               na.action = na.omit)   # Ignore NA values

model.cv



fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           search = "random")  # hyper-parameters random search 

model.cv <- train(mpg ~ .,
               data = mtcars,
               method = "ridge",
               trControl = fitControl,
               preProcess = c('scale', 'center'),
               na.action = na.omit)

model.cv


ggplot(varImp(model.cv))


predictions <- predict(model.cv, mtcars)

predictions
```

