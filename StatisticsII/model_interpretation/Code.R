

##setwd('/Users/giannisdekoulakos/Desktop/karlis')
install.packages("readxl")
##library("readxl")
##d=read_excel( "/Users/giannisdekoulakos/Desktop/karlis/project_I.xls")



d$SUBSCRIBED[d$SUBSCRIBED=="no"] <-0
d$SUBSCRIBED[d$SUBSCRIBED=="yes"] <-1
d$SUBSCRIBED<-as.factor(d$SUBSCRIBED)

d$job<-as.factor(d$job)
d$marital<-as.factor(d$marital)


table(d$education)


educ <- function(educ){
  
  if (educ=='illiterate') {
    return('illiterate')
    
  }else if (educ=='basic.4y' | educ=='basic.6y'|educ=='basic.9y'){
    return('basic')
  }else if (educ=='high.school' ){
    return('intermediate')
  }else if(educ=='university.degree'| educ=='professional.course'){
    return("advance")
  }else{
    return(educ)
  }
}

d$education <- sapply(d$education,educ)



d$education<-as.factor(d$education)

table(d$education)



d$default<-as.factor(d$default)
d$housing<-as.factor(d$housing)
d$loan<-as.factor(d$loan)


d$contact<-as.factor(d$contact)
d$day_of_week<-as.factor(d$day_of_week)

d$month<-as.factor(d$month)


d$poutcome<-as.factor(d$poutcome)


hist(d$pdays, xlab="pdays",main=" ", col = 'lightblue')# vlepoume pws ta perrissotera einai 999, prsosferei idia plhroforia me poutcome to vgazoume

d$pdays<-NULL

d$previous # poly paromoio me poutcome tha doume

hist(d$age, xlab="age",main=" ", col = 'lightblue')## vlepoume oti sto 30-50 paratirountai oi perissoteres parathrhseis opote isws prepei na toi kanoume factor

## METATROPH AGE SE FACTOR
d$age<-cut(d$age,breaks=c(16,25,50,70, max(d$age)),labels=c(">25","25-50","50-70" ,">70"))
d$age<- as.factor(d$age) 
levels(d$age) <- c(">25","25-50","50-70",">70" )


##install.packages("psych")
##require(psych)
index <- sapply(d, class) == "numeric"
d_num <- d[,index]
d_fac<- d[,!index]
str(d_num)
str(d_fac)
str(d)

table(d$previous)
table(d$poutcome)


variable <- c("age",'job','marital','education','default','housing','loan','contact','season','day_of_week','duration','campaign','poutcome','emp.var.rate','cons.price.idx','cons.conf.idx','euribor3m','nr.employed','previous' ,'SUBSCRIBED')
Description<- c("Age of the client",'Type of job','marital status ','education of the client','has credit or default?','Indicates whether client has a housing loan','Indicates whether client has a personal loan','Type of call contact communication','Season of call','day of call ','duration of call in sec ','Number of contacts made during current campaign for this clien','outcome of the previous marketing campaign','Employment variation rate','Consumer price index','Consumer confidence index','Euribor 3 month interest rate','Number of employed citizens in Country','number of contacts performed before this campaign','has the client suscribed the term deposit?')
Type <- c("Factor",'Factor','Factor','Factor','Factor','Factor','Factor','Factor','Factor','Factor','num','num','Factor','num','num','num','num','num','num','Factor')
my_df<- data.frame(variable,Description,Type)
tab_df(my_df,title='Table 1. List of the attributes from the dataset after cleaning', file="variablesf.doc")




######NUMERIC

descr<-describe(d_num)

tab_df(descr, vars=F,  file="description1.doc")



#boxplot
par(mfrow = c(3,3)) 
for ( i in 1:9) {
  boxplot(d_num[[i]]~d$SUBSCRIBED,xlab='SUBSCRIBED',ylab=names(d_num)[i],  data=d,col="lightgrey")
}


install.packages("ggplot2")
library(ggcorrplot)


ggcorrplot(cor(d_num), type = "lower", lab = TRUE, insig = "blank")

library(corrplot)
par(mfrow = c(1,1))
corrplot(cor(d_num), method = "number" ) 

#gia report table correlation
install.packages("tab_corr")
library(tab_corr)
library(sjPlot)
tab_corr(d_num ,corr.method = "pearson",show.p=TRUE,p.numeric = FALSE,fade.ns = TRUE,digits = 3,triangle = "lower",remove.spaces = TRUE , file="corr1.doc")



descr<-describe(d_num)

tab_df(descr, vars=F,  file="description1.doc")


######categorical


##barblots gia na doume pou stoxeoun oi dhmioyrgei tis ereunas 
par(mfrow = c(3,2))
for ( i in 1:6) { 
  barplot(prop.table(table(d_fac[[i]])),main=names(d_fac)[i],col="lightgrey",cex.names=0.8)
}
##h synexeia
par(mfrow = c(3,2))
for ( i in 7:13) { 
  barplot(prop.table(table(d_fac[[i]])),main=names(d_fac)[i],col="lightgrey" , cex.names=0.8)
}


##barplot se sxesh me yes or no gia kateh metavliti

str(data_fac)
par(mfrow = c(4,4))


install.packages('gridExtra')
library('gridExtra')

library('ggplot2')

pl1<-ggplot(d_fac, aes(x = job ,fill =SUBSCRIBED)) +geom_bar(position = "dodge")

pl2<-ggplot(d_fac, aes(x = marital ,fill =SUBSCRIBED)) +geom_bar(position = "dodge")

pl3<-ggplot(d, aes(x = age ,fill =SUBSCRIBED)) +geom_bar(position = "dodge")

pl4<-ggplot(d_fac, aes(x = education ,fill =SUBSCRIBED)) +geom_bar(position = "dodge")
pl5<-ggplot(d_fac, aes(x = default ,fill =SUBSCRIBED)) +geom_bar(position = "dodge")
pl6<-ggplot(d_fac, aes(x = housing ,fill =SUBSCRIBED)) +geom_bar(position = "dodge")
pl7<-ggplot(d_fac, aes(x = loan ,fill =SUBSCRIBED)) +geom_bar(position = "dodge")
pl8<-ggplot(d_fac, aes(x = contact ,fill =SUBSCRIBED)) +geom_bar(position = "dodge")
pl9<-ggplot(d_fac, aes(x = month ,fill =SUBSCRIBED)) +geom_bar(position = "dodge")
pl10<-ggplot(d_fac, aes(x = day_of_week ,fill =SUBSCRIBED)) +geom_bar(position = "dodge")
pl11<-ggplot(d_fac, aes(x = poutcome ,fill =SUBSCRIBED)) +geom_bar(position = "dodge")





pl1
pl9
pl4
grid.arrange(pl1, pl4,pl9,ncol=1)
grid.arrange(pl2, pl3,pl5,pl6,pl7,pl8,pl11,pl10,ncol=2)
par(mfrow = c(3,1))
grid.arrange(pl10, pl4 ,ncol=2)



####################################################################
###### modeling ####################################################
####################################################################
str(d)

full_model<-glm(SUBSCRIBED ~ .,data=d,family=binomial)
summary(full_model)
##Null deviance: 25925  on 39882  degrees of freedom
##Residual deviance: 15598  on 39832  degrees of freedom
##AIC: 15700



str(d)


####################### step with AIC

aic.model <- step(full_model,direction="both")


model_aic<- glm(SUBSCRIBED ~ age + marital + education + default + contact + 
                  month + day_of_week + duration + campaign + poutcome + emp.var.rate + 
                  cons.price.idx + cons.conf.idx + euribor3m, data=d,family=binomial)

summary(model_aic)

##Null deviance: 25925  on 39882  degrees of freedom
##Residual deviance: 15619  on 39848  degrees of freedom
##AIC: 15689


vif(model_aic)
##provlima 


##afairw xerata  emp.var.rate
model_aic2<- glm(SUBSCRIBED ~ age + marital + education + default + contact + 
                   month + day_of_week + duration + campaign + poutcome + 
                   cons.price.idx + cons.conf.idx + euribor3m, data=d,family=binomial)

vif(model_aic2)## pali provlima oriako me euribor 

##polles me tavlites mh shmantikes sto summary kalutera na dokimasw bic 
summary(model_aic2)
##Null deviance: 25925  on 39882  degrees of freedom
##Residual deviance: 15963  on 39849  degrees of freedom
##AIC: 16031
##megalwse h aic tha dokimasoume step me bic 

##vgazw kai euribor logo vif 
model_aic3<- glm(SUBSCRIBED ~ age + marital + education + default + contact + 
                   month + day_of_week + duration + campaign + poutcome + 
                   cons.price.idx + cons.conf.idx , data=d,family=binomial)

summary(model_aic3)
##Residual deviance: 16340  on 39850  degrees of freedom
##AIC: 16406


##xeirotero den mas kanei 


library(stargazer)

##stargazer(model_aic3,title="Final Model",align=TRUE,single.row=T,
##out="/Users/giannisdekoulakos/Desktop/karlis/tableaic.htm")



####################### step with BIC

bic.model <- step(full_model,direction="both",k=log(nrow(d)))

model_bic <- glm(SUBSCRIBED ~ default + contact + month + duration + poutcome + 
                   emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m
                 , data=d, family=binomial)

summary(model_bic)
###Null deviance: 25925  on 39882  degrees of freedom
###Residual deviance: 15702  on 39863  degrees of freedom
###AIC: 15742


vif(model_bic)##provlima vgazw emp.var.rate xerata



model_bic2 <-glm(SUBSCRIBED ~ default + contact + month + duration + poutcome + 
                    cons.price.idx + cons.conf.idx +euribor3m
                 , data=d, family=binomial)


summary(model_bic2)

#Null deviance: 25925  on 39882  degrees of freedom
#Residual deviance: 16075  on 39864  degrees of freedom
#AIC: 16113

vif(model_bic2)##provlima tha vgalw cons.price.idx 

model_bic3 <-glm(SUBSCRIBED ~ default + contact + month + duration + poutcome + 
                    cons.conf.idx +euribor3m
                 , data=d, family=binomial)

vif(model_bic3)
summary(model_bic3)
#Residual deviance: 16105  on 39865  degrees of freedom
#AIC: 16141
#kalytero apo aic tha dokimasoume lasso prwta

##stargazer(model_bic3,title="model_bic3",align=TRUE,single.row=T,
##out="/Users/giannisdekoulakos/Desktop/karlis/model_bic3.htm")






#######################
##########lasso
#######################

lambdas <- 10 ^ seq(8,-4,length=250)

x <- model.matrix(SUBSCRIBED~., d)[,-1]
y <- d$SUBSCRIBED



library(glmnet)

str(d)
cv.lasso <- cv.glmnet(x, y, alpha = 1, lamda =lambdas ,family = "binomial")

plot(cv.lasso)
coef(cv.lasso, s = cv.lasso$lambda.1se)

model_lasso<-glm(SUBSCRIBED ~ age + job+marital+education+ default+housing + contact + month+day_of_week+ duration+ campaign+
                   poutcome+  emp.var.rate +cons.price.idx + nr.employed
                 , data=d, family=binomial)

summary(model_lasso)###polles metavlites mh shmantikes 
##Residual deviance: 15650  on 39835  degrees of freedom
##AIC: 15746

##library(car)
vif(model_lasso) ###provlima me 2 


##tha kanw step me aic sto modelo tou lasso

lasso_aic<- step(model_lasso,direction="both")

model_lasso_aic <-glm(SUBSCRIBED ~ age + marital + education + default + contact + 
                        month + day_of_week + duration + campaign + poutcome + 
                        cons.price.idx + nr.employed, data=d, family=binomial)


##evgala kai  emp.var.rate ektos ogo vif alla kratise pali mh shmantikes mesa 
summary(model_lasso_aic)
##Residual deviance: 15893  on 39850  degrees of freedom
##AIC: 15959
vif(model_lasso_aic )#ok


##stargazer(model_lasso_aic,title="model_lasso_aic",align=TRUE,single.row=T,
##out="/Users/giannisdekoulakos/Desktop/karlis/model_lasso_aic.htm")






##tha kanw step me bic sto modelo tou lasso

lasso_bic<- step(model_lasso,direction="both",k=log(nrow(d)))


model_lasso_bic <-glm(SUBSCRIBED ~ default + contact + month + duration + campaign + 
                        poutcome + emp.var.rate + cons.price.idx + nr.employed
                      , data=d, family=binomial)

summary(model_lasso_bic)

##Residual deviance: 15748  on 39863  degrees of freedom
##AIC: 15788
vif(model_lasso_bic) ##provlima afairw emp.var



model_lasso_bic2 <-glm(SUBSCRIBED ~ default + contact + month + duration + campaign + 
                        poutcome + cons.price.idx + nr.employed
                      , data=d, family=binomial)

summary(model_lasso_bic2)
##Residual deviance: 15978  on 39864  degrees of freedom
##AIC: 16016
vif(model_lasso_bic2)##okk



##twra the eleksw me wald test an oi 4 metavlites(marital,education,day_of_week,age) pou evgale h bic eprepe h oxi 


wald.test(b = coef(model_lasso_aic), Sigma = vcov(model_lasso_aic), Terms = 2:4)##age kakws thn evgale thn kratame 
wald.test(b = coef(model_lasso_aic), Sigma = vcov(model_lasso_aic),Terms = 5:7) ## maritale thn vgazw
wald.test(b = coef(model_lasso_aic), Sigma = vcov(model_lasso_aic), Terms = 8:13)# education kakws thn evgale thn krataw
wald.test(b = coef(model_lasso_aic), Sigma = vcov(model_lasso_aic), Terms = 26:29)# day of week ektos  

wald.test(b = coef(model_lasso), Sigma = vcov(model_lasso), Terms = 16:18)## maritale thn vgazw
wald.test(b = coef(model_lasso), Sigma = vcov(model_lasso), Terms = 2:4)##age kakws thn evgale thn kratame 
wald.test(b = coef(model_lasso), Sigma = vcov(model_lasso), Terms = 19:22)# education kakws thn evgale thn krataw
wald.test(b = coef(model_lasso), Sigma = vcov(model_lasso), Terms = 37:40)# day of week ektos  

#vazw thn age kai thn education mesa symfwna me wald tests
model_lasso_bic3 <- glm(SUBSCRIBED ~+ cons.price.idx+ age+ education+default + contact + month + duration + campaign + 
                          poutcome  + nr.employed
                        , data=d, family=binomial)


summary(model_lasso_bic3 )
#Residual deviance: 15917  on 39857  degrees of freedom
#AIC: 15969

#vlepw pws cons.price.idx den einai shmantiko sto summary kanw kai ena wald test kai epivevaiwnetai tha to afairesw
wald.test(b = coef(model_lasso_bic3), Sigma = vcov(model_lasso_bic3), Terms = 2)

# afairw cons.price.idx
model_lasso_bic4 <- glm(SUBSCRIBED ~ + age+ education + default + contact + month + duration + campaign + 
                          poutcome  + nr.employed 
                        , data=d, family=binomial)


summary(model_lasso_bic4)
#Residual deviance: 15917  on 39858  degrees of freedom
#AIC: 15967




##h aic kai h big xwris to lasso eixan kratisei kai to cons.conf.idx tha to prosthesoume sto modelo na doume 

model_lasso_bic5 <- glm(SUBSCRIBED ~ + cons.conf.idx + age + education + default + contact + month + duration + campaign + 
                          poutcome  + nr.employed 
                        , data=d, family=binomial)






r2_tjur(model_lasso_bic5)
##0.356

summary(model_lasso_bic5)
##Residual deviance: 15906  on 39857  degrees of freedom
##AIC: 15958
## to kalytero modelo apo ta para panw ( apo ayta pou den exoun thema vif)

vif(model_lasso_bic5)

##15906/39857


## the tsekarw kai upoloipes metavlites poy petaksan eksw aic kai bic enw o lasso tis kratise t

wald.test(b = coef(model_lasso), Sigma = vcov(model_lasso), Terms = 5)## job lvl shmantiko , 
wald.test(b = coef(model_lasso), Sigma = vcov(model_lasso), Terms = 25:26) ##  housing swsta ekso
summary(model_lasso)



## vazw to job 
model_lasso_bic6 <- glm(SUBSCRIBED ~ +job+ cons.conf.idx + age + education + default + contact + month + duration + campaign + 
                          poutcome  + nr.employed 
                        , data=d, family=binomial)







summary(model_lasso_bic6)
##Residual deviance: 15875  on 39846  degrees of freedom
##AIC: 15949

model_lasso_bic7<- glm(SUBSCRIBED ~ +job+ cons.conf.idx + age + education + default +  month + duration + campaign + 
                          poutcome  + nr.employed 
                        , data=d, family=binomial)
##vgazw contact den prepei na einai sto montelo statistika mh shmantiko 
wald.test(b = coef(model_lasso_bic6), Sigma = vcov(model_lasso_bic6), Terms = 23)

model_lasso_bic7<- glm(SUBSCRIBED ~+job + cons.conf.idx + age + education + default  + duration + campaign + 
                         poutcome  + nr.employed +month
                       , data=d, family=binomial)


summary(model_lasso_bic7)
##Null deviance: 25925  on 39882  degrees of freedom
##Residual deviance: 16558  on 39856  degrees of freedom
##AIC: 16612


r2_tjur(model_lasso_bic7)

vif(model_lasso_bic7) ##ok


#15878/39847 #0.39 ok

qchisq(0.95,39847)## den thelw kati megalytero apo   40312.48 ayto swsta den exw
with(model_lasso_bic7, pchisq(deviance, df.residual, lower.tail = FALSE)) #1



with(model_lasso_bic7, null.deviance - deviance)
with(model_lasso_bic7, df.null - df.residual)
with(model_lasso_bic7, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)) #0
logLik(mylogit)




library(pscl)
p<- pR2(model_lasso_bic7)
format (p, scientific=FALSE)


##stargazer(model_lasso_bic7,title="Final Model",align=TRUE,single.row=T,
## out="/Users/giannisdekoulakos/Desktop/karlis/model_lasso_bic7.htm")




logLik(model_lasso_bic6)
##-7937.699

logLik(model_lasso_bic5)
##-7953.031

2*(-7937.699-(-7953.031))
##30.664

df
##37-26
## 11 apwxw poly 

##sthn ousia vriskw me ton typo 2*(logLikmegaloumontelou- logLikmikrou) enan aritho autos thelw na einai megalyteros apo to apotelesma qchisq(0.95 , x), opoy x einai h diafora twn df pou mou edwse to loglik, gia na kanw reject

##ho oloi oi syntelstes 0 b3=b4=b5=0
##h1 toulaxiston enas diaferei 
##kanw reject thn HO  ara thn metavlith job thn xreiazomai
## paromoiws kai tis alles me idio tropo test

qchisq(0.95 , 11) ## 19.6 apexw poly 

##paromopia kai gai model (model_lasso_bic7



library(lmtest)
lrtest(model_lasso_bic5,model_lasso_bic7) 
lrtest(model_lasso_bic4,model_lasso_bic7) 
lrtest(model_lasso_bic3,model_lasso_bic7) 
lrtest(model_lasso_bic2,model_lasso_bic7) 

lrtest(model_lasso_bic6,model_lasso_bic7) 
lrtest(model_lasso_aic ,model_lasso_bic7) 
lrtest(model_bic3 ,model_lasso_bic7) 
lrtest(model_aic3 ,model_lasso_bic7) 
##sygkrinomtas ola ta montela kataliksame oti to model_lasso_bic6 exei kalytero fit anamenomeno apo deviance/df


compare_performance(model_lasso_bic6,model_lasso_bic7 )







##### Assuptions


plot(model_lasso_bic7,which =4,id.n=3) ## inlfuencial values 

library(broom)
library(tidyverse)

final_data<- augment(model_lasso_bic7) %>% 
  mutate(index = 1:n()) 

ggplot(final_data, aes(index, .std.resid)) + 
  geom_point(aes(color = SUBSCRIBED), alpha = .5) +
  theme_bw()

final_data%>% top_n(3, .cooksd)

final_data%>% 
  filter(abs(.std.resid) > 3)



library(tab_corr)
library(sjPlot)
frame_df<-vif(model_lasso_bic7) 
tab_df (frame_df, file="vif.doc") 



