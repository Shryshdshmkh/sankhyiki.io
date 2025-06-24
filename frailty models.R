library(CopulaCenR)
library(survival)
library(parfm)
library(frailtyEM)
library(frailtypack)
library(ggplot2)
data(Kidney)
head(Kidney)
Kidney$sex <-Kidney$sex-1

modf <-parfm(Surv(obs_time,status)~sex+age, cluster="id", data=Kidney, dist="exponential", frailty="gamma")

modf

ci.parfm(modf,level=0.05)["sex",]


p<-predict(modf)
p



parfm (Surv(obs_time,status)~sex+age, cluster="id", data=Kidney, dist="exponential",frailty="ingau")



data("cgd")
cgd<-cgd[c("tstart","tstop","status","id","sex","treat")]
head(cgd)


parfm (Surv(obs_time,status)~sex+age, cluster="id", data=Kidney, dist="exponential",frailty="ingau")



data("cgd")
cgd<-cgd[c("tstart","tstop","status","id","sex","treat")]
head(cgd)


gam <-emfrail(Surv(tstart, tstop,status)~ sex+treat+cluster(id), data=cgd)
summary(gam)

p1 <- autoplot(gam, type="pred",
               newdata=data.frame(sex="male", treat="rIFN-g"))+ ggtitle("rIFN-g") +ylim(c(0,2)) + guides(colour=FALSE)
p1


p2 <- autoplot(gam, type="pred",
               newdata=data.frame(sex="male", treat="placebo"))+ ggtitle("placebo") +ylim(c(0,2)) + guides(colour=FALSE)
p2



data("readmission")
frailtyPenal(formula = Surv(time,event)~cluster(id)+as.factor(dukes)+as.factor(charlson)+
               sex + chemo, data=readmission, cross.validation=TRUE, n.knots=10, kappa=100)






data(Kidney)
clayton_cox <- rc_spCox_copula(data = Kidney,
                               var_list = c("age","sex","disease"),
                               copula = "Clayton",
                               method = "BFGS",
                               B = 2)
summary(clayton_cox)
