mexp<-rexp(1000,0.2)
hist(mexp)
abline(v=mean(mexp))
mns = NULL
for (i in 1 : 1000) mns = c(mns, mean(rexp(1000,0.2)))
hist(mns)
abline(v=5,col=2)
qqnorm(mns)
qqline(mns,col=2)
sds = NULL
for (i in 1 : 1000) sds = c(mns, sd(rexp(1000,0.2)))
hist(sds)
abline(v=5,col=2)

tg1 <- ToothGrowth %>% filter(supp=="VC")
tg1<-tg1 %>% mutate(rate=len/dose)

tg2 <- ToothGrowth %>% filter(supp=="OJ")
tg2<-tg2 %>% mutate(rate=len/dose)

tg11<- tg1 %>% filter(dose==0.5)
tg12<- tg1 %>% filter(dose==1)
tg13<- tg1 %>% filter(dose==2)

tg21<- tg2 %>% filter(dose==0.5)
tg22<- tg2 %>% filter(dose==1)
tg23<- tg2 %>% filter(dose==2)

#calculate summaries for each sub group of the two groups 
tg1 %>% group_by(dose) %>% summarise(mean=mean(rate),sd=sd(rate),n=n())
tg2 %>% group_by(dose) %>% summarise(mean=mean(rate),sd=sd(rate),n=n())

#test if growth rates in each subgroup are the same so that we can take the mean
#of the whole group
t.test(tg11$rate,tg12$rate,paired=F,var.equal = F)
t.test(tg11$rate,tg13$rate,paired=F,var.equal = F)
t.test(tg12$rate,tg13$rate,paired=F,var.equal = F)

#CI for mean growth rate for tg11 and tg12 includes 0 so we cannot rule out
#that the mean growth rate for both tg21 and tg22 can be the same
#it might make sense to group tg11 and tg12 together and assume a common mean 
#growth rate. 

#merge tg21 and tg22 into one frame
tg11<-rbind(tg11,tg12)

#reassign tg23 to tg22
tg12<-tg13

#test if growth rates in each of the tg2 groups are the same 
#so that we can take the mean of the whole group
t.test(tg21$rate,tg22$rate,paired=F,var.equal = F)
t.test(tg21$rate,tg23$rate,paired=F,var.equal = F)
t.test(tg22$rate,tg23$rate,paired=F,var.equal = F)

#CI for mean growth rate for tg21 and tg22 includes 0 so we cannot rule out
#that the mean growth rate for both tg21 and tg22 can be the same
#it might make sense to group tg21 and tg22 together and assume a common growth 
#rate. 

#merge tg21 and tg22 into one frame
tg21<-rbind(tg21,tg22)

#reassign tg23 to tg22
tg22<-tg23

#check rates and hypothesis test for groups according to dose
t.test(tg11$rate,tg21$rate,var.equal=F,paired=F)
t.test(tg12$rate,tg22$rate,var.equal=F,paired=F)



t.test(tg1$rate)
t.test(tg2$rate)


plot(x=row.names(tg1),y=tg1$rate,type="p",pch=1,ylab="Rate",
     main="Tooth Growth Rate According to Supplement",
     xlab="Observation No.", ylim=c(0,50))
lines(x=row.names(tg1),y=tg1$rate,type="l",lty=1,lwd=2)
points(x=row.names(tg2),y=tg2$rate,pch=1,col=2)
lines(x=row.names(tg2),y=tg2$rate,type="l",lty=1,lwd=2,col=2)

