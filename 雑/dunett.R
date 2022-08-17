#fxの中で最も若い英数字が対照群に選ばれることに注意
d=read.table("clipboard",header=TRUE)
d=data.frame(d)

library(multcomp)

fx=as.factor(d$fx)
vx=as.numeric(d$vx)

summary(glht(aov(vx~fx),linfct=mcp(fx="Dunnet")))

