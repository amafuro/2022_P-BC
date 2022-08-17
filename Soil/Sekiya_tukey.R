
#生データを読み込む(生データはプロットにつかう)
library(readr)

data <- read_csv("CSV/211126Sekiya_rhizoshere_Ca.csv")
library(multcomp)

#fxの列をfactor型，vxの列をnumeric型にする
fx=as.factor(data$treat)
vx=as.numeric(data$cont)

#ダネット検定する
print(multcomp::cld(multcomp::glht(aov(vx~fx),linfct=multcomp::mcp(fx="Tukey")),level=0.05,decreasing=T))