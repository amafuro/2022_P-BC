#iを定義する
i="rhizoshere"

library(tidyverse)

#CSV読みこみ
raw_data <- read_csv("CSV/2021Sekiya_All_data.csv") %>% 
  #soilの列がiと同じものだけにする
  filter(soil==i) %>% 
  #NAの行を削除
  na.omit()

#dっていうデータフレーム作成
d=raw_data %>% 
  #Noとtreatの列でグループ化する
  group_by(No,treat) %>% 
  #contの列を計算してvxという列名にする（足し算）
  summarise(vx=sum(cont)) 
#dの2列目（treat）をfxという列名にする
d <- dplyr::select(d, "fx" = 2, dplyr::everything())

library(multcomp)

#fxの列をfactor型，vxの列をnumeric型にする
fx=as.factor(d$fx)
vx=as.numeric(d$vx)

#ダネット検定する
print(multcomp::cld(multcomp::glht(aov(vx~fx),linfct=multcomp::mcp(fx="Tukey")),level=0.05,decreasing=T))