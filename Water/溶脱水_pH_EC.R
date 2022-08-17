library(tidyverse)
library(scales)
library(cowplot)
library(patchwork)
library(lubridate)

raw_data=read_csv("CSV/2022_pH_EC_leaching_water.csv")

#日ごとの処理区における平均値計算
#csvファイルでは日付の列は2022-09-05のように表示されるように設定しておく
summarised=raw_data %>% 
  mutate(treat=fct_inorder(treat),
         Day=as.Date(format="%Y-%m-%d",raw_data$Day)) %>% 
  group_by(Day,test,treat) %>% 
  summarise(pH=mean(pH),EC=mean(EC)) %>% 
  ungroup()

#日付を期間に変換
Day1=summarised$Day[1]
Days=summarised$Day
inter=int_length(Day1%--%Days)
inter2=inter/as.numeric(dweeks(1))

summarised=summarised %>% 
  mutate(Day=inter2) %>% 
  group_by(test) %>% 
  mutate(Day=if_else(test=="BC",Day-1,Day))

#pHとECを分ける
pH=summarised %>% 
  select(1:4)
EC=summarised %>% 
  select(1:3,5)

#グラフの文字の大きさ
fs=20

#pH_P試験
####################################################################################################################################
TL="P"

pH_plot_deta=pH %>% 
  filter(test==TL)

#折れ線グラフを描画
pH_plot<- ggplot(pH_plot_deta,
                 aes(x = Day, y = pH,
                     group = treat,
                     colour = treat,
                     linetype = treat
                 )) + 
  geom_line(size=0.9)+
  #横軸の表示を月ごとにする
  # scale_x_date(breaks = date_breaks("months"),labels = date_format("%b"))+
  #図を整える
  theme_cowplot(font_size = fs, line_size = 1.25)+
  theme(plot.title = element_text(hjust = 0, size = 22),aspect.ratio = 1)+
  labs(x="Weeks", y="pH")+
  scale_linetype_manual(values = c("solid","solid","solid","solid",
                                   "dashed","dashed","dashed"),
                        name="Treatment")+
  scale_color_manual(values = c("#E3A452","#D67253","#AD1F45","#5B3178",
                                "mediumseagreen","darkslategray3","darkslategray4"),
                     name="Treatment")

svg(filename = paste("2022_リン＆バイオ炭fig/water_pH_EC/2022_pH_",TL,".svg",sep=""))
print(pH_plot)
dev.off()
png(filename = paste("2022_リン＆バイオ炭fig/water_pH_EC/2022_pH_",TL,".png",sep=""))
print(pH_plot)
dev.off()

#pH_BC試験
######################################################################################################################################################################################################

TL="BC"

pH_plot_deta=pH %>% 
  filter(test==TL)

#折れ線グラフを描画
pH_plot2<- ggplot(pH_plot_deta,
                  aes(x = Day, y = pH,
                      group = treat,
                      colour = treat,
                      linetype = treat
                  )) + 
  geom_line(size=0.9)+
  #横軸の表示を月ごとにする
  # scale_x_date(breaks = date_breaks("months"),labels = date_format("%b"))+
  #図を整える
  theme_cowplot(font_size = fs, line_size = 1.25)+
  theme(plot.title = element_text(hjust = 0, size = 22),aspect.ratio = 1)+
  labs(x="Weeks", y="pH")+
  scale_linetype_manual(values = c("solid","solid","solid","solid"),
                        name="Treatment")+
  scale_color_manual(values = c("gray0","#E3A452","springgreen3","springgreen4"),
                     name="Treatment")


svg(filename = paste("2022_リン＆バイオ炭fig/water_pH_EC/2022_pH_",TL,".svg",sep=""))
print(pH_plot2)
dev.off()
png(filename = paste("2022_リン＆バイオ炭fig/water_pH_EC/2022_pH_",TL,".png",sep=""))
print(pH_plot2)
dev.off()

#EC_P試験
####################################################################################################################################

TL="P"

EC_plot_deta=EC %>% 
  filter(test==TL)

#折れ線グラフを描画
EC_plot<- ggplot(EC_plot_deta,
                 aes(x = Day, y = EC,
                     group = treat,
                     colour = treat,
                     linetype = treat
                 )) + 
  geom_line(size=0.9)+
  #横軸の表示を月ごとにする
  # scale_x_date(breaks = date_breaks("months"),labels = date_format("%b"))+
  #図を整える
  theme_cowplot(font_size = fs, line_size = 1.25)+
  theme(plot.title = element_text(hjust = 0, size = 22),aspect.ratio = 1)+
  labs(x="Weeks", y="EC (μS/cm)")+
  scale_linetype_manual(values = c("solid","solid","solid","solid",
                                   "dashed","dashed","dashed"),
                        name="Treatment")+
  scale_color_manual(values = c("#E3A452","#D67253","#AD1F45","#5B3178",
                                "mediumseagreen","darkslategray3","darkslategray4"),
                     name="Treatment")


svg(filename = paste("2022_リン＆バイオ炭fig/water_pH_EC/2022_EC_",TL,".svg",sep=""))
print(EC_plot)
dev.off()
png(filename = paste("2022_リン＆バイオ炭fig/water_pH_EC/2022_EC_",TL,".png",sep=""))
print(EC_plot)
dev.off()


#EC_BC試験
####################################################################################################################################
TL="BC"

EC_plot_deta=EC %>% 
  filter(test==TL)

#折れ線グラフを描画
EC_plot2<- ggplot(EC_plot_deta,
                  aes(x = Day, y = EC,
                      group = treat,
                      colour = treat,
                      linetype = treat
                  )) + 
  geom_line(size=0.9)+
  #横軸の表示を月ごとにする
  # scale_x_date(breaks = date_breaks("months"),labels = date_format("%b"))+
  #図を整える
  theme_cowplot(font_size = fs, line_size = 1.25)+
  theme(plot.title = element_text(hjust = 0, size = 22),aspect.ratio = 1)+
  labs(x="Weeks", y="EC (μS/cm)")+
  scale_linetype_manual(values = c("solid","solid","solid","solid"),
                        name="Treatment")+
  scale_color_manual(values = c("gray0","#E3A452","springgreen3","springgreen4"),
                     name="Treatment")


svg(filename = paste("2022_リン＆バイオ炭fig/water_pH_EC/2022_EC_",TL,".svg",sep=""))
print(EC_plot2)
dev.off()
png(filename = paste("2022_リン＆バイオ炭fig/water_pH_EC/2022_EC_",TL,".png",sep=""))
print(EC_plot2)
dev.off()


#作った画像を結合して出力する
####################################################################################################################################

Ptest=pH_plot+theme(legend.position="none")+EC_plot
BCtest=pH_plot2+theme(legend.position="none")+EC_plot2

png(filename = "2022_リン＆バイオ炭fig/water_pH_EC/2022_Ptest.png",width=480*2.2,height=480)
print(Ptest)
dev.off()
png(filename = "2022_リン＆バイオ炭fig/water_pH_EC/2022_BCtest.png",width=480*2.2,height=480)
print(BCtest)
dev.off()

svg(filename = "2022_リン＆バイオ炭fig/water_pH_EC/2022_Ptest.svg",width=6*2.2,height=6)
print(Ptest)
dev.off()
svg(filename = "2022_リン＆バイオ炭fig/water_pH_EC/2022_BCtest.svg",width=6*2.2,height=6)
print(BCtest)
dev.off()

cat("fin!")