#おんどとりデータの日付ごとの最大値，最小値を取得
library(readr)
library(tidyverse)
library(dplyr)
raw_data <- read_csv("C:/Users/yanagisawa naoki/Desktop/研究室/2　土耕ポット栽培試験/温度取り/210810_DOKOU_lx_tenp.csv")
raw_data=raw_data%>%
  mutate(date = fct_inorder(date))#読み込んだ順にデータをならべる.

data_max_low=raw_data%>%
  mutate(date = fct_inorder(date))%>%#読み込んだ順にデータをならべる
  group_by(date2)%>%#date2でグループ化
  summarise(max_lx=max(lx,na.rm = TRUE),min_lx=min(lx,na.rm = TRUE),#lxの最大値と最小値を取得（,na.rm = TRUEでNA無視）
            max_tenp=max(tenp,na.rm = TRUE),min_tenp=min(tenp,na.rm = TRUE))#tenpの最大値と最小値を取得（,na.rm = TRUEでNA無視）

#グラフ描画
library(ggplot2)
library(reshape2)
library(cowplot)
library(scales)
#照度と温度をすべて描画する．見にくいから個別のコードを下の方にそれぞれ書く

#データフレームをグラフ描画用に整理する
data_for_plot <- melt(data_max_low, id = "date2", measure = c("max_lx","min_lx", "max_tenp","min_tenp"))
#折れ線グラフを描画
a <- ggplot(data_for_plot, aes(x = date2, y = value, group = variable, colour = variable)) + geom_line()+
  scale_x_date(breaks = date_breaks("months"),labels = date_format("%b"))
#グラグの装飾
a <- a + theme_cowplot(font_size = 16, line_size = 1.25)+
  # 見た目を整える(タイトルの位置，縦横比など)
  theme(plot.title = element_text(hjust = 0, size = 22),aspect.ratio = 0.5)+
  # タイトル，軸ラベルの内容を設定．上付き文字等も可能
  labs(title="おんどとりデータ", x="Month", y="lx・tenp")
#データのポイントをうつ
a <- a + geom_point(aes(colour = variable))
a






#max_lx
png("C:/Users/yanagisawa naoki/Desktop/研究室/2　土耕ポット栽培試験/fig/lx_max  .png")
lx_max_for_plot=data_for_plot %>% filter(variable=="max_lx")#データフレーム選択
a <- ggplot(lx_max_for_plot, aes(x = date2, y = value)) + geom_line(colour="tomato")+
  scale_x_date(breaks = date_breaks("months"),labels = date_format("%b"))
a <- a + theme_cowplot(font_size = 16, line_size = 1.25)+
  theme(plot.title = element_text(hjust = 0, size = 22),aspect.ratio = 0.4)+
  labs(title="おんどとりデータ", x="Month", y="lx")
#a <- a + geom_point(aes(colour = variable))
a
dev.off()

#min_lx
png("C:/Users/yanagisawa naoki/Desktop/研究室/2　土耕ポット栽培試験/fig/lx_min .png")
lx_min_for_plot=data_for_plot %>% filter(variable=="min_lx")#データフレーム選択
a <- ggplot(lx_min_for_plot, aes(x = date2, y = value)) + geom_line(colour = "darkslategray2")+
  scale_x_date(breaks = date_breaks("months"),labels = date_format("%b"))
a <- a + theme_cowplot(font_size = 16, line_size = 1.25)+
  theme(plot.title = element_text(hjust = 0, size = 22),aspect.ratio = 0.3)+
  labs(title="おんどとりデータ", x="Month", y="lx")
#a <- a + geom_point(aes(colour = "darkslategray2"))
a
dev.off()

#tenp
png("C:/R-4.0.3/2021土耕ポット栽培試験/2021土耕fig/tenp.png")
tenp_for_plot=data_for_plot %>% filter(variable=="max_tenp"|variable=="min_tenp")#データフレーム選択
a <- ggplot(tenp_for_plot, aes(x = date2, y = value,colour = variable)) + geom_line()+
  scale_x_date(breaks = date_breaks("months"),labels = date_format("%b"))
a <- a + theme_cowplot(font_size = 16, line_size = 1.25)+
  theme(plot.title = element_text(hjust = 0, size = 22),aspect.ratio = 0.7)+
  labs(title="日ごとの最低気温，最低気温", x="Month", y="Tenperature (°C)")
#a <- a + geom_point(aes(colour = variable))
a
dev.off()

#1日あたりの平均照度
mean_lx=raw_data%>%
  mutate(date = fct_inorder(date))%>%#読み込んだ順にデータをならべる
  group_by(date2)%>%#date2でグループ化
  summarise(mean=mean(lx,na.rm = TRUE))

#lx_mean
png("C:/Users/yanagisawa naoki/Desktop/研究室/2　土耕ポット栽培試験/fig/lx_mean.png")
a <- ggplot(mean_lx , aes(x = date2, y = mean  )) + geom_line(colour ="mediumpurple2")+
  scale_x_date(breaks = date_breaks("months"),labels = date_format("%b"))
a <- a + theme_cowplot(font_size = 16, line_size = 1.25)+
  theme(plot.title = element_text(hjust = 0, size = 22),aspect.ratio = 0.5)+
  labs(title="おんどとりデータ", x="Month", y="lx")
#a <- a + geom_point(aes(colour = variable))
a
dev.off()
