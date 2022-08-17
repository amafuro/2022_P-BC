library(tidyverse)
library(cowplot)
library(ProgressEbar)

#生データを読み込む(生データはプロットにつかう)
raw_data <- read_csv("CSV/2021dokou_P_All_data.csv")
raw_data=raw_data%>%
  mutate(treat = fct_inorder(treat),fraction = fct_inorder(fraction)) %>% 
  mutate(treat=gsub(treat,pattern="P",replacement = "", ignore.case = TRUE)) %>% 
  pivot_wider(names_from = fraction,values_from = cont) %>% 
  mutate(Po=Total-(Al+Fe+Ca)) %>% 
  select(No,treat,soil,Al,Fe,Ca,Po) %>% 
  pivot_longer(4:7,names_to = "fraction",values_to = "cont") %>% 
  # filter(soil=="bulk") %>% 
  group_by(No) %>% 
  arrange(No,cont) %>% 
  pivot_wider(names_from = soil,values_from = cont)
  

for (z in 4:length(raw_data)) {
  
for_plot=raw_data %>% 
  select(1:3,z) %>% 
  na.omit() %>% 
  mutate(fraction=gsub(fraction,pattern="Ca",replacement = "Ca-P", ignore.case = TRUE)) %>% 
  mutate(fraction=gsub(fraction,pattern="Fe",replacement = "Fe-P", ignore.case = TRUE)) %>% 
  mutate(fraction=gsub(fraction,pattern="Al",replacement = "Al-P", ignore.case = TRUE)) %>% 
  mutate(fraction=gsub(fraction,pattern="Po",replacement = "Po", ignore.case = TRUE))

soil_type=colnames(for_plot[4])

for_plot <- dplyr::select(for_plot, "cont" = 4, dplyr::everything())

#生データからmean(平均値)とsd(標準偏差)のデータフレームを作っておく (このデータフレームは棒グラフとエラーバーにつかう)
data_mean_sd=for_plot%>%
  # mutate(treat = fct_inorder(treat),fraction = fct_inorder(fraction))%>%#読み込んだ順にデータをならべる
  group_by(treat,fraction)%>%
  summarise(mean=mean(cont),sd=sd(cont)) %>% 
  arrange(treat,mean)

# 最大値を取得．のちに使用(グラフの上限設定用) 
ylim =1600
  # max(data_mean_sd$mean+data_mean_sd$sd)*1.2


#型変換
data_mean_sd$fraction <- factor(data_mean_sd$fraction, levels = c("Ca-P","Fe-P","Al-P","Po"))


#グラフ描画開始

#平均値積み上げ
g <-ggplot(data_mean_sd, aes(x = treat, y = mean, fill = fraction))+ 
  # scale_y_continuous(expand = c(0,0), limits = c(0,ylim))+
  geom_bar(stat = "identity",color="black")+ 
  scale_fill_manual(values=c("darkorange1","seashell4","cornsilk1","Wheat4"))+
  scale_color_continuous(name="Fraction")+
  labs(fill="Fraction")+
  scale_y_continuous(expand = c(0,0), limits = c(0,1600))
  



#グラフの装飾
g <- g +
  # 論文っぽい見た目にする．フォントサイズと線の太さを設定
  theme_cowplot(font_size = 25)+
  # 見た目を整える(タイトルの位置，縦横比など)
  theme(plot.title = element_text(hjust = 0.5))+
  # タイトル，軸ラベルの内容を設定．上付き文字等も可能
  labs(title=soil_type, x="", y="")#+
#X軸のラベルを45度傾ける
#theme(axis.text.x = element_text(angle = 45, hjust = 1))

assign(paste("g",z-3,sep=""),g)

}

#描画デバイスを開く
png("2021土耕fig/2021全リン積み上げグラフまとめ.png", width=480*1, height=480*1.1)

#patchworkで画像をまとめる
g4<-g1+theme(legend.position="none")+
  g2+theme(legend.position="none")+
  g3+patchwork::plot_layout(ncol = 3)

Xjiku="P application (kg/10 a)"
Yjiku="P concentration (mg/100 g)"
PW.G.labeling(pw = g4,titlelab = NULL,xlab = Xjiku,ylab = Yjiku)

#描画デバイスを閉じる
dev.off()

svg(filename = "2021土耕fig/2021全リン積み上げグラフまとめ.svg",width=14,height=5)
PW.G.labeling(pw = g4,titlelab = NULL,xlab = Xjiku,ylab = Yjiku)
dev.off()

g1=g1+labs(y="P concentration (mg/100 g)")
print(g1)