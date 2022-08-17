# 描画する元素
i="P"

#パッケージ読み込み
library(tidyverse)
library(ggplot2)
library(ggpmisc)
library(cowplot)

#生データを読み込む
raw_data <- read_csv("CSV/211202ICP_plant.csv")
raw_data=raw_data%>%
  #読み込んだ順にデータをならべる
  mutate(plant = fct_inorder(plant),treat = fct_inorder(treat)) %>% 
  #列番号で指定してデータフレームを縦長に
  pivot_longer(4:16, names_to="Element", values_to="cont")



#生データからmean(平均値)とsd(標準偏差)のデータフレームを作っておく
data_mean_sd=raw_data%>%
  #読み込んだ順にデータをならべる
  mutate(plant = fct_inorder(plant),
         treat = fct_inorder(treat),
         Element = fct_inorder(Element))%>%
  group_by(plant,treat,Element)%>%
  summarise(mean=mean(cont),sd=sd(cont))



# 最大値を取得．のちに使用(グラフの上限設定用) 
ylim =max(data_mean_sd$mean+data_mean_sd$sd)*1.2



#横長にして植物の部位ごとの列をつくる
data_long= data_mean_sd%>% 
  pivot_wider(names_fro = plant,values_from = mean) %>% 
  #描画する元素をPだけに絞る
  filter(Element==i)



#部位ごとに繰り返し操作を行う
for (z in 4:length(data_long)){
  
  #植物体の部位に対応した列を選択
  data_for_plot=data_long %>%select(1:3,z)
  #NAを含む列を削除
  data_for_plot<- na.omit(data_for_plot)
  #部位名を取得
  plant_parts=colnames(data_for_plot[4]) 
  #4列目の列名をmeanに変更
  data_for_plot <- dplyr::select(data_for_plot, "mean" = 4, dplyr::everything())
  
  #回帰直線用データフレーム作成
  data_for_regressionline=raw_data %>% 
    filter(plant==plant_parts,Element==i) %>% 
    mutate(treat=gsub(treat,pattern="P",replacement = "", ignore.case = TRUE)) %>% 
    select(treat,cont) %>% 
    #N=5からN=３にする
    group_by(treat) %>% 
    arrange(cont) %>% 
    slice(2:4)
  #型変換
  data_for_regressionline$treat=as.numeric(data_for_regressionline$treat)

  #グラフ描画開始
  # png関数で描画デバイスを開く（部位と元素ごとに名前を変更）
  s=paste("2021土耕fig/ICP/回帰直線/",i,"/211202土耕ポット植物体ICP_",plant_parts,"_",i,".png")
  png(gsub(" ","",s))
  
  #空のグラフ描画
  bdp1<- ggplot(data=data_for_regressionline,
                aes(x= treat,y=cont))
  bdp2<- bdp1+
    labs(title=plant_parts,x="Treatment ( kg/10 a)", y="Concentration ( mg/ g DW)")#+
  #scale_y_continuous(expand = c(0,0), limits = c(0,ylim))+theme_classic()
  
  #生データプロット
  bdp3<-bdp2+geom_point()
  #回帰直線描画
  bdp4 <- bdp3 + geom_smooth(method = "lm", se = FALSE,
                             formula = y ~ x,color="darkorange2",
                             size=1.2)
  #決定係数(R^2)をグラフに表示
  bdp5 <- bdp4 + stat_poly_eq(formula = y ~ x,
                              aes(label = paste(stat(rr.label),sep = "~~~")),
                              parse = TRUE,size=7)
  #グラフの調整
  bdp7<-bdp5+
    theme_cowplot(font_size = 22, line_size = 2.5)+
    theme(plot.title = element_text(hjust = 0, size = 22),aspect.ratio = 1)
  
  # 描画デバイスに反映
  print(bdp7)
  
  dev.off()
}