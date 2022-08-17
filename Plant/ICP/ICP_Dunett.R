#作図する用のコードを検定用に転用した
# 検定する元素
i="P"

#パッケージ読み込み
library(tidyverse)

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
  group_by(plant,treat,Element) %>% #グループ化
  arrange(cont) %>% #グループごとにcontが高い順に並び替え
  slice(2:4) %>% #N=5からN=3
  summarise(mean=mean(cont),sd=sd(cont))%>% #meanとsdを計算
  #treatの列の”P”を消してリン施肥量だけにする
  mutate(treat=gsub(treat,pattern="P",replacement = "", ignore.case = TRUE)) %>%
  #描画したい元素だけにする
  filter(Element==i)



#型変換
data_mean_sd$treat=as.numeric(data_mean_sd$treat)



#横長にして植物の部位ごとの列をつくる
data_long= data_mean_sd%>% 
  pivot_wider(names_fro = plant,values_from = mean)



#部位ごとに繰り返し操作を行う
for (z in 4:length(data_long)){
  
  #植物体の部位に対応した列を選択
  data_for_plot=data_long %>%dplyr::select(1:3,z)
  #NAを含む列を削除
  data_for_plot<- na.omit(data_for_plot)
  #部位名を取得
  plant_parts=colnames(data_for_plot[4]) 
  #4列目の列名をmeanに変更
  data_for_plot <- dplyr::select(data_for_plot, "mean" = 4, dplyr::everything())
  
  #生データプロットと回帰直線用データフレーム作成
  plot_regressionline=raw_data %>% 
    filter(plant==plant_parts,Element==i) %>% 
    mutate(treat=gsub(treat,pattern="P",replacement = "", ignore.case = TRUE)) %>% 
    select(treat,cont) %>% 
    #N=5からN=３にする
    group_by(treat) %>% 
    arrange(cont) %>% 
    slice(2:4)
  #型変換
  plot_regressionline$treat=as.numeric(plot_regressionline$treat)
  
  print(i)
  print(plant_parts)
  
  #生データプロットと回帰直線用データフレーム作成(検定用にこれを転用)
  dx=raw_data %>% 
    filter(plant==plant_parts,Element==i) %>% 
    select(treat,cont) %>% 
    #N=5からN=３にする
    group_by(treat) %>% 
    arrange(cont) %>% 
    slice(2:4)
  #dの2列目（treat）をfxという列名にする
  dx <- dplyr::select(dx, "fx" = 1,"vx"=2, dplyr::everything())
  
  #fxの列をfactor型，vxの列をnumeric型にする
  fx=as.factor(dx$fx)
  vx=as.numeric(dx$vx)
  
  
  #ダネット検定する
  #「パッケージ名::関数」でパッケージを指定して関数を動かす
  #tidyverse とmultcompでselect関数が被ってバクるのを防ぐ
  #これでパッケージ指定で関数動かすとlibraryで読み込む必要もなくなる
  d=summary(multcomp::glht(aov(vx~fx),linfct=multcomp::mcp(fx="Dunnet")))
  print(d)
  
}
