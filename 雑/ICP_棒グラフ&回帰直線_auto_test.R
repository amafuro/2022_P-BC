#描画する元素リスト
element_list=c("Al","B","Ca","Cu","Fe","K","Mg","Mn","Na","P","S","Si","Zn")

#パッケージ読み込み
library(tidyverse)
library(ggplot2)
library(ggpmisc)
library(cowplot)

#生データを読み込む
raw_data <- read_csv("CSV/ICP_plant.csv")
raw_data=raw_data%>%
  #読み込んだ順にデータをならべる
  mutate(plant = fct_inorder(plant),treat = fct_inorder(treat)) %>% 
  #列番号で指定してデータフレームを縦長に
  pivot_longer(4:16, names_to="Element", values_to="cont")

# summariseの警告文を非表示
options(dplyr.summarise.inform = FALSE)

#図を格納するフォルダがなかったら作成する
if (dir.exists("ICP_棒グラフ&回帰直線_test")==F) {
  
  dir.create("ICP_棒グラフ&回帰直線_test")
  
}


for (x in element_list) {
  print(x)
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
    filter(Element==x)
  
  
  
  #型変換
  data_mean_sd$treat=as.numeric(data_mean_sd$treat)
  
  
  
  #横長にして植物の部位ごとの列をつくる
  data_long= data_mean_sd%>% 
    pivot_wider(names_fro = plant,values_from = mean)
  
  
  
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
    
    #生データプロットと回帰直線用データフレーム作成
    plot_regressionline=raw_data %>% 
      filter(plant==plant_parts,Element==x) %>% 
      mutate(treat=gsub(treat,pattern="P",replacement = "", ignore.case = TRUE)) %>% 
      select(treat,cont) %>% 
      #N=5からN=３にする
      group_by(treat) %>% 
      arrange(cont) %>% 
      slice(2:4)
    #型変換
    plot_regressionline$treat=as.numeric(plot_regressionline$treat)
    
    # 最大値を取得．のちに使用(グラフの上限設定用) 
    ylim =max(plot_regressionline$cont)*1.2
    
    #図を格納するフォルダがなかったら作成する
    element_file_name=gsub(" ","",paste("ICP_棒グラフ&回帰直線_test/",x))
    if (dir.exists(element_file_name)==F) {
      
      dir.create(element_file_name)
      
    }
    
    
    #グラフ描画開始
    # png関数で描画デバイスを開く（部位と元素ごとに名前を変更）
    png_name=paste("ICP_棒グラフ&回帰直線_test/",x,"/211202土耕ポット植物体ICP_",plant_parts,"_",x,".png")
    png(gsub(" ","",png_name))
    
    #空のグラフ描画
    bdp1<- ggplot(data=plot_regressionline,#回帰直線のR^２を表示させるために，ここでデータフレーム指定しておく必要アリ？
                  aes(x= treat,y=cont))
    bdp2<- bdp1+
      #軸ラベル設定
      labs(title=plant_parts,x="Treatment ( kg/10 a)", y="Concentration ( mg/ g DW)")+
      #グラフの上限を設定
      scale_y_continuous(expand = c(0,0), limits = c(0,ylim)
      )+theme_classic()
    
    #棒グラフ
    bdp3<-bdp2+
      geom_bar(
        data=data_for_plot,
        aes(x= treat,y=mean),
        stat="identity", #統計処理はしないでグラフを表示する
        color="black",　#線の色は黒
        fill=(values=c("0P"="palegreen","12P"="palegreen2","24P"="palegreen3","48P"="palegreen4")))+
      #X軸のメモリを１２ごとに表示する
      scale_x_continuous(breaks=seq(0,48,length=5),limits=c(-10,60),labels = scales::comma)
    
    #エラーバーお絵かき
    bdp4<-bdp3+
      geom_errorbar(
        data=data_for_plot,
        aes(x=treat,y=mean,
            ymin=mean-sd,ymax=mean+sd),
        width=0.17,#エラーバーの幅を指定
        size=1.05#エラーバーの太さを指定
      )
    
    #生データプロット
    bdp5 <- bdp4 +
      geom_jitter(
        data=plot_regressionline,
        aes(x= treat, y= cont),
        stat="identity",#統計処理はしないでグラフを表示する
        size=3,#プロットの大きさ指定
        pch=1,#プロットの形指定
        position=position_jitter(0))#プロットをどれだけブレさせるか
    
    #回帰直線描画
    bdp6 <- bdp5 + geom_smooth(method = "lm", se = FALSE,
                               formula = y ~ x,color="darkslateblue",
                               size=1.2)
    #決定係数(R^2)をグラフに表示
    bdp7 <- bdp6 + stat_poly_eq(formula = y ~ x,
                                aes(label = paste(stat(rr.label),sep = "~~~")),
                                parse = TRUE,size=7)
    #グラフの調整
    bdp8<-bdp7+
      # 論文っぽい見た目にする．(cowplotパッケージが別途必要)．フォントサイズと線の太さを設定
      theme_cowplot(font_size = 18, line_size = 1.25)+
      # 見た目を整える(タイトルの位置，縦横比など)
      theme(plot.title = element_text(hjust = 0.5, size = 27),aspect.ratio = 1)
    
    # 描画デバイスに反映
    print(bdp8)
    
    dev.off()
    
  }
  
}