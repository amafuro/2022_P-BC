#パッケージ読み込み
library(tidyverse)
library(ggpmisc)
library(cowplot)
library(patchwork)
library(ProgressEbar)
library(grid)

Xjiku="P application (kg/10 a)"
Yjiku="concentration (mg/ g DW)"

#生データを読み込む
raw_data <- read_csv("CSV/211202ICP_plant.csv")
#元素の列名取得，あとでフォルダ作成・画像の名付けに使用
element_list=colnames(raw_data[4:length(raw_data)])
#以下の2つはプログレスバーの表示につかう
#全元素数取得
n<-length(raw_data)-3
#minでループ変数の最小値、maxでループ変数の最大値、styleで表示スタイルを設定

pe<-Progress.Ebar(max = n,style = 3)


raw_data=raw_data%>%
  #読み込んだ順にデータをならべる
  mutate(plant = fct_inorder(plant),treat = fct_inorder(treat)) %>% 
  #列番号で指定してデータフレームを縦長に
  pivot_longer(4:16, names_to="Element", values_to="cont")

# summariseのフレンドリーな警告文を非表示
options(dplyr.summarise.inform = F)

for (x in element_list) {
  
  
  
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
    #元素ごとではなく部位ごとにフォルダを作成して図を保存する
    element_file_name=paste("2021土耕fig/ICP/棒グラフ_回帰直線/",x,sep="")
    if (dir.exists(element_file_name)==F) {
      
      fs::dir_create(element_file_name)
      
    }
    #rとP値
    # cor.test
    cor_test=plot_regressionline %>% 
      select(x=1,y=2)
    cor <- cor.test(cor_test$x,cor_test$y)
    # cor_display
    cor_display <- 
      cor$estimate %>% 
      round(2) %>% 
      format(nsmall = 2)
    # p_display
    p_display <- case_when(
      cor$p.value < 0.001 ~ "< 0.001",
      cor$p.value < 0.01 ~ "< 0.01",
      cor$p.value < 0.05 ~ "< 0.05",
      TRUE ~ "> 0.05 (n.s.)",
    )
    
    #グラフ描画開始
    
    
    #空のグラフ描画
    bdp1<- ggplot(data=plot_regressionline,#回帰直線のR^２を表示させるために，ここでデータフレーム指定しておく必要アリ？
                  aes(x= treat,y=cont))
    bdp2<- bdp1+
      #軸ラベル設定
      labs(title=plant_parts,subtitle=bquote(italic(r)~"="~.(cor_display)~","~italic(p)~.(p_display)) ,
           x=Xjiku, y=Yjiku)+
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
        width=4,#エラーバーの幅を指定
        #size=1.05#エラーバーの太さを指定
      )
    
    #生データプロット
    bdp5 <- bdp4 +
      geom_jitter(
        data=plot_regressionline,
        aes(x= treat, y= cont),
        stat="identity",#統計処理はしないでグラフを表示する
        #size=3,#プロットの大きさ指定
        pch=1,#プロットの形指定
        position=position_jitter(0))#プロットをどれだけブレさせるか
    
    #回帰直線描画
    bdp6 <- bdp5 + geom_smooth(method = "lm", se = FALSE,
                               formula = y ~ x,color="darkslateblue"
                               #size=1.2
                               )
    
    #グラフの調整
    bdp8<-bdp6+
      # 論文っぽい見た目にする．(cowplotパッケージが別途必要)．フォントサイズと線の太さを設定
      theme_cowplot()+
      # 見た目を整える(タイトルの位置，縦横比など)
      theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
            #aspect.ratio = 1
      )+
      theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 1))  
    #できた画像をp1~p7として設定
    assign(paste("p",z-3,sep=""),bdp8)
    
  }
  #描画デバイスを開く
  png(paste(element_file_name,"/ICP_",x,"一覧.png",sep=""), width=480, height=480*1.25)
  #patchworkで画像をまとめる
  bdp8<-(p1+p2+p3)/(p4+p5+p6)/(p7+plot_spacer()+plot_spacer())
  Yjiku2=paste(x," ",Yjiku,sep="")
  PW.G.labeling(pw = bdp8,titlelab = NULL,xlab = Xjiku,ylab = Yjiku2)
  #描画デバイスを閉じる
  dev.off()
  
  svg(filename = paste(element_file_name,"/ICP_",x,"一覧.svg",sep=""),width=8,height=9)
  PW.G.labeling(pw = bdp8,titlelab = NULL,xlab = Xjiku,ylab = Yjiku2)
  dev.off()
  
  bdp8<-p1+p2+p3+p4+p5+p6+p7+plot_layout(ncol = 7)
  svg(filename = paste(element_file_name,"/ICP_",x,"一覧forポスター.svg",sep=""),width=24,height=4)
  PW.G.labeling(pw = bdp8,titlelab = NULL,xlab = Xjiku,ylab = Yjiku2)
  dev.off()
  #やってる元素の順番をyに設定
  y=match(x,element_list)
  #進捗をプログレスバーで表示
  set.PE(pe,y)
  
}

beepr::beep(3)
