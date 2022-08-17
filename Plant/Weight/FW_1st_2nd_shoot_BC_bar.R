library(tidyverse)
library(cowplot)

#CSVファイル読み込み(パスを記入)
raw_data=read_csv("CSV/BCtest_220610_2ndNL_FW_shoot.csv")

#生データからmean(平均値)とsd(標準偏差)のデータフレームを作っておく
data_mean_sd=raw_data%>%
  #読み込んだ順にデータをならべる
  mutate(treat = fct_inorder(treat))%>%
  #グループ化
  group_by(treat) %>% 
  summarise(mean=mean(weight),sd=sd(weight))

# 最大値を取得．のちに使用(グラフの上限設定用) 
ylim =max(data_mean_sd$mean+data_mean_sd$sd)*1.2

----------------------------------------------------------------------------------------
#グラフ描画開始
#棒グラフとドットプロットが一緒になったものを描いていく
#複数種類のデータを使うときにはNULLにしておく
bdp1<- ggplot(NULL) 

#グラフのベースを作っていくよ
bdp2<- bdp1+
  labs(title="2nd NL",x="BC treatment", y="Fresh weight (g/shoot)")+
  
  scale_y_continuous(expand = c(0,0), limits = c(0,ylim))+
  theme_classic()


#棒グラフを描くよ
bdp3<-bdp2+
  geom_bar(
    data=data_mean_sd,
    #X軸はtreatment
    aes(x= treat, 
        #y軸は平均値
        y= mean      
        #treatmentによって塗りつぶしを変える(同じ処理区で複数棒グラフを書く場合)
        #,fill=treat　
    ),
    #統計処理はしないでグラフを表示する
    stat="identity", 
    #線の色は黒
    color="black",
    #棒グラフの色を指定(棒グラフの数の分必要)
    fill=(values=c("palegreen","palegreen2","palegreen3","palegreen4"))
    #棒グラフの間の調整はここを使う
    #,position="dodge"　
  )


#散布図を描くよ
bdp4<-bdp3+
  geom_jitter(
    data=raw_data,
    #xはtreatmentのfactorによって分ける
    aes(x= treat,  
        #Y軸で使う値
        y= weight　
        #shape=treat
    ),
    stat="identity",
    size=3,
    pch=1,
    position=position_jitter(0)
  )

#エラーバーをつけるよ
bdp5<-bdp4+
  geom_errorbar(
    data=data_mean_sd,
    aes(x=treat,
        y=mean,
        ymin=mean-sd,
        ymax=mean+sd),
    width=0.17
    # size=1.05
  )

#装飾をするよ
bdp6<-bdp5+
  #X軸の棒グラフの順番を変える
  # scale_x_discrete(limit=c('0P', '12P', '24P','48P')) +
  # 論文っぽい見た目にする．(cowplotパッケージが別途必要)．フォントサイズと線の太さを設定
  theme_cowplot(font_size = 25)+
  # 見た目を整える(タイトルの位置，縦横比など)
  theme(plot.title = element_text(hjust = 0.5))#+
#theme(axis.text.x = element_text(angle = 45, hjust = 1))


----------------------------------------------------------------------------------------
#画像を保存
svg("2022_リン＆バイオ炭fig/weight/220610_BC_2ndNL_shoot.svg",width=8,height=8)
print(bdp6)
dev.off()
----------------------------------------------------------------------------------------
  
  
#fxの列をfactor型，vxの列をnumeric型にする
fx=as.factor(raw_data$treat)
vx=as.numeric(raw_data$weight)

#Tukey検定する
print(multcomp::cld(multcomp::glht(aov(vx~fx),linfct=multcomp::mcp(fx="Tukey")),level=0.05,decreasing=T))
