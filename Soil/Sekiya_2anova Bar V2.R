library(tidyverse)
library(cowplot)
library(ProgressEbar)
library(patchwork)

d=read_csv("CSV/2021Sekiya_All_data.csv") %>% 
  filter(soil=="rhizoshere"|soil=="noplant") %>% 
  arrange(No) %>% 
  mutate(soil=fct_inorder(soil)) %>% 
  mutate(treat=gsub(treat,pattern="P",replacement = "", ignore.case = TRUE))



fra_list=c("Ca","Fe","Al")

for (z in fra_list) {
  
  d_test=d %>% 
    filter(fraction==z) %>% 
    mutate(Soil=soil,Treat=treat)
  
  #Two-way ANOVAをやる
  anovasummary <- summary(aov(cont~Treat*Soil,d_test)) 
  anovasummary <- anovasummary[[1]]
  #p値を抜き出す
  anovap <- as.data.frame(anovasummary[1:3,5])
  #要因の名前の列を作る
  anovap$factor <- as.character(row.names(anovasummary)[1:3])
  #謎に要因の後ろにスペースができる場合があるので消しておく
  anovap$factor <- sub(" .*", "", anovap$factor) 
  #交互作用のところを良い感じにしてみた。
  anovap$factor <- sub(paste(anovap[1,2],":",anovap[2,2],sep = ""), "Interaction", anovap$factor) 
  anovap$factor <- sub("Treat", "P treatment", anovap$factor) 
  colnames(anovap) <- c("p-value", "factor")
  
  #確認
  head(anovap)
  
  changep <- function(a) {
    #p値の大きさによって、返す文字列を決める
    p <- function(a) {
      if (a < 0.001) {
        
        return("p < 0.001")
      } else if (a < 0.01){
        return("p < 0.01")
      } else if (a < 0.05) {
        return("p < 0.05")
      } else {
        return("n.s.")
      }
    }
    
    #全要因に対して上の関数を当てはめ、結果を返す
    c <- NULL
    for (i in 1:nrow(a)){
      c <- paste0(c, "\n", a[i, 2], " : ", p(a[i, 1]))
    }
    return(c)
  }
  
  
  #作図コーナー
  dp=d %>% 
    filter(fraction==z)
  
  #生データからmean(平均値)とsd(標準偏差)のデータフレームを作っておく (このデータフレームは棒グラフとエラーバーにつかう)
  data_mean_sd=dp%>%
    group_by(treat,soil)%>%#treatとsoilの両方でグループ分けをする
    summarise(mean=mean(cont),sd=sd(cont))
  
  # 最大値を取得．のちに使用(グラフの上限設定用) 
  ylim =570
  
  #グラフ描画開始
  
  #複数種類のデータを使うとき(今回は生データ「raw_data」と平均値，標準偏差「data_mean_sd」)にはNULLにしておく
  bdp1<- ggplot(NULL)
  
  #空のグラフを作る
  bdp2<- bdp1+
    labs(title=paste(z,"-P",sep = ""),x="", y="",caption = changep(anovap))+
    scale_y_continuous(expand = c(0,0), limits = c(0,ylim))+
    theme_classic()
  
  #棒グラフお絵かき
  bdp3<-bdp2+
    geom_bar(
      data=data_mean_sd,#データフレームはdata_mean_sdを使う
      aes(x= treat, #X軸はカードゲームの種類
          y=mean,#y軸は平均点
          fill=soil),#ゲームを行うプレイヤーごとに色を変える      
      stat="identity", #統計処理はしないでグラフを表示する
      position = position_dodge(width = 0.9),#棒グラフが重ならないようにする
      color="black"#枠線の色は黒
    )+
    scale_fill_manual(values=c("palegreen3","Wheat2"),name="Soil",
                      labels=c(rhizoshere="Rhizoshere",noplant="No plant"))
  
  #散布図お絵かき
  bdp4<-bdp3+
    geom_jitter(
      data=dp,#データフレームはraw_dataを使う
      aes(x= treat,  #xはgameによって分ける
          y= cont,　#Y軸で使う値
          shape=soil,#ポケモンごとに形を変える
          group=soil),#ポケモンごとにグループ分けしてpositonでずらせるようにしておく
      stat="identity",
      position = position_jitterdodge(  jitter.width = 0,#jitterdodgeでポケモンごとにずらしつつ，ポイントが重ならないようにしておく
                                        jitter.height = 0,
                                        dodge.width = 0.9,
                                        seed = NA)
      # size=2
      )+#プロットの大きさを指定
    scale_shape_manual(values=c(0,1),name="Soil",
                       labels=c(rhizoshere="Rhizoshere",noplant="No plant"))#プロットの形を手動で設定する
  
  
  #エラーバーお絵かき
  bdp5<-bdp4+
    geom_errorbar(
      data=data_mean_sd,#データフレームはdata_mean_sdを使う
      aes(x=treat,#xはgameによって分ける
          y=mean,#y軸は平均点
          ymin=mean-sd,
          ymax=mean+sd,
          group=soil),#同じゲームでポケモンごとに変える
      position = position_dodge(width = 0.9),
      width=0.17#エラーバーの幅を指定
      # size=0.5#エラーバーの太さを指定
    )
  
  
  #グラフの装飾
  bdp6<-bdp5+
    # 論文っぽい見た目にする．フォントサイズと線の太さを設定
    theme_cowplot(font_size = 25)+
    # 見た目を整える(タイトルの位置，縦横比など)
    theme(plot.title = element_text(hjust = 0.5))
  
  
  assign(z,bdp6)
  
}

pw=Ca+theme(legend.position="none")+
  Fe+theme(legend.position="none")+
  Al+plot_layout(ncol = 3)

print(pw)

# pw2=Ca+theme(legend.position="none")+
#   Fe+theme(legend.position="none")+
#   Al+theme(legend.position="none")+
#   plot_layout(ncol = 2)
# 
# Xjiku="P treatment (kg/10 a)"
# Yjiku="P concentration (mg/100 g)"

# png("2021土耕fig/関谷法/2anova Sekiya.png", width=480, height=480)
# 
# PW.G.labeling(pw = pw,titlelab = NULL,xlab = Xjiku,ylab = Yjiku)
# 
# dev.off()
# 
# svg("2021土耕fig/関谷法/2anova Sekiya.svg", width=8, height=8)
# PW.G.labeling(pw = pw2,titlelab = NULL,xlab = Xjiku,ylab = Yjiku)
# dev.off()
# 
# svg("2021土耕fig/関谷法/2anova Sekiya凡例.svg", width=14, height=5)
# PW.G.labeling(pw = pw,titlelab = NULL,xlab = Xjiku,ylab = Yjiku)
# dev.off()


# #two way anovaと全リン酸のグラフの結合用
# g1=g1+theme(axis.title.y = element_blank())
# bdp9=((g1+theme(legend.position="none"))|
#         (g2+theme(legend.position="none"))|
#         (g3))/
#   (Ca+theme(legend.position="none")|
#   Fe+theme(legend.position="none")|
#   Al)
# svg(filename = "2021土耕fig/交換態&two way anova.svg",width=12*1.3,height=8*1.3)
# PW.G.labeling(pw = bdp9,titlelab = NULL,xlab = Xjiku,ylab = Yjiku)
# dev.off()

