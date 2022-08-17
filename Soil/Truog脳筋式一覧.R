#生データを読み込む(生データはプロットにつかう)
library(tidyverse)
library(ProgressEbar)
library(grid)

konken <- read_csv("CSV/211005-06_Truog_根圏.csv") %>% 
  mutate(Phosphorus=gsub(Phosphorus,pattern="P",replacement = "", ignore.case = TRUE))
#生データからmean(平均値)とsd(標準偏差)のリストを作っておく (このリストは棒グラフとエラーバーにつかう)
mean <- aggregate(x=list(cont=konken$cont),by=list(Phosphorus=konken$Phosphorus),FUN=mean)
sd <- aggregate(x=list(cont=konken$cont),by=list(Phosphorus=konken$Phosphorus),FUN=sd)
konken2<-merge(mean,sd,by='Phosphorus')
colnames(konken2)<-c('Phosphorus','mean','sd')
# 最大値を取得．のちに使用(グラフの上限設定用) 
ylim =max(konken2$mean+konken2$sd)*1.2


#グラフ描画開始
library(ggplot2)
library(cowplot)
#棒グラフとドットプロットが一緒になったものを描いていくよ
bdp1<- ggplot(NULL) #複数種類のデータを使うときにはNULLにしておく
#グラフのベースを作っていくよ
bdp2<- bdp1+
  labs(title="Rhizoshere",x="Treatment", y="P concentration (mg/ 100 g)")+
  
  scale_y_continuous(expand = c(0,0), limits = c(0,ylim))+
  theme_classic()


#棒グラフを描くよ
bdp3<-bdp2+
  geom_bar(
    data=konken2,
    aes(x= Phosphorus, #X軸はPhosphorusment
        y=mean       #y軸は平均値
        #,fill=Phosphorus　#Phosphorusmentによって塗りつぶしを変える(同じ処理区で複数棒グラフを書く場合)
    ),
    stat="identity", #統計処理はしないでグラフを表示する
    color="black",　#線の色は黒
    fill=(values=c("0P"="bisque1","12P"="bisque2","24P"="bisque3","48P"="bisque4"))
    #,position="dodge"　#棒グラフの間の調整はここを使う
  )
#scale_fill_manual(values=c("gray"))


#散布図を描くよ
bdp4<-bdp3+
  geom_jitter(
    data=konken,
    aes(x= Phosphorus,  #xはPhosphorusmentのfactorによって分ける
        y= cont　#Y軸で使う値
        #shape=Phosphorus
    ),
    stat="identity",
    # size=3,
    pch=1,
    position=position_jitter(0))


#エラーバーをつけるよ
bdp5<-bdp4+
  geom_errorbar(
    data=konken2,
    aes(x=Phosphorus,
        y=mean,
        ymin=mean-sd,
        ymax=mean+sd),
    width=0.17
  )


#装飾をするよ
bdp6<-bdp5+
  #X軸の棒グラフの順番を変える
  # scale_x_discrete(limit=c('0P', '12P', '24P','48P')) +
  # 論文っぽい見た目にする．(cowplotパッケージが別途必要)．フォントサイズと線の太さを設定
  theme_cowplot()+
  # 見た目を整える(タイトルの位置，縦横比など)
  theme(plot.title = element_text(hjust = 0.5),axis.title.x = element_blank()
        # axis.title.y = element_blank()
        )#+
#theme(axis.text.x = element_text(angle = 45, hjust = 1))
t1=bdp6






#生データを読み込む(生データはプロットにつかう)
library(readr)

bulk <- read_csv("CSV/211005-06_Truog_バルク.csv") %>% 
  mutate(Phosphorus=gsub(Phosphorus,pattern="P",replacement = "", ignore.case = TRUE))
#生データからmean(平均値)とsd(標準偏差)のリストを作っておく (このリストは棒グラフとエラーバーにつかう)
mean <- aggregate(x=list(cont=bulk$cont),by=list(Phosphorus=bulk$Phosphorus),FUN=mean)
sd <- aggregate(x=list(cont=bulk$cont),by=list(Phosphorus=bulk$Phosphorus),FUN=sd)
bulk2<-merge(mean,sd,by='Phosphorus')
colnames(bulk2)<-c('Phosphorus','mean','sd')
# 最大値を取得．のちに使用(グラフの上限設定用) 
ylim =max(bulk2$mean+bulk2$sd)*1.2


#グラフ描画開始
library(ggplot2)
library(cowplot)
#棒グラフとドットプロットが一緒になったものを描いていくよ
bdp1<- ggplot(NULL) #複数種類のデータを使うときにはNULLにしておく
#グラフのベースを作っていくよ
bdp2<- bdp1+
  labs(title=" Bulk",x="Treatment", y="Concentration( mg/100g )")+
  
  scale_y_continuous(expand = c(0,0), limits = c(0,ylim))+
  theme_classic()


#棒グラフを描くよ
bdp3<-bdp2+
  geom_bar(
    data=bulk2,
    aes(x= Phosphorus, #X軸はPhosphorusment
        y=mean       #y軸は平均値
        #,fill=Phosphorus　#Phosphorusmentによって塗りつぶしを変える(同じ処理区で複数棒グラフを書く場合)
    ),
    stat="identity", #統計処理はしないでグラフを表示する
    color="black",　#線の色は黒
    fill=(values=c("0P"="bisque1","12P"="bisque2","24P"="bisque3","48P"="bisque4"))
    #,position="dodge"　#棒グラフの間の調整はここを使う
  )
#scale_fill_manual(values=c("gray"))


#散布図を描くよ
bdp4<-bdp3+
  geom_jitter(
    data=bulk,
    aes(x= Phosphorus,  #xはPhosphorusmentのfactorによって分ける
        y= cont　#Y軸で使う値
        #shape=Phosphorus
    ),
    stat="identity",
    # size=3,
    pch=1,
    position=position_jitter(0))


#エラーバーをつけるよ
bdp5<-bdp4+
  geom_errorbar(
    data=bulk2,
    aes(x=Phosphorus,
        y=mean,
        ymin=mean-sd,
        ymax=mean+sd),
    width=0.17
  )


#装飾をするよ
bdp6<-bdp5+
  #X軸の棒グラフの順番を変える
  # scale_x_discrete(limit=c('0P', '12P', '24P','48P')) +
  # 論文っぽい見た目にする．(cowplotパッケージが別途必要)．フォントサイズと線の太さを設定
  theme_cowplot()+
  # 見た目を整える(タイトルの位置，縦横比など)
  theme(plot.title = element_text(hjust = 0.5),axis.title.x = element_blank(),
        axis.title.y = element_blank())#+
#theme(axis.text.x = element_text(angle = 45, hjust = 1))
t2=bdp6





#生データを読み込む(生データはプロットにつかう)
library(readr)

noplant <- read_csv("CSV/211005-06_Truog_植物体無し.csv") %>% 
  mutate(Phosphorus=gsub(Phosphorus,pattern="P",replacement = "", ignore.case = TRUE))
#生データからmean(平均値)とsd(標準偏差)のリストを作っておく (このリストは棒グラフとエラーバーにつかう)
mean <- aggregate(x=list(cont=noplant$cont),by=list(Phosphorus=noplant$Phosphorus),FUN=mean)
sd <- aggregate(x=list(cont=noplant$cont),by=list(Phosphorus=noplant$Phosphorus),FUN=sd)
noplant2<-merge(mean,sd,by='Phosphorus')
colnames(noplant2)<-c('Phosphorus','mean','sd')
# 最大値を取得．のちに使用(グラフの上限設定用) 
ylim =max(noplant2$mean+noplant2$sd)*1.2


#グラフ描画開始
library(ggplot2)
library(cowplot)
#棒グラフとドットプロットが一緒になったものを描いていくよ
bdp1<- ggplot(NULL) #複数種類のデータを使うときにはNULLにしておく
#グラフのベースを作っていくよ
bdp2<- bdp1+
  labs(title="No plant",x="Treatment", y="Concentration( mg/100g )")+
  
  scale_y_continuous(expand = c(0,0), limits = c(0,ylim))+
  theme_classic()


#棒グラフを描くよ
bdp3<-bdp2+
  geom_bar(
    data=noplant2,
    aes(x= Phosphorus, #X軸はPhosphorusment
        y=mean       #y軸は平均値
        #,fill=Phosphorus　#Phosphorusmentによって塗りつぶしを変える(同じ処理区で複数棒グラフを書く場合)
    ),
    stat="identity", #統計処理はしないでグラフを表示する
    color="black",　#線の色は黒
    fill=(values=c("0P"="bisque1","12P"="bisque2","24P"="bisque3","48P"="bisque4"))
    #,position="dodge"　#棒グラフの間の調整はここを使う
  )
#scale_fill_manual(values=c("gray"))


#散布図を描くよ
bdp4<-bdp3+
  geom_jitter(
    data=noplant,
    aes(x= Phosphorus,  #xはPhosphorusmentのfactorによって分ける
        y= cont　#Y軸で使う値
        #shape=Phosphorus
    ),
    stat="identity",
    # size=3,
    pch=1,
    position=position_jitter(0))


#エラーバーをつけるよ
bdp5<-bdp4+
  geom_errorbar(
    data=noplant2,
    aes(x=Phosphorus,
        y=mean,
        ymin=mean-sd,
        ymax=mean+sd),
    width=0.17
  )


#装飾をするよ
bdp6<-bdp5+
  #X軸の棒グラフの順番を変える
  # scale_x_discrete(limit=c('0P', '12P', '24P','48P')) +
  # 論文っぽい見た目にする．(cowplotパッケージが別途必要)．フォントサイズと線の太さを設定
  theme_cowplot()+
  # 見た目を整える(タイトルの位置，縦横比など)
  theme(plot.title = element_text(hjust = 0.5),axis.title.x = element_blank(),
        axis.title.y = element_blank())#+
#theme(axis.text.x = element_text(angle = 45, hjust = 1))
t3=bdp6

#描画デバイスを開く
png("2021土耕fig/211005土耕ポットTruog_一覧.png", width=480*1.5, height=480*1.5)
#patchworkで画像をまとめる
bdp8<-(t1+t2+t3)+patchwork::plot_layout(ncol = 3)
Xjiku="P treatment (kg/10 a)"
Yjiku="P concentration (mg/ 100 g)"
PW.G.labeling(pw = bdp8,titlelab = NULL,xlab = Xjiku,ylab = Yjiku)
#描画デバイスを閉じる
dev.off()



svg(filename = "2021土耕fig/211005土耕ポットTruog_一覧.svg",width=10,height=4)
PW.G.labeling(pw = bdp8,titlelab = NULL,xlab = Xjiku,ylab = Yjiku,yfont = 18)
dev.off()






fx=as.factor(konken$Phosphorus)
vx=as.numeric(konken$cont)


#Tukey検定する
#「パッケージ名::関数」でパッケージを指定して関数を動かす
#tidyverse とmultcompでselect関数が被ってバクるのを防ぐ
#これでパッケージ指定で関数動かすとlibraryで読み込む必要もなくなる
print("根圏")
print(multcomp::cld(multcomp::glht(aov(vx~fx),linfct=multcomp::mcp(fx="Tukey")),level=0.05,decreasing=T))


fx=as.factor(bulk$Phosphorus)
vx=as.numeric(bulk$cont)


#Tukey検定する
#「パッケージ名::関数」でパッケージを指定して関数を動かす
#tidyverse とmultcompでselect関数が被ってバクるのを防ぐ
#これでパッケージ指定で関数動かすとlibraryで読み込む必要もなくなる
print("バルク")
print(multcomp::cld(multcomp::glht(aov(vx~fx),linfct=multcomp::mcp(fx="Tukey")),level=0.05,decreasing=T))


fx=as.factor(noplant$Phosphorus)
vx=as.numeric(noplant$cont)


#Tukey検定する
#「パッケージ名::関数」でパッケージを指定して関数を動かす
#tidyverse とmultcompでselect関数が被ってバクるのを防ぐ
#これでパッケージ指定で関数動かすとlibraryで読み込む必要もなくなる
print("植物無し")
print(multcomp::cld(multcomp::glht(aov(vx~fx),linfct=multcomp::mcp(fx="Tukey")),level=0.05,decreasing=T))