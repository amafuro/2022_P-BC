#描画する元素リスト
#X軸がe1,Y軸がe2
e1="Al"
e2="P"

#パッケージ読み込み
library(tidyverse)
library(fs)
library(ggpmisc)
library(cowplot)

#生データを読み込む
raw_data <- read_csv("CSV/211202ICP_plant.csv")
raw_data=raw_data%>%
  #読み込んだ順にデータをならべる
  mutate(plant = fct_inorder(plant),treat = fct_inorder(treat)) %>% 
  #列番号で指定してデータフレームを縦長に
  pivot_longer(4:16, names_to="Element", values_to="cont")

# summariseのフレンドリーな警告文を非表示
options(dplyr.group.inform = F)



#生データからmean(平均値)とsd(標準偏差)のデータフレームを作っておく
data_mean_sd=raw_data%>%
  #読み込んだ順にデータをならべる
  mutate(plant = fct_inorder(plant),
         treat = fct_inorder(treat),
         Element = fct_inorder(Element))%>%
  group_by(plant,treat,Element) %>% #グループ化
  #描画したい元素だけにする
  filter(Element==e1|Element==e2)



#横長にして植物の部位ごとの列をつくる
data_long= data_mean_sd%>% 
  pivot_wider(names_fro = plant,values_from = cont) 

# 
# data_long <- read_csv("test.csv") %>% 
#   mutate(Sum=Sum/7) %>% 
#   mutate("Whole plant body"=Sum) %>% 
#   select(1:10,12)


#部位ごとに繰り返し操作を行う
for (z in 4:length(data_long)){
  #植物体の部位に対応した列を選択
  data_for_plot=data_long %>%
    ungroup() %>% 
    select(1:3,z)
  #NAを含む列を削除
  data_for_plot<- na.omit(data_for_plot)
  #部位名を取得
  plant_parts=colnames(data_for_plot[4]) 
  #横長に
  data_2elements=data_for_plot %>% 
    pivot_wider(names_from = Element,values_from = plant_parts) 
  
  #いまどこの部位やってるか報告
  print(plant_parts)
  
  
  # 最大値を取得．のちに使用(グラフの上限設定用) 
  # ylim =max(data_2elements$Mn)*1.1
  #xlim =max(data_2elements$Al)*1.1
  
  #図を格納するフォルダがなかったら作成する
  element_file_name=gsub(" ","",paste("2021土耕fig/ICP/棒グラフ_2元素_回帰直線/棒グラフ_回帰直線_",e1,"・",e2,"/"))
  if (dir.exists(element_file_name)==F) {
    
    dir_create(element_file_name)
    
  }
  
  
  #グラフ描画開始
  
  #rとP値
  # cor.test
  cor_test=data_2elements %>% 
    select(x=3,y=4)
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
  
  
  #空のグラフ描画
  bdp1<- ggplot(data=data_2elements,#回帰直線のR^２を表示させるために，ここでデータフレーム指定しておく必要アリ？
                #as.name()を入れるときはaesじゃなくてaes_らしい
                aes_(x=as.name(e1),y=as.name(e2)))
  bdp2<- bdp1+
    #軸ラベル設定
    labs(title=plant_parts,x=paste(e1,"( mg/ g DW)"),subtitle=bquote(italic(r)~"="~.(cor_display)~","~italic(p)~.(p_display)) ,
         x=paste(e1,"( mg/ g DW)"), y=paste(e2,"( mg/ g DW)"))+
    #グラフの上限を設定
    scale_y_continuous(#expand = c(0,0)#, limits = c(xlim,ylim)
    )+theme_classic()

  
  
  #生データプロット
  bdp5 <- bdp2 +
    geom_jitter(
      aes(color=treat,shape=treat),
      stat="identity",#統計処理はしないでグラフを表示する
      size=3,#プロットの大きさ指定
      # pch=c(0,1,2,3),#プロットの形指定
      position=position_jitter(0))+#プロットをどれだけブレさせるか
    scale_shape_manual(values=c(15,16,17,3))+
    scale_fill_manual(values = c("#C6D413","#8ED474","#1AA33C","#046C41"))+
    scale_color_manual(values = c("#C6D413","#8ED474","#1AA33C","#046C41"))
  
  #回帰直線描画
  bdp6 <- bdp5 + geom_smooth(method = "lm", se = FALSE,
                             formula = y ~ x,color="black")
  
  #グラフの調整
  bdp8<-bdp6+
    # 論文っぽい見た目にする．(cowplotパッケージが別途必要)．フォントサイズと線の太さを設定
    theme_cowplot()+
    # 見た目を整える(タイトルの位置，縦横比など)
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 描画デバイスに反映
  
  
  assign(paste("p",z-3,sep=""),bdp8)
}


library(ProgressEbar)
pw=p1+theme(legend.position="none")+
      p2+theme(legend.position="none")+
      p3+theme(legend.position="none")+
      p4+theme(legend.position="none")+
      p5+theme(legend.position="none")+
      p6+theme(legend.position="none")+
      p7+theme(legend.position="none")
  # p8+theme(legend.position="none")
  patchwork::plot_layout(ncol = 3)

Xjiku=paste(e1,"concentration (mg/ g DW)")
Yjiku=paste(e2,"concentration (mg/ g DW)")

png("C:/R-Project/2021土耕ポット栽培試験/2021土耕fig/ICP/棒グラフ_2元素_回帰直線/棒グラフ_回帰直線_Al・P/211202土耕ポット植物体ICP_一覧_Al・P.png",
    width=480*1.1, height=480*1.4)
PW.G.labeling(pw = pw,titlelab = "",xlab = Xjiku,ylab = Yjiku)
dev.off()

svg(filename ="C:/R-Project/2021土耕ポット栽培試験/2021土耕fig/ICP/棒グラフ_2元素_回帰直線/棒グラフ_回帰直線_Al・P/211202土耕ポット植物体ICP_一覧_Al・P.svg",width=8,height=9)
PW.G.labeling(pw = pw,titlelab = NULL,xlab = Xjiku,ylab = Yjiku)
dev.off()


pw=p1+theme(legend.position="none")+
  p2+theme(legend.position="none")+
  p3+theme(legend.position="none")+
  p4+theme(legend.position="none")+
  p5+theme(legend.position="none")+
  p6+theme(legend.position="none")+
  p7+
# p8+theme(legend.position="none")
  patchwork::plot_layout(ncol = 7)

svg(filename ="C:/R-Project/2021土耕ポット栽培試験/2021土耕fig/ポスター/211202土耕ポット植物体ICP_一覧_Al・P.svg",width=24,height=4)
PW.G.labeling(pw = pw,titlelab = NULL,xlab = Xjiku,ylab = Yjiku)
dev.off()