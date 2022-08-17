#生データを読み込む(生データはプロットにつかう)
library(readr)
raw_data <- read_csv("C:/R-4.0.3/2021土耕ポット栽培試験/CSV/Result_211028_土耕2nd新芽.csv")

#生データからmean(平均値)のデータフレームを作っておく (このデータフレームは棒グラフとエラーバーにつかう)
library(tidyverse)#パッケージ「tidyverse」と「dplyr」を使って作る

data_mean=raw_data%>%
  mutate(ID = fct_inorder(ID))%>%#読み込んだ順にデータをならべる
  group_by(ID)%>%
  summarise(Aps = mean(Aps),Glu = mean(Glu),Asn = mean(Asn),
            Ser = mean(Ser),Gln = mean(Gln),Arg = mean(Arg),
            Ala = mean(Ala),Thea = mean(Thea),GABA = mean(GABA))

#データフレームの種類をヒートマップ用に変換
data_map <- as.matrix(data_mean[,-1])
rownames(data_map) <- data_mean$ID

#標準化
#data_map_2 = scale(data_map)


#ヒートマップ用カラーパレット作成
my.col <- colorRampPalette(c("seashell4","cornsilk1","darkorange")) 

# png関数で描画デバイスを開く
png("C:/R-4.0.3/2021土耕ポット栽培試験/2021土耕fig/211028土耕ポット2番茶新芽.png")

#ヒートマップ作図
library(gplots)
library(cowplot)
hv <- heatmap.2(data_map,
                Rowv=FALSE,Colv=F,#行,列の並び替え無し
                dendrogram ="none",
                col=my.col,
                #ここで行もしくは列の標準化
                scale = c("row", 
                          "column"
                          ),
                na.rm=TRUE,
                key=T,density.info="none",trace="none",
                main = "土耕ポット　2番茶期新芽FAA")

# 描画デバイスを閉じる
dev.off()

