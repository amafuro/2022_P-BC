#描画デバイスを開く
png("2021土耕fig/交換態Al&Truog一覧.png", width=480*1.1, height=480*1.4)
#patchworkで画像をまとめる
bdp8<-a1+a2+a3+
  patchwork::plot_spacer()+patchwork::plot_spacer()+patchwork::plot_spacer()+
  t1+t2+t3+patchwork::plot_layout(ncol = 3,heights = c(1,1,1))
Xjiku="P treatment (kg/10 a)"
Yjiku=""
PW.G.labeling(pw = bdp8,titlelab = NULL,xlab = Xjiku,ylab = Yjiku)
#描画デバイスを閉じる
dev.off()
library(patchwork)
a1=a1+theme(axis.title.y = element_text("Exchangeable Al (mg/100 g)",angle = 90,vjust = 3))

bdp8<-((a1|a2*plot_layout(tag_level = "new")|a3*plot_layout(tag_level = "new"))/
  plot_spacer()/
  (t1|t2*plot_layout(tag_level = "new")|t3*plot_layout(tag_level = "new")))+
  plot_annotation(tag_levels = "A")+
  plot_layout(heights = c(1,0.3,1))


svg(filename = "2021土耕fig/交換態Al&Truog一覧.svg",width=8,height=8)
PW.G.labeling(pw = bdp8,titlelab = NULL,xlab = Xjiku,ylab = Yjiku)
dev.off()


# bdp8<-((t1|t2*plot_layout(tag_level = "new")|t3*plot_layout(tag_level = "new"))/
#          plot_spacer()/
#          ((g1+theme(legend.position="none"))|
#             (g2+theme(legend.position="none"))&plot_layout(tag_level = "new")|
#             (g3)&plot_layout(tag_level = "new"))/
#          plot_spacer()/
#          (a1|a2&plot_layout(tag_level = "new")|a3&plot_layout(tag_level = "new"))
#          )+
#   plot_annotation(tag_levels = "A")+
#   plot_layout(heights = c(5,0.3,5,0.3,5))
# 
# svg(filename = "2021土耕fig/交換態Al&Truog&全P一覧.svg",width=8*1.5,height=8*1.5)
# PW.G.labeling(pw = bdp8,titlelab = NULL,xlab = Xjiku,ylab = Yjiku)
# dev.off()
# 
# bdp9=a1+a2+a3+plot_layout(ncol = 3)
# svg(filename = "2021土耕fig/交換態Al一覧.svg",width=8,height=3)
# PW.G.labeling(pw = bdp9,titlelab = NULL,xlab = Xjiku,ylab = Yjiku)
# dev.off()
