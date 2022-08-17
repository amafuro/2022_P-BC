library(tidyverse)
library(scales)
library(cowplot)

#パスは全て相対パスで書いています．
#CSVをいれたフォルダは？
CSVf="溶脱水CSV/"
#なんの試験？
tst="P"
#図を入れるフォルダは？無かったら勝手に作ります．
element_file_name=paste("2022_リン＆バイオ炭fig/IC溶脱水/",tst,"/",sep="")



#溶脱水フォルダの中のCSVファイルのリストを取得
CSVnames=list.files(CSVf)



#図を入れるフォルダが無かったら作る
if (dir.exists(element_file_name)==FALSE) {
  fs::dir_create(element_file_name)
}

#取得したリストの順番にmean,sdを計算し，mean,sdごとに1オブジェクトにまとめておく
for (x in CSVnames) {
  
  csv_date=gsub(".csv","",x)
  
  #n.a.を０にsする
  #一瞬データフレームがマトリクス型を経由するため，数値の部分をヌメリックに戻す作業も行う．
  csv_table=read_csv(paste(CSVf,x,sep = ""))
  csv_table=gsub(pattern="n.a.",replacement = "0", as.matrix(csv_table))
  csv_table=as.data.frame(csv_table)%>%
    mutate(treat=fct_inorder(treat)) %>% 
    #mutate_atでも同じことができるみたいだけど，バグがあって修正しないみたいだからmutateとaccrossを使おう
    mutate( across(.cols = 3:14,
                   .fns = ~{as.numeric(.)}) ) %>% 
    filter(test==tst)
  
  for (z in 3:length(csv_table)) {
    csv_table=csv_table %>% 
      mutate(csv_table[z]*100)
  }
  
  #meanを計算．最後にファイル名（日付）の列を追加
  data_mean=csv_table %>%
    group_by(test,treat) %>%
    summarise(across(where(is.numeric), mean)) %>% 
    pivot_longer(cols = 3:14,
                 names_to = "element",values_to = "cont") %>% 
    mutate(date=csv_date,date=as.Date(date))
  
  #sdを計算．最後にファイル名（日付）の列を追加
  data_sd=csv_table %>%
    group_by(test,treat) %>%
    summarise(across(.cols = where(is.numeric), .fns = sd))%>% 
    pivot_longer(cols = 3:14,
                 names_to = "element",values_to = "cont") %>% 
    mutate(date=csv_date,date=as.Date(date))
  
  
  #初回だけオブジェクト名を変更するのを行い，その後はデータフレームを結合していく．
  if (exists("plot_mean")==F) {
    plot_mean=data_mean
    plot_sd=data_sd
  }else{
    plot_mean=bind_rows(plot_mean,data_mean)
    plot_sd=bind_rows(plot_sd,data_sd)
  }

}


#くっつける
plot_sum=plot_mean %>% 
  mutate("mean"=cont) %>% 
  mutate("sd"=plot_sd$cont)

#測定項目の列名取得．
element_list=colnames(csv_table[3:length(csv_table)])

#作図
for (y in element_list) {
  
  plot_date=plot_sum %>% 
    filter(element==y)
  
  plot=ggplot(plot_date, aes(x = date, y = cont, color=treat, shape = treat))+
    #折れ線グラフ
    geom_line()+geom_point(size=3)+
    scale_shape_manual(values = c(16,16,16,16,17,17,17)) +
    # scale_fill_manual(values = c("red","red","red","red","blue","blue","blue"))+
    #エラーバー
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd, width = 0.3))+
    #横軸の表示を月ごとにする
    scale_x_date(breaks = date_breaks("months"),labels = date_format("%b"))+
    #ラベルを設定
    labs(title = y,x="Month",y="Content (mg/L)",color="Treatment",shape = "Treatment")+
    #論文ぽく
    theme_cowplot()
  
  # assign(y,plot)
  
  
  #画像を保存
  png(paste(element_file_name,"/",y,".png",sep = ""))
  print(y)
  print(plot)
  dev.off()
  
}

cat("\nfin!")
