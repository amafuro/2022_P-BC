library(tidyverse)
library(cowplot)

parts_list=c("per_shoot","Shoot","Mature_leaves",
             "Mature_stems","Roots")

Test_list=c("P","BC")

#CSVファイル読み込み(パスを記入)
raw_data=read_csv("CSV/BC_Ptest_220802_3rd_FW.csv")%>% 
  mutate(Treatment=fct_inorder(Treatment),
         "per_shoot"=Shoot/Shoot_number) %>% 
  select(-Shoot_number) %>% 
  pivot_longer(3:7,names_to = "parts")

#生データからmean(平均値)とsd(標準偏差)のデータフレームを作っておく
data_mean_sd=raw_data%>%
  #読み込んだ順にデータをならべる
  mutate(Treatment = fct_inorder(Treatment))%>%
  #グループ化
  group_by(Test,Treatment,parts) %>% 
  summarise(mean=mean(value),sd=sd(value)) %>% 
  ungroup()



  
for (parts_filter in parts_list) {
  print(parts_filter)
  MS_parts_filted=data_mean_sd %>% 
    filter(parts==parts_filter)
  Raw_parts_filted=raw_data %>% 
    filter(parts==parts_filter)
  
  for (Test_filter in Test_list) {
    
    MS_Test_filted=MS_parts_filted %>% 
      filter(Test==Test_filter)
    Raw_Test_filted=Raw_parts_filted %>% 
      filter(Test==Test_filter)
    
    # 最大値を取得．のちに使用(グラフの上限設定用) 
    ylim =max(Raw_Test_filted$value)*1.2
    
    ###################################################################################################################
    #検定のパーツ
    #生データプロットと回帰直線用データフレーム作成(検定用にこれを転用)
    dx=Raw_Test_filted
    #dの2列目（treat）をfxという列名にする
    dx <- dplyr::select(dx,
                        "fx" = Treatment,
                        "vx" = value,
                        dplyr::everything())
    
    #fxの列をfactor型，vxの列をnumeric型にする
    fx=as.factor(dx$fx)
    vx=as.numeric(dx$vx)
    
    
    #Tukey検定する
    #「パッケージ名::関数」でパッケージを指定して関数を動かす
    #tidyverse とmultcompでselect関数が被ってバクるのを防ぐ
    #これでパッケージ指定で関数動かすとlibraryで読み込む必要もなくなる
    tukey_result=multcomp::cld(multcomp::glht(aov(vx~fx),linfct=multcomp::mcp(fx="Tukey")),
                               level=0.05,decreasing=T)
    abc=tukey_result[["mcletters"]][["Letters"]]
    ###################################################################################################################
    
    #図を保存するディレクトリの作成
    fig_dir=paste("2022_リン＆バイオ炭fig/weight/FW/",
                  Test_filter,"/",sep="")
    
    if (dir.exists(fig_dir)==F) {
      fs::dir_create(fig_dir)
    }
    
    #図の作成
    g=ggplot()+
      
      labs(title=parts_filter,
           x="Soil Treatmentment",
           y="Fresh weight (g)")+
      stat_summary(data=MS_Test_filted,
                   aes(x= Treatment,
                       y= mean+sd),
                   geom = 'text',
                   label =abc,
                   fun = max, vjust = -1)+
      scale_y_continuous(expand = c(0,0), limits = c(0,ylim))+
      
      
      geom_bar(data = MS_Test_filted,
               aes(x=Treatment,y = mean,fill=Treatment),
               stat="identity", 
               color="black")+
      scale_fill_manual(values = c("palegreen","palegreen2","palegreen3","palegreen4",
                                   "darkorange","seashell4","cornsilk"))+
      geom_errorbar(data = MS_Test_filted,
                    aes(x=Treatment,y=mean,
                        ymin=mean-sd,
                        ymax=mean+sd),
                    width=0.17)+
      
      
      geom_jitter(data=Raw_Test_filted,
                  aes(x= Treatment,
                      y= value　
                      #shape=treat
                  ),
                  stat="identity",
                  # size=3,
                  pch=1,
                  position=position_jitter(0))+
      theme_classic(base_size = 20)+
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45,hjust = 1),
            legend.position = "none")
    ggsave(filename = paste0(fig_dir,"/",parts_filter,"_FW.png"),g)
    
  }
  
}