library(tidyverse)
library(cowplot)
library(patchwork)
library(ProgressEbar)

Test_list=c("P","BC")

#CSVファイル読み込み(パスを記入)
raw_data=read_csv("CSV/220811_pH_EC_soil.csv")

cal_raw=raw_data %>% 
  mutate(Treatment=fct_inorder(Treatment)) 

pH_EC_list=colnames(cal_raw[5:length(cal_raw)])

long_raw=cal_raw %>% 
  pivot_longer(5:length(raw_data),names_to = "pH_EC")

#生データからmean(平均値)とsd(標準偏差)のデータフレームを作っておく
mean_sd=long_raw%>%
  #読み込んだ順にデータをならべる
  mutate(Treatment = fct_inorder(Treatment))%>%
  #グループ化
  group_by(Test,Treatment,pH_EC) %>% 
  summarise(mean=mean(value),sd=sd(value)) %>% 
  ungroup()

pH_EC_No=0
for (pH_EC_filter in pH_EC_list) {
  
  print(pH_EC_filter)
  
  MS_pH_EC_filted=mean_sd %>% 
    filter(pH_EC==pH_EC_filter)
  Raw_pH_EC_filted=long_raw %>% 
    filter(pH_EC==pH_EC_filter)
  
  pH_EC_No=pH_EC_No+1
  
  for (Test_filter in Test_list) {
    
    MS_Test_filted=MS_pH_EC_filted %>% 
      filter(Test==Test_filter)
    Raw_Test_filted=Raw_pH_EC_filted %>% 
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
    fig_dir=paste("2022_リン＆バイオ炭fig/Soil/pH_EC/",
                  Test_filter,"/",sep="")
    
    if (dir.exists(fig_dir)==F) {
      fs::dir_create(fig_dir)
    }
    
    #図の作成
    g=ggplot()+
      
      labs(title=pH_EC_filter,
           x="Soil Treatmentment",
           y="")+
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
                  position=position_jitter(0))
    g2=g+
      stat_summary(data=MS_Test_filted,
                   aes(x= Treatment,
                       y= mean+sd),
                   geom = 'text',
                   label =abc,size=10,
                   fun = max, vjust = -1)+
      theme_classic(base_size = 30)+
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45,hjust = 1),
            legend.position = "none")
    ggsave(filename = paste0(fig_dir,"/",pH_EC_filter,".png"),g2)
    
    g3=g+
      stat_summary(data=MS_Test_filted,
                   aes(x= Treatment,
                       y= mean+sd),
                   geom = 'text',
                   label =abc,size=5,
                   fun = max, vjust = -1)+
      theme_classic(base_size = 15)+
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 45,hjust = 1),
            legend.position = "none")
    
    assign(paste0(Test_filter,pH_EC_No),g3)
  }
  
}


p_patch=P1+P2+P3

svg("2022_リン＆バイオ炭fig/Soil/pH_EC/P/p_patch.svg",width=8,height=4)
PW.G.labeling(pw = p_patch,titlelab = "P test pH&EC",
              xlab = "Soil Treatmentment",ylab = "")
dev.off()

BC_patch=BC1+BC2+BC3

svg("2022_リン＆バイオ炭fig/Soil/pH_EC/BC/BC_patch.svg",width=8,height=4)
PW.G.labeling(pw = BC_patch,titlelab = "BC test pH&EC",
              xlab = "Soil Treatmentment",ylab = "")
dev.off()


#facet_wrapテスト
for (facet_Test_filter in Test_list) {
  
  MS_Test_filted=mean_sd %>%
    filter(Test==facet_Test_filter)
  Raw_Test_filted=long_raw %>%
    filter(Test==facet_Test_filter)
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
  
  
  #図の作成
  g=ggplot(data=Raw_Test_filted,
           aes(x= Treatment,
               y= value))+
    
    labs(title=facet_Test_filter,
         x="Soil Treatmentment",
         y="")+
    # stat_summary(data=MS_Test_filted,
    #              aes(x= Treatment,
    #                  y= mean+sd),
    #              geom = 'text',
    #              label =abc,size=10,
    #              fun = max, vjust = -1)+
    scale_y_continuous(expand = c(0,0)#, limits = c(0,ylim)
    )+
    
    
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
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45,hjust = 1),
          legend.position = "none")+
    facet_wrap(~pH_EC, scales = "free")
  
  fig_dir=paste("2022_リン＆バイオ炭fig/Soil/pH_EC/",
                facet_Test_filter,"/",sep="")
  ggsave(filename = paste0(fig_dir,"/facet_wrap_pH_EC.png"),g)
}
