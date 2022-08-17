library(tidyverse)
library(scales)
library(cowplot)

#CSvファイルを読み込む
raw_data <- read_csv("CSV/2022_tenp.csv")

tenp_data=raw_data %>% 
  select("date"=1,"tenp"=4) %>%
  mutate(date=as.Date(tenp_data$date,format="%m%d"))


#おんどとりデータの日付ごとの最大値，最小値を取得 （照度，温度）
data_max_low=tenp_data%>%
  mutate(date = as.character(tenp_data$date),
         tenp = as.numeric(tenp_data$tenp))%>%#読み込んだ順にデータをならべる
  group_by(date)%>%#date2でグループ化
  
  #lxの最大値と最小値を取得（,na.rm = TRUEでNA無視）
  summarise(#max_lx=max(lx,na.rm = TRUE),min_lx=min(lx,na.rm = TRUE),
            #tenpの最大値と最小値を取得（,na.rm = TRUEでNA無視）
            max_tenp=max(tenp,na.rm = TRUE),
            min_tenp=min(tenp,na.rm = TRUE),
            mean_tenp=mean(tenp)) 

data_max_low=data_max_low %>% 
   mutate(date = as.Date(data_max_low$date))

#データフレームをグラフ描画用に整理する（データフレームを縦長にする）
data_for_plot=data_max_low %>% 
  pivot_longer(max_tenp:mean_tenp, names_to="Tenperature", values_to="content")

#データフレームから温度の行だけ抽出する
#tenp_for_plot=data_for_plot %>% filter(Tenperature=="max_tenp"|Tenperature=="min_tenp")

#折れ線グラフを描画
a <- ggplot(data_for_plot, aes(x = date, y = content, group = Tenperature, 
                               color = Tenperature,linetype = Tenperature)) + 
  geom_line()+
  #横軸の表示を月ごとにする
  scale_x_date(breaks = date_breaks("months"),labels = date_format("%b"))+

#図を整える

  theme_cowplot(font_size = 16, line_size = 1.25)+
  theme(plot.title = element_text(hjust = 0, size = 22))+
  labs(x="Month", y="Tenperature (°C)")+
  scale_linetype_manual(values = c( "dotted", "solid", "dotted"),
                        name="Temperature",
                        labels=c(max_tenp="Maximum values of the day",
                                 min_tenp="Minimum values of the day",
                                 mean_tenp="Mean values of the day"))+
  scale_color_manual(name="Temperature",values = c("red","black","blue"),
                  labels=c(max_tenp="Maximum values of the day",
                           min_tenp="Minimum values of the day",
                           mean_tenp="Mean values of the day"))


svg(filename = "2022_リン＆バイオ炭fig/土耕栽培気温.svg",width =10,height=4)
print(a)
dev.off()
