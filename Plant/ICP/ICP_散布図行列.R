library(tidyverse)
library(GGally)

a=read_csv("CSV/211202ICP_plant.csv") %>%
  select(2:length(a))

png("2021土耕fig/ICP/ICP相関行列.png", width=480*6, height=480*6)
ggpairs(a,aes_string(colour="plant"))
dev.off()
