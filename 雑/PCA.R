library(tidyverse)

a=read_csv("test.csv")

plant_P=a %>% 
  filter(Element=="P") %>% 
  select(-1:-3) %>% 
  mutate(Sum=Sum/7) 


plant_Al=a %>%
  filter(Element=="Al") %>%
  select(-1:-3) %>%
  mutate(Sum=Sum/7)



b=read_csv("CSV/2021Sekiya_All_data.csv") %>% 
  filter(soil=="bulk"|soil=="rhizoshere") %>% 
  pivot_wider(names_from = "soil",values_from = "cont")

rhizoshere=b %>% 
  select(1:3,rhizoshere) %>% 
  pivot_wider(names_from = "fraction",values_from = "rhizoshere") %>% 
  select("Ca-R"=3,"Fe-R"=4,"Al-R"=5)

bulk=b %>% 
  select(1:3,bulk) %>% 
  pivot_wider(names_from = "fraction",values_from = "bulk") %>% 
  select("Ca-B"=3,"Fe-B"=4,"Al-B"=5)


all=data.frame(plant_Al,rhizoshere) %>% 
  select(8:11)
all=data.frame(plant_P)
# biplot(prcomp(all))
par(xpd=TRUE)
biplot(prcomp(all, scale=TRUE))

cor(all)

r = factanal(all, factors=2)
plot(NULL, xlim=c(0,1), ylim=c(0,1), xlab="因子1", ylab="因子2")
text(r$loadings, names(all))

res1.mds <- metaMDS(plant_P, distance="horn", autotransform = FALSE, trace = 0)
plot(res1.mds, type="t", display="sites")


dis.mh <- vegdist(plant_P, method="horn")
dis.bc <- vegdist(plant_P, method="bray")
res2.mds <- metaMDS(dis.bc, trace=0)
plot(res2.mds, type="t", display="sites")
res.pco <- cmdscale(dis.mh, k=2, eig=TRUE)
ordiplot(res.pco, type="t")
# pro <- protest(res1.mds, res.pco)
# plot(pro)
edat<-data.frame(rhizoshere)
edat <- data.frame(scale(edat))
library(vegan)
res1.rda <- dbrda(plant_P~., data=edat, distance="horn")
plot(res1.rda)


ef <- envfit(res2.mds, rhizoshere, permu=4999)
ef
ordiplot(res2.mds, type="t", display="sites")
plot(ef, p.max=0.1)
