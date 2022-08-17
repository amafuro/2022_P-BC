library(sound)
setWavPlayer("play")
play("kyukyukyunya-.wav")

cl<-makeCluster(1)

clusterEvalQ(cl,{
  library(sound)
  setWavPlayer("play")
})

parLapply(cl,
  play("kyukyukyunya-.wav")
)

stopCluster(cl)

#パッケージの読み込み
library("beepr")

#音の再生
#soundオプション:ping = 1, coin = 2, fanfare = 3, complete = 4,
#treasure = 5, ready = 6, shotgun = 7, mario = 8, whihelm = 9,
#facebook = 10, sword = 11
beep(sound = 8, expr = NULL)

#ランダムで音を再生
beep(0)
# progress bar test 
library(nk)
library(tcltk)

n <- 1000
pb <- txtProgressBar(min = 1, max = n, style = 3)
pi2div6 <- 0.0
for(i in 1:n){
  setTxtProgressBar(pb, i) 
  pi2div6 <- pi2div6 + 1/ (i*i)
  Sys.sleep(0.01)             #速いから
}

print( sqrt(pi2div6 * 6) )  #円周率の計算