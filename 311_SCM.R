#東日本大震災がきっけかで，東北大学文学部の入試倍率がどう変化したかを確かめる
library(tidyverse)
library(dataprep)
library(Synth)
library(SCtools)

#データの読み込み
data <- read.csv("bairitsu.csv")

#ロング形式に変換
#今回はgatherを使う
data <- data %>% 
  gather(-c(univ, unit, ss), key = year, value = odds)
#write_csv(data, "data.csv")

#ここからシンセ
#ちょい変数編集
data <- data %>% 
  mutate(year = stringr::str_sub(year, start = 2, end = 5))
data <- data %>% 
  mutate_at(vars(year, odds, unit, ss), as.numeric )
data <- data %>% 
  mutate(univ = as.character(univ))
view(data)


model <- dataprep(
  foo = data, 
  time.predictors.prior = 2006:2011, 
  special.predictors = list(
    list("odds", 2006 , "mean"),
    list("odds", 2007 , "mean"),
    list("odds", 2008 , "mean"),
    list("odds", 2009 , "mean"),
    list("odds", 2010 , "mean"),
    list("odds", 2011 , "mean")
#    list("ss", 2006:2011 , "mean")
  ),

  dependent = "odds", 
  unit.variable = "unit",
  unit.names.variable = "univ", 
  time.variable = "year", 
  treatment.identifier = 1, 
  controls.identifier = c(2:28),　
  time.optimize.ssr = 2006:2011, 
  time.plot = 2006:2018) 
#model

bairitsu <- synth(model)


#結果の確認
synth.tables <- synth.tab(
  dataprep.res = model,
  synth.res = bairitsu
) 

synth.tables$tab.pred
synth.tables$tab.w %>%　
  arrange(-w.weights)　

model$Y1plot- model$Y0plot %*% bairitsu$solution.w #Synthetic Controlの結果変数

#個別処置効果のプロット
gaps.plot(synth.res = bairitsu,
          dataprep.res = model)

#図示
ggplot()+
  geom_line(aes(x = c(2006:2018), model$Y1plot), color = "royalblue")+
  geom_line(aes(x = c(2006:2018), model$Y0plot %*% bairitsu$solution.w), color = "tomato")+
  labs(x = "year", y = "odds")+
  theme_minimal()

placebo<- generate.placebos(dataprep.out = model,
                            synth.out = bairitsu,
                            Sigf.ipop = 2, 
                            strategy='multicore') 


plot_placebos(tdf = placebo, 
              discard.extreme=TRUE, 
              mspe.limit= 1000,
              family = "HiraKakuProN-W3") 


