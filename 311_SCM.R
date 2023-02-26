#東日本大震災がきっけかで，東北大学文学部の入試倍率がどう変化したかを確かめる
library(tidyverse)
library(dataprep)
library(Synth)
library(SCtools)

#データの読み込み
data <- read.csv("/Users/yamaguchiyuhei/Desktop/bairitsu.csv")

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
  time.predictors.prior = 2006:2011, #処置前の期間
  #共変量の指定と設定(変数，期間，統計量)：YとZに対応
  special.predictors = list(
    list("odds", 2006 , "mean"),
    list("odds", 2007 , "mean"),
    list("odds", 2008 , "mean"),
    list("odds", 2009 , "mean"),
    list("odds", 2010 , "mean"),
    list("odds", 2011 , "mean")
#    list("ss", 2006:2011 , "mean")
  ),

  dependent = "odds", #結果変数
  unit.variable = "unit",　#ユニットを表す変数
  unit.names.variable = "univ", #（あれば）ユニット名を表す変数
  time.variable = "year", #時点を表す変数
  treatment.identifier = 1, #処置群の指定（unit.variable）
  controls.identifier = c(2:28),　#対照群の指定（unit.variable）
  time.optimize.ssr = 2006:2011, #一致させる処置前期間
  time.plot = 2006:2018) #プロットさせる期間（後述）
#model

bairitsu <- synth(model)


#結果の確認
synth.tables <- synth.tab(
  dataprep.res = model,
  synth.res = bairitsu
) 

synth.tables$tab.pred　#共変量のバランスチェック
synth.tables$tab.w %>%　#ウェイトWの確認
  arrange(-w.weights)　

model$Y1plot- #処置群の結果変数
  model$Y0plot %*% bairitsu$solution.w #Synthetic Controlの結果変数

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


plot_placebos(tdf = placebo, #generate.placebos関数の結果
              discard.extreme=TRUE, #mspe.limitで設定したMSPE以上のケースを省く設定
              mspe.limit= 1000,
              family = "HiraKakuProN-W3") #除外するMSPEの基準．デフォルトで20(%)


