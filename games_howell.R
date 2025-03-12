library(ggplot2)
library(dplyr)
library(multcomp)
library(multcompView)
library(rstatix)  
library(tidyverse)
library(DescTools)
library(ggbeeswarm) 
library(car)
library(ggpubr)
library(car)
library(broom)
library(tidyverse)
library(rcompanion)
library(ggsignif)
library(openxlsx)
library(PMCMRplus)

# 必要なパッケージ
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)

# 有意差検定の結果（multcompLetters）のラベルを取得
group_letters <- cld_result$Letters
group_levels <- levels(df$Group)

# グループ A にラベルがなければ "a" を付与
for (g in group_levels) {
  if (!(g %in% names(group_letters))) {
    group_letters[g] <- "a"
  }
}

# 確認
print(group_letters)

# X軸のグループ名を A, B, ..., I に変更
group_labels <- setNames(LETTERS[1:9], group_levels)

# データフレームにラベルを追加
df$group_label <- group_letters[df$Group]

# ボックスプロットの作成（凡例を削除）
ggplot(df, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +  # 平均値の表示
  geom_text(aes(x = Group, y = max(Value) + 1, label = group_label),  # 最大値+1 の位置にラベルを配置
            color = "black", size = 6, fontface = "bold") +
  theme_minimal() +
  
  # タイトルの設定
  ggtitle("Comparison of Groups") +
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold")) +  # タイトルを中央揃え・サイズ22
  
  # X軸の調整
  scale_x_discrete(labels = group_labels) +  # X軸のラベルを A, B, ... I に変更
  theme(axis.text.x = element_text(size = 20),  # X軸のラベルサイズを 20 に
        axis.title.x = element_blank()) +  # X軸のタイトルを削除
  
  # Y軸の調整
  ylab("Length (cm)") +  # Y軸のタイトル
  theme(axis.title.y = element_text(size = 20),  # Y軸のタイトルサイズを 20
        axis.text.y = element_text(size = 18)) +  # Y軸の数値サイズを 18
  
  # 右下に「Games-Howell test」「有意水準」を記入
  annotate("text", x = 8, y = min(df$Value), label = "Games-Howell test\nSignificance level: 0.05", 
           size = 5, hjust = 1, vjust = 0, fontface = "italic") +
  
  # 凡例を削除
  theme(legend.position = "none")

                                                                                                                                                                                                                                                                                       +     )                     