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
library(writexl)
# データの読み込み
df <- read.csv("a.csv", header = T)
df$X <- factor(df$X, levels = unique(df$X))  # 順序を自動設定

# 正規性検定（Shapiro-Wilk検定）
shapiro_results <- df %>%
  group_by(X) %>% 
  summarise(
    Y_p = shapiro.test(Y)$p.value
  )
shapiro_results

write_xlsx(list("Shapiro Results" = shapiro_results), "shapiro_results.xlsx")

# 等分散性検定（Levene検定）
levene_Y <- leveneTest(Y ~ X, data = df, center = median)
levene_Y
write_xlsx(list("Leven Result" = levene_Y), "leven_result.xlsx")

# クラスカル・ウォリス検定（Kruskal-Wallis test）
kruskal_result <- kruskal.test(Y ~ X, data = df)
print(kruskal_result)

# Dunn-Bonferroni 検定（多重比較）
dunn_result <- dunnTest(Y ~ X, data = df, method = "bonferroni")

# Dunn-Bonferroni 検定（多重比較）
dunn_result <- dunnTest(Y ~ X, data = df, method = "bonferroni")

# Dunn検定の結果をデータフレームとして取得
dunn_df <- as.data.frame(dunn_result$res)

# p値を取得し、Comparison（ペア情報）を名前として設定
dunn_p_values <- dunn_df$P.adj
names(dunn_p_values) <- dunn_df$Comparison  # Comparison列を直接使用

# NA を除外
dunn_p_values <- na.omit(dunn_p_values)

# 有意差のグループ分け
if (length(dunn_p_values) > 0) {
  dunn_cld_result <- multcompLetters(dunn_p_values, threshold = 0.05)
  raw_group_letters <- dunn_cld_result$Letters
} else {
  raw_group_letters <- setNames(rep("a", length(levels(df$X))), levels(df$X))
}

# X のレベル順に group_letters を作成
group_levels <- levels(df$X)
group_letters <- setNames(rep("a", length(group_levels)), group_levels)

# Dunn検定の結果があるグループのみ更新
for (grp in names(raw_group_letters)) {
  if (grp %in% names(group_letters)) {
    group_letters[grp] <- raw_group_letters[grp]
  }
}

# データフレームにラベルを追加
df$group_label <- group_letters[df$X]

# 確認
print(group_letters)

df$group_label <- group_letters[df$X]

# 可視化
ggplot(df, aes(x = X, y = Y, fill = X, color = X)) +  
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +  
  geom_beeswarm(size = 2, alpha = 0.8) +  
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +  
  geom_text(aes(x = X, y = max(Y) + 4.5, label = group_label),  
            color = "black", size = 5, fontface = "bold") +
  theme_minimal() + theme_classic2() +
  ggtitle("Dunn-Bonferroni Test") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
  scale_x_discrete(labels = c("A" = "WT", "B" = expression(italic("sep")), "C" = expression(italic("dou"))))+
  theme(axis.text.x = element_text(size = 20), axis.title.x = element_blank()) +
  
  ylab("Title") +
  ylim(0, 55) + 
  theme(axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 18)) +
  
  annotate("text", 
           x = max(as.numeric(df$X)), y = 0,  
           label = "Dunn-Bonferroni test\nSignificance level: 0.05", 
           size = 3, hjust = 0.5, vjust = 0) +
  theme(legend.position = "none") +
  
  scale_fill_manual(values = c("A" = "lightblue", "B" = "pink", "C" = "lightgreen")) +  
  scale_color_manual(values = c("A" = "blue", "B" = "red", "C" = "green"))
