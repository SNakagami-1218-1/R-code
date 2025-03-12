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

df <- read.csv("a.csv",header = T)
df$X <- factor(df$X, levels = c())  # 順序を指定
head(df)
tail(df)
str(df)

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

# Games-Howell 検定
gh_test <- gamesHowellTest(Y ~ X, data = df)
gh_test
# p値行列の取得
p_values <- as.matrix(gh_test$p.value)

# p値行列の実際の行・列名を取得
group_names <- rownames(p_values)

# p_values の行・列名を設定
rownames(p_values) <- colnames(p_values) <- group_names

# 有意差のグループ分け（アルファベット表記）
cld_result <- multcompLetters(p_values, threshold = 0.05)

# 結果の表示
print(cld_result$Letters)


# 有意差検定の結果（multcompLetters）のラベルを取得
group_letters <- cld_result$Letters
group_levels <- levels(df$X)

# X の各グループに対して、ラベルを補完（もし抜けていたら "a" を付ける）
for (g in group_levels) {
  if (!(g %in% names(group_letters))) {
    group_letters[g] <- "a"
  }
}

# データフレームにラベルを追加
df$group_label <- group_letters[df$X]

ggplot(df, aes(x = X, y = Y)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, aes(fill = X)) +  # X群ごとにボックスの色を変更
  geom_beeswarm(aes(color = X), size = 2, alpha = 0.8) +  # X群ごとにドットの色を変更
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +  # 平均値の表示
  geom_text(aes(x = X, y = max(Y) + 4.5, label = group_label),  
            color = "black", size = 3, fontface = "bold") +
  theme_minimal() + theme_classic2() +
  ggtitle("practice") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
  
  scale_x_discrete(labels = c("A" = "WT", "B" = "sep", "C" = "dou")) +
  theme(axis.text.x = element_text(size = 20), axis.title.x = element_blank()) +
  
  ylab("Title") +
  ylim(0, 55) + 
  theme(axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 18)) +
  
  annotate("text", 
           x = max(as.numeric(df$X)), y = 0,  
           label = "Games-Howell test\nSignificance level: 0.05", 
           size =1.5, hjust = .5, vjust = 1) +
  theme(legend.position = "none") +
  
  scale_fill_manual(values = c("A" = "lightblue", "B" = "pink", "C" = "lightgreen")) +  # ボックスの色を手動で変更
  scale_color_manual(values = c("A" = "blue", "B" = "red", "C" = "green"))  # ドットの色を手動で変更

                                                                                                                                                                                                                                                                                       +     )                     