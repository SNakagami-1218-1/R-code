library(ggplot2)
library(dplyr)
library(DescTools)
library(ggbeeswarm)
library(ggpubr)
library(multcompView)
library(openxlsx)
library(multcomp)
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

# ANOVAを実行
anova_result <- aov(Y ~ X, data = df)
summary(anova_result)

# Tukey-Kramer検定を実行
tukey_result <- TukeyHSD(anova_result)

# p値を抽出して適切な名前をつける
p_values <- tukey_result$X[, "p adj"]
names(p_values) <- rownames(tukey_result$X)  # グループペアの名前を付与

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

# 可視化
ggplot(df, aes(x = X, y = Y, fill = X, color = X)) +  # fill と color を明示的に指定
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +  
  geom_beeswarm(size = 2, alpha = 0.8) +  
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +  
  geom_text(aes(x = X, y = max(Y) + 4.5, label = group_label),  
            color = "black", size = 5, fontface = "bold") +
  theme_minimal() + theme_classic2() +
  ggtitle("Tukey-Kramer test") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
  scale_x_discrete(labels = c("A" = "WT", "B" = expression(italic("sep")), "C" = expression(italic("dou"))))+
  theme(axis.text.x = element_text(size = 20), axis.title.x = element_blank()) +
  
  ylab("Title") +
  ylim(0, 55) + 
  theme(axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 18)) +
  
  annotate("text", 
           x = max(as.numeric(df$X)), y = 0,  
           label = "Tukey-Kramer test\nSignificance level: 0.05", 
           size = 3, hjust =0.5, vjust = 0) +
  theme(legend.position = "none") +
  
  scale_fill_manual(values = c("A" = "lightblue", "B" = "pink", "C" = "lightgreen")) +  
  scale_color_manual(values = c("A" = "blue", "B" = "red", "C" = "green"))  

