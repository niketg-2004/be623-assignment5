setwd("~/be623_assignment5")
library(dplyr)
library(ggplot2)
Sample <- c(paste0("S", 1:6), paste0("S", 7:12))
Gene <- c(rep("PER2", 6), rep("BMAL1", 6))
ZT <- rep(c(0, 4, 8, 12, 16, 20), 2)
Expression <- c(1.8, 3.2, 6.5, 9.4, 7.1, 3.6,8.6, 5.9, 2.7, 1.5, 3.4, 6.8)
circ_df <- data.frame(Sample, Gene, ZT, Expression, stringsAsFactors = FALSE)
str(circ_df)
per2_rows <- circ_df[circ_df$Gene == "PER2", ]
mean_per2   <- mean(per2_rows$Expression)
median_per2 <- median(per2_rows$Expression)
sd_per2     <- sd(per2_rows$Expression)
mean_per2
median_per2
sd_per2
write.csv(circ_df, "circadian_expression.csv", row.names = FALSE)
circ_reloaded <- read.csv("circadian_expression.csv")
identical(circ_df$Expression, circ_reloaded$Expression)
per2_sort <- circ_df %>%
  filter(Gene == "PER2") %>%
  arrange(desc(Expression))
print(per2_sort)
bmal1_sort <- circ_df %>%
  filter(Gene == "BMAL1") %>%
  arrange(desc(Expression))

print(bmal1_sort)
circ_df2 <- circ_df %>%
  mutate(dark = ZT >= 12)
dark_summary <- circ_df2 %>%
  filter(dark == TRUE) %>%
  group_by(Gene) %>%
  summarize(mean_expression = mean(Expression), sample_count = n())

print(dark_summary)
light_summary <- circ_df2 %>%
  filter(dark == FALSE) %>%
  group_by(Gene) %>%
  summarize(mean_expression = mean(Expression), sample_count = n())

print(light_summary)
rhythm_plot <- ggplot(circ_df, aes(x = ZT, y = Expression, color = Gene)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("PER2" = "firebrick", "BMAL1" = "dodgerblue")) +
  labs(
    title = "Circadian Gene Expression Over Time",
    x = "Zeitgeber time (h)",
    y = "Relative Expression"
  ) +
  theme_minimal()
print(rhythm_plot)
ggsave("rhythm_plot.png", plot = rhythm_plot, width = 6, height = 4)
expression_boxplot <- ggplot(circ_df, aes(x = Gene, y = Expression, fill = Gene)) +
  geom_boxplot() +
  scale_fill_manual(values = c("PER2" = "firebrick", "BMAL1" = "dodgerblue")) +
  labs(
    title = "Overall Distribution of Gene Expression",
    x = "Gene",
    y = "Expression Level"
  ) +
  theme_minimal()
print(expression_boxplot)
ggsave("expression_boxplot.png", plot = expression_boxplot, width = 5, height = 4)

