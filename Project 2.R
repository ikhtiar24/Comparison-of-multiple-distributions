babies <- na.omit(babies)


#library(graphics)
library(stats) # statistical test
library(ggplot2)
library(ggpubr)
library(cowplot)
library(RColorBrewer)
library(colorspace)

# read the file
df <- read.csv(file = 'D:\\Study\\TU Dortmund\\ICS\\Summer 2023\\ICS 2023\\2nd report\\babies.csv', sep = ",")
data <- data.frame(df)

#------Q1------ descriptive statistics

smoke_stat <- df %>% group_by(smoke) %>% get_summary_stats(wt, show = c("n", "min", "max", "mean", "median", "iqr", "sd")) # to appendix
wt_stat <- df %>% get_summary_stats(wt, show = c("n", "min", "max", "mean", "median", "iqr", "sd"))

smoke_stat
wt_stat


# ---Barplot--- to show Count of mothers of different smoking status.

smokePlot <- table(data$smoke)

barplot(smokePlot,
        main = "Count of mothers of different smoking status",
        col = rainbow_hcl(5),
        xlab = "Smoking status of mothers", ylab = "Frequency", ylim = c(0, 600))

# Adding a legend
legend("topright",
       legend = c("0 - Never", "1 - Smokes now", "2 - Until current pregnancy", "3 - Once did, not now", "9 - Unknown"),
       fill = rainbow_hcl(5),
       title = "Smoking status of mothers")

# ---Histogram--- of babies' weights at birth

ggplot(data, aes(x = wt)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  xlab("Weights of babies") +
  ylab("Density") +
  ggtitle("Histogram of weights of babies at birth")


#------Q2------ difference between categories

diff <- ggplot(data, aes(x=factor(smoke), y=wt, fill=factor(smoke)))+
  geom_boxplot() + xlab("Different smoking status") + ylab("Weights in ounces") + labs(fill = "smoke") +
  scale_fill_manual(values = c("skyblue", "lightgreen", "lightsalmon1", "plum1", "peachpuff1"), 
                    labels = c("0 - never", "1 - smokes now", "2 - until current pregnancy","3 - once did, now now","9 - unknown"))

diff


#------Q3------ difference between categories


?qqline
# check normality assumption

smoke <- subset(df, Category == 'Backstroke')
dffer1 <-  ggqqplot(Backstroke, "Time",xlab="Theoritical Quantile" ,ylab="Sample Quantile for Backstroke", conf.int = FALSE, conf.int.level = 0)
dffer1
















# ---Boxplot--- to show difference between categories

gr2 <- ggplot(df, aes(x=Category, y=Time, fill=Category))+
  geom_boxplot() + xlab("Category") + ylab("Time in seconds") + labs(fill = "Category")+
  theme(legend.position = "none") + coord_flip()

gr2


# ---Assumptions check---
?qqline
# check normality assumption

Backstroke <- subset(df, Category == 'Backstroke')
gr3 <-  ggqqplot(Backstroke, "Time",xlab="Theoritical Quantile" ,ylab="Sample Quantile for Backstroke", conf.int = FALSE, conf.int.level = 0)
gr3

Breaststroke <- subset(df, Category == 'Breaststroke')
gr4 <- ggqqplot(Breaststroke, "Time",xlab="Theoritical Quantile" , ylab="Sample Quantile for Breaststroke",conf.int = FALSE, conf.int.level = 0)
gr4
Butterfly <- subset(df, Category == 'Butterfly')
gr5 <-ggqqplot(Butterfly, "Time",xlab="Theoritical Quantile" , ylab="Sample Quantile for Butterfly",conf.int = FALSE, conf.int.level = 0)
gr5
Freestyle <- subset(df, Category == 'Freestyle')
gr6 <-ggqqplot(Freestyle, "Time",xlab="Theoritical Quantile" , ylab="Sample Quantile for Freestyle",conf.int = FALSE, conf.int.level = 0)
gr6
Medley <- subset(df, Category == 'Medley')
gr7 <-ggqqplot(Medley, "Time",xlab="Theoritical Quantile" ,ylab="Sample Quantile for Medley",conf.int = FALSE, conf.int.level = 0)
gr7
plot_grid(gr3, gr4, gr5, gr6, gr7,  ncol=3, nrow=2)

# ---Global test---
t2 <-aov(Time ~ Category, data = df)
t2
summary(t2)

#summary(model)
#anova(model)

# As the p-value is less than the significance level 0.05,
# we can conclude that there are significant differences
# between the groups 
?pairwise.t.test
# ---Two-sample t-tests---
t3 <- pairwise.t.test(df$Time, df$Category, pool.sd = TRUE, p.adjust.method = "none")
t3

# ---Adjustment for Multiple testing ---

# t-tests with Bonferroni method
t4 <- pairwise.t.test(df$Time, df$Category, pool.sd = TRUE, p.adjust.method = "bonferroni")$p.value
t4

# t-tests with Holm-Bonferroni method
t5 <- pairwise.t.test(df$Time, df$Category, pool.sd = TRUE, p.adjust.method = "holm")$p.value
t5
