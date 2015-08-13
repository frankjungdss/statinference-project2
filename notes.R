library(datasets)
tg <- data.frame(ToothGrowth)
names(tg)
# unique(tg$dose)
# unique(tg$supp)
# range(tg$len)
# oj <- tg[tg$supp == "OJ", c("len", "dose")]
# vc <- tg[tg$supp == "VC", c("len", "dose")]
tg$dose <- factor(tg$dose)
str(tg)
table(oj$dose)
table(vc$dose)
range(tg$len)

# library(pastecs)
# stat.desc(tg$len)

# as sample size for each dose is small (10) plot
library(ggplot2)
ggplot(tg, aes(x = dose, y = len)) +
    geom_violin() +
    geom_boxplot(width = 0.2, fill = "purple", alpha = 0.4) +
    stat_summary(fun.y = mean, geom = "point", colour = "white", shape = 19, size = 2) +
    facet_grid(supp ~ .)

# run associated t-test, see
# - http://localhost/dokuwiki/doku.php?id=frank:datascience:t_tests
# - https://class.coursera.org/statinference-031/wiki/Week_3%3A_Confidence_intervals_and_testing

# by dose, compare len of each supplement
by(tg, tg$dose, function (x) t.test(len ~ supp, x, alternative = c("two.sided"))[c("conf.int", "p.value")])

t.test(len ~ supp, data = tg[tg$dose == 0.5,], alternative = c("two.sided"))
t.test(len ~ supp, data = tg[tg$dose == 1,], alternative = c("two.sided"))
t.test(len ~ supp, data = tg[tg$dose == 2,], alternative = c("two.sided"))
