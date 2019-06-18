# Code for Final Project 
# Name: Xiaozhu Zhang
# Student ID: X1887019

# ----------------------------------------------------
# ----------------------------------------------------
# 1. Introduction
# ----------
# read the data
dat <- read.csv("PRSA_data_2010.1.1-2014.12.31.csv")
dat <- dat[ ,-1]     # delete the index column
# set year, month, day and hour as factors
dat$year <- factor(dat$year, levels = paste(2010:2014),ordered = TRUE)
dat$month <- factor(dat$month, levels = paste(1:12), ordered = TRUE)
dat$day <- factor(dat$day, levels = paste(1:31), ordered = TRUE)
dat$hour <- factor(dat$hour, levels = paste(0:23), ordered = TRUE)



# ----------------------------------------------------
# ----------------------------------------------------
# 2. Preparation
# ----------
# 2.1 Missing values
# ----------
# The only variable with missing values is the response variable. 
# We will deal with the missing values later.


# ----------
# 2.2 Basic understanding of the dataset
# ----------
# EDA: get familiar with the data set
summary(dat)
#   year          month            day             hour           pm2.5             DEWP        
# 2010:8760   1      : 3720   1      : 1440   0      : 1826   Min.   :  0.00   Min.   :-40.000  
# 2011:8760   3      : 3720   2      : 1440   1      : 1826   1st Qu.: 29.00   1st Qu.:-10.000  
# 2012:8784   5      : 3720   3      : 1440   2      : 1826   Median : 72.00   Median :  2.000  
# 2013:8760   7      : 3720   4      : 1440   3      : 1826   Mean   : 98.61   Mean   :  1.817  
# 2014:8760   8      : 3720   5      : 1440   4      : 1826   3rd Qu.:137.00   3rd Qu.: 15.000  
#             10     : 3720   6      : 1440   5      : 1826   Max.   :994.00   Max.   : 28.000  
#             (Other):21504   (Other):35184   (Other):32868   NA's   :2067              
# 
#       TEMP             PRES      cbwd            Iws               Is          
#  Min.   :-19.00   Min.   : 991   cv: 9387   Min.   :  0.45   Min.   : 0.00000  
#  1st Qu.:  2.00   1st Qu.:1008   NE: 4997   1st Qu.:  1.79   1st Qu.: 0.00000  
#  Median : 14.00   Median :1016   NW:14150   Median :  5.37   Median : 0.00000  
#  Mean   : 12.45   Mean   :1016   SE:15290   Mean   : 23.89   Mean   : 0.05273  
#  3rd Qu.: 23.00   3rd Qu.:1025              3rd Qu.: 21.91   3rd Qu.: 0.00000  
#  Max.   : 42.00   Max.   :1046              Max.   :585.60   Max.   :27.00000  
#                                                                                
#        Ir             season     
#  Min.   : 0.0000   fall  :10920  
#  1st Qu.: 0.0000   spring:11040  
#  Median : 0.0000   summer:11040  
#  Mean   : 0.1949   winter:10824  
#  3rd Qu.: 0.0000                 
#  Max.   :36.0000                 


# EDA: Exploratory graphs of continuous variables
library(ggplot2)
ggplot(data = dat, aes(Iws)) + geom_histogram(bins = 50)   # not bell shape (cumulative)
ggplot(data = dat, aes(PRES)) + geom_histogram(bins = 50)  # quasi-bell shape
ggplot(data = dat, aes(TEMP)) + geom_histogram(bins = 50)  # quasi-bell shape
ggplot(data = dat, aes(DEWP)) + geom_histogram(bins = 50)  # quasi-bell shape
ggplot(data = dat, aes(Is)) + geom_histogram(bins = 50)   # not bell shape (cumulative)
ggplot(data = dat, aes(Ir)) + geom_histogram(bins = 50)   # not bell shape (cumulative)
ggplot(data = dat, aes(pm2.5)) + geom_histogram(bins = 50) # not bell shape



# ----------
# 2.3 Data transformation
# ----------
# 2.3.1 Logarithmic transformation
# ----------
ggplot(data = dat, aes(log(1 + Iws))) + geom_histogram(bins = 50)   # transform the variable
ggplot(data = dat, aes(log(1 + pm2.5))) + geom_histogram(bins = 50) # transform the variable
attach(dat)
dat <- cbind(dat, log(1 + Iws))
dat <- cbind(dat, log(1 + pm2.5))
detach(dat)


# ----------
# 2.3.2 Merging levels of categorical variables
# ----------
# add the season column
season <- vector('character', nrow(dat))
season[dat$month == 3 | dat$month == 4 | dat$month == 5] <- 'spring'
season[dat$month == 6 | dat$month == 7 | dat$month == 8] <- 'summer'
season[dat$month == 9 | dat$month == 10 | dat$month == 11] <- 'fall'
season[dat$month == 12 | dat$month == 1 | dat$month == 2] <- 'winter'
dat <- cbind(dat, season)

# add the time-interval column
time <- vector('character', nrow(dat))
time[dat$hour == 1 | dat$hour == 2 | dat$hour == 3 | dat$hour == 4 | dat$hour == 5 | dat$hour == 6] <- 'night'
time[dat$hour == 7 | dat$hour == 8 | dat$hour == 9 | dat$hour == 10 | dat$hour == 11 | dat$hour == 12] <- 'morning'
time[dat$hour == 13 | dat$hour == 14 | dat$hour == 15 | dat$hour == 16 | dat$hour == 17 | dat$hour == 18] <- 'afternoon'
time[dat$hour == 19 | dat$hour == 20 | dat$hour == 21 | dat$hour == 22 | dat$hour == 23 | dat$hour == 0] <- 'evening'
dat <- cbind(dat, time)



# ----------
# 2.3.3 Cluster based on five-number-summary
# ----------
library(sqldf)
sqldf('SELECT * FROM dat WHERE [Is] != 0 and [Ir] !=0')     # no such row exists
# [1] year           month          day            hour           pm2.5          DEWP          
# [7] TEMP           PRES           cbwd           Iws            Is             Ir            
# [13] log(1 + Iws)   log(1 + pm2.5) season         time          
# <0 rows> (or 0-length row.names)
PRECIP <- dat$Is + dat$Ir    # create new variable 'PRECIP'
dat <- cbind(dat, PRECIP)
table(PRECIP)
# PRECIP
# 0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15 
# 41648   595   362   251   167   140   113    95    75    60    51    42    33    28    24    17 
# 16    17    18    19    20    21    22    23    24    25    26    27    28    29    30    31 
# 14    15    14    13    11     9    10     9     4     4     3     3     2     2     2     2 
# 32    33    34    35    36 
# 2     1     1     1     1 


# extract the observations without missing values
dat1 <- sqldf('SELECT * FROM dat WHERE [pm2.5] != \'NA\'')
attach(dat1)

# deal with the variable PRECIP
# cluster based on five-number-summary / outliers
A <- dat1[,c('year', 'month', 'day', 'hour', 'PRECIP','log(1 + pm2.5)')]
A <- A[order(A$PRECIP), ]
k <- 3
cluster <- c()
type <- 1
start <- 1
for(i in 1: nrow(A)){
  q <- quantile(A$PRECIP[start : i])
  if(q[5] - q[4] <= k * (q[4] - q[2])){
    cluster <- c(cluster, type)
  }else{
    type <- type + 1
    cluster <- c(cluster, type)
    start <- i + 1
  }
}
A <- cbind(A, cluster)


# ANOVA test
fit <- aov(`log(1 + pm2.5)` ~ cluster, data = A)
summary(fit, test = c('Wilks'))
#                Df Sum Sq Mean Sq F value   Pr(>F)    
# cluster         1     30   29.67   29.37 6.03e-08 ***
# Residuals   41755  42192    1.01                     
# ---
# Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1


# boxplot after cluster
ggplot(A, aes(x = as.factor(cluster), y = PRECIP)) +
  geom_boxplot() +
  labs(x = "cluster", y = "PRECIP")


# merge some levels
A$cluster[A$cluster >= 2 & A$cluster <= 5] <- 2
A$cluster[A$cluster >= 6 & A$cluster <= 9] <- 3
A$cluster[A$cluster >= 10 & A$cluster <= 13] <- 4
A$cluster[A$cluster >= 14 & A$cluster <= 17] <- 5
A$cluster[A$cluster >= 18 & A$cluster <= 21] <- 6
A$cluster[A$cluster >= 22 & A$cluster <= 24] <- 7
A$cluster[A$cluster >= 25 & A$cluster <= 25] <- 8


# ANOVA test again
fit <- aov(`log(1 + pm2.5)` ~ cluster, data = A)
summary(fit, test = c('Wilks'))
#                Df Sum Sq Mean Sq F value  Pr(>F)   
# cluster         1      9   9.072   8.974 0.00274 **
# Residuals   41755  42212   1.011                   
# ---
# Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1


# boxplot after cluster merging
ggplot(A, aes(x = as.factor(cluster), y = PRECIP)) +
  geom_boxplot() +
  labs(x = "cluster", y = "PRECIP")


# extract PRE_clu (clusters) and put them into the dataset
PRE_clu <- sqldf('SELECT a.cluster
                  FROM dat1 d left JOIN A a
                  ON d.year = a.year AND d.month = a.month 
                  AND d.day = a.day AND d.hour = a.hour')
dat1 <- cbind(dat1, PRE_clu)
colnames(dat1)[colnames(dat1)=='cluster'] <- 'PRE_clu'




# ----------
# 2.4 Relationship between time, wind, other meteorological factors and pm2.5 
# ----------
# 2.4.1 Relationship between seasons, hours and pm2.5
# ----------
# plot1: Violin Plot
ggplot(dat1, aes(x = season, y = `log(1 + pm2.5)`, fill = season)) + 
  geom_violin(alpha=0.8,width=1) +
  geom_boxplot(width = 0.3, position = position_dodge(1))


# plot2: smoothed scatter plot
pm_sp <- sqldf('SELECT hour, AVG([log(1 + pm2.5)]) FROM dat1 WHERE season=\'spring\' GROUP BY hour')
pm_sm <- sqldf('SELECT hour, AVG([log(1 + pm2.5)]) FROM dat1 WHERE season=\'summer\' GROUP BY hour')
pm_fa <- sqldf('SELECT hour, AVG([log(1 + pm2.5)]) FROM dat1 WHERE season=\'fall\' GROUP BY hour')
pm_wt <- sqldf('SELECT hour, AVG([log(1 + pm2.5)]) FROM dat1 WHERE season=\'winter\' GROUP BY hour')

pm_time <- rbind(pm_sp, pm_sm, pm_fa, pm_wt)
season <- c(rep('Spring', 24), rep('Summer', 24), rep('Fall', 24), rep('Winter', 24))
pm_time <- cbind(pm_time, season)


ggplot(pm_time, aes(hour, `AVG([log(1 + pm2.5)])`, color = season)) + 
  geom_point(size = 2.0, shape = 16) +
  facet_wrap( ~ season) +
  geom_smooth(method = "loess")


# ANOVA test
fit <- aov(`log(1 + pm2.5)` ~ season * time, data = dat1)
summary(fit, test = c('Wilks'))
#                Df Sum Sq Mean Sq F value Pr(>F)    
# season          3     78   26.01   26.13 <2e-16 ***
# time            3    420  139.97  140.62 <2e-16 ***
# season:time     9    176   19.52   19.61 <2e-16 ***
# Residuals   41741  41548    1.00                   
# ---
# Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1


# Multi-comparison
library(agricolae)
out_season <- LSD.test(fit, c('season','time') ,p.adj="bonferroni")
out_season$groups
#                  log(1 + pm2.5) groups
# winter:evening         4.392295      a
# fall:evening           4.338525     ab
# summer:night           4.329755     ab
# summer:evening         4.259365     bc
# fall:night             4.246480     bc
# summer:morning         4.241457     bc
# winter:night           4.195397     cd
# spring:morning         4.174356    cde
# spring:night           4.138179     de
# spring:evening         4.134739    def
# summer:afternoon       4.090513    efg
# winter:morning         4.038919    fgh
# fall:morning           4.038793    fgh
# winter:afternoon       4.026857     gh
# spring:afternoon       3.998778     gh
# fall:afternoon         3.961267      h

plot(out_season)



# ----------
# 2.4.2 Relationship between wind and pm2.5
# ----------
# derive the relationship between season and direction
t <- table(cbdw = dat1$cbwd, season = dat1$season)
t
#     season
# cbdw fall spring summer winter
#   cv 2788   1837   2295   2024
#   NE 1226   1027   1084   1419
#   NW 3737   3183   1801   4763
#   SE 2662   4523   5209   2179

# conduct the chi-square test
chisq.test(t)
# Pearson's Chi-squared test
# 
# data:  t
# X-squared = 3408.7, df = 9, p-value < 2.2e-16



# draw the correlation analysis plot
library(ca)
ca_sea_dir <- ca(t)
plot(ca_sea_dir)



# calculate the duration of wind
# dur_cum (cumulative duration of a direction)
dur_cum <- c(1)
flag <- 1
for (i in 2 : nrow(dat1)){
  if(cbwd[i] == cbwd[i-1]){
    flag <- flag + 1
  }else{
    flag <- 1
  }
  dur_cum <- c(dur_cum, flag)
}

dat1 <- cbind(dat1, dur_cum)


# duration (ultimate duration of a direction)
duration <- c()
flag <- 1
for (i in 2 : (nrow(dat1) + 1)){
  # calculate duration for all observations but the last change of direction
  if(i != nrow(dat1) + 1){
    if (cbwd[i] == cbwd[i-1]){
      flag <- flag + 1
    }else{
      for(j in (i-flag) : (i-1)){
        duration[j] <- flag
      }
      flag <- 1
    }
  }
  # calculate the duration of the last change of direction
  else{
    for(j in (i-flag):(i-1)){
      duration[j] <- flag
    }
  }
}

dat1 <- cbind(dat1, duration)


# draw the radar graph
library(ggradar)
rd_sp <- sqldf('SELECT cbwd, AVG(Duration) FROM dat1 WHERE season = \'spring\'
               GROUP BY cbwd')
rd_sm <- sqldf('SELECT AVG(Duration) FROM dat1 WHERE season = \'summer\'
               GROUP BY cbwd')
rd_fa <- sqldf('SELECT AVG(Duration) FROM dat1 WHERE season = \'fall\'
               GROUP BY cbwd')
rd_wt <- sqldf('SELECT AVG(Duration) FROM dat1 WHERE season = \'winter\'
               GROUP BY cbwd')
rownames(rd_sp) <- rd_sp$cbwd 
rd_sp <- rd_sp[, -1, drop = FALSE]
rd_sp <- cbind(rd_sp, rd_sm, rd_fa, rd_wt)
colnames(rd_sp) <- c('Spring', 'Summer', 'Fall', 'Winter')
rd_sp <- as.data.frame(t(rd_sp))
rd_sp <- cbind(Name = rownames(rd_sp), rd_sp)
ggradar(rd_sp, grid.min = 0, grid.mid = 10, grid.max = 20,
        label.gridline.min = FALSE, label.gridline.mid = FALSE, 
        label.gridline.max = FALSE)



# draw the plot of NW direction
pm_NW_sp <- sqldf('SELECT dur_cum, AVG([log(1 + pm2.5)]) FROM dat1 
                  WHERE cbwd = \'NW\' and season = \'spring\'
                  GROUP BY dur_cum ')
pm_NW_sm <- sqldf('SELECT dur_cum, AVG([log(1 + pm2.5)]) FROM dat1 
                  WHERE cbwd = \'NW\' and season = \'summer\'
                  GROUP BY dur_cum ')
pm_NW_fa <- sqldf('SELECT dur_cum, AVG([log(1 + pm2.5)]) FROM dat1 
                  WHERE cbwd = \'NW\' and season = \'fall\'
                  GROUP BY dur_cum ')
pm_NW_wt <- sqldf('SELECT dur_cum, AVG([log(1 + pm2.5)]) FROM dat1 
                  WHERE cbwd = \'NW\' and season = \'winter\'
                  GROUP BY dur_cum ')
pm_NW <- rbind(pm_NW_sp, pm_NW_sm, pm_NW_fa, pm_NW_wt)
Season <- c(rep('Spring', nrow(pm_NW_sp)), 
            rep('Summer', nrow(pm_NW_sm)), 
            rep('Fall', nrow(pm_NW_fa)),
            rep('Winter', nrow(pm_NW_wt)))
pm_NW <- cbind(pm_NW, Season)
ggplot(pm_NW, aes(dur_cum, `AVG([log(1 + pm2.5)])`, color = Season)) + 
  geom_point(size = 2.0, shape = 16) +
  facet_wrap( ~ Season) +
  geom_smooth(method = "loess")



# draw the plot of SE direction
pm_SE_sp <- sqldf('SELECT dur_cum, AVG([log(1 + pm2.5)]) FROM dat1 
                  WHERE cbwd = \'SE\' and season = \'spring\'
                  GROUP BY dur_cum ')
pm_SE_sm <- sqldf('SELECT dur_cum, AVG([log(1 + pm2.5)]) FROM dat1 
                  WHERE cbwd = \'SE\' and season = \'summer\'
                  GROUP BY dur_cum ')
pm_SE_fa <- sqldf('SELECT dur_cum, AVG([log(1 + pm2.5)]) FROM dat1 
                  WHERE cbwd = \'SE\' and season = \'fall\'
                  GROUP BY dur_cum ')
pm_SE_wt <- sqldf('SELECT dur_cum, AVG([log(1 + pm2.5)]) FROM dat1 
                  WHERE cbwd = \'SE\' and season = \'winter\'
                  GROUP BY dur_cum ')
pm_SE <- rbind(pm_SE_sp, pm_SE_sm, pm_SE_fa, pm_SE_wt)
Season <- c(rep('Spring', nrow(pm_SE_sp)), 
            rep('Summer', nrow(pm_SE_sm)), 
            rep('Fall', nrow(pm_SE_fa)),
            rep('Winter', nrow(pm_SE_wt)))
pm_SE <- cbind(pm_SE, Season)
ggplot(pm_SE, aes(dur_cum, `AVG([log(1 + pm2.5)])`, color = Season)) + 
  geom_point(size = 2.0, shape = 16) +
  facet_wrap( ~ Season) +
  geom_smooth(method = "loess")



# ----------
# 2.4.3 Relationship between precipitation and pm2.5
# ----------
# create a new dataset
dat2 <- dat1[ ,c('year', 'month', 'day', 'hour', 'PRECIP', 'PRE_clu','log(1 + pm2.5)')]


# Binary categorical variables
cluster_cat <- levels(factor(dat2$PRE_clu))
binarycluster <- function(c) {return(dat2$PRE_clu == c)}
newVars <- sapply(cluster_cat, binarycluster)
colnames(newVars) <- c('c_1', 'c_2', 'c_3', 'c_4', 'c_5', 'c_6', 'c_7', 'c_8')
bin_matrix <- cbind(dat2$PRE_clu, newVars)

# rule out the variable 'c_8' to avoid multicollinearity
dat2 <- cbind(dat2, bin_matrix[, c('c_1', 'c_2',  'c_3', 'c_4', 'c_5', 'c_6', 'c_7')])

# build the linear regression model to explore
lm_clu <- lm(log(1 + pm2.5) ~ c_1 + c_2 + c_3 + c_4 + c_5 + c_6 + c_7, data = dat2)
summary(lm_clu)
# Call:
# lm(formula = log(1 + pm2.5) ~ c_1 + c_2 + c_3 + c_4 + c_5 + c_6 + 
#        c_7, data = dat2)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -4.1571 -0.7559  0.1333  0.7628  2.7456 
# 
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   3.1930     0.1897  16.835  < 2e-16 ***
# c_1           0.9641     0.1897   5.082 3.76e-07 ***
# c_2           1.2462     0.1916   6.503 7.97e-11 ***
# c_3           0.9499     0.1960   4.845 1.27e-06 ***
# c_4           0.6612     0.2044   3.234  0.00122 ** 
# c_5           0.6185     0.2204   2.807  0.00501 ** 
# c_6           0.6215     0.2361   2.633  0.00847 ** 
# c_7           0.4122     0.2707   1.523  0.12779    
# ---
# Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
# 
# Residual standard error: 1.004 on 41749 degrees of freedom
# Multiple R-squared:  0.004049,	Adjusted R-squared:  0.003882 
# F-statistic: 24.25 on 7 and 41749 DF,  p-value: < 2.2e-16



# ----------
# 2.4.4 Relationship between other meteorological factors and pm2.5
# ----------
# draw the correlation chart
library(PerformanceAnalytics)
chart.Correlation(dat1[ ,c('DEWP', 'TEMP', 'PRES', 'log(1 + Iws)', 'log(1 + pm2.5)')], 
                  histogram=TRUE, pch=19)



# ----------------------------------------------------
# ----------------------------------------------------
# 3. Machine learning algorithms
# ----------
# 3.1 Decision Trees
# ----------
library(rpart)
library(rpart.plot)
library(rattle)
# create new dataset for decision tree algorithm
dat3 <- dat1[, c('year', 'month', 'day', 'hour',
                 'DEWP', 'TEMP', 'PRES', 'log(1 + Iws)',
                 'time', 'season', 'cbwd', 'PRE_clu',
                 'log(1 + pm2.5)')]
colnames(dat3) <- c('year', 'month', 'day', 'hour', 'DEWP', 'TEMP', 'PRES',
                    'log_Iws', 'time', 'season', 'cbwd', 'PRE_clu', 'log_pm2.5')


# split the dataset into training set and test set
set.seed(314)
n <- nrow(dat3)  # Number of observations = 392
ntrain <- round(n * 0.6)    # 60% for training set
tindex <- sample(n, ntrain) # Create an index


# construct the tree model
tree_dat3 <- rpart(log_pm2.5 ~ DEWP + TEMP + PRES + log_Iws +
                     time + season + cbwd + PRE_clu,
                   data = dat3, subset = tindex, method="anova")
printcp(tree_dat3)
# Regression tree:
# rpart(formula = log_pm2.5 ~ DEWP + TEMP + PRES + log_Iws + time + 
#         season + cbwd + PRE_clu, data = dat3, subset = tindex, method = "anova")
# 
# Variables actually used in tree construction:
# [1] cbwd    DEWP    log_Iws season 
# 
# Root node error: 25382/25054 = 1.0131
# 
# n= 25054 
# 
# CP nsplit rel error  xerror      xstd
# 1 0.139735      0   1.00000 1.00010 0.0073616
# 2 0.059336      1   0.86027 0.86041 0.0073425
# 3 0.054405      2   0.80093 0.80110 0.0068563
# 4 0.052228      3   0.74652 0.74364 0.0063905
# 5 0.039884      4   0.69430 0.69592 0.0062423
# 6 0.011897      5   0.65441 0.65605 0.0060587
# 7 0.010000      6   0.64252 0.64391 0.0059721



# ----------
# 3.2 random forest
# ----------
library(randomForest)
# scale the dataset for random forest algorithm
set.seed(1)
scale <- scale(dat3[,c('DEWP', 'TEMP', 'PRES', 'log_Iws')])
dat3_scale <- cbind(dat3[, c('year', 'month', 'day', 'hour')],
                    scale,
                    dat3[, c('time', 'season', 'cbwd', 'PRE_clu', 'log_pm2.5')])


# build the model
set.seed(1)
rf_dat3 <- randomForest(log_pm2.5 ~ DEWP + TEMP + PRES + log_Iws + 
                          time + season + cbwd + PRE_clu, 
                        data = dat3_scale, subset = tindex, 
                        ntree = 100, mtry = 3, importance = T)


# output the importance of variables
I <- importance(rf_dat3)
I <- I[order(I[,2], decreasing = T),]
print(I)
#          %IncMSE IncNodePurity
# DEWP    99.63201     5970.2993
# log_Iws 80.37499     5134.1801
# TEMP    65.70037     3464.7520
# PRES    42.72223     2813.1675
# cbwd    75.48313     2697.1244
# season  38.35377     1752.4393
# time    56.52678     1016.6786
# PRE_clu 20.44668      266.7258



# calculate the test error
ytrue <- dat3$log_pm2.5[-tindex]
yhat <- predict(rf_dat3, newdata = dat3_scale[-tindex, ])
mean((yhat - ytrue)^2) / mean((ytrue - mean(ytrue))^2)
# [1] 0.3530796



# ----------------------------------------------------
# ----------------------------------------------------
# 4. Projection
# ----------
# 4.1 imputation of missing pm2.5
# ----------
# extract the observations with missing values
set.seed(1)
# assign values of PRE_clu to observations with missing values 
PRE_clu.p <- vector('integer', nrow(dat))
PRE_clu.p[dat$PRECIP == 0] <- 1
PRE_clu.p[dat$PRECIP >= 1 & dat$PRECIP <= 4] <- 2
PRE_clu.p[dat$PRECIP >= 5 & dat$PRECIP <= 8] <- 3
PRE_clu.p[dat$PRECIP >= 9 & dat$PRECIP <= 12] <- 4
PRE_clu.p[dat$PRECIP >= 13 & dat$PRECIP <= 16] <- 5
PRE_clu.p[dat$PRECIP >= 17 & dat$PRECIP <= 20] <- 6
PRE_clu.p[dat$PRECIP >= 21 & dat$PRECIP <= 23] <- 7
PRE_clu.p[dat$PRECIP >= 24 & dat$PRECIP <= 36] <- 8

# create new dataset for projection
dat.p <- data.frame(year = dat$year, month = dat$month, day = dat$day, hour = dat$hour,
               DEWP = dat$DEWP, TEMP = dat$TEMP, PRES = dat$PRES, 
               log_Iws = log(1+dat$Iws), time = dat$time, season = dat$season, 
               cbwd = dat$cbwd, PRE_clu = PRE_clu.p, log_pm2.5 = log(1 + dat$pm2.5))

# scale the new dataset
scale <- scale(dat.p[,c('DEWP', 'TEMP', 'PRES', 'log_Iws')])
dat.p_scale <- cbind(dat.p[, c('year', 'month', 'day', 'hour')],
                     scale,
                     dat.p[, c('time', 'season', 'cbwd', 'PRE_clu', 'log_pm2.5')])

# split the dataset based on the existence of missing values
dat.p_FULL <- dat.p[complete.cases(dat.p),]
dat.p_FULL <- cbind(dat.p_FULL, flag = rep(1, nrow(dat.p_FULL)))
dat.p_NA <- dat.p[!complete.cases(dat.p),]
dat.p_NA <- cbind(dat.p_NA, flag = rep(0, nrow(dat.p_NA)))
dat.p_scale_NA <- dat.p_scale[!complete.cases(dat.p_scale),]


# impute missing values based on random forest model
dat.p_NA$log_pm2.5 <- predict(rf_dat3, newdata = dat.p_scale_NA)
dat.p <- rbind(dat.p_FULL, dat.p_NA)
dat.p <- dat.p[order(dat.p$year, dat.p$month, dat.p$day, dat.p$hour), ]


# draw the time series plot
library(lubridate)
datetime <- make_datetime(year = as.integer(levels(dat.p$year)[dat.p$year]), 
                         month = as.integer(levels(dat.p$month)[dat.p$month]),
                         day = as.integer(levels(dat.p$day)[dat.p$day]),
                         hour = as.integer(levels(dat.p$hour)[dat.p$hour]))

ts <- data.frame(datetime, log_pm2.5 = dat.p$log_pm2.5, flag = as.factor(dat.p$flag))

ggplot(data = ts, aes(x = datetime, y = log_pm2.5)) + 
  geom_line(color = "#00AFBB", size = 0.4) +
  geom_smooth()


# ----------
# 4.2 Trend of pm2.5 from 2010 to 2014
# ----------
# use the non-parametric method 'Cox-Stuart test' to analyze trend
library(randtests)
cox.stuart.test(ts$log_pm2.5, 'two.sided')
#         Cox Stuart test
# 
# data:  ts$log_pm2.5
# statistic = 10688, n = 21792, p-value = 0.004934
# alternative hypothesis: non randomness


cox.stuart.test(ts$log_pm2.5, 'left.sided')
#        Cox Stuart test
# 
# data:  ts$log_pm2.5
# statistic = 10688, n = 21792, p-value = 0.002467
# alternative hypothesis: decreasing trend


cox.stuart.test(ts$log_pm2.5, 'right.sided')
#         Cox Stuart test
# 
# data:  ts$log_pm2.5
# statistic = 10688, n = 21792, p-value = 0.9976
# alternative hypothesis: increasing trend

detach(dat1)
