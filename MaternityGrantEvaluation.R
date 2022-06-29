library(Epi)
library(dplyr)
library(ggplot2)
library(readxl)
library(Synth)
library(SCtools)
library(car)

# ITS data
data_38 <- read_excel("data_38.xlsx")
data_38tr<- read_excel("data_38tr.xlsx")
data_49 <- read_excel("data_49.xlsx")
data_49tr <- read_excel("data_49tr.xlsx")


# SC data
data_sc <- read_excel("data_sc.xlsx")
data_sc <- as.data.frame(data_sc)

# Initial plot IMRs across study time frame ----
ggplot(data_49, aes(x=year, y=imr)) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept=1937.5,color="red") +
    geom_vline(xintercept=1948.5,color="red") +
    xlab(expression(bold(paste("Year")))) +  
    ylab(expression(bold(paste("Infant Mortality Rate")))) +
    scale_x_continuous(breaks=seq(1925, 1975, 10)) +
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(size = 20),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size = 1.2))
    



#---------------INTERRUPTED TIME SERIES--------------------------#

# 1938 model 1 (un-adjusted) ----

model_1 <- lm(imr ~ time + level + trend, data=data_38)
summary(model_1) 
output_1 <- round(ci.lin(model_1, Exp = F), 2)

# ACF, P-ACF, Q-Q plot of residuals
par(mfrow = c(2,2))
acf(residuals(model_1), 
    main = "a) 1938 (main)")
pacf(residuals(model_1),
     main = "b) 1938 (main)")
qqPlot(residuals(model_1),
       main = "c) 1938 (main)",
       ylab = "Residuals",
       xlab = "Norm quantiles")

# Plot of predicted trend
data_38$pred = predict(model_1)
ggplot(data_38,aes(y=imr,x=year,linetype="dashed")) + geom_line() +
    geom_line(aes(y=pred,linetype="solid")) +
    geom_vline(xintercept=1937.5,color="red") +
    geom_point() +
    scale_x_continuous(breaks=seq(1925, 1955, 5)) +
    scale_y_continuous(breaks=seq(30, 100, 20)) +
    xlab(expression(bold(paste("Year")))) +  
    ylab(expression(bold(paste("Infant Mortality Rate")))) + 
    scale_linetype_manual(name="Trend",values=c("solid","dashed"),labels=c("Actual","Predicted"))+
    ggtitle("a) Interrupted time series, 1938") +
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 17),
          axis.title.y = element_text(size = 17),
          plot.title = element_text(size = 20),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size = 1.2),
          legend.position = c(0.01,0.01), legend.justification = c("left","bottom"),
          legend.key.size = unit(1, 'cm'), 
          legend.key.height = unit(1, 'cm'), 
          legend.key.width = unit(1, 'cm'), 
          legend.title = element_text(size=17), 
          legend.text = element_text(size=15))


# Predicted value in 1939, the year after introduction
pred <- fitted(model_1)[18]
# Counterfactual value in 1939
cfac <- model_1$coef[1] + model_1$coef[2]*18
# Absolute change in 1939
pred - cfac 
# Relative change in 1939
(pred - cfac) / cfac #This represents a 23% increase 


# 1938 model 2 (wild-point) ----
model_2 <- lm(imr ~ time + level + trend + wild.point, data=data_38)
summary(model_2)
output_2 <- round(ci.lin(model_2, Exp = F), 2)

# ACF, P-ACF, and Q-Q plot of residuals
par(mfrow = c(2,3))
acf(residuals(model_2),
    main = "Maternity Grant introduction 1938 
    (model 2)")
pacf(residuals(model_2),
    main = "Maternity Grant introduction 1938
    (model 2)")
qqPlot(residuals(model_2),
       main = "Maternity Grant introduction 1938
       (model 2)",
       ylab = "Residuals",
       xlab = "Norm quantiles")


pred1938 <- fitted(model_1938b)[18]
cfac1938 <- model_1938b$coef[1] + model_1938b$coef[2]*18
# Absolute change in 1939
pred1938 - cfac1938 
#Relative change in 1939
(pred1938 - cfac1938) / cfac1938 


# 1938 model 3 (truncated) ----

model_3 <- lm(imr ~ time + level + trend, data=data_38tr)
summary(model_3) 
output_3 <- round(ci.lin(model_3, Exp = F), 2)

# ACF, P-ACF & Q-Q plot of residuals
par(mfrow = c(2,2))
acf(residuals(model_3),
    main = "Maternity Grant introduction 1938 
    (model 3)")
pacf(residuals(model_3), 
     main = "Maternity Grant introduction 1938 
     (model 3)")
qqPlot(residuals(model_3),
       main = "Maternity Grant introduction 1938 
       (model 3)",
       ylab = "Residuals",
       xlab = "Norm quantiles")

# 1938 model 4 (temporal 1936) ----
model_4 <- lm(imr ~ time + level.36 + trend.36, data=data_38)
summary(model_4) 
output_4 <- round(ci.lin(model_4, Exp = F), 2)

# P-ACF & ACF & Q-Q plot of residuals 
par(mfrow = c(2,2))
acf(residuals(model_4),
    main = "Maternity Grant introduction 1938
    (model 4)")
pacf(residuals(model_4), 
     main = "Maternity Grant introduction 1938
     (model 4)")
qqPlot(residuals(model_4),
       main = "Maternity Grant introduction 1938 
       (model 4)",
       ylab = "Residuals",
       xlab = "Norm quantiles")


# 1938 model 5 (temporal 1940)----
model_5 <- lm(imr ~ time + level.40 + trend.40, data=data_38)
summary(model_5) 
output_5 <- round(ci.lin(model_5, Exp = F), 2)

# P-ACF & ACF & Q-Q plot of residuals 
par(mfrow = c(2,2))
acf(residuals(model_5),
    main = "Maternity Grant introduction 1938 
    (model 5)")
pacf(residuals(model_5), 
     main = "Maternity Grant introduction 1938 
     (model 5)")

qqPlot(residuals(model_5),
       main = "Maternity Grant introduction 1938 
       (model 5)",
       ylab = "Residuals",
       xlab = "Norm quantiles")

# 1949 model 1 (un-adjusted) ----
model_1 <- lm(imr ~ time + level + trend, data=data_49)
summary(model_1) 
output_1 <- round(ci.lin(model_1, Exp = F), 2)


# ACF & P-ACF & Q-Q plot of residuals
par(mfrow = c(2,2))
acf(residuals(model_1),
    main = "a) 1949 (main)")
pacf(residuals(model_1),
     main = "b) 1949 (main)")

qqPlot(residuals(model_1), ylab = "Residuals",
       xlab = "Norm quantiles",
       main = "c) 1949 (main)")

# Plot of predicted trend
data_49$pred = predict(model_1)
ggplot(data_49,aes(y=imr,x=year,linetype="dashed")) + geom_line() +
    geom_line(aes(y=pred,linetype="solid")) +
    geom_vline(xintercept=1948.5,color="red") +
    geom_point() +
    scale_x_continuous(breaks=seq(1925, 1975, 10)) +
    xlab(expression(bold(paste("Year")))) +  
    ylab(expression(bold(paste("Infant Mortality Rate")))) + 
    scale_linetype_manual(name="Trend",values=c("solid","dashed"),labels=c("Actual","Predicted"))+
    ggtitle("a) Interrupted time series, 1949") +
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 17),
          axis.title.y = element_text(size = 17),
          plot.title = element_text(size = 20),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size = 1.2),
          legend.position = c(0.01,0.01), legend.justification = c("left","bottom"),
          legend.key.size = unit(1, 'cm'), 
          legend.key.height = unit(1, 'cm'), 
          legend.key.width = unit(1, 'cm'), 
          legend.title = element_text(size=17), 
          legend.text = element_text(size=15))


#Predicted value in 1950, the year after introduction
pred <- fitted(model_1)[29]
#Counterfactual in 1950
cfac <- model_1$coef[1] + model_1$coef[2]*29
#Absolute change in 1950
pred - cfac #IMR was -13.54 lower in 1950 than predicted
#Relative change in 1950
(pred - cfac) / cfac #This represents a 27% decrease

# 1949 model 2 (wild point) ----
model2 <- lm(imr ~ time + level + trend + wild.point, data=data_49)
summary(model2) 
output_2 <- round(ci.lin(model2, Exp = F), 2)

# ACF & P-ACF & Q-Q plot of residuals
par(mfrow = c(2,2))
acf(residuals(model2),
    main = "Maternity Grant universalisation 1949
    (model 2)")
pacf(residuals(model2),
     main = "Maternity Grant universalisation 1949
    (model 2)")
qqPlot(residuals(model2), ylab = "Residuals",
       xlab = "Norm quantiles",
       main = "Maternity Grant universalisation 1949
    (model 2)")

#Predicted value in 1950, the year after introduction
pred <- fitted(model2)[29]
#Counterfactual in 1950
cfac <- model2$coef[1] + model2$coef[2]*29
#Absolute change in 1950
pred - cfac #IMR was -12.27 lower in 1950 than predicted
#[model 1938] Relative change in 1950
(pred - cfac) / cfac #This represents a 0.178% increase


# 1949 model 3 (truncated) ------------------------

model_3 <- lm(imr ~ time + level + trend, data=data_49tr)
summary(model_3) 
output_3 <- round(ci.lin(model_3, Exp = F), 2)

# ACF & P-ACF & Q-Q plot
par(mfrow = c(2,2))
acf(residuals(model_3),
    main = "Maternity Grant universalisation 1949
    (model 3)")
pacf(residuals(model_3), main = "Maternity Grant universalisation 1949
    (model 3)")
qqPlot(residuals(model_3),
       main = "Maternity Grant universalisation 1949
    (model 3)",
       ylab = "Residuals",
       xlab = "Norm quantiles")

# 1949 model 4 (temporal 1947) ----

model_4 <- lm(imr ~ time + level.47 + trend.47, data = data_49)
summary(model_4) 
output_4 <- round(ci.lin(model_4, Exp = F), 2)

# ACF & P-ACF & Q-Q plot of residuals
par(mfrow = c(2,2))
acf(residuals(model_4),
    main = "Maternity Grant universalisation 1949
    (model 4)")
pacf(residuals(model_4), main = "Maternity Grant universalisation 1949
    (model 4)")
qqPlot(residuals(model_4), main = "Maternity Grant universalisation 1949
    (model 4)",
       ylab = "Residuals",
       xlab = "Norm quantiles")

# 1949 model 5 (temporal 1951)----
model_5 <- lm(imr ~ time + level.51 + trend.51, data = data_49)
summary(model_5) 
output_5 <- round(ci.lin(model_5, Exp = F), 2)

# ACF & P-ACF & Q-Q plot of residuals
par(mfrow = c(2,2))
acf(residuals(model_5),
    main = "Maternity Grant universalisation 1949
    (model 5)")
pacf(residuals(model_5), main = "Maternity Grant universalisation 1949
    (model 5)")

qqPlot(residuals(model_5), main = "Maternity Grant universalisation 1949
    (model 5)",
       ylab = "Residuals",
       xlab = "Norm quantiles")



# 1949 model 6 (quadratic term)-----------------------

model6 <- lm(imr ~ time + level + trend + trend.squared, data=data_49)
summary(model6) 
output_6 <- round(ci.lin(model6, Exp = F), 2)


par(mfrow = c(2,2))
acf(residuals(model6), 
    main = "Maternity Grant universalisation 1949
    (model 6)")
pacf(residuals(model6),  main = "Maternity Grant universalisation 1949
    (model 6)")
qqPlot(residuals(model6), ylab = "Residuals",
       xlab = "Norm quantiles",
       main = "Maternity Grant universalisation 1949
    (model 6)")


#---------------SYNTHETIC CONTROL---------------------------------#


#--------------SYNTHETIC CONTROL ---------------------------------#

# 1938 (main sc) ####
dataprep.out38 <- dataprep(foo = data_sc, 
                           time.predictors.prior = 1922:1937, 
                           special.predictors = list(list("imr", 1922, "mean"),
                                                     list("imr", 1923, "mean"), 
                                                     list("imr", 1924, "mean"),
                                                     list("imr", 1925, "mean"), 
                                                     list("imr", 1926, "mean"), 
                                                     list("imr", 1927, "mean"), 
                                                     list("imr", 1928, "mean"),
                                                     list("imr", 1929, "mean"), 
                                                     list("imr", 1930, "mean"), 
                                                     list("imr", 1931, "mean"),
                                                     list("imr", 1932, "mean"), 
                                                     list("imr", 1933, "mean"), 
                                                     list("imr", 1934, "mean"), 
                                                     list("imr", 1935, "mean"), 
                                                     list("imr", 1936, "mean"), 
                                                     list("imr", 1937, "mean")), 
                           dependent = "imr", unit.variable = "countryno", 
                           unit.names.variable = "countryname", 
                           time.variable = "year", 
                           treatment.identifier = 1, 
                           controls.identifier = c(2:12),
                           time.optimize.ssr = 1922:1937,  
                           time.plot = 1922:1953)

synth.out38 <- synth(data.prep.obj = dataprep.out38, method = "BFGS")

gaps <- dataprep.out38$Y1plot - (dataprep.out38$Y0plot %*% synth.out38$solution.w)
gaps[1:3, 1]
synth.tables <- synth.tab(dataprep.res = dataprep.out38, synth.res = synth.out38)
synth.tables$tab.loss
synth.tables$tab.pred
synth.tables$tab.v
synth.tables$tab.w
path.plot(synth.res = synth.out38, dataprep.res = dataprep.out38, Main = "Maternity Grant introduction 1938", Ylab = "IMR", Xlab = "Year", Ylim = c(0, 180), Legend = c("Finland", "Synthetic Control"), Legend.position = "topright", Z.plot = FALSE, tr.intake = 1938)

#extract data and improve plot

synth_data_out = data.frame(dataprep.out38$Y0plot%*%synth.out38$solution.w) 
date = as.numeric(row.names(synth_data_out))
plot.38 = data.frame(y=data_sc$imr[data_sc$countryno==1 & data_sc$year %in% date])
plot.38$synth = synth_data_out$w.weight
plot.38$date <- data_sc$date[data_sc$countryno==1 & data_sc$year %in% date]

ggplot(plot.38,aes(y=y,x=date,linetype="dashed")) + geom_line() +
    geom_line(aes(y=synth,x=date,linetype="solid")) +
    geom_vline(xintercept=1937.5,color="red") +
    geom_point() +
    scale_x_continuous(breaks=seq(1920, 1955, 5)) +
    scale_y_continuous(breaks=seq(30, 100, 20)) +
    xlab(expression(bold(paste("Year")))) +  
    ylab(expression(bold(paste("Infant Mortality Rate")))) + 
    scale_linetype_manual(name="Trend",values=c("solid","dashed"),labels=c("Actual","Synthetic control"))+
    ggtitle("b) Synthetic control, 1938") +
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 17),
          axis.title.y = element_text(size = 17),
          plot.title = element_text(size = 20),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size = 1.2),
          legend.position = c(0.01,0.01), legend.justification = c("left","bottom"),
          legend.key.size = unit(1, 'cm'), 
          legend.key.height = unit(1, 'cm'), 
          legend.key.width = unit(1, 'cm'), 
          legend.title = element_text(size=17), 
          legend.text = element_text(size=15))




# 1938 (truncated sc) ---- 
dataprep.out38tr <- dataprep(foo = data_sc, 
                             time.predictors.prior = 1928:1937, 
                             special.predictors = list(list("imr", 1928, "mean"), 
                                                       list("imr", 1929, "mean"),
                                                       list("imr", 1930, "mean"),
                                                       list("imr", 1931, "mean"), 
                                                       list("imr", 1932, "mean"), 
                                                       list("imr", 1933, "mean"), 
                                                       list("imr", 1934, "mean"),
                                                       list("imr", 1935, "mean"), 
                                                       list("imr", 1936, "mean"), 
                                                       list("imr", 1937, "mean")),
                             dependent = "imr", 
                             unit.variable = "countryno", 
                             unit.names.variable = "countryname", 
                             time.variable = "year", 
                             treatment.identifier = 1, 
                             controls.identifier = c(2:12),
                             time.optimize.ssr = 1928:1937, 
                             time.plot = 1928:1947)

synth.out38tr <- synth(data.prep.obj = dataprep.out38tr, method = "BFGS")
path.plot(synth.res = synth.out38tr, dataprep.res = dataprep.out38tr, Ylab = "Infant Mortality Rate", Xlab = "Year", Ylim = c(0, 180), Legend = c("Finland", "Synthetic Control"), Legend.position = "topright", Z.plot = FALSE, tr.intake = 1938,
          Main = "Introduction of Maternity Grant in 1938
    (model 2)")
gaps.plot(synth.res = synth.out38tr, dataprep.res = dataprep.out38tr, Ylab = c("Gap"), Xlab = c("Year"), tr.intake = 1938, Z.plot = FALSE,
          Main = "Introduction of Maternity Grant in 1938
    (model 2)")
synth.tables <- synth.tab(dataprep.res = dataprep.out38tr, synth.res = synth.out38tr)
synth.tables$tab.w
# 1938 (temporal sc) ####
dataprep.out38re <- dataprep(foo = data_sc, 
                             time.predictors.prior = 1922:1932, 
                             special.predictors = list(list("imr", 1922, "mean"), 
                                                       list("imr", 1923, "mean"), 
                                                       list("imr", 1924, "mean"), 
                                                       list("imr", 1925, "mean"),
                                                       list("imr", 1926, "mean"),
                                                       list("imr", 1927, "mean"), 
                                                       list("imr", 1928, "mean"), 
                                                       list("imr", 1929, "mean"), 
                                                       list("imr", 1930, "mean"), 
                                                       list("imr", 1931, "mean"), 
                                                       list("imr", 1932, "mean")),
                             dependent = "imr",
                             unit.variable = "countryno", 
                             unit.names.variable = "countryname",
                             time.variable = "year", 
                             treatment.identifier = 1, 
                             controls.identifier = c(2:12), 
                             time.optimize.ssr = 1922:1932,  
                             time.plot = 1922:1953)
synth.out38re <- synth(data.prep.obj = dataprep.out38re, method = "BFGS")
path.plot(synth.res = synth.out38re, dataprep.res = dataprep.out38re, Ylab = "IMR", Xlab = "Year", Ylim = c(0, 180), Legend = c("Finland", "Synthetic Control"), Legend.position = "topright", Z.plot = FALSE, tr.intake = 1933,
          Main = "Introduction of Maternity Grant in 1938
    (model 3)")





# 1938 (exposure status sc) ----
dataprep.out38it <- dataprep(foo = data_sc, time.predictors.prior = 1922:1937, 
                             special.predictors = list(list("imr", 1922, "mean"), 
                                                       list("imr", 1923, "mean"),
                                                       list("imr", 1924, "mean"),
                                                       list("imr", 1925, "mean"),
                                                       list("imr", 1926, "mean"),
                                                       list("imr", 1927, "mean"),
                                                       list("imr", 1928, "mean"),
                                                       list("imr", 1929, "mean"),
                                                       list("imr", 1930, "mean"),
                                                       list("imr", 1931, "mean"),
                                                       list("imr", 1932, "mean"), 
                                                       list("imr", 1933, "mean"),
                                                       list("imr", 1934, "mean"), 
                                                       list("imr", 1935, "mean"),
                                                       list("imr", 1936, "mean"),
                                                       list("imr", 1937, "mean")),
                             dependent = "imr", 
                             unit.variable = "countryno",
                             unit.names.variable = "countryname", 
                             time.variable = "year", 
                             treatment.identifier = 7, 
                             controls.identifier = c(1:6,8:12),
                             time.optimize.ssr = 1922:1937, 
                             time.plot = 1922:1953)

synth.out38it <- synth(data.prep.obj = dataprep.out38it, method = "BFGS")
synth.tables <- synth.tab(dataprep.res = dataprep.out38it, synth.res = synth.out38it)
synth.tables$tab.v
synth.tables$tab.w
path.plot(synth.res = synth.out38it, dataprep.res = dataprep.out38it, 
          Main = "Maternity Grant introduction 1938
    (model 4)", Ylab = "IMR", Xlab = "Year", Ylim = c(0, 180), Legend = c("Italy", "Synthetic Control"), Legend.position = "topright", Z.plot = FALSE, tr.intake = 1938)
gaps.plot(synth.res = synth.out38it, dataprep.res = dataprep.out38it, Ylab = c("Gap"), Xlab = c("Year"), tr.intake = 1938, Z.plot = TRUE,
          Main = "Introduction of Maternity Grant in 1938
    (model 4)")


# 1949 (main sc) ####
dataprep.out49 <- dataprep(foo = data_sc, 
                           time.predictors.prior = 1922:1948, 
                           special.predictors = list(list("imr", 1922, "mean"), 
                                                     list("imr", 1923, "mean"), 
                                                     list("imr", 1924, "mean"), 
                                                     list("imr", 1925, "mean"), 
                                                     list("imr", 1926, "mean"), 
                                                     list("imr", 1927, "mean"), 
                                                     list("imr", 1928, "mean"), 
                                                     list("imr", 1929, "mean"), 
                                                     list("imr", 1930, "mean"), 
                                                     list("imr", 1931, "mean"), 
                                                     list("imr", 1932, "mean"), 
                                                     list("imr", 1933, "mean"), 
                                                     list("imr", 1934, "mean"),
                                                     list("imr", 1935, "mean"), 
                                                     list("imr", 1936, "mean"), 
                                                     list("imr", 1937, "mean"),
                                                     list("imr", 1938, "mean"),
                                                     list("imr", 1939, "mean"), 
                                                     list("imr", 1940, "mean"), 
                                                     list("imr", 1941, "mean"), 
                                                     list("imr", 1942, "mean"), 
                                                     list("imr", 1943, "mean"),
                                                     list("imr", 1944, "mean"),
                                                     list("imr", 1945, "mean"), 
                                                     list("imr", 1946, "mean"), 
                                                     list("imr", 1947, "mean"), 
                                                     list("imr", 1948, "mean")),
                           dependent = "imr", unit.variable = "countryno", 
                           unit.names.variable = "countryname", 
                           time.variable = "year", 
                           treatment.identifier = 1, 
                           controls.identifier = c(2:12), 
                           time.optimize.ssr = 1922:1948,  time.plot = 1922:1975)
synth.out49 <- synth(data.prep.obj = dataprep.out49, method = "BFGS")
synth.tables <- synth.tab(dataprep.res = dataprep.out49, synth.res = synth.out49)
synth.tables$tab.v
synth.tables$tab.w
path.plot(synth.res = synth.out49, dataprep.res = dataprep.out49,
          Ylab  = "IMR", Xlab = "Year", Ylim = c(0, 180), 
          Legend = c("Finland", "Synthetic Control"), 
          Legend.position = "topright", Z.plot = FALSE, tr.intake = 1949,
          Main = "Universalisation of Maternity Grant in 1949
    (model 1)")
gaps.plot(synth.res = synth.out49, dataprep.res = dataprep.out49, Ylab = c("Gap"), Xlab = c("Year"), tr.intake = 1949, Z.plot = FALSE,
          Main = "Universalisation of Maternity Grant in 1949
    (model 1)")


#extract data and improve plot
synth_data_out = data.frame(dataprep.out49$Y0plot%*%synth.out49$solution.w) 
date = as.numeric(row.names(synth_data_out))
plot.49 = data.frame(y=data_sc$imr[data_sc$countryno==1 & data_sc$year %in% date])
plot.49$synth = synth_data_out$w.weight
plot.49$date <- data_sc$date[data_sc$countryno==1 & data_sc$year %in% date]

ggplot(plot.49,aes(y=y,x=date,linetype="dashed")) + geom_line() +
    geom_line(aes(y=synth,x=date,linetype="solid")) +
    geom_vline(xintercept=1948.5,color="red") +
    geom_point() +
    scale_x_continuous(breaks=seq(1925, 1975, 10)) +
    xlab(expression(bold(paste("Year")))) +  
    ylab(expression(bold(paste("Infant Mortality Rate")))) + 
    scale_linetype_manual(name="Trend",values=c("solid","dashed"),labels=c("Actual","Synthetic control"))+
    ggtitle("b) Synthetic control, 1949") +
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 17),
          axis.title.y = element_text(size = 17),
          plot.title = element_text(size = 20),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size = 1.2),
          legend.position = c(0.01,0.01), legend.justification = c("left","bottom"),
          legend.key.size = unit(1, 'cm'), 
          legend.key.height = unit(1, 'cm'), 
          legend.key.width = unit(1, 'cm'), 
          legend.title = element_text(size=17), 
          legend.text = element_text(size=15))






# 1949 (truncated sc) ####
dataprep.out49tr <- dataprep(foo = data_sc, time.predictors.prior = 1939:1948, 
                             special.predictors = list(list("imr", 1939, "mean"),
                                                       list("imr", 1940, "mean"),
                                                       list("imr", 1941, "mean"),
                                                       list("imr", 1942, "mean"),
                                                       list("imr", 1943, "mean"),
                                                       list("imr", 1944, "mean"),
                                                       list("imr", 1945, "mean"),
                                                       list("imr", 1946, "mean"),
                                                       list("imr", 1947, "mean"),
                                                       list("imr", 1948, "mean")), 
                             dependent = "imr", unit.variable = "countryno", 
                             unit.names.variable = "countryname", 
                             time.variable = "year", treatment.identifier = 1,
                             controls.identifier = c(2:12), 
                             time.optimize.ssr = 1939:1948, 
                             time.plot = 1939:1958)
synth.out49tr <- synth(data.prep.obj = dataprep.out49tr, method = "BFGS")
synth.tables <- synth.tab(dataprep.res = dataprep.out49tr, synth.res = synth.out49tr)
synth.tables$tab.v
synth.tables$tab.w
path.plot(synth.res = synth.out49tr, dataprep.res = dataprep.out49tr,
          Ylab  = "IMR", Xlab = "Year", Ylim = c(0, 180), 
          Legend = c("Finland", "Synthetic Control"), 
          Legend.position = "topright", Z.plot = FALSE, tr.intake = 1949,
          Main = "Universalisation of Maternity Grant in 1949
    (model 2)")
gaps.plot(synth.res = synth.out49tr, dataprep.res = dataprep.out49tr, Ylab = c("Gap"), Xlab = c("Year"), tr.intake = 1949, Z.plot = FALSE,
          Main = "Universalisation of Maternity Grant in 1949
    (model 2)")

# 1949 (temporal sc) ----
dataprep.out49re <- dataprep(foo = data_sc, 
                             time.predictors.prior = 1922:1943, 
                             special.predictors = list(list("imr", 1922, "mean"), 
                                                       list("imr", 1923, "mean"), 
                                                       list("imr", 1924, "mean"), 
                                                       list("imr", 1925, "mean"), 
                                                       list("imr", 1926, "mean"), 
                                                       list("imr", 1927, "mean"), 
                                                       list("imr", 1928, "mean"), 
                                                       list("imr", 1929, "mean"), 
                                                       list("imr", 1930, "mean"), 
                                                       list("imr", 1931, "mean"), 
                                                       list("imr", 1932, "mean"), 
                                                       list("imr", 1933, "mean"), 
                                                       list("imr", 1934, "mean"),
                                                       list("imr", 1935, "mean"), 
                                                       list("imr", 1936, "mean"), 
                                                       list("imr", 1937, "mean"),
                                                       list("imr", 1938, "mean"),
                                                       list("imr", 1939, "mean"), 
                                                       list("imr", 1940, "mean"), 
                                                       list("imr", 1941, "mean"), 
                                                       list("imr", 1942, "mean"), 
                                                       list("imr", 1943, "mean")),
                             dependent = "imr", unit.variable = "countryno", 
                             unit.names.variable = "countryname", 
                             time.variable = "year", 
                             treatment.identifier = 1, 
                             controls.identifier = c(2:12), 
                             time.optimize.ssr = 1922:1943,  time.plot = 1922:1975)

synth.out49re <- synth(data.prep.obj = dataprep.out49re, method = "BFGS")
synth.tables <- synth.tab(dataprep.res = dataprep.out49re, synth.res = synth.out49re)
synth.tables$tab.v
synth.tables$tab.w
path.plot(synth.res = synth.out49re, dataprep.res = dataprep.out49re,
          Ylab  = "IMR", Xlab = "Year", Ylim = c(0, 180), 
          Legend = c("Finland", "Synthetic Control"), 
          Legend.position = "topright", Z.plot = FALSE, tr.intake = 1944,
          Main = "Universalisation of Maternity Grant in 1949
    (model 3)")

# 1949 (exposure status sc) ----
dataprep.out49be <- dataprep(foo = data_sc, 
                             time.predictors.prior = 1922:1948, 
                             special.predictors = list(list("imr", 1922, "mean"), 
                                                       list("imr", 1923, "mean"), 
                                                       list("imr", 1924, "mean"), 
                                                       list("imr", 1925, "mean"), 
                                                       list("imr", 1926, "mean"), 
                                                       list("imr", 1927, "mean"), 
                                                       list("imr", 1928, "mean"), 
                                                       list("imr", 1929, "mean"), 
                                                       list("imr", 1930, "mean"), 
                                                       list("imr", 1931, "mean"), 
                                                       list("imr", 1932, "mean"), 
                                                       list("imr", 1933, "mean"), 
                                                       list("imr", 1934, "mean"),
                                                       list("imr", 1935, "mean"), 
                                                       list("imr", 1936, "mean"), 
                                                       list("imr", 1937, "mean"),
                                                       list("imr", 1938, "mean"),
                                                       list("imr", 1939, "mean"), 
                                                       list("imr", 1940, "mean"), 
                                                       list("imr", 1941, "mean"), 
                                                       list("imr", 1942, "mean"), 
                                                       list("imr", 1943, "mean"),
                                                       list("imr", 1944, "mean"),
                                                       list("imr", 1945, "mean"), 
                                                       list("imr", 1946, "mean"), 
                                                       list("imr", 1947, "mean"), 
                                                       list("imr", 1948, "mean")),
                             dependent = "imr", unit.variable = "countryno", 
                             unit.names.variable = "countryname", 
                             time.variable = "year", 
                             treatment.identifier = 9, 
                             controls.identifier = c(1:8, 10:12), 
                             time.optimize.ssr = 1922:1948,  time.plot = 1922:1975)
synth.out49be <- synth(data.prep.obj = dataprep.out49be, method = "BFGS")
synth.tables <- synth.tab(dataprep.res = dataprep.out49be, synth.res = synth.out49be)
synth.tables$tab.v
synth.tables$tab.w
path.plot(synth.res = synth.out49be, dataprep.res = dataprep.out49be,
          Ylab  = "IMR", Xlab = "Year", Ylim = c(0, 180), 
          Legend = c("Belgium", "Synthetic Control"), 
          Legend.position = "topright", Z.plot = FALSE, tr.intake = 1949,
          Main = "Universalisation of Maternity Grant in 1949
    (model 4)")
gaps.plot(synth.res = synth.out49be, dataprep.res = dataprep.out49be, Ylab = c("Gap"), Xlab = c("Year"), tr.intake = 1949, Z.plot = FALSE,
          Main = "Universalisation of Maternity Grant in 1949
    (model 4)")
