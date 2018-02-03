#####################################
# 
# UHS ED Consultants Working overnight
# Interrupted Time Series Modelling Script
# Modelling - Total Time in Department (monthly average)
# Intervention group: night time 10pm to 6am
# Control group: day time 6am to 10pm
# Interruption occurs at time point 35 (November 2015)
# Total 54 observations.Unit of analysis = month.  
# 
# Pre = 34; Post = 20
# T.Monks
#####################################

###################################
# Dependencies
###################################

library(nlme)
library(car)
library(tseries)
library(ggplot2)



########################
# Read in the dataset
########################

#read in manually.


########################
# Descriptives: Plot the series - sample size for each point
########################



# Plot the time series for the nights where consultants worked in ED.
plot(data_ed$time[1:54],data_ed$patients_n[1:54],
     ylab="Sample size (patients)",
     ylim=c(0,4500),
     xlab="Year-Month",
     type="l",
     col="blue",
     xaxt="n")

# Add in control group flow into Lake Huron
points(data_ed$time[55:108],data_ed$patients_n[55:108],
       type='l',
       col="red")

# Add x-axis year labels
axis(1, at=1:54, labels=data_ed$x_labels[1:54])

# Add in the points for the figure
points(data_ed$time[1:54],data_ed$patients_n[1:54],
       col="blue",
       pch=20)

points(data_ed$time[55:108],data_ed$patients_n[55:108],
       col="red",
       pch=20)

# Label the weather change
abline(v=34.5,lty=2, lwd = 2)

# Add in a legend
legend(x=0.2, y=4500, legend=c("Night (Intervention)","Day (Control)"),
       col=c("blue","red"),pch=20)



########################
# Plotting a smoothing line
########################
par(mfrow=c(1,1))
smoother = c(1/24,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/24)
trendpattern = filter (data_ed$mean_total_time[1:54], filter = smoother, sides=2)
trendpattern2 = filter (data_ed$mean_total_time[55:108], filter = smoother, sides=2)



##########
# 12 Month smoother to visualise difference.
#########

plot (data_ed$time[1:54], data_ed$mean_total_time[1:54], 
      type= "p", 
      ylab="Total Time in Department (mean mins)",
      ylim=c(0,300),
      xlab="Month",
      col='lightblue',
      pch=20,
      xaxt="n")

# Add x-axis year labels
axis(1, at=1:54, labels=data_ed$x_labels[1:54])

points(data_ed$time[1:54],data_ed$mean_total_time[55:108],
       type='p',
       col="pink",
       pch=20)

lines (trendpattern, col='blue', lwd = 3)
lines (trendpattern2, col='red', lwd = 3)

abline(v=34.5,lty=2)


legend(x=1, y=100, legend=c("Night (intervention)","Day (control)"),
       col=c("blue","red"),pch=20)



########################
# Model 1: Simple OLS with seasonal dummies
########################

#check for unit root - all okay ADF(4) = -4.6 p < 0.01
adf.test(data_ed$mean_total_time)


# A preliminary OLS regression
model_ols<-lm(mean_total_time ~ time + group + group_time + level + trend + group_level + 
          group_trend + 
            M1 +
            M2 +
            M3 +
            M4 +
            M5 +
            M5 +
            M6 +
            M7 +
            M8 +
            M9 + 
            M10 + 
            M11 , data=data_ed)
summary(model_ols)
confint(model_ols)

influence(model_ols)
cooks.distance(model_ols)

################################
# Assessing Autocorrelation
################################

# Durbin-watson test to 12 lags
dwt(model_ols,max.lag=12,alternative="two.sided")

# Graph the residuals from the OLS regression to check for serially correlated errors
plot(data_ed$time[1:54],
	residuals(model_ols)[1:54],
	type='o',
	pch=16,
	xlab='Time',
	ylab='OLS Residuals',
	col="red")
abline(h=0,lty=2)

# Plot ACF and PACF
# Set plotting to two records on one page
par(mfrow=c(1,2))

# Produce Plots
acf(residuals(model_ols), lag.max = 50)
acf(residuals(model_ols),type='partial')
# Note decay in ACF, significant spike at 10 in PACF, model p=10




########################
# Run the final model
########################
  
# Fit the GLS regression model
model_p10 <- gls(mean_total_time ~ time + group + group_time + level + trend + group_level + 
             group_trend + 
               M1 +
               M2 +
               M3 +
               M4 +
               M5 +
               M5 +
               M6 +
               M7 +
               M8 +
               M9 + 
               M10 + 
               M11 ,
             data=data_ed,
             correlation=corARMA(p=10,form=~time|group),
             method="ML")
summary(model_p10)
confint(model_p10)


########################
# Sensitivity analysis
########################

#ACF exp decay to zero indicating AR
#PACF spikes at 2, 10 and 14 (just).

# Likelihood-ratio tests
model_p2<- update(model_p10,correlation=corARMA(p=2,form=~time|group))
anova(model_p2,model_p10)
summary(model_p2)
#AR(10) has lower AIC.  Conclusions are the same.
  
model_p14 <- update(model_p10,correlation=corARMA(p=14, form=~time|group))
anova(model_p10,model_p14)
summary(model_p14)
#AR(14) has lower AIC (marginally).  Substantive conclusions are the same.

# Put plotting back to one chart
par(mfrow=c(1,1))

model_final <- model_p10

# Residual plot
qqPlot(residuals(model_final))

influence(model_p16, do.coef = TRUE)

########################
# Plot results  
#########################

# First plot the raw data points for the Nile
plot(data_ed$time[1:54],data_ed$mean_total_time[1:54],
          ylim=c(0,450),
          ylab="Total Time in Dept. (Mean Mins)",
          xlab="Year/Mnth",
          pch=20,
          col="lightblue",
          xaxt="n")

# Add x-axis year labels
axis(1, at=1:54, labels=data_ed$x_labels[1:54])
# Label the policy change
abline(v=34.5,lty=2)

# Add in the points for the control
points(data_ed$time[55:108],data_ed$mean_total_time[55:108],
       col="pink",
       pch=20)

# Plot the first line segment for the intervention group
lines(data_ed$time[1:34], fitted(model_final)[1:34], col="blue",lwd=2)

# Add the second line segment for the intervention group
lines(data_ed$time[35:54], fitted(model_final)[35:54], col="blue",lwd=2)


segments(35, model_final$coef[1] + model_final$coef[2]*35 + model_final$coef[3]+model_final$coef[4]*35 + 
           model_final$coef[5] + model_final$coef[6],
         54, model_final$coef[1] + model_final$coef[2]*54 + model_final$coef[3]+model_final$coef[4]*54 + 
           model_final$coef[5] + model_final$coef[6]*20,
         lty=2,col='blue',lwd=2)




# Plot the first line segment for the control group
lines(data_ed$time[55:88], fitted(model_final)[55:88], col="red",lwd=2)

# Add the second line segment for the control
lines(data_ed$time[89:108], fitted(model_final)[89:108], col="red",lwd=2)

# Add the counterfactual for the control group
#segments(1, model_final$coef[1]+model_final$coef[2],
        #60,model_final$coef[1]+model_final$coef[2]*54,
         #lty=2,col='red',lwd=2)

# Add in a legend
legend(x=40, y=430, legend=c("Night Worked","On Call"), col=c("blue","red"),pch=20)


########################
# Model 2: Seasonality (summer) modelled as single variable
########################

# A preliminary OLS regression
model_ols<-lm(mean_total_time ~ time + group + group_time + level + trend + group_level + 
                group_trend + ed_summer, data=data_ed)
summary(model_ols)
confint(model_ols)

influence(model_ols)
cooks.distance(model_ols)

################################
# Assessing Autocorrelation
################################

# Durbin-watson test to 12 lags
dwt(model_ols,max.lag=12,alternative="two.sided")
pac#suggestive of a prblem at lag 12

# Graph the residuals from the OLS regression to check for serially correlated errors
plot(data_ed$time[1:54],
     residuals(model_ols)[1:54],
     type='o',
     pch=16,
     xlab='Time',
     ylab='OLS Residuals',
     col="red")
abline(h=0,lty=2)

# Plot ACF and PACF
# Set plotting to two records on one page
par(mfrow=c(1,2))

# Produce Plots
acf(residuals(model_ols), lag.max = 12)
acf(residuals(model_ols),type='partial', lag.max = 12)
# Note decay in ACF, significant spike at 10 in PACF, model p=10

########################
# Run the final model
########################

# Fit the GLS regression model
model_p2 <- gls(mean_total_time ~ time + group + group_time + level + trend + group_level + 
                   group_trend + ed_summer, 
                 data=data_ed,
                 correlation=corARMA(p=2,form=~time|group),
                 method="ML")
summary(model_p2)
confint(model_p2)


outlierTest(model_ols) # Bonferonni p-value for most extreme obs
cutoff <- 4/((nrow(data_ed)-length(model_p2$coefficients)-2)) 
plot(model_p2, which=4, cook.levels=cutoff)

influencePlot(model_ols,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )


########################
# Sensitivity
########################
#ACF arguably decays to zero.  MA(1) to be tested for sensitivity.
#PACF spikes at 2. Durbin-Watson suggestive of lag 12.

# Likelihood-ratio tests
model_p2q1 <- update(model_p2,correlation=corARMA(p=2,q=1,form=~time|group))
anova(model_p2q1,model_p2)
summary(model_p2q1)
#non-significant.  Substantive conclusions the same.

model_p12 <- update(model_p2,correlation=corARMA(p=12, form=~time|group))
anova(model_p12,model_p2)
summary(model_p12)
#non-significant.  Substantive conclusions the same.

model_p12q1 <- update(model_p2,correlation=corARMA(p=12, q = 1, form=~time|group))
anova(model_p12q1,model_p2)
summary(model_p12q1)
#non-significant.  Substantive conclusions the same.

# Put plotting back to one chart
par(mfrow=c(1,1))

model_final <- model_p2
summary(model_final)

#psuedo R squared
R2 <- cor(data_ed$mean_total_time,predict(model_final))^2
R2

# Residual plot
qqPlot(residuals(model_final))

influence(model_p2, do.coef = TRUE)

########################
# Plot regression results - average out seasonality
#########################


plot(data_ed$time[1:54],data_ed$mean_total_time[1:54],
     ylim=c(0,300),
     ylab="Total Time in Dept. (Mean Mins)",
     xlab="Month",
     pch=20,
     col="lightblue",
     xaxt="n")

# Add x-axis year labels
axis(1, at=1:54, labels=data_ed$x_labels[1:54])
# Label the policy change
abline(v=34.5,lty=2)

# Add in the points for the control
points(data_ed$time[55:108],data_ed$mean_total_time[55:108],
       col="pink",
       pch=20)

# Plot the first line segment for the intervention group
#lines(data_ed$time[1:34], fitted(model_final)[1:34], col="blue",lwd=2)

# Add the second line segment for the intervention group
#lines(data_ed$time[35:54], fitted(model_final)[35:54], col="blue",lwd=2)


#segments(35, model_final$coef[1] + model_final$coef[2]*35 + model_final$coef[3]+model_final$coef[4]*35 + 
#           model_final$coef[5] + model_final$coef[6],
#         54, model_final$coef[1] + model_final$coef[2]*54 + model_final$coef[3]+model_final$coef[4]*54 + 
#           model_final$coef[5] + model_final$coef[6]*20,
#         lty=2,col='blue',lwd=2)




# Plot the first line segment for the control group
#lines(data_ed$time[55:88], fitted(model_final)[55:88], col="red",lwd=2)

# Add the second line segment for the control
#lines(data_ed$time[89:108], fitted(model_final)[89:108], col="red",lwd=2)








# Calculate the offset to average out seasonality in the visualisation
offset <- mean(data_ed$ed_summer) * model_final$coef[9]
offset




#Plot the first line for the intervention group (averaging out seasonality)
segments(1,
         model_final$coef[1] + model_final$coef[2] + model_final$coef[3] + offset,
         34,
         model_final$coef[1] + (model_final$coef[2]*34) +
           + model_final$coef[3] + (model_final$coef[4]*34) + offset,
         lty=1, lwd=2, col='blue')




#intervention group post period line (averaging out seasonality)  CORRECT.
segments(35,
         model_final$coef[1] + model_final$coef[3] + 
           (model_final$coef[2] +  model_final$coef[4])*35 +
           model_final$coef[5] + model_final$coef[6] +
           model_final$coef[7] + model_final$coef[8] + offset,
         54,
         model_final$coef[1] + model_final$coef[3] + 
           (model_final$coef[2] +  model_final$coef[4])*35 +
           model_final$coef[5] + model_final$coef[7] +
           (model_final$coef[6] + model_final$coef[8])*20 + offset,
         lty=1, lwd=2, col='blue')







# Plot the first line segment for the control group (averaging out seasonality)
segments(1,
         model_final$coef[1] + model_final$coef[2] + offset,
         34,
         model_final$coef[1] + model_final$coef[2]*34 + offset,
         lty=1, lwd=2, col='red')



#Plot the second line for the control group (averaging out seasonality)
#correct 23/01/18
segments(35,
         model_final$coef[1] + (model_final$coef[2]*35) +
           model_final$coef[5] + model_final$coef[6] + offset,
         54,
         model_final$coef[1] + (model_final$coef[2]*35) +
           model_final$coef[5] + (model_final$coef[6]*54) + offset,
         lty=1, lwd=2, col='red')


#counterfactual for intervention 23/01/2018
segments(35,
         model_final$coef[1] + (model_final$coef[2]*35) +
           model_final$coef[5] + model_final$coef[6] + offset +
           model_final$coef[3] + model_final$coef[4]*35,
         54,
         model_final$coef[1] + (model_final$coef[2]*35) +
           model_final$coef[5] + (model_final$coef[6]*20) + offset+ 
           model_final$coef[3] + model_final$coef[4]*54,
         lty=2, lwd=2, col='blue')



# Add in a legend
legend(x=2, y=100, legend=c("Night (intervention)","Day (control)"), col=c("blue","red"),pch=20)




##############################################
# Sensitivity to wild points - cookes distance > 4/n
##############################################


# significant - same substantive findings
model_p2 <- gls(mean_total_time ~ time + group + group_time + level + trend + group_level + 
                  group_trend + ed_summer + wild, 
                data=data_ed,
                correlation=corARMA(p=2,form=~time|group),
                method="ML")
summary(model_p2)
confint(model_p2)



##############################################
# Sensitivity to interaction between group and seasonality
##############################################


# not significant exclude from model. (also tried with p = 12.  same results)
model_p2 <- gls(mean_total_time ~ time + group + group_time + level + trend + group_level + 
                  group_trend + ed_summer + summer_group, 
                data=data_ed,
                correlation=corARMA(p=2, form=~time|group),
                method="ML")
summary(model_p2)
confint(model_p2)


##############################################
# Predict absolute and relative changes
##############################################

prediction <- function(model, time, time_after){
  pred <- fitted(model)[time]
  
  cfac <- model_final$coef[1] + model_final$coef[2]*time +
    model_final$coef[3] + model_final$coef[4]*time +
    model_final$coef[5] + model_final$coef[6]*time_after
  
  abs <- pred - cfac
  rel <- (pred - cfac) / cfac * 100
  
  return(list(absolute=abs, relative=rel))
}

# Predicted value at 3 months after introducing consultants at night (month 37)
prediction(model_final, 37, 3)

# Predicted value at 6 months after introducing consultants at night (month 40)
prediction(model_final, 40, 6)

# Predicted value at 12 months after introducing consultants at night (month 46)
prediction(model_final, 46, 12)

# END