# Plot the time series for the nights where consultants worked in ED.
plot(data_ed$time[1:54],data_ed$patients_n[1:54],
     ylab="Sample size (patients)",
     ylim=c(0,5500),
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
legend(x=0.2, y=5600, legend=c("Night (Intervention)","Day (Control)"),
       col=c("blue","red"),pch=20)

#mean

#nigh pre
mean(data_ed$patients_n[1:34])
#night post
mean(data_ed$patients_n[35:54])

#day pre
mean(data_ed$patients_n[55:88])

#day post
mean(data_ed$patients_n[89:108])

#post total
x = data_ed$patients_n[35:54] + data_ed$patients_n[89:108]
summary(x)
sd(x)


#pre total
x = data_ed$patients_n[1:34] + data_ed$patients_n[55:88]
summary(x)
sd(x)

#STDEV

#night pre
sd(data_ed$patients_n[1:34])
#night post
sd(data_ed$patients_n[35:54])


#day pre
sd(data_ed$patients_n[55:88])

#day post
sd(data_ed$patients_n[89:108])


