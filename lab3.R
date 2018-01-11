set.seed(42)


#-------------------excersise 1:

#parameters
#number of clients
clients = 10000
#shape
a = 0.6500
#a = 0,5556
#scale
b = 1

#THEORY
#firstly we define the expected service time, based on theory
#Mean E[x]
Theor_mean<- b * gamma((a + 1)/a)
#Variance Var[X]
Theor_var <- b^2 *(gamma((a+2)/a) - (gamma((a+1)/a))^2)
#COefficent of variation
Theor_cv <- sqrt(Theor_var/(Theor_mean)^2)


#EXPERIMENT
#then we define our weibull distribution
service_time <-rweibull(clients, a, b)
#in order to analyze the parameters we convert it into a vector
st <- as.vector(service_time)

Obtained_mean <- mean(st)
Obtained_var <- var(st)
Obtained_median <- median(st)
Obtained_cv <- sqrt(var(st))/mean(st)

sprintf("Mean from theory = %s", Theor_mean)
sprintf("Mean from experiment = %s", Obtained_mean)

sprintf("Variance from theory = %s", Theor_var)
sprintf("Vatiance from experiment = %s", Obtained_var)

sprintf("CV from theory = %s", Theor_cv)
sprintf("CV from experiment = %s", Obtained_cv)


sprintf("Median from experiment = %s", Obtained_median)

hist(st)


#-----------------------excersise 2

theta <- traffic_factors
startTime <- Sys.time()
x11()
par(mfrow=c(2,2))

  