rm(list = ls()) #used to clear any previous calculations
library(deSolve) #If you get an error from this line, go to 'tools' above
#and select `install packages` and install `deSolve`.
#This is a package that has the R methods for numerically
#solving differential equations.
# To (numerically) solve differential equations, R requires 
# you to define a function (in this case we are calling it 
# `SIR_birth_death_equations`). 
#
# The function needs to take in 3 inputs:
# time (the current time)
# variables (a list of functions of time that you are trying to find)
# parameters (any constants the differential equations depend on)
#
# The function then calculates the derivatives of the functions using
# the current time, their current values, and the parameters.
#
# The function returns a list that has the derivatives.
#
SIR_birth_death_equations <- function(time, variables, parameters) {
  # This function calculates the derivatives of the SIR model 
  #
  # INPUTS:
  #`time` gives the current time of the simulation 
  # (it's not actually used for the SIR model, but 
  # R expects this function to have that as an input)
  #
  #`variables` gives the current values of `S`, `I`, and `R`
  #
  #`parameters` gives `beta` and `gamma`
  
  S <- variables[1] #this reads the current values of
  I <- variables[2] #S, I, and R from the input 
  R <- variables[3]
  
  beta <- parameters["beta"]
  gamma <- parameters["gamma"]
  mu <- parameters["mu"]
  
  #now we calculate all of the derivatives
  dSdt <- mu*(S+I+R) -beta * S * I - mu*S #this is dS/dt
  dIdt <- beta * S * I - gamma * I -mu*I #this is dI/dt
  dRdt <- gamma * I - mu*R #this is dR/dt
  
  dVariablesdt <- c(dSdt, dIdt, dRdt) #puts the derivatives into a single
  #object which it will return.
  
  return(list(dVariablesdt)) #Now it returns the derivatives
}
plot_solution_SIR <- function(solution){
  dev.new() #opens a new plot window
  with(solution, {
    plot(time, S, type = "l", col = "green",
         xlab = "t (days)", ylab = "Number of pigs", ylim = c(0, 500))
    lines(time, I, col = "red")
    lines(time, R, col = "blue")
  }
  
  )
  legend("right", c("S(t)", "I(t)", "R(t)"), col = c("green", "red", "blue"), lty 
         = 1, bty = "n")
  
}
#Now we define our parameters
N <-2000
R0 <- 
  my_gamma <-
  my_mu <- 
  simulation_duration <- 1000
#Set up the SIR simulation - define parameters, initial values, and the
#times of interest. 
parameters_values <- c(beta=my_beta, gamma=my_gamma, mu=my_mu)
initial_values <- c(S=N-1, I=1, R=0)
times_values <- seq(0, simulation_duration, by = 0.1) # "ode" will return lists
# giving S, I, and R at 
# the times given in
# times_values
#This block of code gets the solution and stores it with the name
#SIR_birth_death_solution. 
SIR_birth_death_solution <- ode(
  y = initial_values,
  times = times_values,
  func = SIR_birth_death_equations,
  parms = parameters_values 
)
#This converts the SIR_birth_death_solution into a format that is easy to plot.
#then it plots it.
SIR_birth_death_solution <- as.data.frame(SIR_birth_death_solution)
plot_solution_SIR(SIR_birth_death_solution)
print("The plot may show up in a new window. It may not come to the front")
print("of your view, so you may have to look for it.")