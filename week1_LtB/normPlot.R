# Draw sample from random distribution
x <- rnorm(30,0,1)

# Test for normality
w <- round(shapiro.test(x)$statistic[['W']], 2)
p <- round(shapiro.test(x)$p.value, 2)

# Create dataframe with normal QQ values (for help, type ?qqnorm)
qq <- data.frame(qqnorm(x)) %>% rename(normed=x,sample=y)

# Plot 1: density plot of sample and norm values
p1 <- qq %>% 
  gather(variable,value) %>%
  ggplot(aes(x=value,
             color=factor(variable)))+
  geom_density()+
  labs(title=paste('W:',w,'p:',p),
       color='variable')+
  theme(legend.position=c(0.15,0.92))

# Get the 1st/3rd quartiles of both variables
quartiles <- data.frame(sample=c(quantile(qq$sample)[[2]],
                                 quantile(qq$sample)[[4]]),
                        normed=c(quantile(qq$normed)[[2]],
                                 quantile(qq$normed)[[4]]))

# Use a linear model to get the slope/intercept for a line joining the 1st/3rd quantiles
mod.quartiles <- lm(normed~sample,quartiles)
intercept <- coef(mod.quartiles)[[1]]
slope <- coef(mod.quartiles)[[2]]

# Plot 2: plot sample vs normed values, along with a line joining 1st/3rd quantiles
p2 <- qq %>% 
  ggplot(aes(x=sample,
             y=normed))+
  geom_point()+
  geom_abline(slope=slope,
              intercept=intercept,
              color='blue')+
  labs(title="Q-Q norm plot",
       x="Sample Quantiles",
       y="Theoretical Quantiles")

# Plot both plots in one frame
multiplot(p1,p2, cols=2)

# Note: the axes of the QQ plot here are flipped, 
# compared what you'd see if you just used qqnorm() to produce a plot
