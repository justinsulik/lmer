library(tidyverse)
library(magrittr)

# Generate some random data
dataFake <- data.frame(a=rnorm(500,10,2),b=rnorm(500,10,2))

# Create an outcome that is log-related to the random variables (here 2^(a+b))
dataFake %<>% mutate(y=2^(a+b)) 

# Plot 1: untransformed data
p1<- dataFake %>% 
  ggplot(aes(x=a,
             y=y))+
  geom_point()+
  labs(title='Untransformed:\nnon-linear relationship')

# Plot 2: log-transformed data
p2 <- dataFake %>% 
  mutate(transformed=log(y)) %>%
  ggplot(aes(x=a,
             y=transformed))+
  geom_point()+
  labs(y='log(y)',
       title='Transformed:\nlinear relationship')


# Plot both plots in one frame
multiplot(p1,p2, cols=2)