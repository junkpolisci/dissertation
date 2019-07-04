library(tidyverse)
loop <- data.frame(term = as.character(), 
                   estimate = as.numeric(), 
                   std.error = as.numeric(), 
                   statistic = as.numeric(), 
                   p.value = as.numeric(),
                   Right = as.character(),
                   it = as.numeric())

for(i in 1:10000){
  test <- data.frame(x1 = rnorm(10000, 1, 2), 
                     x2 = rnorm(10000, 2, 2),
                     x3 = rnorm(10000, 3, 2),
                     x4 = rnorm(10000, 4, 2),
                     x5 = rnorm(10000, 5, 2),
                     e = rnorm(10000, 0, 1)) %>% 
    mutate(y = 1 + .1 * x1 + .5 * x2  + 1 * x3 + 0*x4 + e)
  
  lm <- broom::tidy(lm(y~x1+x2+x3+x4,
                         data = test)) %>% 
    mutate(it = i,
           Right = 'Correct Model')
  
  lm_wrong <- broom::tidy(lm(y~x1+x2+x3+x4+x5,
                         data = test)) %>% 
    mutate(it = i, 
           Right = 'Wrong Model')
  
  loop <- rbind(loop, lm, lm_wrong)
}

loop %>% 
  filter(term %in% c("x1", "x2", "x3", "x4", "x5")) %>% 
  mutate(sig = ifelse(p.value<=.05, 
                      1, 
                      0)) %>% 
  group_by(term, Right) %>% 
  add_count(term, name = 'n_term') %>% 
  add_count(sig) %>%
  summarise(n_term = mean(n_term), 
            n = mean(n)) %>% 
  ungroup() %>%
  mutate(percent = n / n_term) %>% 
  uncount(2) %>% 
  group_by(term, Right) %>% 
  mutate(seq = seq(term),
         sig = ifelse(seq ==1, 
                      "Insig.", 
                      "Sig."),
         percent = ifelse(sig == "Sig.", 
                          percent, 
                          1 - percent)) %>% 
  ggplot(aes(x = sig, 
             y = percent)) + 
  geom_col(aes(fill = Right)) +
  geom_text(aes(label = paste0(round(percent, 3) *100, "%"))) +
  facet_wrap(Right~term, ncol = 4, scales = "free_x") + 
  scale_x_discrete() +
  labs(title = "Percent of significant coefficients for OLS across 10,000 iterations",
       y = "Percent", 
       x = "Significant or Not", 
       caption = "Generative equation: y = 1 + .1(x1) + .5(x2) + 1(x3) + 0(x4) + e. e ~ N(0,1). All x ~ N(x#,2)") + 
  theme(plot.title = element_text(hjust = .5))

loop %>% 
  mutate(p = round(p.value, 3)) %>% 
  ggplot(aes()) + 
  geom_boxplot() + 
  facet_grid(Right~term)

require('Hmisc')
rcorr(as.matrix(test))

loop %>% 
  count(term)

loop %>% 
  filter(term %in% c("x1", "x2", "x3")) %>% 
  ggplot(aes(x = n, 
             y = statistic,
             color = p.value)) + 
  geom_line(size = 1) + 
  scale_color_gradient(high = 'red',
                      low = 'dark grey') +
  facet_wrap(.~term) + 
  labs(title = "Figure 1: T-Statistics for simple OLS as sample size increases",
       y = "Student's t", 
       x = "Sample Size", 
       caption = "Generative equation: y = 1 + .1(x1) + .5(x2) + 1(x3) + e. e ~ N(0,1). All x ~ N(5,2)") + 
  theme(plot.title = element_text(hjust = .5))

loop %>% 
  filter(term == "x3") %>% 
  mutate(sig = ifelse(abs(p.value <= .05),
                      1,
                      0)) %>% 
  ggplot(aes(x = n, y = cumsum(sig))) +
  geom_line()
  
  
loop %>% 
  filter(term %in% c("x1", "x2")) %>% 
  ggplot(aes(x = n, 
             y = estimate)) + 
  geom_line() + 
  facet_wrap(.~term) + 
  labs(title = "Figure 2: Coefficients for simple OLS as sample size increases",
       y = "Coefficient", 
       x = "Sample Size", 
       caption = "Generative equation: y = 1 + .1(x1) + .5(x2) + 1(x3) + e. e ~ N(0,1). All x ~ N(5,2)") + 
  theme(plot.title = element_text(hjust = .5))

time <- data.frame(x1 = rnorm(224, 10, 3), 
                   x2 = rnorm(224, 20, 3), 
                   random = rnorm(224, 50, 20), 
                   e = rnorm(224, 0, 1), 
                   time = 1:224) %>% 
  mutate(y_d1 = 1*lag(random, 1) + 
           .9*lag(random, 2) + 
           .8*lag(random, 3) + 
           .7*lag(random, 4) + 
           .6*lag(random, 5) +
           .5*lag(random, 6) + e,
         y_s1 = 50 + 1*lag(random, 12) + e,
         y_s2 = 50 + 1*lag(random, 24) + e,
         y_s12 = 50 + 1*lag(random, 12) +
           1*lag(random, 24) + e,
         y_exp = 1.2*lag(random, 1) + e)

t <- data.frame(n = 1:224) %>% 
  mutate(rand = 0, 
         test = 0)
t[1, 'rand'] <- 10

dim(t)

t[1, 'rand'] * 1 + rnorm(1)

for(i in 2:dim(t)[1]){
  t[1, 'test'] <- 10
  t[i, 'test'] <- t[i-1, 'test'] * 1 + rnorm(1)
}

t <- t %>% mutate(e = rnorm(224, 0, 1), 
                  y = 1 * lag(rand, 1) + e, 
                  rand = lag(rand, 1))



time %>% 
  ggplot(aes(x = time)) + 
  geom_line(aes(y = y_d1), color = "red") + 
  geom_line(aes(y = y_s1), color = "green") + 
  geom_line(aes(y = y_s2), color = "blue") + 
  geom_line(aes(y = y_s12))

time %>% 
  ggplot(aes(x = time, 
             y = y_exp - lag(y_exp, 1))) + 
  geom_line() + 
  geom_line(aes(y = random), color = "green")
