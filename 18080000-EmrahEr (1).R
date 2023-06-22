# -------------------------------------------------------------------------- ###
# Soru 1a ---- https://github.com/goksu-V/butsinav.git
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 2a ---- 

library(dplyr)

titanic %>%
  group_by(sex) %>%
  summarise(mean_fare = mean(fare))


# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 2b ---- 
library(tidyverse)

titanic %>%
  drop_na() %>%
  ggplot(aes(x = sex, y = age)) +
  geom_boxplot()
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 2c ----
survey %>%
  ggplot(aes(x = age)) +
  geom_histogram(aes(y = ..density..),
                 colour = "black", 
                 fill = "white") +
  geom_density(alpha = 0.5, 
               fill="#FFC0CB")
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3a ---- 
`11 14`
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3b ----
dat3 <- inner_join(dat1, dat2)
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3c ----
ggplot(dat, aes(x = X, y = Y)) +
geom_line() +
  geom_point() +
  labs(title = "Sekil 2")
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3d ---- myresult 2 4 olur
mylist <- list(1:3, c(3:5, NA))
myresult <- map(mylist, ~ mean(.x, na.rm = TRUE)) %>% unlist()


# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3e ---- 
pnorm(1, mean = µ, sd = σ/sqrt(25))

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3f ----library(ggplot2)

simulate_dice_roll <- function(num_rolls) {
  results <- data.frame(Dice1 = numeric(num_rolls), Dice2 = numeric(num_rolls))  
  
  for (i in 1:num_rolls) {
    dice1 <- sample(1:6, 1, replace = TRUE)  
    dice2 <- sample(1:6, 1, replace = TRUE)  
    
    results$Dice1[i] <- dice1 
    results$Dice2[i] <- dice2 
  }
  
  return(results)  
}


ggplot(simulated_data, aes(x = Dice1, y = Dice2)) +
  geom_point() +
  labs(x = "Zar 1", y = "Zar 2", title = "Zar Atışlarının Saçılım Grafiği")
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3g ----survived_data <- filter(titanic, survived == 1)
survived_ages <- survived_data$age


not_survived_data <- filter(titanic, survived == 0)
not_survived_ages <- not_survived_data$age


t_test_result <- t.test(survived_ages, not_survived_ages, var.equal = TRUE)


cat("Test Statistics:", t_test_result$statistic, "\n")
cat("P-value:", t_test_result$p.value, "\n")
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 4a ----library(tidyr)

dat <- data.frame(
  country = c("_Ingiltere", "Almanya"),
  '2018' = c(8000, 10000),
  '2019' = c(8100, 11000),
  '2020' = c(8500, 10200)
)

dat2 <- dat %>%
  pivot_longer(cols = -country, names_to = "year", values_to = "gdp") %>%
  mutate(year = as.integer(year))
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 5a ---- library(ggplot2)


dat <- data.frame(
  price = c(326, 326, 327, 334, 335),
  cut = c("Ideal", "Premium", "Good", "Premium", "Good"),
  depth = c(61.5, 59.8, 56.9, 62.4, 63.3),
  color = c("E", "E", "E", "I", "J")
)


ggplot(dat, aes(x = cut, y = price, color = color)) +
  geom_point() +
  labs(x = "Cut", y = "Price", color = "Color") +
  theme_minimal()
# -------------------------------------------------------------------------- ###