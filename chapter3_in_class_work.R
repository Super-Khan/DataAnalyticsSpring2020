library(dplyr)
df_mtcars <- mtcars
head(mtcars)
filter(df_mtcars, mpg > 20)
sample_n(filter(df_mtcars, mpg > 20), 10)
results_mpg <- arrange(sample_n(filter(df_mtcars, mpg > 20), 10), desc(mpg))
results_mpg

a1 <- filter(df_mtcars, mpg > 20)
a2 <- sample_n(df_mtcars, 5)
results_mpg_des <- arrange(a2, desc(mpg))
results_mpg_des

df_mtcars %>% filter(mpg > 20) %>% sample_n(5) %>% arrange(desc(mpg))
results <- df_mtcars %>% filter(mpg > 20) %>% sample_n(5) %>% arrange(desc(mpg))
results
