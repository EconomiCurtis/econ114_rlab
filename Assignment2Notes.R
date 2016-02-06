install.packages("dplyr")
library(dplyr)


mjna.2 <- 
  mjna %>%
  arrange(Date) %>%
  mutate(
    returns = log(Adj.Close) - lag(log(Adj.Close))
  ) %>%
  tbl_df()


skwness <- function(x){
  mean((x - mean(x, na.rm = T))^3 / ((sd(x, na.rm = T))^3),
       na.rm = T)
}
#
skwness(rnorm(50))

ktrss <- function(x){
  mean((x - mean(x, na.rm = T))^4 / ((sd(x, na.rm = T))^4),
       na.rm = T)
}

mjna.sum <- mjna.2 %>%
  summarise(
    skewness = skwness(returns),
    kurtosis = ktrss(returns)
  )

dist.norm <- data.frame(returns = rnorm(99999,0,1)) %>%
  summarise(
    skewness = skwness(returns),
    kurtosis = ktrss(returns)
  )



qqnorm(y = mjna.2$returns,
       datax = T)
qqline(y = mjna.2$returns,
       datax = T)


#-----
N = 10000

{
# bin <- data.frame(
#   result = rep(NA, N),
#   dist = rep(NA,N)
# ) %>% tbl_df()
# 
# system.time({
#   for (i in 1:N){
#     
#     d100 = runif(n = 1, 0,1)
#     d_norm = rnorm(1)
#     d_norm_5.72 = rnorm(1, mean = 5, sd = 7.2)
#     d_norm_2.11 = rnorm(1, mean = 2, sd = 11)  
#     
#     if (d100 <= 0.87){
#       result = d_norm
#       dist = "d_norm"
#     } else if (d100 <= 0.97){
#       result = d_norm_5.72    
#       dist = "d_norm_5.72"
#     } else {
#       result = d_norm_2.11
#       dist = "d_norm_2.11"
#     }
#     
#     bin$result[i] <- result
#     bin$dist[i] <- dist
#     
#   }
# })
# bin
# 
}


N = 10000

Sampler.kurt <- function(N = 100){
  
  q2.a <- data.frame(
    p87 = rnorm(N),
    p10 = rnorm(N, mean = 5, sd = 7.2),
    p03 = rnorm(N, mean = 2, sd = 11),
    d100 = runif(N, 0 , 1)
  ) %>%
    mutate(
      result = ifelse(
        d100 <= 0.87,
        yes = p87, 
        no = ifelse(
          d100 <= 0.97,
          yes = p10,
          no = p03
        )
      ),
      dist = ifelse(
        d100 <= 0.87,
        yes = "p87", 
        no = ifelse(
          d100 <= 0.97,
          yes = "p10",
          no = "p03"
        )
      )
    )
  
  return(ktrss(q2.a$result))
}

bin.n10 = c()
for (i in 1:1000){
  bin.n10 <- c(bin.n10, Sampler.kurt(100))
}
bin.n1000 = c()
for (i in 1:1000){
  bin.n100 <- c(bin.n100, Sampler.kurt(1000))
}
bin.n10000 = c()
for (i in 1:10000){
  bin.n1000 <- c(bin.n100, Sampler.kurt(10000))
}

hist(bin.n10, 100)
hist(bin.n1000, 100)
hist(bin.n10000)











