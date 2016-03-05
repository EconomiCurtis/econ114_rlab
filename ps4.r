df <- read.table(
  "C:/Users/OKComputer/Dropbox/R Lab/Econ114_RLab/Econ114_Rlab/guessTheModel1.txt", 
  quote="\"", comment.char="")


df$V1
df$x = 1:100000


library(ggplot2)

ggplot(
  df, 
  aes(x = x,
      y = V1)
)  +
  geom_point(alpha = 0.05)

ggplot(
  df, 
  aes(x = x,
      y = V1)
)  +
  geom_point(alpha = 0.05) + 
  stat_smooth()

reg.1 <-  lm(V1 ~ x, data = df)
reg.1 <- glm(V1 ~ x, data = df)
summary(reg.1)


ggplot(
  df, 
  aes(x = V1)
)  +
  geom_histogram(bins = 60)

summary(df$V1)

ar(df)

#-----------------------------------------
library(zoo)

df.ts <- ts(df$V1)
plot(df.ts)

reg.3 <- ar(df.ts, method = "mle")
summary(reg.3)
reg.3



reg.4 <- ar.ols(df.ts)
summary(reg.4)




reg.5 <- arima(df.ts)
summary(reg.5)


# ------------
df$V1CumSum <- cumsum(df$V1)
ggplot(
  df, 
  aes(x = x,
      y = V1CumSum)
) + 
  geom_line()

acf(df$V1, 50)
fit <- 
