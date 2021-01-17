# 베이즈정리

install.packages("LaplacesDemon")
library(LaplacesDemon)

# pr(A=유방암, A'=건강)
# pr(B=positive, B'=negative)
# pr(A=유방암)=0.008
# pr(A'=건강)=0.992
# Pr(B|A) = 0.9
# Pr(B|A') = 0.07

PrA <- c(0.008, 0.992)
PrBA <- c(0.9, 0.07)
BayesTheorem(PrA, PrBA)


PrA <- c(50, 30, 20) / sum(50, 30, 20)
PrBA <- c(0.001, 0.005, 0.008)
BayesTheorem(PrA, PrBA)
