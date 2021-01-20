##01. CART: Gini index 계산 

## 성별
top.g <- 1-(5/10)^2-(5/10)^2
top.g
man.g <- 1-(5/6)^2-(1/6)^2
man.g
woman.g <- 1-(0/4)^2-(4/4)^2
woman.g
sex.g <- (6/10)*man.g+(4/10)*woman.g
sex.g

## 결혼
top.g <- 1-(5/10)^2-(5/10)^2
top.g
md.g <- 1-(2/5)^2-(3/5)^2
md.g
ms.g <- 1-(3/5)^2-(2/5)^2
ms.g
mrg.g <- (5/10)*md.g+(5/10)*ms.g
mrg.g
