## C4.5: Entropy and Information Gain 

## 성별
top.e <- -(5/10)*log2(5/10)-(5/10)*log2(5/10)
top.e
man.e <- -(5/6)*log2(5/6)-(1/6)*log2(1/6)
man.e
woman.e <- -(4/4)*log2(4/4)
woman.e
sex.e <- (6/10)*man.e+(4/10)*woman.e
sex.ig <- top.e-sex.e
sex.ig

## 결혼
top.e <- -(5/10)*log2(5/10)-(5/10)*log2(5/10)
top.e
md.e <- -(2/5)*log2(2/5)-(3/5)*log2(3/5)
md.e
ms.e <- -(3/5)*log2(3/5)-(2/5)*log2(2/5)
ms.e
mrg.e <- (5/10)*md.e+(5/10)*ms.e
mrg.e
mrg.ig <- top.e-mrg.e
mrg.ig
