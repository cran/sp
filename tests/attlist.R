set.seed(131)
x = list(a = 1:10, b = 10:1, c = letters[1:10], d = rep("xx", 10), 
	e = factor(1:10), f = rnorm(10), sample(10))
library(sp)
x = AttributeList(x)
x
dim(x)
nrow(x)
as.data.frame(x)
x[1]
x[1:3]
x[1:3,]
x[1:3, c("a", "d")]
x[, c("a", "d")]
x$a
x$a = 10:1
x[["a"]]
x[["a"]] = 1:10
names(x)
names(x) = letters[1:7]
names(x)
