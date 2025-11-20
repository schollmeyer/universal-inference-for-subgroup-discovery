E1 <- Lattice$extents[1000,]
E2 <- Lattice$extents[2000,]
E3 <- Lattice$extents[3000,]
E4 <- Lattice$extents[4000,]

N <- ncol(Lattice$extents)
set.seed(1234567)
y1 <- sample(c(0,1), p=c(0.1,0.9),size = N,replace=TRUE)
y2 <- sample(c(0,1), p=c(0.2,.8),size = N,replace=TRUE)
y3 <- sample(c(0,1), p=c(0.3,0.7),size = N,replace=TRUE)
y4 <- sample(c(0,1), p=c(0.4,0.6),size = N,replace=TRUE)

y <- sample(c(0,1),p=c(0.05,0.95),size=N, replace=TRUE)
y[which(E1==1)] <- y1[which(E1==1)]
y[which(E2==1)] <- y2[which(E2==1)]
y[which(E3==1)] <- y3[which(E3==1)]
y[which(E4==1)] <- y4[which(E4==1)]

obj <- oofos::compute_objective(data.frame(y=y),"y","1")
table(obj)

model <- oofos::optimize_on_context_extents(CT1,objective=obj)
res1 <- gurobi::gurobi(model)
