vech <- function (x) {
    x <- as.matrix(x)
    if (dim(x)[1] != dim(x)[2]) {
        stop("Non-square matrix passed to vech().\n")
    }
    output <- x[lower.tri(x, diag = TRUE)]
    dim(output) <- NULL
    return(output)
}

xpnd <- function (x, nrow = NULL) {
    dim(x) <- NULL
    if (is.null(nrow)) 
        nrow <- (-1 + sqrt(1 + 8 * length(x)))/2
    output <- matrix(0, nrow, nrow)
    output[lower.tri(output, diag = TRUE)] <- x
    hold <- output
    hold[upper.tri(hold, diag = TRUE)] <- 0
    output <- output + t(hold)
    return(output)
}

p <- 4  
th <- c(0.7, 0.7, 0.5, 0.51, 0.51, 0.51, 0.51)
delta <- 0.5 
Ftype <- "OLS" #"OLS" or "ML"

model <- function(th) {
	lam <- cbind(c(th[1], th[1], 0, 0), c(0, 0, th[2], th[2]))
	phi <- cbind(c(1, th[3]), c(th[3], 1))
	psi <- diag(th[4:7])
	sig <- lam %*% phi %*% t(lam) + psi
	return(sig)
}

makederiv <- function(th) {
	library(lavaan)
	sig <- model(th)
	dv <- list()
	for(np in 1:length(th)) {
		h <- th[np]
		eps <- 0.000000001 * h
		th[np] <- h + eps
		dv[[np]] <- vech( (model(th) - sig) / eps)
		th[np] <- h
	}
	dv <- do.call(cbind, dv)
	return(dv)
}

sig <- model(th)
sigiv <- solve(sig)
pstar <- p * (p + 1) / 2
M <- 2 * matrix(1, p, p) - diag(p)
Dk <- diag(vech(M))
deriv <- makederiv(th)
W <- sigiv
if(Ftype == "OLS") W <- diag(nrow(sig))

B <- list()
for(np in 1:length(th)) {
	B[[np]] <- -Dk %*% vech(W %*% xpnd(deriv[,np]) %*% W)
}
B <- do.call(cbind, B)

y <- runif(pstar)

v <- coef(lm(y ~ B - 1))

etilde <- y - (B %*% as.matrix(v)) 
Etil <- xpnd(etilde)

sigstar <- sig + Etil; 

G <- W %*% Etil
ID <- diag(nrow(G))
kappa <- sqrt(2 * delta / sum(diag(G %*% G)))

dif <- kappa * sum(diag(G)) - log(det(ID + kappa * G))

#######################################################

for(loop in 1:15) {
	tk <- dif - delta
	tkprime <- sum(diag(G)) - sum(diag(solve(ID + kappa * G) %*% G))
	kappan <- kappa - (tk / tkprime)
	dif <- kappan * sum(diag(G)) - log(det(ID + kappan * G))
	kappa <- kappan
	if(tk < 1e-5) break
}

sigstar <- sig + kappa * Etil

print(kappa * Etil)
print(sigstar)
print(det(sigstar))

if (Ftype == "ML") {;
  Fml = log(det(sig)) - log(det(sigstar)) + sum(diag(sigstar%*%sigiv)) - p;
  print(Fml)
} else if (Ftype == "OLS") {
   D <- sigstar - sig
   Fols <- sum(diag( D %*% D ))/2
   print(Fols)
}
 