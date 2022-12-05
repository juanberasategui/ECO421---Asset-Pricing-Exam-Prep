---
title: "A"
output: html_document
---

```{r setup, include = FALSE}
knitr::opts_chunk$set(eval = FALSE)
```


In this document stand the formulas to calculate the main calculations in the Asset Pricing course

```{r}
setwd("/Users/juanberasateguigallego/Desktop/NHH Høst 2022/Asset Pricing/Prep")

```


1. INVESTOR PREFERENCES AND RISK AVERSION

EXAMPLES OF COMMON UTILITY FUNCTIONS & RISK AVERSION

This code defines four utility functions and their corresponding measures of absolute and relative risk aversion. A utility function is a mathematical function that describes an individual's preferences over different outcomes. It is used in decision making and economics to represent the utility or value that an individual derives from a given outcome.

The code defines four different types of utility functions: quadratic, exponential, power, and logarithmic. For each utility function, it defines a function u(x) that calculates the utility of a given outcome x. It also defines functions ARA(W) and RRA(W) that calculate the absolute and relative risk aversion for a given level of wealth W.

The quadratic utility function is defined as u(x) = (x-a)-b/2*(x-a)^2, where a and b are constants. The absolute and relative risk aversion measures for this utility function are defined as ARA(W) = b/(1-b*(W-a)) and RRA(W) = W/(1-W), respectively.

The exponential utility function is defined as u(x) = -1/Bexp(-bx), where B and b are constants. The absolute and relative risk aversion measures for this utility function are defined as ARA(W) = b and RRA(W) = bW, respectively.

The power utility function is defined as u(x) = 1/(1-b)*x^(1-b), where b is a constant. The absolute and relative risk aversion measures for this utility function are defined as ARA(W) = b/W and RRA(W) = b, respectively.

Finally, the logarithmic utility function is defined as u(x) = log(x). The absolute and relative risk aversion measures for this utility function are defined as ARA(W) = 1/W and RRA(W) = 1, respectively.

These definitions show how different utility functions can be used to model an individual's preferences over different outcomes, and how measures of absolute and relative risk aversion can be calculated from the utility function and the individual's level of wealth. These measures are useful for analyzing an individual's risk attitudes and decision making, and can be used in a variety of applications, such as portfolio optimization and risk management.

```{r}
#1. QUADRATIC UTILITY FUNCTION // ABSOLUTE RISK AVERSION // RELATIVE RISK AVERSION
u(x) = function (x) { (x-a)-b/2*(x-a)^2}; ARA(W) = function (W) { b/(1-b*(W-a))}; RRA(W) = function (W) {W/(1-W)};
#2. EXPONENTIAL UTILITY FUNCTION // ABSOLUTE RISK AVERSION // RELATIVE RISK AVERSION
u(x) = function (x) { -1/B*exp(-b*x)}; ARA(W) = function (W) {b}; RRA(W) = function (W) {bW};
#3. POWER UTILITY FUNCTION // ABSOLUTE RISK AVERSION // RELATIVE RISK AVERSION
u(x) = function (x) {1/(1-b)*x^(1-b)}; ARA(W) = function (W) {b/W}; RRA(W) = function (W) {b};
#4. LOGARITHMIC UTILITY FUNCTION // ABSOLUTE RISK AVERSION // RELATIVE RISK AVERSION
u(x) = function (x) {log(x)}; ARA(W) = function (W) {1/W}; RRA(W) = function (W) {1};
```
#PLOT ONE OF THESE UTILITY FUNCTIONS
```{r}
b = #number
curve(u(x), 1.0, 4.0, ylab = "Utility", xlab = "Consumption", col = "blue", lwd = 2)

```
CERTAINTY EQUIVALENT

This code defines a utility function and a certainty equivalent function, and uses them to calculate the certainty equivalent of a lottery. The utility function is defined as u(x) = -exp(-b*x)/b, where b is a constant representing an individual's risk measure. The certainty equivalent function is defined as CE(x) = u(xL)p + u(xH)(1-p) - u(w+x), where xL and xH are the lower and upper bounds of the lottery, p is the probability of winning the lottery, and w is the individual's current wealth.

The code then uses the uniroot function to find the root of the certainty equivalent function, which is the value of x that makes the certainty equivalent function equal to zero. This value represents the certainty equivalent of the lottery, which is the amount of money that the individual would need to receive with certainty in order to have the same utility as the lottery.

Once the certainty equivalent is calculated, the code calculates the expected utility of the lottery as the sum of the utilities of the possible outcomes weighted by their probabilities. It then plots the utility function and the expected utility of the lottery as a horizontal line, to visualize the relationship between the certainty equivalent and the expected utility of the lottery. This plot can help the individual to decide whether to accept the lottery or to prefer the certainty equivalent.
```{r}
#EXAMPLE OF CERTAINTY EQUIVALENT
w = 10; #wealth
b = 2; #individual risk measure
p = 0.1 #probability
xL = 5; #lower bound of lotery
xH = 25; #upper bound of lotery
u = function (x) {-exp(-b*x)/b } #utility function
CE = function(x) { u(xL)*p + u(xH)*(1-p) - u(w+x) } #certainty equivalent function

# Calculate the certainty equivalent
ce = uniroot(CE, c(xL, xH))$root

#Expected utility
EU = u(xL)*p + u(xH)*(1-p)

# Plot the CE function
curve(u(w+x), 5.5, 7, xlab = "x= Candidate P", ylab = "Utility")
abline(h = EU, col = "red", lwd = 2)
```

2.  MEAN-VARIANCE PORTFOLIO CHOICE

This code calculates the expected return and variance of a portfolio of assets. The FF3.csv file is loaded and the data is checked using the head function. The expected returns and risks of the assets are then calculated using the colMeans and var functions. The variance matrix is then checked to see if it is invertible, by calculating its determinant using the det function. If the determinant is zero, then the variance matrix is not invertible and cannot be used in the optimization problem.

Once the expected returns and risks of the assets are calculated and the variance matrix is verified to be invertible, the code sets the weights of the assets in the portfolio using a vector of constants. The expected return of the portfolio is then calculated by multiplying the weights vector by the expected returns vector using the %*% operator. The variance of the portfolio is then calculated by multiplying the weights vector by the variance matrix and the transpose of the weights vector, also using the %*% operator.

This code provides an example of how to calculate the expected return and variance of a portfolio of assets using the Markowitz framework. It demonstrates the use of the colMeans, var, and det functions to calculate the expected returns and risks of the assets, and the use of the %*% operator to calculate the expected return and variance of the portfolio. By using these calculations, it is possible to optimize the portfolio to maximize the expected return for a given level of risk, or to minimize the risk for a given level of expected return.
```{r}
#SETUP
FF3 = read.csv("FF3.csv", header = TRUE) #load data
head(FF3) #check data
m = colMeans(FF3[,2:4]); m#calculate mean of each asset
V = var(FF3[,2:4]);V #calculate variance of each asset & covariance between assets
det(V) #check if variance matrix is invertible  (if det(V) = 0, then V is not invertible)
phi = c(0.3, 0.2, 0.5) #set weights of each of the assets in the portfolio
phi %*% m #calculate expected return of the portfolio
t(phi) %*% V %*% phi #calculate variance of the portfolio
```
MARKOWITZ PORTFOLIO

This code calculates the optimal portfolio weights and variance of the portfolio using the Markowitz framework. The FF3.csv file is loaded and the expected returns and risks of the assets are calculated from the data. The phi function is then defined to calculate the optimal portfolio weights for a given expected return mu and the expected returns and risks of the assets represented by the vectors m and V, respectively. The phi function uses the Markowitz framework to solve the optimization problem and find the portfolio weights that maximize the expected return for a given level of risk, or minimize the risk for a given level of expected return.

Once the phi function is defined, it is called with the value of mu and the vectors m and V as inputs. This calculates the optimal portfolio weights and stores them in the phi vector. The code then calculates the variance of the portfolio using the formula 1/C + (mu-B/C)^2 * C/D, where C and D are constants calculated from the expected returns and risks of the assets, and mu and B are the expected return and the portfolio weights calculated by the phi function.

Finally, the code defines a function f that calculates the variance of the portfolio as a function of the expected return. This function is then plotted using the curve function, which shows the relationship between the expected return and variance of the portfolio. This plot, known as the efficient frontier, shows the set of optimal portfolios that offer the highest expected return for a given level of risk, or the lowest risk for a given level of expected return. It is a useful tool for visualizing the tradeoff between expected return and risk in a portfolio.
```{r}
FF3 = read.csv("FF3.csv", header = TRUE) #load data
m = colMeans(FF3[,2:4]); m#calculate mean of each asset
V = var(FF3[,2:4]);V #calculate variance of each asset & covariance between assets
mu = 0.5 #expected return

phi = function (mu,m,V) {
  i =c(rep(1, length(m))) #vector of ones

  A = c(t(m) %*% VI %*% m) #calculate A
  B = c(t(m) %*% VI %*% i) #calculate B
  C = c(t(i) %*% VI %*% i) #calculate C
  D = c(A*C - B^2) #calculate D
  a = VI %*% (A*i - B*m) / D #calculate a
  b = VI %*% (C*m - B*i) / D #calculate b

  return(a +b*mu)
}


phi(mu,m,V)

1/C + (mu-B/C)^{2}*C/D #calculate variance of the portfolio

#PLOT THE PORTFOLIO
f = function(x) {1/C + (x-B/C)^{2}*C/D}
curve(f(x), 0.05, 0.6, xlab = "Expected Return", ylab = "Variance")
#make same graph but with variance on the x-axis and expected return on the y-axis using ggplot

```
EXERCISE MARKOWITZ PORTFOLIO
```{r}
#SETUP
m = c(1.1, 1.2)
V = matrix(c(0.04, 0.0032, 0.0032, 0.16), nrow = 2)


#PART a)
phi(m[1],m,V) #calculate weights of each asset in the portfolio (Markowitz portfolio / Frontier Portfolio)
phi(m[2],m,V) #calculate weights of each asset in the portfolio (Markowitz portfolio / Frontier Portfolio)
#If weight is 1 then the asset is the portfolio, else it is not in the portfolio

#PART b)
phi(1.25,m,V); phi

#PART c)
(m[1]-1.25) / (m[2]-1.25)

#PART d)
library(ggplot2)
pf = ggplot(data = data.frame(x=0), aes(x=x)) + xlab("Expected return") + ylab("Variance")
pf + stat_function(fun=f, n=100) + xlim(0,2.25) + coord_flip() #coord_flip() flips the axes
```

THE OPTIMAL PORTFOLIO


This code plots the optimal portfolio using the Markowitz framework. It defines a function f(x) that calculates the optimal portfolio for a given level of risk x, using the formula sqrt((x-1/C)*D/C)+B/C, where C, B, and D are constants calculated from the expected returns and risks of the assets.

The code then plots the function f(x) using the curve function, which shows the relationship between the expected return and variance of the portfolio. This plot, known as the efficient frontier, shows the set of optimal portfolios that offer the highest expected return for a given level of risk, or the lowest risk for a given level of expected return.

The code also calculates the intersect of the tangent to the efficient frontier and the curve of certainty equivalents (CE), and the slope of the tangent at this point. It then plots the tangent using the abline function, to show the portfolio that maximizes the expected utility for a given level of risk aversion, as measured by the relative risk aversion RRA. This plot can be used to visualize the tradeoff between expected return and risk in a portfolio, and to understand the impact of risk aversion on the optimal portfolio.
```{r}
RRA= 0.01 #relative risk aversion
f = function (x) {sqrt((x-1/C)*D/C)+B/C} #function to calculate the optimal portfolio
curve(f(x), 0.0, 400, xlab = "V(R)", ylab = "E[R]")
xI = B/C+D/2/RRA/C - RRA/2/C #intersect of tangent & CE
xs = RRA/2 #slope of tangent
abline(xI, xs, col = "red", lwd = 2) #plot tangent
```
MARKOWITZ WITH CONSTRAINTS (SHORT SALES IN THIS EXAMPLE)


This code solves the Markowitz optimization problem with constraints using the quadprog library in R. The FF3.csv file is loaded and the expected returns and risks of the assets are calculated using the colMeans and var functions. A matrix A of constraints and a vector b0 of constraint values are then defined. These constraints specify that the sum of the portfolio weights must be equal to 1, the expected return of the portfolio must be at least 0.5, and that short selling is not allowed, which means that the weights of the assets in the portfolio must be non-negative.

The optimization problem is then solved using the solve.QP function, which uses the quadratic programming algorithm to find the portfolio weights that maximize the expected return for a given level of risk, subject to the constraints specified in A and b0. The solution to the optimization problem is stored in the phi vector, which contains the optimal weights of the assets in the portfolio.

This code demonstrates how to use the quadprog library and the solve.QP function to solve the Markowitz optimization problem with constraints. It shows how to define the constraints and how to use the solve.QP function to find the optimal portfolio weights that maximize the expected return for a given level of risk, subject to the constraints. This approach can be used to solve a variety of optimization problems in finance and other fields, where the objective is to maximize or minimize a function subject to constraints on the variables.
```{r}
library(quadprog)
FF3 = read.csv("FF3.csv", header = TRUE) #load data
m = colMeans(FF3[,2:4]); #calculate mean of each asset
A = t(rbind(1, m, diag(1, length(m)))) #matrix with constraints
b0 = c(1, 0.5, rep(0, length(m))) #vector with constraints
res <- solve.QP(Dmat = V, dvec = m, Amat = A, bvec = b0, meq = 2) #solve quadratic programming problem
#syntax of solve.QR ->solve.QP(Covariance matrix (V);d our contraint; A; b0 = list constraint values; meq = number of equality constraints)
phi = res$solution; phi #weights of the portfolio

```
MARKOWITZ SOLUTION & BLACK-LITTERMAN

This code is implementing the Black-Litterman model, a variant of the Markowitz model that allows investors to incorporate their own beliefs about the expected returns of different assets into the portfolio optimization process.

The first few lines of code load a data set, in this case a file called "FF3.csv", and calculate the variance-covariance matrix of the data. The variance-covariance matrix is a matrix that shows the relationship between the returns of different assets. It is an important input in portfolio optimization, as it allows us to calculate the risk and return of different portfolios.

Next, the code calculates the inverse of the variance-covariance matrix, which is used in the calculation of the optimal portfolio weights. The code then sets hypothetical market weights for the assets in the portfolio, and calculates the expected return of the portfolio using these weights.

The code then sets the "pick matrix" and the investor's beliefs about the expected returns of the assets in the portfolio. The pick matrix is a matrix that specifies which assets the investor has views on, and the direction of those views (positive or negative). The investor's beliefs are the expected returns of the assets that they have views on.

Next, the code calculates the uncertainty in the investor's beliefs, using the pick matrix, the variance-covariance matrix, and the confidence in the beliefs. This uncertainty is then used to compute the solution parameters, expected returns, and covariances that weight the private and market views in the optimization process.

Finally, the code calculates the optimal portfolio weights for a given required return using the phi() function, which takes the required return, the expected return, and the covariance matrix as inputs. These weights represent the optimal allocation of capital across the assets in the portfolio, given the investor's beliefs and the market data.

```{r}
FF3 = read.csv("FF3.csv", header = TRUE) #load data
V = var(FF3[,2:4]);V
VI = solve(V); VI #inverse of the variance matrix

phiM = c(0.2,0.5,0.3) #Hypothetical market weights
lamda = var(FF3[,2])/mean(FF3[,2]) #calculate lambda
m = V %*% phiM /lamda

tau = 1/(2017-1925)
Vm = tau*V
VmI = solve(Vm)



#IMPLEMENTING BELIEFS

#Enter pick matrix and beliefs
P = matrix(c(1,0,0,1,0,-1),2, 3) #pick matrix
mp = c(1.1*m[1], -1.0*(m[2]-m[3])) #beliefs

#Compute uncertainty in beliefs
c= 16 #confidence in beliefs
Vp = P %*% V %*% t(p)/c #uncertainty in beliefs
VpI = solve(Vp)

#Compute solution parameters, expected returns and covariances that weight private and market views
W = solve(VmI + t(P) %*% VpI %*% P) #weight matrix
mBL = W %*% (VmI %*% m + t(P) %*% VpI %*% mp) #expected return
VBL = V + W #covariance matrix

mM = 0.2 #require return

phi(mM,mBL,VBL) #calculate weights of the portfolio
```

4. CAPM
```{r}
# Exercise 1

R0 = 1.05; I = 30
RM = c(0.9, 1.1, 1.3); X = c(30,70,80)
muM = mean(RM)
EX = mean(X)
RX = X/I    # This is an approximation as 'I' is not a present value!

# Project beta and required rate of return
betaRX = cov(RM, RX) / var(RM); betaRX
muX = R0 + betaRX*(muM-R0); muX

# Present value estimate
EX/muX

# If 'I' < 'true PV' then we have overstated project returns RX, which causes
# an upward bias in betaRX. The result is an underestimate of true project
# present value


# Exercise 2

# Cash flow beta
betaX = cov(RM,X)/var(RM); betaX

# Present value estimate
(EX - betaX*(muM-R0))/R0 #This is the CE

```



5.ARROW - DEBREU SECURITIES

In the Arrow-Debreu model, securities are financial instruments that provide a known payoff in each possible state of the world. The payoffs of these securities are represented in a payoff matrix, with columns representing the securities and rows representing the different states of the world.

The code provided shows the creation of two payoff matrices, called X1 and X2. These matrices show the payoffs of two securities in two different states of the world. For example, the payoffs of security 2 in the first payoff matrix (X1) are 3 and 0 in states 1 and 2, respectively.

The code also creates an identity matrix, which represents pure securities. A pure security is a security that provides a known payoff in only one state of the world. For example, the possible payoffs of pure security 2 in states 1 and 2 would be 0 and 1, respectively.

Overall, the Arrow-Debreu model and the code provided are used to represent the payoffs of securities in different states of the world, and to calculate the payoffs of pure securities.
```{r}
#TWO ALTERNATIVE PAYOFF MATRICES
#columns are securities and and rows are states of the world
X1 = matrix(c(-1,3,2,0), nrow = 2, ncol = 2); X1 #payoff matrix 1
X2 = matrix(c(-1,3,-2,6), nrow = 2, ncol = 2); X2 #payoff matrix 2
#Possible payoffs from security 2 in example 1, in states 1 and 2
X1[,2]


#PURE SECURITIES
I = diag(nrow=2) #identity matrix & pure securities (2 pure securities and 2 states of the world)
#Possible payoffs from pure security 2 in states 1 and 2
I[,2]

```
COMPLETE MARKETS

In a complete market, there is a security for every possible payoff, so that any desired payoff can be obtained by trading a combination of these securities. The code provided appears to be implementing this concept by calculating the amount of each pure security that would be needed to obtain a given payoff (called theta), and then using this information to calculate the price of the pure securities in the market.

The code also shows the opposite process, where the prices of the securities in the market are used to calculate the prices of the pure securities. This is done by first calculating the payoffs of the securities in the market using the payoff matrix, and then using the prices of the pure securities to calculate the prices of the securities in the market.

Overall, the code is demonstrating the concept of complete markets, where there is a security for every possible payoff and the prices of the securities can be calculated using the prices of the pure securities.
```{r}
z = c(5,2) #payoffs
I_inv = solve(I) #inverse of the identity matrix
theta = I_inv %*% z #calculate theta
#theta is the amount of each pure security that we need to buy to get the payoff z

X1_inv = solve(X1) #inverse of payoff matrix 1
theta = X1_inv %*% z #calculate theta

P = c(1.5,1) #price of the securities in the market
i_1= c(1,0) #pure security 1
i_2= c(0,1) #pure security 2
theta = X1_inv %*% i_1#calculate theta
pi_1 = as.numeric(theta) %*% P #calculate price of pure security 1
#Now do the same for pure security 2
theta = X1_inv %*% i_2#calculate theta & this is the portfolio of market securities that replicates i_2!
pi_2 = as.numeric(theta) %*% P #calculate price of pure security 2


#NOW WE DO THE OPPOSITE, CALCULATE THE PRICE OF THE SECURITIES IN THE MARKET WITH THE PURE SECURITIES
psp = c(pi_1, pi_2) #Pure security prices

#Payouts and psp imply market prices
t(X1) %*% psp #market prices of securities in X1

#Market prices and payoff imply psp
t(X1_inv) %*% P #market prices of securities in X1

```
DETERMINE IF THE MARKET IS COMPLETE


In order for a market to be considered complete, there must be a security for every possible payoff. This means that the number of independent payoffs (the rank of the payoff matrix) must be equal to the number of states of the world.

The code provided uses the rankMatrix() function from the Matrix package to calculate the rank of the payoff matrices X1 and X2. The rank of a matrix is the number of independent rows or columns in the matrix. If the rank of the payoff matrix is equal to the number of states of the world, then the market is complete.

In this case, the code shows that the market represented by X1 is complete, as the rank of X1 is equal to the number of states of the world. However, the market represented by X2 is not complete, as the rank of X2 is less than the number of states of the world.

Overall, the code is using the rank of the payoff matrix to determine if a market is complete. In a complete market, there is a security for every possible payoff, so that any desired payoff can be obtained by trading a combination of these securities.


```{r}
library(Matrix)

#X1 is complete
rankMatrix(X1)[1] #get # of independent payoffs

#X2 is not complete
rankMatrix(X2)[1] #get # of independent payoffs

```
ARBITRAGE IN ARROW DEBREU MARKETS

In the Arrow-Debreu model, the concept of arbitrage refers to the possibility of making a risk-free profit by taking advantage of differences in the prices of securities. In the code below, the matrix X1_inv is a matrix of coefficients that represents the relationship between the market prices of the securities and their "pure" prices, which reflect the intrinsic value of the securities.

In the first example, the market prices P are (-0.15, 1.5), and when these prices are multiplied by the inverse of X1, the result is a set of pure security prices that do not contain any arbitrage opportunities. This means that it is not possible to make a risk-free profit by trading the securities based on their market prices.

In the second example, the market prices P are (-1.35, 1.5), and when these prices are multiplied by the inverse of X1, the result is a set of pure security prices that contain arbitrage opportunities. This means that it is possible to make a risk-free profit by trading the securities based on their market prices.

Overall, the presence of arbitrage opportunities in an Arrow-Debreu market indicates that the market prices do not accurately reflect the intrinsic value of the securities, and that it is possible to make risk-free profits by trading the securities.
```{r}
P = c(-0.15, 1.5) #Market prices
t(X1_inv) %*% P #pure security prices // No arbitrage

P = c(-1.35, 1.5) #Market prices
t(X1_inv) %*% P #pure security prices // Arbitrage
```
BINOMIAL OPTION PRICING

The binomial option pricing model is a mathematical model used to determine the price of an option, which is a financial derivative contract that gives the holder the right to buy or sell an underlying asset at a specified price on or before a certain date. In the code below, the X matrix represents the payoff matrix of the option, which specifies the possible payoffs of the option at different points in time. The P vector represents the market prices of the underlying securities, and the R0 variable represents the risk-free interest rate.

To calculate the price of the option, the code first uses the market prices to calculate the parameters u, d, and q, which represent the probabilities of the option's price increasing, decreasing, and remaining unchanged, respectively. The code then calculates the payoff of the option at the end of its term, using the k variable to represent the strike price of the option. Finally, the code calculates the price of the option by multiplying the option's payoff by the probabilities of its possible outcomes and dividing the result by the risk-free interest rate.

Alternatively, the code can also use the concept of pure security prices to calculate the price of the option. In this case, the code first calculates the pure security prices by taking the inverse of the payoff matrix and multiplying it by the market prices. The code then calculates the price of the option by multiplying the option's payoff by the pure security prices.

Overall, the binomial option pricing model is a useful tool for determining the fair price of an option, taking into account the market prices of the underlying securities and the option's characteristics.
```{r}
#CALCULATE PRICE OF AN OPTION
X = matrix(c(1,1,0.5,4), nrow = 2, ncol = 2); X #payoff matrix
P = c(0.8,1.45 ) #market prices
R0 = 1/P[1]; u= X[1,2]/P[2]; d = X[2,2]/P[2]; q = (R0-d)/(u-d) #calculate parameters
k=2.5 #strike price
x3 = pmax(k-X[,2],0) #payoff of the option
P3 = (q*x3[1] + (1-q)*x3[2])/R0; P3 #price of the option

#NOW USING PURE SECURITIES PRICING
X_inv = solve(X) #inverse of payoff matrix
pure = t(X_inv) %*% P #pure security prices
t(x3) %*% pure #price of the option

```
IGNORE THE FOLLOWING CODE
```{r}
library(rmarkdown)
render("asset pricing exam prep 1.rmd")
```




