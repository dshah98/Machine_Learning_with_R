# --------------------------------------------------- R Squared ---------------------------------------------- #

# SSres = SUM(Yi - Yi hat)^2
# where SSres is Sum of Square of Residuals.

# SStot = SUM(Yi - Yavg)^2 
# where SStot is Total Sum of Squares.

# - R Squared = 1 - (SSres / SStot)`
# - If SS residual is zero, so basically your trend line that your modeling goes through all your records then in 
# that case R-squared is equal to 1.
# - The closer the R-squared to 1 the better the further away it is from 1 so, the lower it is the worse.
# - R Squared can be negative as if SS residual fits your data worse than then your average line.

# ----------------------------------------------- Adjusted R Squared ----------------------------------------- #

# Adj R Squared = 1 - (1 - R Squared) [ (n - 1) / (n - p - 1) ] )
# where, p is number of regressors and n is sample size.

# - Adjusted R Squared has a penalization factor.
# - It penalizes you for adding independent variable that dont help your model.
# - When p increases, `(n - p -1)` decreases, and when this whole part decreases the denominator decreases the 
# ratio increases and as the ratio increases `(1 - R Squared) [ (n - 1) / (n - p - 1) ] )` increases as well.
# - As adding more regressor the adjusted r-squared is decreasing.
# - As R-squared increases `(1 - R Squared)` decreases so whole adjusted r-squared increases.