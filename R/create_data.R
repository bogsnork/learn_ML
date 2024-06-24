#this script creates sample data for experimentation

#sample peat data ----

# this is an extremely simplified peat presence-absence model, that has lowland
# peat and blanket bogs. It is driven only by elevation and slope and 
# is completely deterministic (there is no error term). 
pres <- function(elev_i, slope_i){
  
  if(
    (elev_i < 100 & slope_i < 3) | #lowland peat
#    (elev_i < 100 + rnorm(1, 0, 20) & slope_i < 3 + rnorm(1, 0, 0.5)) | #lowland peat with an error
    (elev_i > 300 & slope_i < 7)) #blanket bog
    TRUE
  else(FALSE)
}

#these are the possible values of elevation and slope
elev_vals <- seq(0, 400, by = 1)
slope_vals <- seq(0, 10, by = 0.5)

#create predictor data
n_feat <- 1000 #number of features

(elev <- runif(n_feat, min(elev_vals), max(elev_vals)) )|> hist()
(slope <- runif(n_feat, min(slope_vals), max(slope_vals)) )|> hist()
pres_peat <- as.logical()
for (i in 1:n_feat) {
  pres_peat[i] <- pres(elev[i], slope[i])
} 
table(pres_peat)  

plot(slope, elev, col = as.numeric(pres_peat)+1, pch = 20)
