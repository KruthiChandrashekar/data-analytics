set.seed(42)
hc <- hclust(dist(USArrests), method = "complete")

ct <- cutree(hc, 3)
sapply(1:3, function(i) names(ct)[ct == i])

hc2 <- hclust(dist(scale(USArrests)), method = "complete")

ct <- cutree(hc, 3)
sapply(1:3, function(i) names(ct)[ct == i])


# Scaling results in different clusters and the choice of whether to scale or 
# not depends on the data in question. In this case, the variables are:
  
# - Murder    numeric  Murder arrests (per 100,000)  
# - Assault   numeric  Assault arrests (per 100,000) 
# - UrbanPop  numeric  Percent urban population      
# - Rape      numeric  Rape arrests (per 100,000)    

# These variables are not naturally on the same unit and the units involved are
# somewhat arbitrary (so for example, Murder could be measured per 1 million 
#                   rather than per 100,000) so in this case I would argue the data should be 
# scaled.
