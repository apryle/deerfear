library(dplyr)
# acc will have DeerIds that vid obs doesn't have
deerid <- c( "Deer1", "Deer1", "Deer1", 
             "Deer2", "Deer2", "Deer2", 
             "Deer3ACConly", "Deer3ACConly", "Deer3ACConly")
accx <- c( "d1x1", "d1x2", "d1x3", "d2x1", "d2x1", "d2x3", "d3x1", "d3x2", "d3x3")
accy <- c( "d1y1", "d1y2", "d1y3", "d2y1", "d2y1", "d2y3", "d3y1", "d3y2", "d3y3")
accz <- c( "d1z1", "d1z2", "d1z3", "d2z1", "d2z1", "d2z3", "d3z1", "d3z2", "d3z3")
myacc <- data.frame(deerid, accx, accy, accz)

# vid obs will have records that acc doesn't have
deerid2 <- c( "Deer1", "Deer2", "DeerVidOnly")
durf <- c( 6, 4, 0)
durg <- c( 0, 5, 5)
durt <- c( 4, 1, 4)
obs <- data.frame("deerid" = deerid2, durf, durg, durt)

# this only keeps deerids that are present in both dfs
# this repeats the vidobs "copies-down" for each repetition of deerid in acc
myjoin <- myacc %>% inner_join(obs, by="deerid")
myjoin




