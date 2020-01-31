fs <- 10 # make this global

mypsd <- function(obs){

  myx <- obs$accx
  myy <- obs$accy
  myz <- obs$accz
  
  N <- length(myz)
  
  power.x <- (1/N)*abs(fft(myx)^2)
  power.y <- (1/N)*abs(fft(myy)^2)
  power.z <- (1/N)*abs(fft(myz)^2)
  
  df <- fs/N
  fmax <- fs/2
  f    <- seq(0, fmax-df, by=df)
  print(f)
  
  mypower <- as.data.frame(cbind(f, power.x[1:length(f)], 
                                 power.y[1:length(f)], 
                                 power.z[1:length(f)]))

  return(mypower)
    }


# For one deer at a time

head(purevidACC2)
levels(purevidACC$behaviorcat)
runningDeer <- purevidACC[purevidACC$behaviorcat == "RunOnly", ]

runningDeer # view it and choose one DeerDate

myDeerDateDate <- "NW1MF11_20140129_080336"

OneDeerDate <- purevidACC[purevidACC$DeerDate == myDeerDateDate, ]
nrow(OneDeerDate) # make sure this is 101

mypsd(OneDeerDate)

plot()

################################

# all deerdates in a given behavior

oneBehav <- purevidACC[purevidACC$behaviorcat == "RunOnly", ]
head(oneBehav)

oneBehav %>%
  group_by(DeerDate) %>% 
  mypsd()

