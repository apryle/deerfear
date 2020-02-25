################################

# all deerdates in a given behavior

vigonly <- purevidACC[purevidACC$behaviorcat == "VigilantOnly", ]
vigonly$deerID <- factor(vigonly$deerID)
vigonly$behaviorcat <- factor(vigonly$behaviorcat)
vigonly$DeerDate <- factor(vigonly$DeerDate)


head(vigonly)
nrow(vigonly)
numdeerdates <- length(levels(vigonly$DeerDate))
levels(vigonly$DeerDate)[4]

table(vigonly$deerID)
table(vigonly$DeerDate)

uniquedates <- levels(vigonly$DeerDate)

for(thisdeerdate in uniquedates) {
  print(thisdeerdate)
  
  thisdeerdate2 <- vigonly[vigonly$DeerDate == thisdeerdate, ]
  
  hereispsd <- mypsd(thisdeerdate2)
  
  print(head(hereispsd))
}


