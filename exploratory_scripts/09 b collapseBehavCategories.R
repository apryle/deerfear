
table(widePCA4$behaviorcat)
widePCA4$collapsedBehaviors <- "placeholder"

# map collapsed categories
widePCA4[widePCA4$behaviorcat == "BeddedGeneral", 
         c("collapsedBehaviors")] <- "Bedded"

widePCA4[widePCA4$behaviorcat == "BeddedHeadUpNOTRum", 
         c("collapsedBehaviors")] <- "Bedded"

widePCA4[widePCA4$behaviorcat == "BeddedRuminateKnown", 
         c("collapsedBehaviors")] <- "Bedded"

widePCA4[widePCA4$behaviorcat == "ForageOnly", 
         c("collapsedBehaviors")] <- "ForageOnly"

widePCA4[widePCA4$behaviorcat == "ForageTravel", 
         c("collapsedBehaviors")] <- "RunOrWalk"

widePCA4[widePCA4$behaviorcat == "Grooming", 
         c("collapsedBehaviors")] <- "Grooming"

widePCA4[widePCA4$behaviorcat == "RunOnly", 
         c("collapsedBehaviors")] <- "RunOrWalk"

widePCA4[widePCA4$behaviorcat == "VigilantOnly", 
         c("collapsedBehaviors")] <- "VigilantOnly"

widePCA4[widePCA4$behaviorcat == "WalkOnly", 
         c("collapsedBehaviors")] <- "RunOrWalk"

withoutGrooming <- widePCA4[!(widePCA4$behaviorcat == "Grooming"), ] 
table(withoutGrooming$collapsedBehaviors)
widePCA4 <- withoutGrooming

table(widePCA4$collapsedBehaviors)

