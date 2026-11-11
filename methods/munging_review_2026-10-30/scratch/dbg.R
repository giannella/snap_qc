load("methods/munging_review_2026-10-30/scratch/pre7_env.RData")
mydata <- calculate_raw_benefits(mydata)
print(table(is.na(mydata$ELEMENT1))); print(table(mydata$STATUS)); print(head(sort(table(mydata$correctednotes),dec=TRUE)))
print(table(rb=(mydata$rawben_recreated == mydata$RAWBEN), useNA="a")); print(summary(mydata$rawhomeless_ded)); print(class(mydata$STATUS))
p1 <- is.na(mydata$ELEMENT1) & mydata$STATUS %in% 1; p2 <- (mydata$rawben_recreated == mydata$RAWBEN) %in% TRUE
p3 <- mydata$rawhomeless_ded == 0 & mydata$correctednotes == "no_change"; p4 <- mydata$RAWBEN > mydata$rawminimum_ben & mydata$RAWBEN < mydata$rawbenmax
print(c(sum(p1), sum(p1&p2), sum(p1&p2&p3), sum(p1&p2&p3&p4, na.rm=TRUE), sum(is.na(p4))))
