#read data
joker_numbers<- read.csv(file="joker_number.csv")

#compute frequencies
frequencies <- as.numeric(table(joker_numbers))

#plot
barplot(frequencies,
        col = "deepskyblue4",
        main = "Plot of frequencies",
        xlab = "Joker Number",
        ylab = "Frequency",
        names.arg = seq(from = 1, to = 20, by = 1),
        cex.names = 0.5)
#chi test
chisq.test(frequencies)
