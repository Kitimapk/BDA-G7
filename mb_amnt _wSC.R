# install.packages("arules")
# install.packages("arulesViz")

library(dplyr)
library(arules)
library(arulesViz)


# Read basket format data into a "transactions" object 
amntSC = read.transactions('amntSC.dat',
                           format="basket",sep=",")


stat = summary(amntSC)
stat@density       # Density of the dataset
stat@itemSummary   # Top 1-itemsets
stat@lengths       # Length distribution of baskets


# A plot of the length distributions of baskets
barplot(stat@lengths,
        xlab="no. of amenities in a basket",
        ylab="frequency",
        main="Distribution of basket size",
        ylim=c(0,1000))

# create an object for RHS
SCItems <- c("RT5","CL5")

# generate rules 
amntSC_rules = apriori(amntSC,
                       parameter = list(supp=0.4,conf=0.5,minlen=2, maxlen=96),
                       appearance = list(rhs= SCItems))

# take a look at a few rules
inspect(head(amntSC_rules))

# Be more specific with the life >1.2
highlift = subset(amntSC_rules,lift>1.2)
inspect(highlift)

highlift_rules_df = DATAFRAME(highlift)


# write csv file
write.csv(highlift_rules_df,file="HighliftRules.csv", row.names = FALSE)


# Interactive matrix visualization
# Rules with high lifts are located on the top-right hand corner
plot(highlift, method="matrix", engine="htmlwidget")

# This grouped matrix plot below made error for this transaction
# Interactive grouped matrix visualization
# Similar rules are grouped together for easier analysis
plot(highlift, method="grouped matrix", engine="htmlwidget")

# Graph visualization for showing relationship between antecedent, rule and consequent
plot(highlift, method="graph", engine="htmlwidget")

# Scatter plot (default)
plot(highlift, method="scatterplot", engine="htmlwidget")

# Scatter plot with custom settings
plot(amntSC_rules, method="scatterplot", measure=c("support","lift"),shading="confidence", engine="htmlwidget")
