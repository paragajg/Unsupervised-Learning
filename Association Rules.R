# Load packages
library(arules) # used for plotting 
library(arulesViz) # used for plotting rules
#library(plotly) # used for plotting interactive rules visualization

# Read your data
df = read.csv("Online Retail.csv",sep = ",")

write.csv(df[,c("InvoiceNo","Description")],"transactions.csv")

# read.transsactions will import the data in the structure required by apriori / association rule mining algorithm

trans = read.transactions('transactions.csv',format = "single",sep = ",",cols = c("InvoiceNo","Description"),rm.duplicates = T)

rules = apriori(trans,parameter = list(minlen=2, supp=0.013, conf=0.98))
summary(rules)
## order rules by support
rules.sorted <- sort(rules, by="support")
inspect(rules.sorted)


## Removing redunct rules

subsetRules <- which(colSums(is.subset(rules.sorted, rules.sorted)) > 1)
## Subest rules
length(subsetRules)
rules.prunned <- rules.sorted[-subsetRules]
length(rules.prunned)

## which rules are redundant
#which(redundant)
## remove redundant rules
inspect(rules.prunned)

# Visualize Rules
plot(rules.prunned)
``
# Summarize the rules
head(quality(rules.prunned))

plot(rules.prunned, measure=c("support", "lift"), shading="confidence")

#sel <- plot(rules.prunned, measure=c("support", "lift"), shading="confidence", interactive=T)

subrules2 <- head(sort(rules.prunned, by="lift"),50)
p <- plotly_arules(subrules2)
p
