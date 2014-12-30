# Test to establish a better function...
# Refer to http://stackoverflow.com/questions/21801111/r-for-each-row-calculate-a-sum-taking-values-of-one-of-the-columns-from-the-rows

dataCol1 <- c('A','A','B','C','B','A')
dataCol2 <- c(12, 23, 12, 45, 10, 6)
dataCol3 <- c(1, 0, 1, 1, 0, 0)
df <- data.frame(dataCol1, dataCol2, dataCol3)

df
#       dataCol1 dataCol2 dataCol3
# 1        A       12        1
# 2        A       23        0
# 3        B       12        1
# 4        C       45        1
# 5        B       10        0
# 6        A        6        0

ave(df$dataCol2, df$dataCol1, FUN=function(x) sum(x))
# [1] 41 41 22 45 22 41 # Provide the sum of df$dataCol2 for all 'A' each time 'A' is found
ave(df$dataCol2, df$dataCol3, FUN=function(x) sum(x))
# [1] 69 39 69 69 39 39 # Provide the sum of df$dataCol2 for all '0/1' each time '0/1' is found

cumsum(dataCol2)
# [1]  12  35  47  92 102 108 # In the wrong order; switch to a function reversing the order of a vector:
rev(dataCol2)
# [1]  6 10 45 12 23 12

head(dataCol2, -1)
# [1] 12 23 12 45 10 # Last one being removed, which is incorrect.  Need the first one to be removed.
head(rev(dataCol2), -1)
# [1]  6 10 45 12 23

# combining...
df$dataColTst <- ave(df$dataCol2, df$dataCol1, FUN=function(x) cumsum( c(0, head(x, -1) ) ) )
df # Error: cumsum is done in reversed order, which is incorrect.
#       dataCol1 dataCol2 dataCol3 dataColTst
# 1        A       12        1          0
# 2        A       23        0         12
# 3        B       12        1          0
# 4        C       45        1          0
# 5        B       10        0         12
# 6        A        6        0         35

df$dataColTst <- ave(df$dataCol2, df$dataCol1, FUN=function(x) rev(cumsum(c(0, head(rev(x), -1)))))
df # THIS ONE IS CORRECT!!!  I would probably need to undestand why the 2 'rev' are required though...??
#       dataCol1 dataCol2 dataCol3 dataColTst
# 1        A       12        1         29
# 2        A       23        0          6
# 3        B       12        1         10
# 4        C       45        1          0
# 5        B       10        0          0
# 6        A        6        0          0
