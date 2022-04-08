# data frame 2
df1 = data.frame(CustomerId = c(1:6), Product = c("Oven","Television","Mobile","WashingMachine","Lightings","Ipad"))
df1 
 
# data frame 2
df2 = data.frame(CustomerId = c(2, 4, 6, 7, 8), State = c("California","Newyork","Santiago","Texas","Indiana")) 
df2 


#### inner Join using merge function

df = merge(x=df1,y=df2,by="CustomerId")
df

###### outer join in R using merge() function
df = merge(x=df1,y=df2,by="CustomerId",all=TRUE)
df

#### left Join using merge function
df = merge(x=df1,y=df2,by="CustomerId",all.x=TRUE)
df

###### right join in R using merge() function 
df = merge(x=df1,y=df2,by="CustomerId",all.y=TRUE)
df


##### cross join in R
 
df = merge(x = df1, y = df2, by = NULL)
df

