model <- 'hello'
#
typeof(model)

class(model)

#data types
real_num <- 100.7
class(real_num)
typeof(real_num)
int_num <- 100L
typeof(int_num)
a= TRUE
(10<20)&(10>20)
(10<20)|(10>20)
!(10>20)

vgsales <- read.csv('C:\\Users\\tele1\\OneDrive\\Desktop\\dataset\\vgsales.csv')
View(vgsales)
vgsales[vgsales$Year== 1999|vgsales$Publisher == 'Nintendo' ,]
model[[1]]
x<-10:30
x
typeof(x)
class(x)


mt <- mtcars

print(mtcars)

sprintf("%s perfect ", 'Ashley')