#create original data frame



#  Data
data <- data.frame( '1ABC' = rnorm(3),
            '8ABC' = rnorm(3),
            'ABC_2' = rnorm(3),
            'XYZ_1' = rnorm(3),
            'XYZ_2' = rnorm(3), check.names=FALSE )


mid <- stack(data[ grepl( "^[0-9]" , colnames( data )) ])
stacked<-cbind(data[, c("ABC_2", "XYZ_1")], stack(data[ grepl( "^[0-9]" , colnames( data )) ]))
print(mid)