facebook = read.csv("C:/Users/jcmis/Downloads/School/Stats and Prob/Linear Regression Model/UCI_Data_Sets/UCI_Data_Sets/Facebook-Metrics/dataset_Facebook.csv")
head(facebook)

#response will be reach.liked, or, Lifetime Post reach by
#people who like your Page

plot(facebook)
plot(facebook[, 14:19])


#possible predictors:
#likedengaged, comments, likes, shares, interactions



#-----One predictor Model -----
m1.likedengaged = lm(Reach.liked ~ likedengaged, data=facebook)
summary(m1.likedengaged)
#adjusted r^2 = .4196

m1.comments = lm(Reach.liked ~ comments, data=facebook)
summary(m1.comments)
#adjusted r^2 = 0.2216

m1.likes =  lm(Reach.liked ~ likes, data=facebook)
summary(m1.likes)
#adjusted r^2 = .3988

m1.interactions = lm(Reach.liked ~ interactions, data=facebook)
summary(m1.interactions)
#r^2 = 0.3806 

m1.shares = lm(Reach.liked ~ shares , data=facebook)
summary(m1.shares)
#r^2 = .2352


AIC(m1.likedengaged, m1.comments, m1.likes, m1.shares, m1.interactions)
#lowest AIC = m1.likes 10096.40


#likedengaged and likes have similar AIC, but we will go with the former
#because it has a higher r^2, likedengaged is defined as:
#People who have liked your Page and engaged with your post



plot( facebook$likedengaged, facebook$Reach.liked, xlab="likedengaged", ylab="Reach.liked", main = "Reach.liked vs likedengaged")
abline(m1.likedengaged, col="orange", lwd=2)

legend("topright", # where to draw the legend
       c("linear"), # line names
       lty=c(1),  # just leave this as 1 for each line you're drawing)
       col=c("orange") # colors of lines
)



#try adjusting
m1.likedengaged.sq = lm(Reach.liked ~ likedengaged + I(likedengaged^2), data=facebook)
summary(m1.likedengaged.sq)
#Adjusted R-squared:  0.507  

AIC(m1.likedengaged,m1.likedengaged.sq, m1.comments, m1.likes, m1.shares, m1.interactions)
#new AIC = 10016.91

x <- facebook$likedengaged
# getting lots of x values to draw the line through
xmesh <- seq(0.5*min(x), 2*max(x), by=0.1)
yhat <- predict(m1.likedengaged.sq , 
                newdata=data.frame(likedengaged=xmesh))


# lines(...) connects x and y vectors to each other sequentially
lines(xmesh, yhat, col="red") 
legend("topright", # where to draw the legend
       c("linear", "square"), # line names
       lty=c(1,1),  # just leave this as 1 for each line you're drawing)
       col=c("orange", "red") # colors of lines
)
summary(m1.likedengaged.sq)
plot(m1.likedengaged.sq)

m1 = m1.likedengaged.sq
#new AIC = 10016.91
#Adjusted R-squared:  0.507



#----Multipredictor Model

head(facebook)

m2.likedengaged.sq.comments = lm(Reach.liked ~ likedengaged + I(likedengaged^2) +
            comments, data=facebook)
summary(m2.likedengaged.sq.comments)
#Adjusted R-squared:  0.564


m2.likedengaged.sq.likes = lm(Reach.liked ~ likedengaged + I(likedengaged^2) +
             likes, data=facebook)
summary(m2.likedengaged.sq.likes)
#Adjusted R-squared:  0.6102 

m2.likedengaged.sq.likes.sq = lm(Reach.liked ~ likedengaged + I(likedengaged^2) +
                                likes + I(likes^2), data=facebook)
summary(m2.likedengaged.sq.likes.sq)
#Adjusted R-squared:  0.6192 


m2.likedengaged.sq.likes.sq.shares = lm(Reach.liked ~ likedengaged + I(likedengaged^2) +
        likes + I(likes^2) + shares, data=facebook)
summary(m2.likedengaged.sq.likes.sq.shares)
#Adjusted R-squared:  0.6365

m2.likedengaged.sq.likes.sq.shares.sq = lm(Reach.liked ~ likedengaged + I(likedengaged^2) +
        likes + I(likes^2) + shares + I(shares^2), data=facebook)
summary(m2.likedengaged.sq.likes.sq.shares.sq)
#Adjusted R-squared:  0.6536 

AIC(m2.likedengaged.sq.comments,
    m2.likedengaged.sq.likes,
    m2.likedengaged.sq.likes.sq,
    m2.likedengaged.sq.likes.sq.shares,
    m2.likedengaged.sq.likes.sq.shares.sq)
#lowest AIC = m2.likedengaged.sq.likes.sq.shares.sq = 9767.480




