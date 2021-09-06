#' ---
#' title: "ChemHairData Exploration"
#' author: "Shuyao Wang"
#' output: html_document
#' ---
#' 
#' # 1. Read the dataset.
#+ echo = FALSE 
x <- read.csv('ChemHairData_Nov2020.csv', stringsAsFactors = FALSE)
head(x)

#' # 2.Review the dataset.
#' ### For column I to P:
#' * Only continuous quantitative variables. 
#' * Represent the various proxy measures of melanins, 
#' * I would like to make them into the response variable.
#' * There are five different type of the chemical elements 
#' which includes H to L, and from M to P, they are the combination of elements 
#' from H to L.
#' Thus, I decide to find the relationship for the the variables from H to L first.
#' 
#' ### For column A:
#' * Discrete quantitative variables
#' * Represent different tested sample. 
#' * But they are disordered which seems to be shown in random ways.
#' * I guess they are just some examples which are picked through a large 
#' sample size.
#' 
#' ### For column D:
#' * Discrete quantitative variables
#' * Represent the number of ancestories each sample has.
#' * But it is surprised to see it include zero value in the column. 
#' * I guess maybe there is no record in the data. 
#'  
#' ### For column B, E, F:
#' * Categorical variables. 
#' * Only include  0 and 1 which represent yes or no
#' * They are Duplicate, Treated_mirco, Treated_survey
#' 
#' ### For column C: 
#' * Categorical variable 
#' which represent type of education samples had. 
#' 
#+ echo = FALSE
unique(x$Study)

#' * We can see that there are three types included in this variables. 
#' 
#' ### For column G: 
#' * Categorical variable 
#' * Represent the race of each samples. 
#+ echo = FALSE 
unique(x$Perceived_color)

#' * We can see that there are seven types included in this variables. 
#' 
#' ### For column K and N:
#' * They are quantitative variables. 
#' * However, it includes many values of NA.
#' * The other values in these two columns are much larger than zero.
#' * That's I plan to either giving up these two columns or using 
#' the average of the other value instead. 
#' * Then I am going to check for the variance of the other value to see whether 
#' we could use the average instead.
#' 
#+ echo = FALSE  
new1 <- na.omit(x$TTCA)
new2 <- na.omit(x$TTCA_PTCA)
var(new1)
var(new2)

#' * We get a variance of 594.8536 for TTCA and 1.08309 for TTCA_PTCA. 
#' * I think it is not fine to use the values. 
#' 
#' # 3. explore the data.
#' ### I. First, I would like to see the chemical elements response to the 
#' ### different perceived colors.
#' 
#+ echo = FALSE  
boxplot(x$A500 ~ x$Perceived_color, main = "Boxplot between colors and A500")
boxplot(x$A650 ~ x$Perceived_color, main = "Boxplot between colors and A650")
boxplot(x$PTCA ~ x$Perceived_color, main = "Boxplot between colors and PTCA")
boxplot(x$H_4AHP ~ x$Perceived_color, main = "Boxplot between colors and H_4AHP")

#' ### It is surprise to see the first three graph have similar trends which 
#' ### show that Black color seems to have outstanding highest A500, A650, 
#' ### PTCA. However, the difference is not very big especially for the first 
#' ### two graphs since the scales are small. However, red color tends to 
#' ### have more H_4AHP. But all the others has pretty small value of H_4AHP. 
#' ### The difference seems to be big. <br/>
#' 
#' ### II. Next, I would like to check for the influence on ancestry. <br/>
#' ### Firstly, I see some zero value in the column which I would like to remove 
#' ### since they're meaningless. <br/>
#'
#+ include = FALSE 
x[x$Ancestry == 0,]
x <- x[-c(87:88),]

#' ### Then I make plots to see the influence.
#' 
#+ echo = FALSE
plot(A500 ~ Ancestry, data = x) 

#' ### I don't see a outstanding pattern relationship between chemicals and 
#' ### ancestry. Thus, I would like to put them into boxplot. 
#' 
#+ echo = FALSE
boxplot(A500 ~ Ancestry, data = x) 
boxplot(A650 ~ Ancestry, data = x)
boxplot(PTCA ~ Ancestry, data = x)
boxplot(H_4AHP ~ Ancestry, data = x)

#' ### From the graph of A500, A650, PTCA, we can see that they show that most 
#' ### dots fall between 1 and 2 since the range of them are the largest. And 
#' ### for the ancestry of 1,3,6, they tend to have bigger value of chemicals. 
#' ### However, for the graph of H_4AHP, it is salient to see that the mode of 
#' ### the data falls on 2. It is also happy to see that the highest value and 
#' ### medium also falls onto it. <br/>
#' 
#' ### III. let's further explore whether there are some relationship between 
#' ### chemicals. 
#' 
#+ echo = FALSE
plot(A500 ~ A650, data = x)
plot(A500 ~ PTCA, data = x)
plot(A650 ~ PTCA, data = x)

#' ### We can see that those three graphs display a clear positive linear 
#' ### relationship. Thus, I expect to see a simple linear regression model for 
#' ### it. Thus, I put them into the format. 
#' 
#+ echo = FALSE
sl1 <- lm(A500 ~ A650, data = x)
sl2 <- lm(A500 ~ PTCA, data = x)
sl3 <- lm(A650 ~ PTCA, data = x)
summary(sl1)
summary(sl2)
summary(sl3)

#' ### From the result, we see that the p-value for the first and second model 
#' ### are very small and the adjusted R-squared are higher than 0.9. However, 
#' ### for the third model, we can see that the p-value for intercept is big, 
#' ### even though its adjusted R squared is large. Thus, it definitely needs 
#' ### further check. <br/>
#' 
#' ### Let's see the confidence interval.
#' 
#+ echo = FALSE
confint(sl1)
confint(sl2)
confint(sl3)

#' ### And from the confidence interval of three models, we don't see the value 
#' ### of zero for model 1 and 2 which also indicates that there is a 
#' ### relationship. However, for the model 3, the result is not very ideal. 
#' <br/>
#' 
#' ### Let's further check for the correlation test.
#' 
#+ echo = FALSE
cor.test(x$A650, x$A500)
cor.test(x$PTCA, x$A500)
cor.test(x$PTCA, x$A650)

#' ### However, from the result, we see that all three of these p-value for the 
#' ### test are very small. And the result of the correlation is big. Thay are 
#' ### all over 0.9 which indicates a strong linear relationship. <br/>
#' 
#' ### As a result, we can see that all of those three variables display a 
#' ### relationship between each other which indicates they have impact on each 
#' ### other. Moreover, from adjusted R^2 and correction value, we get the idea
#' ### that A500 and A650 have the most obvious relationships between each other. 
#' ### A500 and PTCA also shows a strong linear relationship with high adjusted 
#' ### R^2. And even though there are some outliers violate the linear 
#' ### relationship between A650 and PTCA, it still displays a relationship from 
#' ### the graph shown. 
#' 
#' # Conclusion
#' ### In conclusion, I first explore the relationship between of races and 
#' ### chemicals. From the graph, I find that people in Black have outstanding 
#' ### highest A500, A650, PTCA. People in red tend to have more H_4AHP and their 
#' ### difference of scales also seems to be big. Then I explore the relationship 
#' ### between ancestry and chemicals. From the data, I discover that most data 
#' ### of A500, A650, PTCA include 1 or 2 ancestry. And for the ancestry of 1,3,
#' ### 6, they tend to have bigger value of chemicals. However, for the graph of 
#' ### H_4AHP, most data includes the 2 ancestry. It is also happy to see that 
#' ### the highest value and medium also falls onto it. Lastly, I further explore 
#' ### the relationship between chemicals to see whether they influence each other. 
#' ### It is clear to see that A500, A650 and PTCA all show a relationship between 
#' ### each other. And A500, A650 influence each other the most. A500 and PTCA 
#' ### also display a very strong positive linear relationship to influence each 
#' ### other.
#'  

