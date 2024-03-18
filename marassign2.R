## installing and importing packages

install.packages("conjoint")
install.packages("data.table")
library(conjoint)
library(data.table)


set.seed(14)

### defining attribute levels 

attrib.level <- list(brand = c("Apple", "Lenovo", "Dell", "Acer"),
                     Harddrive = c("128", "256", "512"),
                     RAM = c("2", "4", "8","16"),
                     Screensize = c("12.1", "15.4", "17.3"),
                     Price = c("900", "1200", "1500","2000"))


## creating product profiles

design <- read.csv(file.choose())
design
### calculating the correlation between attributes to check if correlations are satisfactory or not
print(cor(caEncodedDesign(design)))

caEncodedDesign(design)


### Importing customer rankings
pref <- read.csv(file.choose())

### creating a column vector containing all attribute levels
attrib.vector <- data.frame(unlist(attrib.level,use.names=FALSE))
colnames(attrib.vector) <- c("levels")
part.worths <- NULL

for (i in 1:ncol(pref)){
  temp <- caPartUtilities(pref[,i], design, attrib.vector)
  ## Pick the baseline case
  Base_Brand <- temp[,"Apple"]; Base_Hard <- temp[,"128"]; Base_RAM <- temp[,"2"];
  Base_Screen <- temp[,"12.1"]; Base_Price <- temp[,"900"]
  ## Adjust Intercept
  temp[,"intercept"] <- temp[,"intercept"] - Base_Brand - Base_Hard - Base_RAM -
    Base_Screen - Base_Price
  ## Adjust Coefficients
  ## Brand
  L1 <- length(attrib.level$brand) + 1 ## Add 1 for the intercept
  for (j in 2:L1){temp[,j] <- temp[,j] - Base_Brand}
  ## Harddrive
  L2 <- length(attrib.level$Harddrive) + L1
  for (k in (L1+1):L2){temp[,k] <- temp[,k] - Base_Hard}
  ## RAM
  L3 <- length(attrib.level$RAM) + L2
  for (l in (L2+1):L3){temp[,l] <- temp[,l] - Base_RAM}
  ## Screensize
  L4 <- length(attrib.level$Screensize) + L3
  for (m in (L3+1):L4){temp[,m] <- temp[,m] - Base_Screen}
  ## Price
  L5 <- length(attrib.level$Price) + L4
  for (n in (L4+1):L5){temp[,n] <- temp[,n] - Base_Price}
  part.worths <- rbind(part.worths, temp)
}

rownames(part.worths) <- colnames(pref)

##Export part-worths from analysis
write.csv(part.worths, file.choose(new=TRUE), row.names = FALSE)

summary(part_worths)
part_worths

## Principal Component Analysis and Perceptual Maps

## Load Packages and Set Seed
library(data.table)
library(fastDummies)
set.seed(1)


###Read in perception data – 8 product profiles from sheet 2(Perceptions)
per <- read.csv(file.choose()) 

# Create dummy variable
per <- dummy_cols(per)
per

## Run Principle Components Analysis on Perceptions
pca <- prcomp(per[,6:length(per)], retx=TRUE, scale=TRUE)
pca
names(pca)

pca$sdev

### Loadings 

pca$rotation

## Perceptual Map Data - Attribute Factors and CSV File
attribute <- as.data.table(colnames(per[,2:length(per)])); setnames(attribute, 1, "Attribute")

## calculating factors

factor1 <- pca$rotation[,1]*pca$sdev[1]; 
factor2 <- pca$rotation[,2]*pca$sdev[2]; 

## creating a path

path <- rep(1, nrow(attribute))


## extracting factors
pca_factors <- subset(cbind(attribute, factor1, factor2, path), select = c(Attribute, factor1, factor2, path))

pca_origin <- cbind(attribute, factor1 = rep(0,nrow(attribute)), factor2 = rep(0,nrow(attribute)), path = rep(0,nrow(attribute)))

pca_attributes <- rbind(pca_factors, pca_origin)

write.csv(pca_attributes, file = file.choose(new=TRUE), row.names = FALSE) ## Name file perceptions_attributes.csv

## Perceptual Map Data - Brand Factors and CSV File

score1 <- (pca$x[,1]/apply(abs(pca$x),2,max)[1])
score2 <- (pca$x[,2]/apply(abs(pca$x),2,max)[2])
pca_scores <- subset(cbind(per, score1, score2), select = c(Profile, score1, score2))
write.csv(pca_scores, file = file.choose(new=TRUE), row.names = FALSE) ## Name file perceptions_scores.csv

## Calculating singular values
cx <- sweep(per[,6:length(per)], 2, colMeans(per[,6:length(per)]), "-")
sv <- svd(cx)
names(sv)

## singular value decomposition
sv$d
sv$u
sv$v

##########Calculating PVE###########

###https://rpubs.com/cbolch/531355

# Create a function that creates a new data frame with centered variables
center_apply <- function(x) {
  apply(x, 2, function(y) y - mean(y))
}

##https://rpubs.com/cbolch/531355
# Apply the function
data_centered <- center_apply(per[,6:length(per)])

###Calculate the covariance matrix
data_centered_cov <- cov(data_centered)


# Calculate the eigenvalues of the matrix
data_centered_eigen <- eigen(data_centered_cov)

##https://rpubs.com/cbolch/531355
# Structure of the object contains the ordered eigenvalues and the corresponding eigenvector matrix
str(data_centered_eigen)

##https://rpubs.com/cbolch/531355
###Selecting the Number of Principal Components
##Proportion of variance explained (PVE)

PVE <- data_centered_eigen$values / sum(data_centered_eigen$values)

round(PVE, 2)