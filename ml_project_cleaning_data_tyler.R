library(dplyr)

# read in whole training set
data = read.csv('train.csv', stringsAsFactors = F)

# Electrical => As-is

# X1stFlrSF => log
data = data %>% mutate(., LogFirstFlrSF = log(X1stFlrSF))

# X2ndFlrSF => log(+1)
data = data %>% mutate(., LogSecFlrSF = log(X2ndFlrSF + 1))

# LowQualFinSF As-is

# GrLivArea => log
data = data %>% mutate(., LogGrLivArea = log(GrLivArea))

# BsmtFullBath => As-is
# BsmtHalfBath => As-is
# FullBath => As-is
# HalfBath => As-is
# BedroomAbvGR => As-is
# KitchenAbvGr => As-is

# KitchenQual => ordinal ; Po = 0, Fa = 1, TA = 2, Gd = 3, Ex = 4
kitchen_quality_converter = function(x){
  if(x == 'Po'){
      return(0)
  } else if(x == 'Fa'){
      return(1)
  } else if(x == 'TA'){
      return(2)
  } else if(x == 'Gd'){
      return(3)
  } else if(x == 'Ex'){
      return(4)
  }
}
data$OrdKitchenQual = sapply(data$KitchenQual, kitchen_quality_converter)

# TotalRmsAbvGrd => As-is

# Functional Columns => Reducing the total number of groups; Min = Min1 | Min2, Maj = Maj1 | Maj2
# Remaining groups are: Typ, Min, Mod, Maj, Sev, Sal
functional_converter = function(x){
  if( x == 'Min1' | x == 'Min2'){
      return('Min')
  } else if( x == 'Maj1' | x == 'Maj2'){
      return('Maj')
  } else {
      return(x)
  }
}
data$GroupedFuncitonal = sapply(data$Functional, functional_converter)

# Fireplaces => As-is

# FireplaceQu => ordinal; NA = 0, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5
fireplaceQu_converter = function(x, na.rm = TRUE){
    if(is.na(x) == TRUE){
      return(0)
  } else if(x == 'Po'){
      return(1)
  } else if(x == 'Fa'){
      return(2)
  } else if(x == 'TA'){
      return(3)
  } else if(x == 'Gd'){
      return(4)
  } else if(x == 'Ex'){
      return(5)
  }
}
data$OrdFireplaceQu = sapply(data$FireplaceQu, fireplaceQu_converter)

# GarageType => Reducing the number of groups; 2Types, Attached = Attchd | Basment | BuiltIn, CarPort, Detchd, None = NA
garagetype_converter = function(x, na.rm = TRUE){
  if(is.na(x) == TRUE){
      return('No Garage')
  } else if(x == 'Attchd' | x == 'Basment' | x == 'BuiltIn'){
      return('Attached')
  } else{
      return(x)
  }
}

data$GroupedGarageType = sapply(data$GarageType, garagetype_converter)

# Adding new field: New_Field_1 = log(YrSold - GarageYrBuilt) | I don't know why we want this 
#### There are NA values here and I'm not sure the proper way of imputing them
data = data %>% mutate(., New_Field_1 = log((YrSold - GarageYrBlt)+1))

# GarageFinish => update NA to No Garage
garagena_converter = function(x){
  if(is.na(x) == TRUE){
      return('No Garage')
  } else{
      return(x)
  }
}
data$GarageFinish = sapply(data$GarageFinish, garagena_converter)
# GarageCars => update NA to No Garage
data$GarageCars = sapply(data$GarageCars, garagena_converter)

# GarageArea => log + 1
data = data %>% mutate(., LogGarageArea = log(GarageArea + 1))

# GarageQual => ordinal; NA = 0, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5
garagequal_converter = function(x){
  if(is.na(x) == TRUE){
      return(0)
  } else if(x == 'Po'){
      return(1)
  } else if(x == 'Fa'){
      return(2)
  } else if(x == 'TA'){
      return(3)
  } else if(x == 'Gd'){
      return(4)
  } else if(x == 'Ex'){
      return(5)
  }
}
data$OrdGarageQual = sapply(data$GarageQual, garagequal_converter)

# GarageCond => ordinal; NA = 0, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5
garagecond_converter = function(x){
  if(is.na(x) == TRUE){
    return(0)
  } else if(x == 'Po'){
    return(1)
  } else if(x == 'Fa'){
    return(2)
  } else if(x == 'TA'){
    return(3)
  } else if(x == 'Gd'){
    return(4)
  } else if(x == 'Ex'){
    return(5)
  }
}
data$OrdGarageCond = sapply(data$GarageCond, garagecond_converter)









