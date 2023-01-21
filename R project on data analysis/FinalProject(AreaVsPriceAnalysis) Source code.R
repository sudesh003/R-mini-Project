
#imported data set
data=read.csv("Bangalore.csv")
#removed uncertain data
data[data==9]=NA
data=na.omit(data)

#analysis of the dataset imported
datasetAnalysis<-function(){
#made factor of Location column to see frequency of each location
lf=factor(data$Location)
cat("summary of the imported dataset\n")
print(summary(lf))

#Removing unnecessary data
df=as.data.frame(summary(lf))
df[df<12]=NA
df=na.omit(df)

#bar plot of frequency of locations
name=rownames(df)
barplot(df$`summary(lf)`,names.arg = name,las=2,cex.names =0.5,col = "green",main = "frequency of different locations in a data set that has been imported.\n")
}


overall<-function(){
  #axis values in normal notation
  options(scipen=999)
  plot(x = data$Area, y = data$Price,
     xlab = "Area in sqft",
     ylab = "Price in INR",
     main = "Area vs Price ",las=1,col="blue",pch=1,cex=0.75,cex.axis=0.5
)
cat("Statistics:\n")
print(summary(data$Price))
print(summary(data$Area))
}

LocationWise<-function(){
  #we have selected only few locations which have greater frequencies
  cat("Enter the location:\n")
  repeat{
    cat("
     1.JP Nagar Phase 3
     2.Begur
     3.Electronic City Phase 1
     4.Bommasandra
     5.Electronic City Phase 2
     6.Horamavu
     7.Krishnarajapura
     8.Narayanapura on Hennur Main Road
     9.Anagalapura Near Hennur Main Road
     10.Devanahalli
     11.Anjanapura 
     12.Sahakar Nagar 
     13.Gottigere 
     14.Yelahanka 
     15.Kasavanahalli 
     16.Ramamurthy Nagar 
     17.Bellandur 
     18.JP Nagar Phase 4 
     19.Bommanahalli 
     20.Kadugodi 
     21.Koramangala 
     22.Jakkur 
     23.Hulimavu 
     24.Konanakunte 
     25.Singasandra 
     26.Banashankari 
     27.Carmelaram 
     28.Nagarbhavi 
     29.Nelamangala 
     30.Exit\n")
    choice=scan(nlines=1)
    val=switch(choice,"JP Nagar Phase 3",
               "Begur",
               "Electronic City Phase 1",
               "Bommasandra",
               "Electronic City Phase 2",
               "Horamavu",
               "Krishnarajapura",
               "Narayanapura on Hennur Main Road",
               "Anagalapura Near Hennur Main Road",
               "Devanahalli",
               "Anjanapura",
               "Sahakar Nagar",
               "Gottigere",
               "Yelahanka",
               "Kasavanahalli",
               "Ramamurthy Nagar",
               "Bellandur",
               "JP Nagar Phase 4",
               "Bommanahalli",
               "Kadugodi",
               "Koramangala", 
               "Jakkur",
               "Hulimavu",
               "Konanakunte",
               "Singasandra",
               "Banashankari",
               "Carmelaram",
               "Nagarbhavi",
               "Nelamangala",
               30
    )
    if(val==30){break}
    #selecting particular location
    new=subset(data,Location==val)
    #taking relation between price and area 
    rel=lm(Price~Area,data=new)
    #axis values in normal notation
    options(scipen=999)
    #drawing regression line
    plot(new$Area,new$Price,ylab="Price in INR",xlab="Area in sq ft"
         ,cex.axis=0.5,cex=0.75,main=val,las=1,pch = 24, col="blue", bg="red"
           ,abline(lm(new$Price~new$Area)))
    
    cat("Summary and Prediction:\n")
    cat("Area: \n")
    #printing the summary of area
    print(summary(new$Area))
    cat("\n")
    cat("Price: \n")
    #printing the summary of price
    print(summary(new$Price))
    
    #prediction
    while(TRUE){
    cat("\nEnter Area in sq ft: \n")
    area=scan(nlines=1)
    print(paste("Price in Rs: ",predict(rel,data.frame(Area=area))))
    cat("Do you want to predict again?(0/1)\n")
    ch2=scan(nlines = 1)
    if(ch2==0){break}
    }
    cat("Do you want to continue Areawise Analysis?(0/1)\n")
    y=scan(nlines=1)
    if(y==0){break}
    
  }
}

#main function
main=function(){
  
  while(TRUE){
    
  cat("\n~~~Analysis of Area vs Price of Apartments in Bengaluru Metropolitan Area~~~\n")
  cat("1.Analysis of entire Bengaluru\n2.Location wise Analysis\n3.Analysis of the imported data set\n4.Exit\n")        
  cat("Enter your choice\n")
  ch=scan(nlines = 1)
  if(ch==1){
      overall()
  }else if(ch==2){
      LocationWise()
  }else if(ch==3){
    datasetAnalysis();  
  }else if(ch==4){
      break;  
  }else{
      cat("Invalid choice\n")
  }
  }
  
}

#MAIN FUNCTION CALL
main()
