# Restructing RungeKutta
 ebolaModel <- function(b1,ca,hcf, bp,ebolaBed,deathRate,S0,E0,I10,I20,H0,R0,D0,B0,migration,ncoun){
  # b = infection rate, ca = case ascertainment , ebolaBed = hospital 
  # capacity and hcf -case fatatality
  #print(ebolaBed)
  #ncoun is the number of country
  b0 = 0.1
  b2 = 0.02
  b3 = 0.2
  lat1 = 0.047
  preDet = 0.25
  postDet = 0.16
  caseFat = 0.7
  m = migration
  #total population
  N0 = S0+E0+I10+I20+H0+R0
  output <- matrix(0,nrow = ncoun*8,ncol =1)
  #making some key parameter matrix like
  
  # else if (indInterv == 4){
  #   b1 = b1Interv*rep(1,ncoun)
  #   ca = caInterv*rep(1,ncoun)
  #   hcf = ebolaBedInterv*rep(1,ncoun)
  # }

  output[1:ncoun] <- N0*deathRate -(S0*(b1*I20+b0*I10+b2*H0+b3*D0)/N0) - rowSums(m*(S0/N0))+ m%*%(S0/N0)-S0*deathRate
  
  #print(Sdf)
  #print(output[1:ncoun])
  #print(sum(N0*deathRate-S0*deathRate))
  #print(sum(- rowSums(m*(S0/N0))+ m%*%(S0/N0)))
  #Exposed
  output[(ncoun+1): (2*ncoun),1] <-(S0*(b1*I20+b0*I10+b2*H0+b3*D0)/N0)-lat1*E0-rowSums(m*(E0/N0),1)+ m%*%(E0/N0)-E0*deathRate
  
  #Pre Infected
  output[(2*ncoun+1): (3*ncoun),1] <- lat1*E0-preDet*I10-rowSums(m*(I10/N0))+m%*%(I10/N0)
  -I10*deathRate
  # Infected
  output[(3*ncoun+1): (4*ncoun),1] <- (1-ca*pmax(0,(1-H0/ebolaBed)))*preDet*I10- postDet*I20- rowSums(m*(I20/N0))+ m%*%(I20/N0)-I20*deathRate
  #Hospitalised
  
  output[(4*ncoun+1): (5*ncoun),1] <- (ca*pmax(0,(1-H0/ebolaBed)))*preDet*I10-postDet*H0 -H0*deathRate
  
  #Recoverd
  output[(5*ncoun+1): (6*ncoun),1] <-(1-hcf)*postDet*H0+(1-caseFat)*postDet*I20-rowSums(m*(R0/N0))+m%*%(R0/N0)-R0*deathRate
  
  #Dead
  output[(6*ncoun+1): (7*ncoun),1] <- hcf*postDet*H0+caseFat*postDet*I20-bp*D0#-rowSums(m*R[,j])+m%*%R[,j])
  #print(I21k1)
  #Buried
  output[(7*ncoun+1): (8*ncoun),1] <- bp*D0
  #-rowSums(m*R[,j])+m%*%R[,j])
  # #print(sum(-rowSums(m*(R0/N0))+m%*%(R0/N0)- rowSums(m*(I20/N0))+ m%*%(I20/N0)
  #           -rowSums(m*(I10/N0))+m%*%(I10/N0)-rowSums(m*(E0/N0),1)+ m%*%(E0/N0)
  #           - rowSums(m*(S0/N0))+ m%*%(S0/N0)))
  #           print
  return(output)
}
rungeK_EbolaMe<- function(ind,indInterv,timet,b1,ca,hcf, bp,ebolaBed,deathRate,S0,E0,I10,I20,H0,R0,D0,B0,numSteps,
                        stepsize,ncoun,migration, newCumInitial,b1Interv,caInterv,ebolaBedInterv,indsource,indwatch){
  #adjusting the parameter
  b1 = b1*rep(1,ncoun)
  ca = ca*rep(1,ncoun)
  ebolaBed = ebolaBed*rep(1,ncoun)
  #print(dim(ebolaBed))
  #Calculating
  #print(ebolaBed)
  #print(indsource)
  if (indInterv == 1){
    
    b1[indsource] = b1Interv
    ca[indsource] = caInterv
    ebolaBed[indsource] = ebolaBedInterv  #hcfIn
 
  }
  else if (indInterv == 2){
    b1[indwatch] = b1Interv
    ca[indwatch] = caInterv
    ebolaBed[indwatch] = ebolaBedInterv
  }
  else if (indInterv == 3){
    b1[indsource] = b1Interv
    ca[indsource] = caInterv
    ebolaBed[indsource] = ebolaBedInterv
    b1[indwatch] = b1Interv
    ca[indwatch] = caInterv
    ebolaBed[indwatch] = ebolaBedInterv
  }
  ####################
  preDet = 0.25
  m = migration
  S <- matrix(0 , nrow = ncoun, ncol = numSteps)
  E <- matrix(0 , nrow = ncoun, ncol = numSteps)
  I1 <- matrix(0 , nrow = ncoun, ncol = numSteps)
  I2 <- matrix(0 , nrow = ncoun, ncol = numSteps)
  H <- matrix(0 , nrow = ncoun, ncol = numSteps) #Hospitalised
  D <- matrix(0 , nrow = ncoun, ncol = numSteps) # Dead
  R <- matrix(0 , nrow = ncoun, ncol = numSteps) #recovered
  B <- matrix(0 , nrow = ncoun, ncol = numSteps) #Buried
  N <- matrix(0 , nrow = ncoun, ncol = numSteps) #Total population
  #N1 <- matrix(0 , nrow = ncountry, ncol = n) #Total population possible to move
  Icum <- matrix(0 , nrow = ncoun, ncol =  numSteps)
  if (ind == 1){
      Icum[,1] = I20}
  else if(ind == 2){
    Icum[,1] = newCumInitial}
  #Define intial condition
  S[,1] = S0
  E[,1] = E0
  I1[,1] = I10
  I2[,1] = I20
  H[,1] = H0
  D[,1] = D0
  R[,1] = R0
  B[,1] = B0
  N[,1] = S[,1]+ E[,1] + I1[,1] +I2[,1]+ H[,1] + R[,1]
  #print(sum(N,1))
  valuek1 <- matrix(0,nrow = ncoun*8,ncol =1)
  valuek2 <- matrix(0,nrow = ncoun*8,ncol =1)
  valuek3 <- matrix(0,nrow = ncoun*8,ncol =1)
  valuek4 <- matrix(0,nrow = ncoun*8,ncol =1)
  valuePlus <- matrix(0,nrow = ncoun*8,ncol =1)
  for (i in seq(1,numSteps -1,by= 1)){
    myS = S[,i]
    myE = E[,i]
    myI1 = I1[,i]
    myI2 = I2[,i]
    myH = H[,i]
    myR = R[,i]
    myD = D[,i]
    myB = B[,i]
    valuek1 = ebolaModel(b1,ca,hcf, bp,ebolaBed,deathRate,
                         myS,myE,myI1,myI2, myH,myR, myD, myB,m, ncoun)#step k1
    
    myS = S[,i]+ 0.5*stepsize*valuek1[1:ncoun,1]
    myE = E[,i]+ 0.5*stepsize*valuek1[(ncoun+1):(2*ncoun), 1]
    myI1 = I1[,i]+ 0.5*stepsize*valuek1[(2*ncoun+1):(3*ncoun),1]
    myI2 = I2[,i]+ 0.5*stepsize*valuek1[(3*ncoun+1):(4*ncoun),1]
    myH = H[,i]+ 0.5*stepsize*valuek1[(4*ncoun+1):(5*ncoun),1]
    myR = R[,i]+ 0.5*stepsize*valuek1[(5*ncoun+1):(6*ncoun),1]
    myD = D[,i]+ 0.5*stepsize*valuek1[(6*ncoun+1):(7*ncoun),1]
    myB = B[,i]+ 0.5*stepsize*valuek1[(7*ncoun+1):(8*ncoun),1]
    valuek2 = ebolaModel(b1,ca,hcf, bp,ebolaBed,deathRate,
                         myS,myE,myI1,myI2, myH,myR, myD, myB,m, ncoun)#step k2
    
    myS = S[,i]+ 0.5*stepsize*valuek2[1:ncoun,1]
    myE = E[,i]+ 0.5*stepsize*valuek2[(ncoun+1):(2*ncoun),1]
    myI1 = I1[,i]+ 0.5*stepsize*valuek2[(2*ncoun+1):(3*ncoun),1]
    myI2 = I2[,i]+ 0.5*stepsize*valuek2[(3*ncoun+1):(4*ncoun),1]
    myH = H[,i]+ 0.5*stepsize*valuek2[(4*ncoun+1):(5*ncoun), 1]
    myR = R[,i]+ 0.5*stepsize*valuek2[(5*ncoun+1):(6*ncoun),1]
    myD = D[,i]+ 0.5*stepsize*valuek2[(6*ncoun+1):(7*ncoun),1]
    myB = B[,i]+ 0.5*stepsize*valuek2[(7*ncoun+1):(8*ncoun),1]
    valuek3 = ebolaModel(b1,ca,hcf, bp,ebolaBed,deathRate,
               myS,myE,myI1,myI2, myH,myR, myD, myB,m, ncoun)#step k3
    
    myS = S[,i]+ stepsize*valuek3[1:ncoun,1]
    myE = E[,i]+ stepsize*valuek3[(ncoun+1): (2*ncoun),1]
    myI1 = I1[,i]+ stepsize*valuek3[(2*ncoun+1): (3*ncoun),1]
    myI2 = I2[,i]+ stepsize*valuek3[(3*ncoun+1):(4*ncoun),1]
    myH = H[,i]+ stepsize*valuek3[(4*ncoun+1):(5*ncoun),1]
    myR = R[,i]+ stepsize*valuek3[(5*ncoun+1):(6*ncoun),1]
    myD = D[,i]+ stepsize*valuek3[(6*ncoun+1):(7*ncoun),1]
    myB = B[,i]+ stepsize*valuek3[(7*ncoun+1):(8*ncoun),1]
    valuek4 = ebolaModel(b1,ca,hcf, bp,ebolaBed,deathRate,
                          myS,myE,myI1,myI2, myH,myR, myD, myB,m, ncoun)#step k2
   
    valuePlus = (stepsize/6)*(valuek1 + 2*valuek2+ 2*valuek3 + valuek4)
    
    S[,i+1] = pmax(0,S[,i]+ round(valuePlus[1: ncoun,1]))
    
    E[,i+1] = pmax(0,E[,i]+ round(valuePlus[(ncoun+1): (2*ncoun),1]))
    I1[,i+1] = pmax(0,I1[,i]+ round(valuePlus[(2*ncoun+1): (3*ncoun),1]))
    I2[,i+1] = pmax(0,I2[,i]+ round(valuePlus[(3*ncoun+1): (4*ncoun),1]))
    H[,i+1] = pmax(0,H[,i]+ round(valuePlus[(4*ncoun+1): (5*ncoun),1]))
    R[,i+1] = pmax(0,R[,i]+ round(valuePlus[(5*ncoun+1): (6*ncoun),1]))
    D[,i+1] = pmax(0,D[,i]+ round(valuePlus[(6*ncoun+1): (7*ncoun),1]))
    B[,i+1] = pmax(0,B[,i]+ round(valuePlus[(7*ncoun+1): (8*ncoun),1]))
    N[,i+1] = S[,i+1]+ E[,i+1] + I1[,i+1] +I2[,i+1]+ H[,i+1] + R[,i+1]
  
    ###########################Calculate cumulative incidence
    # print(length(H[,i]))
    # print(length(ebolaBed))
    # print(length(ca))
    Icum1 <- Icum[,i]+ ((1-ca*pmax(0,(1-(H[,i]/ebolaBed))))*preDet*I1[,i]
                        +m%*%(I2[,i]/N[,i]))
    Icumadj <- pmax(0,Icum1)
    Icum[,i+1] <- round(Icumadj) 
    
  }
  result = rbind(timet,S)
  result = rbind(result, E)
  result = rbind(result, I1)
  result = rbind(result, I2)
  result = rbind(result,R)
  result = rbind(result, H)
  result = rbind(result, D)
  result = rbind(result, B)
  #print(result)
  #print(dim(result))
  return(list(Dyna =t(result),Inci = t(Icum))) 
}

###########
EbolaRunModel<-function(tday,b1,ca,hcf, bp,ebolaBed,deathRate,ncountry,migration,
              dataCountry,sourceCountry,startinter,newInitial,newCumInitial,TravelBan,ind,indInterv,
             b1Interv,caInterv,ebolaBedInterv,indsource,indwatch){
  # dataCountry has info about country
  #R0 reproduction number, g recovery rate , m= migrate rate 146 by 146 matrix
  #m = migration
  #ebolaBed = dataCountry$BedsAvailEbola
  #ebolaBed = 50000
  #startinter is a variable to determine intervention
  ##newInitial is the initial state of at start time intervetnion
  ##Travelban level
  ##Ind to say there is intervention
  ##indInterv is to indicate the type of intervention  (1-source only, 2- watch only, 3- both watch and source,
  ##4- all countries)
  ##indsource is source country indicator
  ##indwatch is watch country indicator
  bp = 1/bp
  if (ind == 1){
    h1=1 #step size
    if (tday > 0){
      
      #parameters <- c(bet= R0*g  , gamm=g)
      timet <- seq(0,tday,by=h1)
      S0 = dataCountry$PopulationWorldData
      E0 = matrix(0,nrow = 1, ncol = ncountry)
      I10 = matrix(0,nrow = 1, ncol = ncountry)
      I20 = matrix(0,nrow = 1, ncol = ncountry)
      H0 = matrix(0,nrow = 1, ncol = ncountry)
      D0 = matrix(0,nrow = 1, ncol = ncountry)
      R0 = matrix(0,nrow = 1, ncol = ncountry)
      B0 = matrix(0,nrow = 1, ncol = ncountry)
      ##Outbreak in country 142
      
      if (sourceCountry[2] >= S0[sourceCountry[1]]){
        I20[sourceCountry[1]] = S0[sourceCountry[1]]
        S0[sourceCountry[1]] = S0[sourceCountry[1]]-I20[sourceCountry[1]]
      }
      else{
        I20[sourceCountry[1]] = sourceCountry[2]
        S0[sourceCountry[1]] = S0[sourceCountry[1]]-I20[sourceCountry[1]]
      }
    
      n = length(timet)
      #print(n)
      out<- rungeK_EbolaMe(ind,indInterv,timet,b1,ca,hcf, bp,ebolaBed,deathRate,S0,E0,I10,I20,H0,
                         R0,D0,B0,n,h1,ncountry,migration,b1Interv,caInterv,ebolaBedInterv,indsource,indwatch)
    }
  }
  else if (ind == 2){#Intervention
    h1=1 #step size
    if (tday > 0){
    if (indInterv == 1){#source only
      
      timetnew <- seq(startinter,tday,by=h1)
      migration[indsource,] = migration[indsource,]*TravelBan
      migration[,indsource] = migration[,indsource]*TravelBan
      S0 = newInitial[1:ncountry]
      E0 = newInitial[(ncountry+1):(2*ncountry)]
      I10 = newInitial[(2*ncountry+1):(3*ncountry)]
      I20 = newInitial[(3*ncountry+1):(4*ncountry)]
      H0 = newInitial[(4*ncountry+1):(5*ncountry)]
      R0 = newInitial[(5*ncountry+1):(6*ncountry)]
      D0 = newInitial[(6*ncountry+1):(7*ncountry)]
      B0 = newInitial[(7*ncountry+1):(8*ncountry)]
      n1 = length(timetnew)
      out<- rungeK_EbolaMe(ind,indInterv,timetnew,b1,ca,hcf, bp,ebolaBed,deathRate,S0,E0,I10,I20,H0,
                         R0,D0,B0,n1,h1,ncountry,migration,newCumInitial,
                         b1Interv,caInterv,ebolaBedInterv,indsource,indwatch)
      
    }
    else if(indInterv == 2){#watch only
      timetnew <- seq(startinter,tday,by=h1)
      migration[indwatch,] = migration[indwatch,]*TravelBan
      migration[,indwatch] = migration[,indwatch]*TravelBan
      S0 = newInitial[1:ncountry]
      E0 = newInitial[(ncountry+1):(2*ncountry)]
      I10 = newInitial[(2*ncountry+1):(3*ncountry)]
      I20 = newInitial[(3*ncountry+1):(4*ncountry)]
      H0 = newInitial[(4*ncountry+1):(5*ncountry)]
      R0 = newInitial[(5*ncountry+1):(6*ncountry)]
      D0 = newInitial[(6*ncountry+1):(7*ncountry)]
      B0 = newInitial[(7*ncountry+1):(8*ncountry)]
      n1 = length(timetnew)
      out<- rungeK_EbolaMe(ind,indInterv,timetnew,b1,ca,hcf, bp,ebolaBed,deathRate,S0,E0,I10,I20,H0,
                         R0,D0,B0,n1,h1,ncountry,migration,newCumInitial,
                         b1Interv,caInterv,ebolaBedInterv,indsource,indwatch)
    }
    else if(indInterv == 3){#Both watch and source
      timetnew <- seq(startinter,tday,by=h1)
      if (indsource != indwatch){# I do not want double calculation if source = watch
        migration[indsource,] = migration[indsource,]*TravelBan
        migration[,indsource] = migration[,indsource]*TravelBan
        migration[indwatch,] = migration[indwatch,]*TravelBan
        migration[,indwatch] = migration[,indwatch]*TravelBan}
      else if(indsource == indwatch){
        migration[indsource,] = migration[indsource,]*TravelBan
        migration[,indsource] = migration[,indsource]*TravelBan
      }
      S0 = newInitial[1:ncountry]
      E0 = newInitial[(ncountry+1):(2*ncountry)]
      I10 = newInitial[(2*ncountry+1):(3*ncountry)]
      I20 = newInitial[(3*ncountry+1):(4*ncountry)]
      H0 = newInitial[(4*ncountry+1):(5*ncountry)]
      R0 = newInitial[(5*ncountry+1):(6*ncountry)]
      D0 = newInitial[(6*ncountry+1):(7*ncountry)]
      B0 = newInitial[(7*ncountry+1):(8*ncountry)]
      n1 = length(timetnew)
      out<- rungeK_EbolaMe(ind,indInterv,timetnew,b1,ca,hcf, bp,ebolaBed,deathRate,S0,E0,I10,I20,H0,
                         R0,D0,B0,n1,h1,ncountry,migration,newCumInitial,
                         b1Interv,caInterv,ebolaBedInterv,indsource,indwatch)
    }
    else if(indInterv == 4){#all countries
      timetnew <- seq(startinter,tday,by=h1)
      migration = migration*TravelBan
      S0 = newInitial[1:ncountry]
      E0 = newInitial[(ncountry+1):(2*ncountry)]
      I10 = newInitial[(2*ncountry+1):(3*ncountry)]
      I20 = newInitial[(3*ncountry+1):(4*ncountry)]
      H0 = newInitial[(4*ncountry+1):(5*ncountry)]
      R0 = newInitial[(5*ncountry+1):(6*ncountry)]
      D0 = newInitial[(6*ncountry+1):(7*ncountry)]
      B0 = newInitial[(7*ncountry+1):(8*ncountry)]
      n1 = length(timetnew)
      #print(n1)
      #print(I20)
      #Solve the systems of Ode
      out<- rungeK_EbolaMe(ind,indInterv,timetnew,b1,ca,hcf, bp,ebolaBed,deathRate,S0,E0,I10,I20,H0,
                         R0,D0,B0,n1,h1,ncountry,migration,newCumInitial,
                         b1Interv,caInterv,ebolaBedInterv,indsource,indwatch)
    }
    }
  }
  #varName = dataCountry$Country
  #varName = c("Day",varName)
  #out = data.frame(out)
  #$names(out) = varName
  
  return (out)
}
 