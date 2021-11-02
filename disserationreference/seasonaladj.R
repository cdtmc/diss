coeff <- 20

ggplot(oildata, aes(x=observation_date)) +
  
  geom_line(aes(y=POILBREUSDQ), color="black") + 
  geom_line(aes(y=oil_adj), color="green")+
  geom_line(aes(y=aberdeen_unemp * coeff), color="red") + # Divide by 10 to get the same range than the temperature
  #geom_line(aes(y=scotland_unemp * coeff), color="blue") +
  #geom_line(aes(y=UK_unemp * coeff), color="green") +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Price per barrell USD",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Unemployment")
  )



unemp <- ts(oildata$aberdeen_unemp, start=c(2004,1), end=c(2020,4) ,frequency = 4)

oilP <- ts(oildata$oil_adj, start=c(2004, 1), end=c(2019,3), frequency=4)
plot(oilP)
cycle(oilP)

trend_oilP <- rollmean(oilP, k=4)
ts.plot(oilP, trend_oilP, gpars=list(xlab="year", ylab="price", lty=c(1:2)))

oil_detrend <- oilP - trend_oilP

plot(oil_detrend)

m_oil <- t(matrix(data=oil_detrend, nrow=4))
seasonal_oilP <- colMeans(m_oil, na.rm=TRUE)

plot(seasonal_oilP)

random_oilP <- oil_detrend - seasonal_oilP
recon_oilP <- trend_oilP + seasonal_oilP + random_oilP
ts.plot(oilP, recon_oilP, gpars=list(xlab="year", ylab="price", lty=c(1:2)))
decom_oilP <- decompose(oilP, type="additive")



ts.stl <- stl(oilP, s.window = 4)
ts.sa <- seasadj(ts.stl)
plot(oilP)
plot(ts.sa)
seasonplot(ts.sa, 4, col=rainbow(16), year.labels = TRUE)

unemp <- ts(oildata$aberdeen_unemp, start=c(2004,1), end=c(2020,4) ,frequency = 4)
unemp.stl <- stl(unemp, s.window=4)
unemp.sa <- seasadj(unemp.stl)
seasonplot(unemp.sa, 4, col=rainbow(16), year.labels = TRUE)
ts.plot(unemp, unemp.sa, gpars=list(xlab="year", ylab="rate", lty=c(1:2)))

