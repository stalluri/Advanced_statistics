# Q2, 
urban<-c(133,134,155,170,175,179,181,184,188,189,190,196,197,199,200,200,201,
	201,204,205,205,205,206,214,217,222,222,227,227,228,234,234,236,239,
	241,242,244,249,252,273,279,284,284,284,330)
ulen<-length(urban)
rural<-c(95,108,108,114,115,124,129,129,131,131,135,136,136,139,140,142,142,143,
	143,144,144,145,145,148,152,152,155,157,158,158,162,165,166,171,172,173,
	174,175,180,181,189,192,194,197,204,220,223,226,231)
rlen <- length(rural)

tlen <- ulen + rlen
count <- 0
hbs <- rep(0, (ulen*(ulen-1)/2) )

for ( i in 1 : ulen) {
for ( j in 1 : ulen) {

if (j>i ) { count <- count +1 
hbs[count] <- (urban[i] + urban[j] )/2 }
} }

hbs_urban_estimator <- median(hbs)

cat ("hbs urban estimator:", hbs_urban_estimator,"\n")

# This is for rural residents.
count <- 0
hbs <- rep(0, (ulen*(ulen-1)/2) )

for ( i in 1 : rlen) {
for ( j in 1 : rlen) {

if (j>i ) { count <- count +1 
hbs[count] <- (rural[i] + rural[j] )/2 }
} }

hbs_rural_estimator <- median(hbs)

cat ("hbs urban estimator:", hbs_rural_estimator,"\n")


cat (" difference :", hbs_urban_estimator -hbs_rural_estimator,"\n")


# difference is around 60.25, which is close to the one predicted earlier through permutation test : 59

# part b : hubers estimator

huber(urban)



# bootstrapping to find.

perm <- rep (0, ulen)

hbr_bt <- rep(0,10000)
hdl_bt <- rep(0,10000)

for (i in 1: 10000) {
perm <- sample (urban, replace=T)

# calculating the huber's estimator as above or write a function for it and use it here and do bootstrap

#hbr_bt [i] <- huber(perm)

# hodges -lehmann

count <- 0
hbs <- rep(0, (ulen*(ulen-1)/2) )

for ( k in 1 : ulen) {
for ( j in 1 : ulen) {

if (j>k ) { count <- count +1 
hbs[count] <- (perm[k] + perm[j] )/2 }
} }

hdl_bt[i] <- median(hbs)


}

cat ("95% confid interval of hodges-lehmann :", quantile(hdl_bt,.025),"::",quantile(hdl_bt,.975),"\n")
cat ("95% confid interval of huber :", quantile(hbr_bt,.025),"::",quantile(hbr_bt,.975),"\n")






