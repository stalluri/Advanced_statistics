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

seq <- c(urban,rural)

count <-0
perm <- rep (0, tlen)
t <- rep(0,10000)

abs_diff <- abs( mean(urban) - mean(rural))

for (i in 1: 10000) {
perm <- sample (seq, replace=F)

t[i] <- abs (mean(perm[1:ulen]) - mean(perm[(ulen+1):tlen]))

if (t[i] > abs_diff) {
count <- count+1 }

}

hist(t,"FD") 
pval <- count/10000

cat ("mean diff", abs_diff,"\n")
cat ("pvalue :", pval,"\n")






