# husky



# Preamble

The calculation of the MRI groundfish survey indices have for quite some time been done via a bundle of R-scripts and data objects that reside in various .RData binary directories in the reikn/Splus5 space. These have been moved from the blackholes to the `husky`-package. What this means is that those interested in calculating survey indices no longer need to "attach" to .RData binary directories, only install and load the husky-package and run the usual code (examples provided below).

This move is just a stepping stone in the evolution of survey index calculation, the next generation is being developed within the [pax-package](https://github.com/fishvice/pax).



```r
library(ggplot2)
library(dplyr)
library(fjolst)
# devtools::install_github("fishvice/husky", dependencies = FALSE)
library(husky)
```

# Length based SMB indices

__Minimum fuzz adaptation__

__Specs__:

```r
SPECIES <- 1
YEARS <- 1985:2016
KYN <- FALSE # only for the huskyverse
lwcoeff <- LWCOEFF[[as.character(SPECIES)]]
lengdir <- LENGDIR[[as.character(SPECIES)]]
```

NB: The next generation of indices calculation has been seeded in `/net/hafkaldi/export/u2/reikn/Splus5/SurveyWork`. According to Husky (December 2016) this has not "formally" been adopted.

__Script location__: Husky's master scripts is `/net/hafkaldi/export/u2/reikn/Splus5/SMB/allarteg.sh`. This script calls for each species two additional shell scripts:
* `/net/hafkaldi/export/u2/reikn/Splus5/SMB/BIOVISIT_R.sh`
* `/net/hafkaldi/export/u2/reikn/Splus5/SMB/BIOVISIT_R/BIOVISIT_R.sh`

The difference in the two scripts is:
```
 diff BIOVISIT_R.sh BIOVISIT.fastar_R.sh 
10c10,11
< st1 <- STODVAR$y999[STODVAR$y999$tognumer %in% 1:39,]
---
> ind <- c(31931,31932,32131,36731,37031,37131,37132,37231,41431,41531,42231,42232,47431,52331)
> st1 <- STODVAR$y999[STODVAR$y999$tognumer %in% 1:19 | !is.na(match(STODVAR$y999$index,ind)) ,]
129,130c130,131
<     base.visit <- tmp.visit1a
<     aggr.visit <- tmp.visit2a
---
>     base.visit.fastar <- tmp.visit1a
>     aggr.visit.fastar <- tmp.visit2a
133,134c134,135
<   base.visit <- rbind(base.visit,tmp.visit1a)
<   aggr.visit <- rbind(aggr.visit,tmp.visit2a) 
---
>   base.visit.fastar <- rbind(base.visit.fastar,tmp.visit1a)
>   aggr.visit.fastar <- rbind(aggr.visit.fastar,tmp.visit2a) 
145,146c146,147
< aggr.visit$bio <- aggr.visit$fj*lwcoeff[1]*aggr.visit$lengd^lwcoeff[2]/1e3
< base.visit$bio <- base.visit$fj*lwcoeff[1]*base.visit$lengd^lwcoeff[2]/1e3
---
> aggr.visit.fastar$bio <- aggr.visit.fastar$fj*lwcoeff[1]*aggr.visit.fastar$lengd^lwcoeff[2]/1e3
> base.visit.fastar$bio <- base.visit.fastar$fj*lwcoeff[1]*base.visit.fastar$lengd^lwcoeff[2]/1e3
```
The following is based on the script `/net/hafkaldi/export/u2/reikn/Splus5/SMB/BIOVISIT_R.sh` with some minimum adaptations.


```r
base.visit2 <- list()
aggr.visit2 <- list()
STODVAR2 <- dplyr::bind_rows(STODVAR)  # EH added

for (j in 1:length(YEARS)) {
  #print(YEARS[j])
  #st1 <- STODVAR$'y1985'[STODVAR$'y1985'$tognumer %in% 1:39,]
  st1 <- STODVAR2[STODVAR2$ar == YEARS[j] & STODVAR2$tognumer %in% 1:39,] # EH added
  tmp <- lesa.lengdir(st1$synis.id,SPECIES,col.names="kyn")
  tmp1 <- lesa.numer(st1$synis.id,SPECIES) 
  tmp <- Skala.med.toldum(tmp,tmp1)
  i <- is.na(tmp$fj.alls) 
  tmp$fj.alls[i] <- 0
  tmp$bio <- tmp$fj.alls*lwcoeff[1]*tmp$lengd^lwcoeff[2]/1e6 # tonn
  tmp$fj.alls <- tmp$fj.alls/1e3 # þúsundir
  
  
  for( i in 1:length(lengdir)) {
    
    #print(i) 
    tmp1 <- tmp[tmp$lengd==lengdir[i],]
    
    if(nrow(tmp1) > 0) {
      x <- apply.shrink(tmp1$fj.alls,tmp1$synis.id,sum) 
      names(x) <- c("synis.id","fj")
      st <- husky:::join(st1[,c("newstrata","toglengd","synis.id")],x,"synis.id",set=0)
    } else {
      st <- st1[,c("newstrata","toglengd","synis.id")]
      st$fj <- rep(0,nrow(st)) 
    }
    
    if(KYN) {
      tmp1 <- tmp[tmp$lengd==lengdir[i] & tmp$kyn==1 & !is.na(tmp$kyn),]
      if(nrow(tmp1) > 0) {
        x <- apply.shrink(tmp1$fj.alls,tmp1$synis.id,sum) 
        names(x) <- c("synis.id","fjhaenga")
        st <- husky:::join(st,x,"synis.id",set=0)
      } else {
        st$fjhaenga <- rep(0,nrow(st)) 
      }
      
      tmp1 <- tmp[tmp$lengd==lengdir[i] & tmp$kyn==2 & !is.na(tmp$kyn),]
      if(nrow(tmp1) > 0) {
        x <- apply.shrink(tmp1$fj.alls,tmp1$synis.id,sum) 
        names(x) <- c("synis.id","fjhrygna")
        st <- husky:::join(st,x,"synis.id",set=0)
      }
      else {
        st$fjhrygna <- rep(0,nrow(st)) 
      }
    }
    
    tmp1 <- tmp[tmp$lengd >= lengdir[i],]
    if(nrow(tmp1) > 0) {
      x <- apply.shrink(tmp1$bio,tmp1$synis.id,sum) 
      names(x) <- c("synis.id","bioge") 
      st <- husky:::join(st,x,"synis.id",set=0)
    } else {
      st$bioge <- rep(0,nrow(st)) 
    }
    
    tmp1 <- tmp[tmp$lengd <= lengdir[i],]
    if(nrow(tmp1) > 0) {
      x <- apply.shrink(tmp1$fj.alls,tmp1$synis.id,sum) 
      names(x) <- c("synis.id","fjle")
      st <- husky:::join(st,x,"synis.id",set=0)
    } else {
      st$fjle <- rep(0,nrow(st)) 
    }
    
    
    tmp.visit <- Calc.index(st,"fj")
    
    tmp.biovisit <- Calc.index(st,"bioge")
    tmp.seidavisit <- Calc.index(st,"fjle")
    
    if(KYN) {
      tmp.haengavisit <- Calc.index(st,"fjhaenga")
      tmp.hrygnuvisit <- Calc.index(st,"fjhrygna")
    }
    
    tmp.visit1 <- tmp.visit$result[,c("strata","total","cv")]
    names(tmp.visit1)[2:3] <- c("fj","cv.fj")
    tmp.visit1$bio.staerri <- tmp.biovisit$result[,"total"]
    tmp.visit1$cv.bio.staerri <- tmp.biovisit$result[,"cv"]
    tmp.visit1$fj.minni <- tmp.seidavisit$result[,"total"]
    tmp.visit1$cv.fj.minni <- tmp.seidavisit$result[,"cv"]
    
    if(KYN) {
      tmp.visit1$fj.haenga <- tmp.haengavisit$result[,"total"]
      tmp.visit1$cv.fj.haenga <- tmp.haengavisit$result[,"cv"]
      tmp.visit1$fj.hrygna <- tmp.hrygnuvisit$result[,"total"]
      tmp.visit1$cv.fj.hrygna <- tmp.hrygnuvisit$result[,"cv"]
    }
    
    tmp.visit2 <- tmp.visit$aggr.output[,c("total","cv")]
    names(tmp.visit2) <- c("fj","cv.fj") 
    tmp.visit2$bio.staerri <- tmp.biovisit$aggr.output[,"total"]
    tmp.visit2$cv.bio.staerri <- tmp.biovisit$aggr.output[,"cv"]
    tmp.visit2$fj.minni <- tmp.seidavisit$aggr.output[,"total"]
    tmp.visit2$cv.fj.minni <- tmp.seidavisit$aggr.output[,"cv"]
    
    if(KYN) {
      tmp.visit2$fj.haenga <- tmp.haengavisit$aggr.output[,"total"]
      tmp.visit2$cv.fj.haenga <- tmp.haengavisit$aggr.output[,"cv"]
      tmp.visit2$fj.hrygna <- tmp.hrygnuvisit$aggr.output[,"total"]
      tmp.visit2$cv.fj.hrygna <- tmp.hrygnuvisit$aggr.output[,"cv"]
    }
    
    
    tmp.visit2$svaedi <- dimnames(tmp.visit2)[[1]]
    tmp.visit2$svaedisnr <- 1:nrow(tmp.visit2)
    dimnames(tmp.visit2)[[1]] <- 1:nrow(tmp.visit2)
    tmp.visit1$lengd <- rep(lengdir[i],nrow(tmp.visit1))
    tmp.visit2$lengd <- rep(lengdir[i],nrow(tmp.visit2))
    tmp.visit1$ar <- rep(YEARS[j],nrow(tmp.visit1))
    tmp.visit2$ar <- rep(YEARS[j],nrow(tmp.visit2))
    if(i == 1 ) {
      tmp.visit1a <- tmp.visit1
      tmp.visit2a <- tmp.visit2
    } else {
      tmp.visit1a <- rbind(tmp.visit1a,tmp.visit1)
      tmp.visit2a <- rbind(tmp.visit2a,tmp.visit2)
    }
  }
  base.visit2[[j]] <- tmp.visit1a
  aggr.visit2[[j]] <- tmp.visit2a
} # End year loop

base.visit2 <- dplyr::bind_rows(base.visit2)
aggr.visit2 <- dplyr::bind_rows(aggr.visit2)
# tonn því fjöldi er þegar í 1000 en lengd-þyngdar gefur grömm.  
aggr.visit2$bio <-
  aggr.visit2$fj*lwcoeff[1]*aggr.visit2$lengd^lwcoeff[2]/1e3
base.visit2$bio <-
  base.visit2$fj*lwcoeff[1]*base.visit2$lengd^lwcoeff[2]/1e3
```

__Internal huskyverse comparison__: rough double testing

Load the "official calculation":

```r
attach('/net/hafkaldi/export/u2/reikn/Splus5/SMB/TORSKUR/.RData')
```


```r
ggplot() +
  geom_pointrange(data = aggr.visit2 %>% filter(svaedi == "Heild", lengd == min(lengd)),
                  aes(ar, bio.staerri, 
                      ymin = bio.staerri * (1 - cv.bio.staerri),
                      ymax = bio.staerri * (1 + cv.bio.staerri)),
                  lwd = 1,
                  size = 4) +
  geom_pointrange(data = aggr.visit %>% filter(svaedi == "Heild", lengd == min(lengd)),
                  aes(ar, bio.staerri, 
                      ymin = bio.staerri * (1 - cv.bio.staerri),
                      ymax = bio.staerri * (1 + cv.bio.staerri)),
                  colour = "red") +
  labs(x = NULL, y = NULL, title = "Standardized biomass index")
```

![](README_files/figure-html/smb_biomass-1.png)<!-- -->

In the above the black line is the calculation based on the function and objects in the husky-package, the red is the "official calculation". Bottom line: The two huskyverse things are, not surprisingly, the same (visally).

TODO: A more detailed comparison, including cv-calculations


```r
detach("file:/net/hafkaldi/export/u2/reikn/Splus5/SMB/TORSKUR/.RData")
```

# Note the difference in the "old" and new strata scrips
particularily the Calc.index function call, see below:
```
hafstokkur/net/hafkaldi/export/u2/reikn/Splus5 [1149] diff SMB/BIOVISIT_R.sh SMBNewstrata/BIOVISIT_R.sh 
1c1
< for yr in 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012
---
> for yr in 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013
6,7c6,8
< Rattach("..")
< Rattach("../GEOMETRY.NEW")
---
> Rattach("../../SMBNewstrata")
> Rattach("../../SMB/GEOMETRY.NEW")
> attach("../../HAUSTRALLNewStrata/Stratifiering/.RData",pos=2)
10,12c11,13
< st1 <- STODVAR$y999[STODVAR$y999$tognumer %in% 1:39,]
< tmp <- lesa.lengdir(st1$synis.id,TEG,col.names="kyn")
< tmp1 <- lesa.numer(st1$synis.id,TEG) 
---
> tmp8 <- STODVAR.all[(STODVAR.all$tognumer %in% 1:39 | is.na(STODVAR.all$tognumer)) & STODVAR.all$ar==999,]
> tmp <- lesa.lengdir(tmp8$synis.id,TEG,col.names="kyn")
> tmp1 <- lesa.numer(tmp8$synis.id,TEG) 
26c27
<      st <- join(st1[,c("newstrata","toglengd","synis.id")],x,"synis.id",set=0)
---
>      st <- join(tmp8[,c("newstrata","toglengd","synis.id")],x,"synis.id",set=0)
29c30
<     st <- st1[,c("newstrata","toglengd","synis.id")]
---
>     st <- tmp8[,c("newstrata","toglengd","synis.id")]
75,77c76,78
<   tmp.visit <- Calc.index(st,"fj")
<   tmp.biovisit <- Calc.index(st,"bioge")
<   tmp.seidavisit <- Calc.index(st,"fjle")
---
>   tmp.visit <- Calc.index(st,"fj",combine.output=SMBaggregation)
>   tmp.biovisit <- Calc.index(st,"bioge",combine.output=SMBaggregation)
>   tmp.seidavisit <- Calc.index(st,"fjle",combine.output=SMBaggregation)
79,80c80,81
<     tmp.haengavisit <- Calc.index(st,"fjhaenga")
<     tmp.hrygnuvisit <- Calc.index(st,"fjhrygna")
---
>     tmp.haengavisit <- Calc.index(st,"fjhaenga",combine.output=SMBaggregation)
>     tmp.hrygnuvisit <- Calc.index(st,"fjhrygna",combine.output=SMBaggregation)
141,142c142,144
< Rattach("..")
< Rattach("../GEOMETRY.NEW")
---
> Rattach("../../SMBNewstrata")
> Rattach("../../SMB/GEOMETRY.NEW")
> attach("../../HAUSTRALLNewStrata/Stratifiering/.RData",pos=2)
```


#  Age based SMB indices - thorskur

The following is based on the script `/net/hafkaldi/export/u2/reikn/Splus5/GETALK/AGEVISIT/torskur.2016/torskur.visit.r` with some minimum adaptations.


```r
ind <- c(31931,31932,32131,36731,37031,37131,37132,37231,41431,41531,42231,42232,47431,52331)
fj  <- list()

years <- 1985:2016
regs <- list(S=c(1,9:10),N=2:8)
regname <- names(regs)
aldur <- 1:14
lengd <- c(seq(4.5,109.5,by=5),119.5,139.5)

for( i in 1:length(years) ) {
 # print(i) 
  fj[[i]] <- data.frame()
  if(i == 17)  stauka <- lesa.stodvar(leidangur="A4-2001")  
  
  for( j in 1:2) {
    st <- STODVAR[[i]][!is.na(match(STODVAR[[i]]$area,regs[[j]])) & (STODVAR[[i]]$tognumer < 20 | !is.na(match(STODVAR[[i]]$index,ind))) ,]
    st.all <- STODVAR[[i]][!is.na(match(STODVAR[[i]]$area,regs[[j]])),]
    if(i == 17) {
      cn <- c("lat","lon","synis.id")
      sttmp <- stauka[stauka$area %in% regs[[j]],cn]
      st.all <- rbind(st.all[,cn],sttmp[,cn])
    }
    
    le <- lesa.lengdir(st$synis.id,1)
    kv <- lesa.kvarnir(st.all$synis.id,1,col.names=c("kyn","kynthroski"),oracle=F)
    
    nu <- lesa.numer(st$synis.id,1)
    
    alk <- MakeAlk(kv, 1, lengd=lengd, aldur = aldur, FilterAldurLengd = F, kynth = T) # Tók filteraldurlengd af
    
    ar <- i+1984
    if(ar < 1993){ 
      tmp <- torskur.marsrall.wt.8592[torskur.marsrall.wt.8592$reg==regname[j],]
      } else {
      tmp <- torskur.marsrall.wt[torskur.marsrall.wt$reg==regname[j] & torskur.marsrall.wt$ar==ar,]}
    ldist <- MakeLdistbyStation(le,nu,1,lengd=lengd,Stodvar=st,talid=T,lengd.thyngd.data=tmp)
    fj[[i]]  <- rbind.alk(fj[[i]],Calc.fj.per.station(alk,ldist))
  }
}

torskur.fj <- fj
names(torskur.fj) <- as.character(years)

biomass <- kynthbiomass <-  meanlefj <- sdevfj <- kynthfj <- fj <- list()
biovisit <- fjvisit <- kynthvisit <- meanlevisit <-  sdevvisit <- kynthbiovisit  <- list()

for(i in 1:length(years)) {
  #print(i)
  st <- attributes(torskur.fj[[i]])$Stodvar
  fj <- kynthfj <- biomass <- meanlefj <- sdevfj <- kynthbiomass <- list()
  biom <- torskur.fj[[i]]$WtPerAldur*torskur.fj[[i]]$FjPerAldur
  biom[is.na(biom)] <- 0
  kynthbiom <- torskur.fj[[i]]$KynthWtPerAldur*torskur.fj[[i]]$KynthFjPerAldur
  kynthbiom[is.na(kynthbiom)] <- 0
  for(j in 1:14) {
    #cat(paste(j," "))
    fj[[j]] <- Calc.index(st,z=torskur.fj[[i]]$FjPerAldur[,j])$aggr.output
    kynthfj[[j]] <- Calc.index(st,z=torskur.fj[[i]]$KynthFjPerAldur[,j])$aggr.output
    biomass[[j]] <- Calc.index(st,z=biom[,j])$aggr.output
    meanlefj[[j]] <- Calc.index(st,z=torskur.fj[[i]]$LengdSinnumFjPerAldur[,j])$aggr.output
    sdevfj[[j]] <- Calc.index(st,z=torskur.fj[[i]]$Lengd2SinnumFjPerAldur[,j])$aggr.output
    kynthbiomass[[j]] <- Calc.index(st,z=kynthbiom[,j])$aggr.output
  }
  biovisit[[i]] <- biomass
  fjvisit[[i]] <- fj
  kynthvisit[[i]] <- kynthfj
  kynthbiovisit[[i]] <- kynthbiomass
  sdevvisit[[i]] <- sdevfj
  meanlevisit[[i]] <- meanlefj
}
```


```r
# net/hafkaldi/export/u2/reikn/Splus5/GETALK/AGEVISIT/torskur.2016/combine.r
#Rattach("/home/hoski/GETALK")
torskur.visit.n <- combine.alk.visit(fjvisit, kynthvisit, biovisit,meanlevisit,sdevvisit,kynthbiovisit ,row = 13, aldur = 1:14,ar=1985:2016)
torskur.visit.s <- combine.alk.visit(fjvisit, kynthvisit, biovisit,meanlevisit,sdevvisit,kynthbiovisit,row = c(14), aldur = 1:14,ar=1985:2016)
torskur.visit.tot  <- combine.alk.visit(fjvisit, kynthvisit, biovisit, meanlevisit,sdevvisit,kynthbiovisit,row = c(22), aldur = 1:14,ar=1985:2016
)

torskur.visit.n$fj <- t(round(torskur.visit.n$fj,2))
torskur.visit.n$cv <- t(round(torskur.visit.n$cv,3))
torskur.visit.n$kynthhlutfall<- t(round(torskur.visit.n$kynthhlutfall*100,1))
torskur.visit.n$wt <- t(round(torskur.visit.n$wt*1000))
torskur.visit.n$kynthwt <- t(round(torskur.visit.n$kynthwt*1000))
torskur.visit.n$meanle <- t(round(torskur.visit.n$meanle,1))
torskur.visit.n$sdev <- t(round(torskur.visit.n$sdev,1))

torskur.visit.s$cv <- t(round(torskur.visit.s$cv,3))
torskur.visit.s$fj <- t(round(torskur.visit.s$fj,2))
torskur.visit.s$kynthhlutfall<- t(round(torskur.visit.s$kynthhlutfall*100,1))
torskur.visit.s$wt <- t(round(torskur.visit.s$wt*1000))
torskur.visit.s$kynthwt <- t(round(torskur.visit.s$kynthwt*1000))
torskur.visit.s$meanle <- t(round(torskur.visit.s$meanle,1))
torskur.visit.s$sdev <- t(round(torskur.visit.s$sdev,1))

torskur.visit.tot$cv <- t(round(torskur.visit.tot$cv,3))
torskur.visit.tot$fj <- t(round(torskur.visit.tot$fj,2))
torskur.visit.tot$kynthhlutfall<- t(round(torskur.visit.tot$kynthhlutfall*100,1))
torskur.visit.tot$wt <- t(round(torskur.visit.tot$wt*1000))
torskur.visit.tot$kynthwt <- t(round(torskur.visit.tot$kynthwt*1000))
torskur.visit.tot$meanle <- t(round(torskur.visit.tot$meanle,1))
torskur.visit.tot$sdev <- t(round(torskur.visit.tot$sdev,1))


# Setja saman fyrir ORACLE

age <- 1:14
year <- 1985:2016
n <- length(age)
age <- matrix(age,n,length(year)) 
year <- matrix(year,n,length(year),byrow=T)
yearage <- data.frame(year=c(year),age=c(age))

tmp <- yearage
tmp$reg <- rep("Tot",nrow(tmp))
tmp$fj <- c(torskur.visit.tot$fj)
tmp$cv <- c(torskur.visit.tot$cv)
tmp$wt <- c(torskur.visit.tot$wt)
tmp$kynthhlutfall <- c(torskur.visit.tot$kynthhlutfall)
tmp$meanle <- c(torskur.visit.tot$meanle)
tmp$sdev <- c(torskur.visit.tot$sdev)
tmp$kynthwt <- c(torskur.visit.tot$kynthwt)

tmp1 <- yearage
tmp1$reg <- rep("N",nrow(tmp1))
tmp1$fj <- c(torskur.visit.n$fj)
tmp1$cv <- c(torskur.visit.n$cv)
tmp1$wt <- c(torskur.visit.n$wt)
tmp1$kynthhlutfall <- c(torskur.visit.n$kynthhlutfall)
tmp1$meanle <- c(torskur.visit.n$meanle)
tmp1$sdev <- c(torskur.visit.n$sdev)
tmp1$kynthwt <- c(torskur.visit.n$kynthwt)

tmp2 <- yearage
tmp2$reg <- rep("S",nrow(tmp2))
tmp2$fj <- c(torskur.visit.s$fj)
tmp2$cv <- c(torskur.visit.s$cv)
tmp2$wt <- c(torskur.visit.s$wt)
tmp2$kynthhlutfall <- c(torskur.visit.s$kynthhlutfall)
tmp2$meanle <- c(torskur.visit.s$meanle)
tmp2$sdev <- c(torskur.visit.s$sdev)
tmp2$kynthwt <- c(torskur.visit.s$kynthwt)

torskur.visit.oracle <- rbind(tmp,tmp1,tmp2)
```

