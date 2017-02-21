FindAngle <- function(a,b,c) {
  KeyRatio <- function(a,b,c) {
    (a^2+b^2-c^2)/(2*a*b)
  }
  acos(KeyRatio(a,b,c))*180/pi
}

AllAngles <- function(a,b,c) {
  A <- FindAngle(b,c,a)
  B <- FindAngle(c,a,b)
  C <- 180-A-B
  c(A,B,C)
}

NiceQ <- function(x) {
  abs(round(x)-x)<0.15
}

NiceAnglesQ <- function(a,b,c) {
  all(NiceQ(AllAngles(a,b,c)))
}

SideList <- function(n) {
  m <- NULL
  for(c in 1:n) {
    for(b in 1:c) {
      for(a in c-b+1:b)
        if(NiceAnglesQ(a,b,c)) m <- rbind(m,c(a,b,c,AllAngles(a,b,c)))
    }
  }
  m
}

#checks to see if all the angles of a row are the same as the angles of a given row
SimilarQ <- function(m,a,b) {
  all(m[a,4:6]==m[b,4:6])  
}

SimilarList <- function(m,a) {
  sim <- logical()
  for(i in 1:dim(m)[[1]])
    sim <- c(sim,SimilarQ(m,a,i))
  matrix(sim,ncol=1)
}

RepeatFilter <- function(m){
  DropRepeats <- function(m,a) {
    rbind(m[a,],m[!SimilarList(m,a),])
  }
  i <- 1
  repeat{
    m <- DropRepeats(m,i)
    i <- i+1
    if(i>dim(m)[1]) break
  }
  m
}

RawList <- function(n) {
  m <- SideList(n)
  m <- RepeatFilter(m)
  m <- m[order(m[,4]),]
  colnames(m) <- c("a","b","c","A","B","C")
  m
}

RoundedList <- function(n) {
  m <- RawList(n)
  m[,4:6] <- round(m[,4:6])
  m
}

a <- RawList(10)
a

b <- RoundedList(10)
b

write.csv(b,"law of cosines, nice values.csv")


