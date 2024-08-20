library(tidyverse)
library(imager)
library(magick)
library(data.table)

# Main function, need makePos to run first
frameDiff <- function(inPath, outPath, zones = 1:23, printit = TRUE)
{
  folders <- list.files(inPath)
  makes <- list.files(outPath)
  
  all(!(folders %in% makes))
  
  if(all(!(folders %in% makes))){cat("Didnt found all input folders in the output, check make...", "\n")}
  mc <- c() ; for(f in folders){mc <- append(mc, !("dfPos.csv" %in% list.files(paste0(outPath, "/", f))))}
  if(sum(mc) > 0){stop("POS file for ", paste(folders[mc], collapse = "/"), " not found...")} else {
    readline(paste0(length(folders), " folder(s) were found, POS file ready, Press [enter] to continue..."))
  }
  
  f = folders[1]
  for(f in folders)
  {
    subinPath <- paste0(inPath, "/", f)
    suboutPath <- paste0(outPath, "/", f)
    dir.create(paste0(suboutPath, "/frames"))
    dfPos <- read.csv(paste0(suboutPath, "/dfPos.csv"))
    
    dims <- c(dfPos[dfPos$type == "origin",4] - dfPos[dfPos$type == "origin",2], dfPos[dfPos$type == "origin",5] - dfPos[dfPos$type == "origin",3])
    iw <- dims[1]/16 ; ih <- dims[2]/9
    cat("Starting ", f, " | squares are ", iw, " x ", ih, " = ", round(iw/ih,3), "\n")
    
    files <- list.files(subinPath)[grep("frame", list.files(subinPath))]
    if(length(files) == 0){stop("Couldnt find any file, check your inPath")}
    
    MGX <- matrix(nrow = dims[1], ncol = length(files)-1)
    MGY <- matrix(nrow = dims[2], ncol = length(files)-1)
    
    dfVar <- data.frame("frame" = 1:(length(files)-1),
                        "z1" = vector(mode = "numeric", length = length(files)-1),
                        "z2" = vector(mode = "numeric", length = length(files)-1),
                        "z3" = vector(mode = "numeric", length = length(files)-1),
                        "z4" = vector(mode = "numeric", length = length(files)-1),
                        "z5" = vector(mode = "numeric", length = length(files)-1),
                        "z6" = vector(mode = "numeric", length = length(files)-1),
                        "z7" = vector(mode = "numeric", length = length(files)-1),
                        "z8" = vector(mode = "numeric", length = length(files)-1),
                        "z9" = vector(mode = "numeric", length = length(files)-1),
                        "z10" = vector(mode = "numeric", length = length(files)-1),
                        "z11" = vector(mode = "numeric", length = length(files)-1),
                        "z12" = vector(mode = "numeric", length = length(files)-1),
                        "z13" = vector(mode = "numeric", length = length(files)-1),
                        "z14" = vector(mode = "numeric", length = length(files)-1),
                        "z15" = vector(mode = "numeric", length = length(files)-1),
                        "z16" = vector(mode = "numeric", length = length(files)-1),
                        "z17" = vector(mode = "numeric", length = length(files)-1),
                        "z18" = vector(mode = "numeric", length = length(files)-1),
                        "z19" = vector(mode = "numeric", length = length(files)-1),
                        "z20" = vector(mode = "numeric", length = length(files)-1),
                        "z21" = vector(mode = "numeric", length = length(files)-1),
                        "z22" = vector(mode = "numeric", length = length(files)-1),
                        "z23" = vector(mode = "numeric", length = length(files)-1),
                        "R1" = vector(mode = "numeric", length = length(files)-1),
                        "R2" = vector(mode = "numeric", length = length(files)-1),
                        "R3" = vector(mode = "numeric", length = length(files)-1))
    dfMean <- dfVar
    
    i = 1
    for(i in 1:length(files))
    {
      cat(i, "/", length(files), "\n")
      if(i == length(files)){next()}
      f1 <- files[i]
      f2 <- files[i+1]
      im01 <- load.image(paste0(subinPath, "/", f1))
      im02 <- load.image(paste0(subinPath, "/", f2))
      
      origin <- unlist(dfPos[dfPos$type == "origin", 2:5])
      im1 <- imsub(im01, x >= origin[1] & x < origin[3], y >= origin[2] & y < origin[4])
      im2 <- imsub(im02, x >= origin[1] & x < origin[3], y >= origin[2] & y < origin[4])
      
      # do reference calculation
      R1x <- unlist(dfPos[dfPos$type == "ref1", 2:5])
      R2x <- unlist(dfPos[dfPos$type == "ref2", 2:5])
      R3x <- unlist(dfPos[dfPos$type == "ref3", 2:5])
      im1R1 <- imsub(im01, x >= R1x[1] & x < R1x[3], y >= R1x[2] & y < R1x[4])
      im2R1 <- imsub(im02, x >= R1x[1] & x < R1x[3], y >= R1x[2] & y < R1x[4])
      im1R2 <- imsub(im01, x >= R2x[1] & x < R2x[3], y >= R2x[2] & y < R2x[4])
      im2R2 <- imsub(im02, x >= R2x[1] & x < R2x[3], y >= R2x[2] & y < R2x[4])
      im1R3 <- imsub(im01, x >= R3x[1] & x < R3x[3], y >= R3x[2] & y < R3x[4])
      im2R3 <- imsub(im02, x >= R3x[1] & x < R3x[3], y >= R3x[2] & y < R3x[4])
      
      R1rx <- c(unlist(abs(im2R1[,,,1] - im1R1[,,,1]))); R2rx <- c(unlist(abs(im2R2[,,,1] - im1R2[,,,1]))); R3rx <- c(unlist(abs(im2R3[,,,1] - im1R3[,,,1])))
      R1gx <- c(unlist(abs(im2R1[,,,2] - im1R1[,,,2]))); R2gx <- c(unlist(abs(im2R2[,,,2] - im1R2[,,,2]))); R3gx <- c(unlist(abs(im2R3[,,,2] - im1R3[,,,2])))
      R1bx <- c(unlist(abs(im2R1[,,,3] - im1R1[,,,3]))); R2bx <- c(unlist(abs(im2R2[,,,3] - im1R2[,,,3]))); R3bx <- c(unlist(abs(im2R3[,,,3] - im1R3[,,,3])))
      
      var_R1 <- (var(R1rx) + var(R1gx) + var(R1bx))
      var_R2 <- (var(R2rx) + var(R2gx) + var(R2bx))
      var_R3 <- (var(R3rx) + var(R3gx) + var(R3bx))
      mean_R1 <- (mean(R1rx) + mean(R1gx) + mean(R1bx)) / 3
      mean_R2 <- (mean(R2rx) + mean(R2gx) + mean(R2bx)) / 3
      mean_R3 <- (mean(R3rx) + mean(R3gx) + mean(R3bx)) / 3
      
      dfVar[i,"R1"] <- var_R1
      dfVar[i,"R2"] <- var_R2
      dfVar[i,"R3"] <- var_R3
      dfMean[i,"R1"] <- mean_R1
      dfMean[i,"R2"] <- mean_R2
      dfMean[i,"R3"] <- mean_R3
      
      # Do zones calculations
      z = 1
      for(z in zones)
      {
        zdims <- zoneDefs(z)
        if(sum(zdims) == 0){
          im1s <- im1
          im2s <- im2
          r1 <- c(im1s[1:iw,1:dims[2],,1], im1s[(dims[1]-iw+1):dims[1],1:dims[2],,1], im1s[(iw+1):(dims[1]-iw),1:ih,,1], im1s[(iw+1):(dims[1]-iw),(dims[2]-ih+1):dims[2],,1])
          r2 <- c(im2s[1:iw,1:dims[2],,1], im2s[(dims[1]-iw+1):dims[1],1:dims[2],,1], im2s[(iw+1):(dims[1]-iw),1:ih,,1], im2s[(iw+1):(dims[1]-iw),(dims[2]-ih+1):dims[2],,1])
          g1 <- c(im1s[1:iw,1:dims[2],,2], im1s[(dims[1]-iw+1):dims[1],1:dims[2],,2], im1s[(iw+1):(dims[1]-iw),1:ih,,2], im1s[(iw+1):(dims[1]-iw),(dims[2]-ih+1):dims[2],,2])
          g2 <- c(im2s[1:iw,1:dims[2],,2], im2s[(dims[1]-iw+1):dims[1],1:dims[2],,2], im2s[(iw+1):(dims[1]-iw),1:ih,,2], im2s[(iw+1):(dims[1]-iw),(dims[2]-ih+1):dims[2],,2])
          b1 <- c(im1s[1:iw,1:dims[2],,3], im1s[(dims[1]-iw+1):dims[1],1:dims[2],,3], im1s[(iw+1):(dims[1]-iw),1:ih,,3], im1s[(iw+1):(dims[1]-iw),(dims[2]-ih+1):dims[2],,3])
          b2 <- c(im2s[1:iw,1:dims[2],,3], im2s[(dims[1]-iw+1):dims[1],1:dims[2],,3], im2s[(iw+1):(dims[1]-iw),1:ih,,3], im2s[(iw+1):(dims[1]-iw),(dims[2]-ih+1):dims[2],,3])
        } else {
          zdims[1] <- zdims[1] * dims[2]/9
          zdims[2] <- zdims[2] * dims[1]/16
          zdims[3] <- zdims[3] * dims[2]/9
          zdims[4] <- zdims[4] * dims[1]/16
          im1s <- imsub(im1, x >= zdims[2] & x <= zdims[4], y >= zdims[1] & y <= zdims[3])
          im2s <- imsub(im2, x >= zdims[2] & x <= zdims[4], y >= zdims[1] & y <= zdims[3])
          r1 <- im1s[,,,1]
          r2 <- im2s[,,,1]
          g1 <- im1s[,,,2]
          g2 <- im2s[,,,2]
          b1 <- im1s[,,,3]
          b2 <- im2s[,,,3]
        }
        
        rd <- abs(r2 - r1)
        gd <- abs(g2 - g1)
        bd <- abs(b2 - b1)
        rx <- c(unlist(rd))
        gx <- c(unlist(gd))
        bx <- c(unlist(bd))
        
        dd <- (rd + gd + bd)/3
        
        var_frame <- (var(rx) + var(gx) + var(bx))
        mean_frame <- (mean(rx) + mean(gx) + mean(bx)) / 3
        dfVar[i,z+1] <- var_frame
        dfMean[i,z+1] <- mean_frame
        
        # Motiongrams
        if(z == 1){
          MGX[,i] <- rowMeans(dd) - ((mean_R1+mean_R3+mean_R3)/3)
          MGY[,i] <- colMeans(dd) - ((mean_R1+mean_R3+mean_R3)/3)
        }
        
        if(z == 1 & printit == TRUE){
          rb <- (r2 - r1)
          gb <- (g2 - g1)
          bb <- (b2 - b1)
          ddb <- (rb + gb + bb)/3
          ddp <- ddb
          ddn <- ddb
          ddp[ddp < 0] <- 0
          ddn[ddn > 0] <- 0
          ddn <- 1 - (ddn + 1)
          
          Mpn <- round(as.data.frame(rbind(ddp, ddn)),7)
          data.table::setDT(Mpn)
          fwrite(Mpn, paste0(suboutPath, "/frames/posneg_",i,".csv"))
          
          ### If I want t osave as picture (slower but lower file size)
          # imdp <- as.cimg(ddp) ; # imd[1:3,1:3]
          # imdn <- as.cimg(ddn) ; # imd[1:3,1:3]
          # imp <- cimg(array(c(rep(0,2073600), rep(1,2073600), rep(0,2073600)),c(1920,1080,1,3)))
          # imn <- cimg(array(c(rep(1,2073600), rep(0,2073600), rep(0,2073600)),c(1920,1080,1,3)))
          # imp.a <- imlist(imp,imdp) %>% imappend("c")
          # imn.a <- imlist(imn,imdn) %>% imappend("c")
          # save.image(imp.a, paste0(outPath, "/pos/frame_",i,".png"))
          # save.image(imn.a, paste0(outPath, "/neg/frame_",i,".png"))
          
          # imd <- as.cimg(dd)
          # save.image(imd, paste0(outPath, "/frames/frame_",i,".png"))
        }
        gc()
      }
      gc()
    }
    MGX <- as.data.frame(MGX)
    MGY <- as.data.frame(MGY)
    setDT(dfMean); setDT(dfVar); setDT(MGX); setDT(MGY); 
    fwrite(dfMean, paste0(suboutPath, "/dfMeans.csv"))
    fwrite(dfVar, paste0(suboutPath, "/dfVar.csv"))
    fwrite(MGX, paste0(suboutPath, "/dfMGX.csv"))
    fwrite(MGY, paste0(suboutPath, "/dfMGY.csv"))
  }
  #lOut <- list(dfMeans, dfVar, MGX, MGY)
  # return(lOut)
}
