library(tidyverse)
library(imager)
library(magick)
library(data.table)

# Gather position file
MakePos <- function(inPath, outPath)
{
  folders <- list.files(inPath)
  readline(paste0(length(folders), " folder(s) were found, Press [enter] to continue..."))
  
  cat("Defining origin points for all folders", "\n")
  f = folders[1]
  for(f in folders)
  {
    # Storing origin points for all pictures
    dfO <- data.frame("folder" = f, "x0" = vector(mode = "numeric", length = length(f)), "y0" = vector(mode = "numeric", length = length(f)), "x1" = vector(mode = "numeric", length = length(f)), "y1" = vector(mode = "numeric", length = length(f)))
    dfR1 <- data.frame("folder" = f, "x0" = vector(mode = "numeric", length = length(f)), "y0" = vector(mode = "numeric", length = length(f)), "x1" = vector(mode = "numeric", length = length(f)), "y1" = vector(mode = "numeric", length = length(f)))
    dfR2 <- data.frame("folder" = f, "x0" = vector(mode = "numeric", length = length(f)), "y0" = vector(mode = "numeric", length = length(f)), "x1" = vector(mode = "numeric", length = length(f)), "y1" = vector(mode = "numeric", length = length(f)))
    dfR3 <- data.frame("folder" = f, "x0" = vector(mode = "numeric", length = length(f)), "y0" = vector(mode = "numeric", length = length(f)), "x1" = vector(mode = "numeric", length = length(f)), "y1" = vector(mode = "numeric", length = length(f)))
    dfQ <- data.frame("folder" = f, "w" = vector(mode = "numeric", length = length(f)), "h" = vector(mode = "numeric", length = length(f)))
    
    ifile <- list.files(paste0(inPath, "/", f, "/"))[1]
    suboutPath <- paste0(outPath, "/", f)
    dir.create(suboutPath)
    
    imx <- load.image(paste0(inPath, "/", f, "/",ifile))
    readline("Press [enter] choose cropping frame for next picture...")
    
    resp1 = "n"
    while(resp1 == "n"){
      cat("Defining origin for this folder:", "\n")
      O <- grabRect(imx)
      cat("Defining first reference zone for this folder:", "\n")
      R1 <- grabRect(imx)
      cat("Defining second reference zone for this folder:", "\n")
      R2 <- grabRect(imx)
      cat("Defining third reference zone for this folder:", "\n")
      R3 <- grabRect(imx)
      
      # Making it divisible by 16 or 9
      w <- round((O[3]-O[1])/16)
      h <- round((O[4]-O[2])/9)
      O[3] <- O[1] + round((O[3]-O[1])/16)*16
      O[4] <- O[2] + round((O[4]-O[2])/9)*9
      
      cat("Printing final picture", "\n")
      png(filename = paste0(suboutPath, "/", f, ".png"), width = 1920, height = 1080, units = "px")
      par(mar = c(0,0,0,0), oma = c(0,0,0,0), bty = "O")
      plot(imx, axes = FALSE, xlim = c(0,3840), ylim = c(2160,0))
      arrows(x0 = seq(O[1], O[3], w), x1 = seq(O[1], O[3], w), y0 = O[2], y1 = O[4], col = "gray", length = 0)
      arrows(y0 = seq(O[2], O[4], h), y1 = seq(O[2], O[4], h), x0 = O[1], x1 = O[3], col = "gray", length = 0)
      polygon(x = c(O[1], O[1], O[3], O[3]), y = c(O[2], O[4], O[4], O[2]), border = "red", lwd = 3)
      polygon(x = c(R1[1], R1[1], R1[3], R1[3]), y = c(R1[2], R1[4], R1[4], R1[2]), border = "blue", lwd = 3)
      polygon(x = c(R2[1], R2[1], R2[3], R2[3]), y = c(R2[2], R2[4], R2[4], R2[2]), border = "blue", lwd = 3)
      polygon(x = c(R3[1], R3[1], R3[3], R3[3]), y = c(R3[2], R3[4], R3[4], R3[2]), border = "blue", lwd = 3)
      text(x = (R1[1]+R1[3])/2, y = (R1[2]+R1[4])/2, labels = "R1", cex = 3, col = "blue", font = 4)
      text(x = (R2[1]+R2[3])/2, y = (R2[2]+R2[4])/2, labels = "R2", cex = 3, col = "blue", font = 4)
      text(x = (R3[1]+R3[3])/2, y = (R3[2]+R3[4])/2, labels = "R3", cex = 3, col = "blue", font = 4)
      dev.off()
      
      resp1 <- readline("Continue (y) or redefine origin (n)? (y/n)")
    }
    
    dfR1[dfR1$folder == f,2:5] <- R1
    dfR2[dfR2$folder == f,2:5] <- R2
    dfR3[dfR3$folder == f,2:5] <- R3
    dfO[dfO$folder == f,2:5] <- O
    dfQ[dfQ$folder == f,2:3] <- c(w,h)
    
    dfO$type <- "origin" ; dfR1$type <- "ref1" ; dfR2$type <- "ref2" ; dfR3$type <- "ref3"
    dfPos <- rbind(dfO, dfR1)
    dfPos <- rbind(dfPos, dfR2)
    dfPos <- rbind(dfPos, dfR3)
    dfPos <- merge(dfPos, dfQ, by = "folder")
    dfPos$inPath <- inPath
    dfPos$outPath <- suboutPath
    setDT(dfPos); fwrite(dfPos, paste0(suboutPath, "/dfPos.csv"))
    
    # ims <- imsub(imx, x >= O[1] & x < O[1]+1920, y >= O[2] & y < O[2]+1080)
    # w <- dim(ims)[1]
    # h <- dim(ims)[2]
    # cw <- 120
    # ch <- 120
    # nw <- w / cw
    # nh <- h / ch
    # 
    # plot(ims)
    # abline(v = seq(0,w,cw), col = "gray")
    # abline(h = seq(0,h,ch), col = "gray")
    
    if(f == folders[length(folders)]){
      cat("Finalizing...", "\n")
    } else {
      cat("Doing next picture...", "\n")
    }
  }
}
