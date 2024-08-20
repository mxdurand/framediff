### Zone definitions ############################
# 1: Whole frame
# 2: Whole frame without edge (- 1 row/column) 
# 3: Edge only
# 4: Left side (7x7)
# 5: Right side (7x7)
# 6: Left top left (3x3) [left side]
# 7: Left top middle (3x3) [left side]
# 8: Left top right (3x3) [left side]
# 9: Middle middle left (3x3) [left side]
# 10: Middle middle middle (3x3) [left side]
# 11 Middle middle right (3x3) [left side]
# 12: Left bottom left (3x3) [left side]
# 13: Left middle left (3x3) [left side]
# 14: Left bottom right (3x3) [left side]
# 15: Left top left (3x3) [right side]
# 16: Left top middle (3x3) [right side]
# 17: Left top right (3x3) [right side]
# 18: Middle middle left (3x3) [right side]
# 19: Middle middle middle (3x3) [right side]
# 20 Middle middle right (3x3) [right side]
# 21: Left bottom left (3x3) [right side]
# 22: Left middle left (3x3) [right side]
# 23: Left bottom right (3x3) [right side]
################################################

# Define zones
zoneDefs <- function(zone)
{
  dfZ <- data.frame(
    "y0" = c(0,1,0,1,1,1,1,1,3,3,3,5,5,5,1,1,1,3,3,3,5,5,5),
    "x0" = c(0,1,0,1,8,1,3,5,1,3,5,1,3,5,8,10,12,8,10,12,8,10,12),
    "y1" = c(9,8,0,8,8,4,4,4,6,6,6,8,8,8,4,4,4,6,6,6,8,8,8),
    "x1" = c(16,15,0,8,15,4,6,8,4,6,8,4,6,8,11,13,15,11,13,15,11,13,15))
  
  # plot(-500, xlim = c(-1,17), ylim = c(10,-1))
  # i = 7
  # for(i in 1:nrow(dfZ))
  # {
  #   y0 <- dfZ[i,"y0"]
  #   x0 <- dfZ[i,"x0"]
  #   y1 <- dfZ[i,"y1"]
  #   x1 <- dfZ[i,"x1"]
  #   polygon(x = c(x0,x0,x1,x1), y = c(y0,y1,y1,y0), border = i)
  #   readline("press yo mama")
  # }
  
  zonedims <- unlist(dfZ[zone,]) 
  return(zonedims)
}
