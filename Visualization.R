library(magrittr)
library(dplyr)
library(MASS)
library(RgoogleMaps)
library(RColorBrewer)

# load the data and subset those whose complaint type is traffic
load("final_df.Rdata")
traffic = subset(final.df,Complaint.Type=="Traffic")

# addalpha()
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

# colorRampPaletteAlpha()
colorRampPaletteAlpha <- function(colors, n=32, interpolate='linear') {
  # Create the color ramp normally
  cr <- colorRampPalette(colors, interpolate=interpolate)(n)
  # Find the alpha channel
  a <- col2rgb(colors, alpha=T)[4,]
  # Interpolate
  if (interpolate=='linear') {
    l <- approx(a, n=n)
  } else {
    l <- spline(a, n=n)
  }
  l$y[l$y > 255] <- 255 # Clamp if spline is > 255
  cr <- addalpha(cr, l$y/255.0)
  return(cr)
}

# Keep the lon and lat data
rawdata <- data.frame(as.numeric(traffic$longitude), as.numeric(traffic$latitude))
names(rawdata) <- c("lon", "lat")
data <- as.matrix(rawdata)

# Rotate the lat-lon coordinates using a rotation matrix
# Trial and error lead to pi/15.0 = 12 degrees
theta = pi/15.0
m = matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow=2)
data <- as.matrix(data) %*% m

# Reproduce William's original map
par(bg='black')
plot(data, cex=0.1, col="white", pch=16)

# Create heatmap with kde2d and overplot
k <- kde2d(data[,1], data[,2], n=500)
# Intensity from green to red
cols <- rev(colorRampPalette(brewer.pal(8, 'RdYlGn'))(100))
par(bg='white')
image(k, col=cols, xaxt='n', yaxt='n')
points(data, cex=0.1, pch=16)

# Mapping via RgoogleMaps
# Find map center and get map
center <- rev(sapply(rawdata, mean))
map <- GetMap(center=center, zoom=11)
# Translate original data
coords <- LatLon2XY.centered(map, rawdata$lat, rawdata$lon, 11)
coords <- data.frame(coords)

# Rerun heatmap
k2 <- kde2d(coords$newX, coords$newY, n=500)

# Create exponential transparency vector and add
alpha <- seq.int(0.5, 0.95, length.out=100)
alpha <- exp(alpha^6-1)
cols2 <- addalpha(cols, alpha)

# Prepare saving the plot
png("traffic.png")

# Plot
PlotOnStaticMap(map)
image(k2, col=cols2, add=T)
points(coords$newX, coords$newY, pch=16, cex=0.3)

#save the plot3
dev.off()
