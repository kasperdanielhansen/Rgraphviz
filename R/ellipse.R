### Written by Sundar Dorai-Raj, sundar.dorai-raj@pdf.com

# begin ellipse
ellipse <- function(x,y,
                    width,height=width,theta=2*pi,
                    npoints=100,plot=TRUE, fg=par("fg"),
                    bg=par("bg")) {
  # x = x coordinate of center
  # y = y coordinate of center
  # width = length of major axis
  # height = length of minor axis
  # theta = rotation
  # npoints = number of points to send to polygon
  # plot = if TRUE, add to current device
  # = if FALSE, return list of components
  # bg - color to fill the ellipse with
  a <- width/2
  b <- height/2
  xcoord <- seq(-a,a,length=npoints)
  ycoord.neg <- sqrt(b^2*(1-(xcoord)^2/a^2))
  ycoord.pos <- -sqrt(b^2*(1-(xcoord)^2/a^2))
  xx <- c(xcoord,xcoord[npoints:1])
  yy <- c(ycoord.neg,ycoord.pos)
  x.theta <- xx*cos(2*pi-theta)+yy*sin(2*pi-theta)+x
  y.theta <- yy*cos(2*pi-theta)-xx*sin(2*pi-theta)+y
  if(plot)
    invisible(polygon(x.theta,y.theta, density=NA, border=fg, col=bg))
  else
    invisible(list(coords=data.frame(x=x.theta,y=y.theta),
                   center=c(x,y),
                   theta=theta))
}
# end ellipse
