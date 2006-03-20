[R] image() and non-well-ordered colours
Paul Murrell p.murrell at auckland.ac.nz
Wed May 25 00:52:07 CEST 2005

    * Previous message: [R] image() and non-well-ordered colours
    * Next message: [R] Tracking progress during iterations
    * Messages sorted by: [ date ] [ thread ] [ subject ] [ author ]

Hi


Robin Hankin wrote:
> Hi.
> 
> I want to use image() with colours that are indexed by two variables.
> Indexing by one variable is easy:
> 
> library(colorspace)
> x <- seq(from=0, to=1,len=30)
> z <- outer(x,1i*x,"+")
> image(Re(z),col=hcl(seq(from=0,to=100,len=15),c=100))
> 
> OK, so far so good.  Now, I want the colour to be a more complicated 
> function
> of z, in which both the hue and luminance change (thus the colours cannot
> be ordered):
> 
> 
> f <- function(z){hcl(h=100*Re(z),l=100*Im(z))}
> 
> I want to draw z in terms of the colour defined by f():
> 
> image(z,col=f)
> image(f(z))
> 
> but these don't work as intended.  How do I use image() to get what I want?
> I can get close using plot():
> 
> x <- runif(1000)
> y <- (1:1000)/10
> g <- function(x){hcl(h=80*x,l=(1:1000)/10,c=300)}
> plot(x,y,col=g(x),pch=16)
> 
> [note that one cannot draw nontrivial "contour lines"  joining points of 
> identical colours on this
> plot: top left to lower right goes from pink to black; top right to low 
> left goes from yellow to reddy orange]
> 
> 
> It'd be nice to make image() do what I want. Anyone?


How about a brute-force approach using rect() ... ?

x <- seq(from=0, to=1,len=30)
z <- outer(x,1i*x,"+")

f <- function(z){hcl(h=100*Re(z),l=100*Im(z))}

plot(x, x, type="n")
step <- diff(x)[1]/2
xmid <- rep(x, 30)
ymid <- rep(x, each=30)
rect(xmid - step, ymid - step,
      xmid + step, ymid + step,
      col=f(z), border=NA)
box()

Paul
-- 
Dr Paul Murrell
Department of Statistics
The University of Auckland
Private Bag 92019
Auckland
New Zealand
64 9 3737599 x85392
paul at stat.auckland.ac.nz
http://www.stat.auckland.ac.nz/~paul/


