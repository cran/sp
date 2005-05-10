
.shp2srs <- function(shp, nParts, proj4string=CRS(as.character(NA))) {
	Pstart <- shp$Pstart
	nVerts <- nrow(shp$verts)
	from <- integer(nParts)
	to <- integer(nParts)
	from[1] <- 1
	for (j in 1:nParts) {
		if (j == nParts) to[j] <- nVerts
		else {
			to[j] <- Pstart[j+1]
			from[j+1] <- to[j]+1
		}
	}
	srl <- vector(mode="list", length=nParts)
	for (j in 1:nParts) {
		srl[[j]] <- Sring(coords=shp$verts[from[j]:to[j],], 
			proj4string=proj4string)
	}
	srl
}

nParts.shp <- function(shp) attr(shp, "nParts")


.NAmat2xyList <- function(xy) {
	NAs <- unclass(attr(na.omit(xy), "na.action"))
	if ((length(NAs) == 1) && (NAs == nrow(xy))) {
		xy <- xy[-nrow(xy)]
		NAs <- NULL
	}
	nParts <- length(NAs) + 1
	res <- vector(mode="list", length=nParts)
	from <- integer(nParts)
	to <- integer(nParts)
	from[1] <- 1
	to[nParts] <- nrow(xy)
	if (nParts > 1) {
		for (i in 2:nParts) {
			to[(i-1)] <- NAs[(i-1)]-1
			from[i] <- NAs[(i-1)]+1
		}
	}
	for (i in 1:nParts) res[[i]] <- xy[from[i]:to[i],]
	res
}


.ringDirxy <- function(xy) {
	a <- xy[,1]
	b <- xy[,2]
	nvx <- length(b)

	if((a[1] == a[nvx]) && (b[1] == b[nvx])) {
		a <- a[-nvx]
		b <- b[-nvx]
		nvx <- nvx - 1
	}
	if (nvx < 3) return(1)

	tX <- 0.0
	dfYMax <- max(b)
	ti <- 1
	for (i in 1:nvx) {
		if (b[i] == dfYMax && a[i] > tX) ti <- i
	}
	if ( (ti > 1) & (ti < nvx) ) { 
		dx0 = a[ti-1] - a[ti]
      		dx1 = a[ti+1] - a[ti]
      		dy0 = b[ti-1] - b[ti]
      		dy1 = b[ti+1] - b[ti]
   	} else if (ti == nvx) {
		dx0 = a[ti-1] - a[ti]
      		dx1 = a[1] - a[ti]
      		dy0 = b[ti-1] - b[ti]
      		dy1 = b[1] - b[ti]
   	} else {
#   /* if the tested vertex is at the origin then continue from 0 (1) */ 
     		dx1 = a[2] - a[1]
      		dx0 = a[nvx] - a[1]
      		dy1 = b[2] - b[1]
      		dy0 = b[nvx] - b[1]
   	}
	v3 = ( (dx0 * dy1) - (dx1 * dy0) )
	if ( v3 > 0 ) return(as.integer(1))
   	else return(as.integer(-1))
}

.bboxSlot <- function(x) {
	r1 <- range(x[,1], na.rm=TRUE)
	r2 <- range(x[,2], na.rm=TRUE)
	res <- rbind(r1, r2)
	colnames(res) <- c("min", "max")
	res
}

.bboxR4s <- function(R4s) {
	x <- sapply(R4s, function(x) bbox.R4(x)[1,])
	y <- sapply(R4s, function(x) bbox.R4(x)[2,])
	r1 <- range(x, na.rm=TRUE)
	r2 <- range(y, na.rm=TRUE)
	res <- rbind(r1, r2)
	colnames(res) <- c("min", "max")
	res

}

.bboxSrs <- function(R4s) {
	x <- sapply(R4s, function(x) bbox.R4(x)[1,])
	y <- sapply(R4s, function(x) bbox.R4(x)[2,])
	r1 <- range(x, na.rm=TRUE)
	r2 <- range(y, na.rm=TRUE)
	res <- rbind(r1, r2)
	colnames(res) <- c("min", "max")
	res

}

bbox.R4 <- function(x) {
	x@bbox
}

.bbox1 <- function(x) {
	r1 <- range(x[,1], na.rm=TRUE)
	r2 <- range(x[,2], na.rm=TRUE)
	res <- c(r1[1], r2[1], r1[2], r2[2])
	res
}

.bbox2 <- function(x) {
	r1 <- range(x@coords[,1], na.rm=TRUE)
	r2 <- range(x@coords[,2], na.rm=TRUE)
	res <- c(r1[1], r2[1], r1[2], r2[2])
	res
}


.saneRD <- function(rD) {
	if (length(rD) == 0) stop("Not a valid polygon: rD length 0")
	if (any(is.na(rD))) stop("Not a valid polygon: NA rD value")
	if (any(abs(rD) != 1)) stop("Not a valid polygon: abs(rD) != 1")
	invisible(NULL)
}

.RingCentrd_2d <- function(plmat) {
	nVert <- nrow(plmat)
	x_base <- plmat[1,1]
	y_base <- plmat[1,2]
	Cy_accum <- 0.0
	Cx_accum <- 0.0
	Area <- 0.0
	ppx <- plmat[2,1] - x_base
	ppy <- plmat[2,2] - y_base
	for (iv in 2:(nVert-1)) {
		x = plmat[iv,1] - x_base
		y = plmat[iv,2] - y_base
		dx_Area <-  ((x * ppy) - (y * ppx)) * 0.5
		Area <- Area + dx_Area
		Cx_accum <- Cx_accum + ( ppx + x ) * dx_Area      
		Cy_accum <- Cy_accum + ( ppy + y ) * dx_Area
		ppx <- x
		ppy <- y
	}
	xc <- (Cx_accum / (Area * 3)) + x_base
	yc <- (Cy_accum / (Area * 3)) + y_base
	list(xc=xc, yc=yc, area=abs(Area))	
}


