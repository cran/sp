	CRS,
	identicalCRS,
	# print.CRS, -> S3Method()

	# bbox,
	getGridIndex,
	points2grid,
	bpy.colors,
	bubble,
	mapasp,
	point.in.polygon,
	spmap.to.lev,
	zerodist,
	zerodist2,
	remove.duplicates,
	degAxis,
	"%over%",

	get_ll_warn,
	set_ll_warn,
	get_ll_TOL,
	set_ll_TOL,
	get_ReplCRS_warn,
	set_ReplCRS_warn,
	get_Polypath,
	set_Polypath,
	get_PolypathRule, 
	set_PolypathRule,

	coordinatevalues,

	dimensions,

	Spatial,
 
	SpatialPoints,
	SpatialPointsDataFrame,

	Line,
	Lines,
	SpatialLines,
	SpatialLinesDataFrame,
	as.SpatialLines.SLDF,
	gridat,
	LineLength,
	LinesLength,
	SpatialLinesLengths,

	layout.scale.bar,
	layout.north.arrow,
	sp.theme,
	mapLegendGrob,
	spplot.key,
	panel.gridplot,
	panel.polygonsplot,
	panel.pointsplot,
	sp.polygons,
	sp.text,
	sp.points,
	sp.lines,
	sp.grid,
	SpatialPolygonsRescale,
	spplot.locator,
	sp.panel.layout,

	as.image.SpatialGridDataFrame,
	image2Grid,
	
	gridparameters,
	SpatialPixels,
	SpatialPixelsDataFrame,
	SpatialGrid,
	SpatialGridDataFrame,
	GridTopology,
	getGridTopology,
	areaSpatialGrid,
	gridlines,
	gridat,
	gridIndex2nb,

	getSLlinesSlot,
	getLinesLinesSlot, 
	getLinesIDSlot,
	getSLLinesIDSlots,

	nowrapSpatialLines,
	getSpatialLinesMidPoints,

	getPolygonCoordsSlot,
	getPolygonLabptSlot,
	getPolygonAreaSlot,
	getPolygonHoleSlot,
	getPolygonsPolygonsSlot,
	getPolygonsplotOrderSlot,
	getPolygonsLabptSlot,
	getPolygonsIDSlot,
	getSpPpolygonsSlot,
	getSpPplotOrderSlot,
	getSpPPolygonsLabptSlots,
	getSpPPolygonsIDSlots,
	getSpPnParts,
	getSpPnHoles,
	getSpatialPolygonsLabelPoints,

	select.spatial,

	as.SpatialPolygons.PolygonsList,
  # as.SpatialPolygonsDataFrame.SpatialPolygons, -> use coerce()

  # DMS:
	dd2dms, 
	print.DMS, 
	char2dms, 
	as.character.DMS,
	as.double.DMS, 
	as.numeric.DMS,

	# is coerce, but needed to add proj4string:
	as.SpatialPolygons.GridTopology, 

	# as.SpatialPolygons.SpatialPixels, -> is a coerce(), but plotKML uses it
	IDvaluesGridTopology, 
	IDvaluesSpatialPixels, 
	HexPoints2SpatialPolygons,

	flipHorizontal,
	flipVertical,

	loadMeuse,

	makegrid,

	read.asciigrid, 

	Polygon,
	Polygons,

	rbind.SpatialPoints,
	rbind.SpatialPointsDataFrame, 
	rbind.SpatialPixels,
	rbind.SpatialPixelsDataFrame,
	rbind.SpatialPolygons,
	rbind.SpatialPolygonsDataFrame,
	rbind.SpatialLines,
	rbind.SpatialLinesDataFrame,

	# sample.Spatial, -> coerce()...
	# sample.Line,
	# sample.Polygon,
	# sample.Polygons,
	# sample.Sgrid,

	# ShowSpatialPointsDataFrame -> coerce()...

	SpatialPolygons,
	SpatialPolygonsDataFrame,

	spDistsN1,
	spDists,

	write.asciigrid
