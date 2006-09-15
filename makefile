Sdest       = Splus
Rdir        = R
SEXEC       = Splus
TMPFILE		= $(Sdest)/tmpfile

Sfiles      = $(wildcard $(Sdir)/*.q)
Rfiles      = $(wildcard $(Rdir)/*.R)

S-Plus:
	make S
	make c
	make stage2

S:	
	(cd R; cat AAA.R Class-CRS.R CRS-methods.R Class-Spatial.R Spatial-methods.R projected.R Class-SpatialPoints.R SpatialPoints-methods.R Class-AttributeList.R AttributeList-methods.R Class-SpatialPointsDataFrame.R SpatialPointsDataFrame-methods.R Class-GridTopology.R Class-SpatialGrid.R Class-SpatialGridDataFrame.R Class-SpatialLines.R SpatialLines-methods.R Class-SpatialLinesDataFrame.R SpatialLinesDataFrame-methods.R Class-SpatialPolygons.R Class-SpatialPolygonsDataFrame.R SpatialPolygons-methods.R SpatialPolygonsDataFrame-methods.R GridTopology-methods.R SpatialGrid-methods.R SpatialGridDataFrame-methods.R SpatialPolygons-internals.R point.in.polygon.R SpatialPolygons-displayMethods.R zerodist.R image.R stack.R bpy.colors.R bubble.R mapasp.R select.spatial.R gridded.R asciigrid.R spplot.R overlay.R spsample.R recenter.R dms.R | sed 's/_/\./g' | sed 's/^#S//g' > ../$(TMPFILE))
	cat data/*R | sed 's/_/./g' >> $(TMPFILE)
	./scripts/preproc.pl -v SP5 $(TMPFILE) > $(Sdest)/all.q

c:
	./scripts/preproc.pl -v SP5 ./src/pip.c | tail +2 > $(Sdest)/pip.c
	./scripts/preproc.pl -v SP5 ./src/gcdist.c | tail +2 > $(Sdest)/gcdist.c
	./scripts/preproc.pl -v SP5 ./src/Rcentroid.c | tail +2 > $(Sdest)/Rcentroid.c
	./scripts/preproc.pl -v SP5 ./src/sp.h > $(Sdest)/sp.h
	
stage2:
	(cd $(Sdest); $(SEXEC) CHAPTER)
	rm -f $(Sdest)/.Data/.Audit
	(cd $(Sdest); $(SEXEC) make > make.out 2>&1)
	make S-test

S-test:
	echo "options(echo=T)" > $(Sdest)/tests.pass
	tail -q -n +3 tests/[^f]*.R >> $(Sdest)/tests.pass
	echo "options(echo=T)" > $(Sdest)/tests.fail
	tail -q -n +3 tests/f*.R >> $(Sdest)/tests.fail
	(cd $(Sdest); $(SEXEC) < tests.fail > tests.fail.out 2>&1 )
	(cd $(Sdest); $(SEXEC) < tests.pass > tests.pass.out 2>&1 )

clean:
	rm -f $(Rdir)/*.R ; rm -f src/*.c src/*.o src/*.so

veryclean:
	rm -fr $(Sdest)
