# Copyright (c) 2003-4 by Barry Rowlingson and Roger Bivand

setClass("CRS", representation(projargs = "character"),
    		prototype = list(projargs = character(1)))
