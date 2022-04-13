import arcpy
from arcpy import env

#Roads.tif from https://doi.org/10.1038/sdata.2016.67

# set env
arcpy.env.workspace = "..."
# project to wgs84
roads = arcpy.management.ProjectRaster("Roads.tif", r"roads_wgs84", 'GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]', "NEAREST", "9,72073312788084E-03 9,72073312788084E-03", None, None, 'PROJCS["World_Mollweide",GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Mollweide"],PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",0.0],UNIT["Meter",1.0]]', "NO_VERTICAL")
# convert to integer
int_roads = arcpy.ia.Int(roads)
# make polylines
roads_lns = arcpy.conversion.RasterToPolyline(int_roads, r"roads_lines", "ZERO", 0, "SIMPLIFY", "Value")
# calculate line density in 0.5 degree cells
out_raster = arcpy.sa.LineDensity(roads_lns, "NONE", "0.5", 1, "SQUARE_KILOMETERS"); out_raster.save(r"road_density")


