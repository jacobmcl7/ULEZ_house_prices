# This script draws the regions containing the houses used in the analysis

# It must be run within ArcGIS in a project titled 'analysis_zone_graph'

import arcpy

# Import the ULEZ shapefiles
ULEZ_2023 = r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers\ULEZ on house prices\Data\Input\London-wide_ULEZ_expansion\LEZ_Boundary_20071113.shp"
ULEZ_2023 = arcpy.management.MakeFeatureLayer(ULEZ_2023, "2023 ULEZ expansion")

ULEZ_2021 = r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers\ULEZ on house prices\Data\Input\InnerUltraLowEmissionZone\InnerUltraLowEmissionZone.shp"
ULEZ_2021 = arcpy.management.MakeFeatureLayer(ULEZ_2021, "2021 ULEZ expansion")

ULEZ_2019 = r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers\ULEZ on house prices\Data\ULEZCentral_CongestionChargingZone\Shapefile\UltraLowEmissionsZoneBoundary(ULEZ).shp"
ULEZ_2019 = arcpy.management.MakeFeatureLayer(ULEZ_2019, "2019 ULEZ implementation")


# Now get the borders of the ULEZs
ULEZ_2023_border = arcpy.management.PolygonToLine(ULEZ_2023, "ULEZ_2023_border")
ULEZ_2021_border = arcpy.management.PolygonToLine(ULEZ_2021, "ULEZ_2021_border")
ULEZ_2019_border = arcpy.management.PolygonToLine(ULEZ_2019, "ULEZ_2019_border")


# The 2023 region is a bit weird because it is formed of one main region and lots of smaller ones - to make future commands work, dissolve the zone and the border into one zone and one border respectively
arcpy.analysis.PairwiseDissolve(
    in_features="2023 ULEZ expansion",
    out_feature_class=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\analysis_zone_graph\analysis_zone_graph.gdb\ULEZ2023dissolve",
    dissolve_field=None,
    statistics_fields=None,
    multi_part="MULTI_PART",
    concatenation_separator=""
)


# Make 5km buffers of each of the ULEZ zones, as well as a 10km buffer for the 2023 one
arcpy.analysis.Buffer(
    in_features="2019 ULEZ implementation",
    out_feature_class=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\analysis_zone_graph\analysis_zone_graph.gdb\ULEZ2019buffer5km",
    buffer_distance_or_field="5 Kilometers",
    line_side="FULL",
    line_end_type="ROUND",
    dissolve_option="NONE",
    dissolve_field=None,
    method="PLANAR"
)

arcpy.analysis.Buffer(
    in_features="2021 ULEZ expansion",
    out_feature_class=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\analysis_zone_graph\analysis_zone_graph.gdb\ULEZ2021buffer5km",
    buffer_distance_or_field="5 Kilometers",
    line_side="FULL",
    line_end_type="ROUND",
    dissolve_option="NONE",
    dissolve_field=None,
    method="PLANAR"
)

arcpy.analysis.Buffer(
    in_features="ULEZ2023dissolve",
    out_feature_class=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\analysis_zone_graph\analysis_zone_graph.gdb\ULEZ2023buffer5km",
    buffer_distance_or_field="5 Kilometers",
    line_side="FULL",
    line_end_type="ROUND",
    dissolve_option="NONE",
    dissolve_field=None,
    method="PLANAR"
)

arcpy.analysis.Buffer(
    in_features="ULEZ2023dissolve",
    out_feature_class=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\analysis_zone_graph\analysis_zone_graph.gdb\ULEZ2023buffer10km",
    buffer_distance_or_field="10 Kilometers",
    line_side="FULL",
    line_end_type="ROUND",
    dissolve_option="NONE",
    dissolve_field=None,
    method="PLANAR"
)


# Now remove the buffers from the ULEZ within it, as well as removing the 5km 2023 ULEZ buffer from the 10km one within it
arcpy.analysis.Erase(
    in_features="2021 ULEZ expansion",
    erase_features="ULEZ2019buffer5km",
    out_feature_class=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\analysis_zone_graph\analysis_zone_graph.gdb\ULEZ2021analysiszone",
    cluster_tolerance=None
)

arcpy.analysis.Erase(
    in_features="ULEZ2023dissolve",
    erase_features="ULEZ2021buffer5km",
    out_feature_class=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\analysis_zone_graph\analysis_zone_graph.gdb\ULEZ2023analysiszone",
    cluster_tolerance=None
)

arcpy.analysis.Erase(
    in_features="ULEZ2023buffer10km",
    erase_features="ULEZ2023buffer5km",
    out_feature_class=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\analysis_zone_graph\analysis_zone_graph.gdb\ULEZcontrolzone",
    cluster_tolerance=None
)


# Now do the rest manually:
# 1. Choose appropriate colours for each zone and delete the irrelevant polygons used in their construction
# 2. Change their names manually to '2021 Analysis Zone', '2023 Analysis Zone' and 'Control Zone'
# 3. Change border properties of the zones and the borders in the Symbology tab
# 4. Open a new layout
# 5. Copy over the map into the layout, with the appropriate zoom, using Map Frame
# 6. Add a legend, north arrow, scale bar and customise them
# 7. Export the map as a .pdf, to 'analysis_zone_graph.pdf' in the 'Figures' folder within the 'Output' folder