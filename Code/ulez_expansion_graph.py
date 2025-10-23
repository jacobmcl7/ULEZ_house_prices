# This script makes and exports the expansion of the ULEZ over time

import arcpy

# Import the ULEZ shapefiles
ULEZ_2023 = r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers\ULEZ on house prices\Data\Input\London-wide_ULEZ_expansion\LEZ_Boundary_20071113.shp"
ULEZ_2023 = arcpy.management.MakeFeatureLayer(ULEZ_2023, "2023 ULEZ expansion")

ULEZ_2021 = r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers\ULEZ on house prices\Data\Input\InnerUltraLowEmissionZone\InnerUltraLowEmissionZone.shp"
ULEZ_2021 = arcpy.management.MakeFeatureLayer(ULEZ_2021, "2021 ULEZ expansion")

ULEZ_2019 = r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers\ULEZ on house prices\Data\Input\ULEZCentral_CongestionChargingZone\Shapefile\UltraLowEmissionsZoneBoundary(ULEZ).shp"
ULEZ_2019 = arcpy.management.MakeFeatureLayer(ULEZ_2019, "2019 ULEZ implementation")

# Now get the borders of the ULEZs
ULEZ_2023_border = arcpy.management.PolygonToLine(ULEZ_2023, "ULEZ_2023_border")
ULEZ_2021_border = arcpy.management.PolygonToLine(ULEZ_2021, "ULEZ_2021_border")
ULEZ_2019_border = arcpy.management.PolygonToLine(ULEZ_2019, "ULEZ_2019_border")


# Now do the rest manually:
# 1. Choose appropriate colours for each ULEZ
# 2. Open a new layout
# 3. Copy over the map into the layout, with the appropriate zoom, using Map Frame
# 4. Add a legend, north arrow, scale bar and customise them
# 5. Export the map as a .pdf, to 'ulez_expansion_graph.pdf' in the 'Figures' folder within the 'Output' folder