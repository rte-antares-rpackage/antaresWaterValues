
# The goal of the following lines is only to remove many useless warnings in 
# R CMD CHECK: "no visible binding for global variable 'XXX'".
# They come from the use of the data.table syntax.
# (from antaresRead)
utils::globalVariables(
  c("timeId", "area", "hydroStorage", "thermalAvailability",
    "cluster", "FLOW LIN.", "direction", "flow", "hstorPMaxHigh",
    "BALANCE", "totalFlow", "prop", "to", "link", "change",
    "district", "must.run", ".txt", "detailsLength",
    "linkLength", "connectedToVirtualArea", "from", "correction",
    "nominalcapacity", "unitcount", "capacity", "minGenModulation",
    "production", "mustRunPartial", "mustRunTotal", "mcYear",
    "J", "N", "PSP", "availableUnits", "color", "corrPSP", "fromDistrict", 
    "pumpingCapacity", "pumpingCapacity.x", "pumpingCapacity.y", "rarea", 
    "storageCapacity", "storageCapacity.x", "storageCapacity.y", "toDistrict", 
    "transCapacityDirect", "transCapacityIndirect", "varea", "x", "y",
    "NODU", "min.stable.power", "thermalPmin",
    "weeks", "statesid", "tsId", "level_low", "level_high", "meanGridLayer",
    "states_next", "years", "value_node_year", "value_node", "time",
    "vny", "value_node_dif", "states_dif", "vu", "vu_band", "states", "band",
    "band_lin", "band_spl", "generatingMaxPower")
)
