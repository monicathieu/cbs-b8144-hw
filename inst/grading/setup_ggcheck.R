## setup ----

# god bless this pkg
require(magrittr)
require(purrr)
require(rlang)
require(ggplot2)
require(ggcheck)

## user defined functions in the style of ggcheck ----

# only works on whole plots... only whole plots have labs
get_labs <- function (p) {
  p$labels
}

# in e.g. uses_mappings(), exact = T requires no additional mappings to be set
# while exact = F checks only that the requested mappings match, ignoring extras
# currently this only checks whether non-default labs are set at all
# expect labs as named list of specific labs OR as char vector of labs to check are set at all
# doesn't currently work on legends, but use same logic as x and y
uses_labs <- function (p, geom = NULL, labs) {
  if (is.null(geom)) {
    current_xy_mapping_names <- map(get_mappings(p), as_label)[c("x", "y")]
  } else {
    current_xy_mapping_names <- map(get_mappings(get_geom_layer(p, geom)), as_label)[c("x", "y")]
  }
  
  if (!("y" %in% names(current_xy_mapping_names))) {
    if (geom == "histogram") {
      current_xy_mapping_names[["y"]] <- "count"
    } else if (geom == "density") {
      current_xy_mapping_names[["y"]] <- "density" 
    }
  }
  
  current_xy_mapping_names <- current_xy_mapping_names[c("x", "y")]
  
  current_labs <- get_labs(p)
  
  pass <- NULL
  
  if (is.list(labs)) {
    for (lab_name in names(labs)) {
        # in this case, strategy doesn't differ based on lab type
        this_pass <- current_labs[[lab_name]] == labs[[lab_name]]
        
      pass <- c(pass, this_pass)
    }
  } else {
    for (lab_name in labs) {
      
      if (lab_name %in% c("x", "y")) {
        # for x and y, check that current labs differ from current mappings
        # which would imply that labs have been set
        # this logic can generalize to legend key names later
        this_pass <- current_labs[[lab_name]] != current_xy_mapping_names[[lab_name]]
      } else {
        # for any other labs, check that they exist in names
        this_pass <- lab_name %in% names(current_labs)
      }
      
      pass <- c(pass, this_pass)
    }
  }
  
  return (all(pass))
}

get_facets <- function (p) {
  
}