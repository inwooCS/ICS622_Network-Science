######################################################################
# Nonzero Degree Distribution
#
# If R plot is given a degree distribution vector with 0.0 for some of
# the values, these are plotted. Since typical networks will have many
# values of k that are 0, this clutters the plot, making it hard to
# see other low probability data points. The solution is to replace
# all 0.0 values with NA, so they are not plotted.
#
# Use this for the y axis value (2nd argument) to plot, in conjunction
# with degree_domain for the x axis.
# 
# This correction applies only for lin-lin plots, as log-log will
# throw out the 0 values (generating warnings that can be reduced
# with this function). 
#
# Dan Suthers, Feb 4, 2021 (comment updated March 2, 2021)
######################################################################

nonzero_degree_distribution <- function(g, mode="all") {
  dd <- degree_distribution(g, mode=mode) # compute once 
  ifelse(dd == 0.0, NA, dd)
}

######################################################################
# Pau 
