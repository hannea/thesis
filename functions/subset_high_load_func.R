
subset_high_load = function(dataframe, load = "GeneratorSpeed", load_min = 17){
  ii = dataframe[[load]]>load_min
  dataframe[ii,]
}


## tests
# data_subset = subset_high_load(data)
# length(data_subset)
# class(data_subset)
# head(data_subset)
# head(data)