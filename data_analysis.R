csv_file <- 'D:\\Practicas\\R\\covid_tracking_project\\covid_tracking_project\\all-states-history.csv'
data <- read.csv(csv_file, stringsAsFactors = FALSE)

cat("Data columns: ",colnames(data),"\n")
print(head(data,5))

print_death_by_state <- function(data) {
  cat("\n Total quantity of deaths confirmed by state (descending)\n")
  cat("--------------------------------------------\n")
  death_by_state <- aggregate(deathConfirmed ~ state, data = data, sum, na.rm = TRUE)
  death_by_state <- death_by_state[order(-death_by_state$deathConfirmed), ]
  for (i in 1:nrow(death_by_state)) {
    cat(death_by_state$state[i], ": ", death_by_state$deathConfirmed[i], "\n")
  }
}

print_top20_hospitalized <- function(data){
  cat("\n Top 20 states with more hospitalized people \n")
  cat("--------------------------------------------\n")
  hospitalize_by_state <- aggregate(hospitalizedCumulative ~ state,data = data,sum,na.rm = TRUE)
  hospitalize_by_state <- hospitalize_by_state[order(-hospitalize_by_state$hospitalizedCumulative),]
  top20_hospitalized <- head(hospitalize_by_state,20)
  for (i in 1:nrow(top20_hospitalized)) {
    cat(top20_hospitalized$state[i], ": ", top20_hospitalized$hospitalizedCumulative[i], "\n")
  }
}

print_top10_ventilator_percentage <- function(data){
  cat("\n Top 10 states with more assited breathing patients \n")
  cat("--------------------------------------------\n")
  ventilator_sum_by_state <- aggregate(onVentilatorCurrently ~ state,data = data,sum,na.rm = TRUE)
  positive_sum_by_state <- aggregate(positive ~ state,data = data,sum,na.rm = TRUE)
  merged_data <- merge(ventilator_sum_by_state,positive_sum_by_state,by = "state")
  merged_data$percentage <- ifelse(merged_data$positive != 0,
                                   (merged_data$onVentilatorCurrently / 
                                      merged_data$positive) * 100,0)
  merged_data <- merged_data[order(-merged_data$percentage),]
  for(i in 1:min(20,nrow(merged_data))){
    cat(merged_data$state[i],": ",sprintf("%.2f%%",merged_data$percentage[i]),"\n")
  }
}

print_death_by_state(data)
print_top20_hospitalized(data)
print_top10_ventilator_percentage(data)