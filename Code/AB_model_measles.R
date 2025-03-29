library(dplyr)
library(ggplot2)
library(tidyr)

## Main function for a day of simulation ------
simulate_day <- function(agents) {
  for (household in unique(agents$household_id)) {
    household_agents <- agents %>% filter(household_id == household)
    if (any(household_agents$state == "I")) {
      susceptible_agents <- household_agents %>% filter(state == "S")
      newly_exposed <- rbinom(nrow(susceptible_agents), 1, beta_within)
      agents <- agents %>%
        mutate(state = ifelse(household_id == household & state == "S",
                              ifelse(newly_exposed == 1, "E", state),
                              state))
    }
  }
  # 2. Between-household transmission: Large group gatherings for the 0-4 age group (Daycare)
  all_agents_0_4 <- agents %>% filter(age_group == "0-4")
  all_agents_18_plus <- agents %>% filter(age_group == "18+")
  if (nrow(all_agents_0_4) >= 16 && nrow(all_agents_18_plus) >= 2) {  
    remaining_children <- all_agents_0_4$row_id  # List of available children
    remaining_adults <- all_agents_18_plus$row_id  # List of available adults
    while (length(remaining_children) >= 16 && length(remaining_adults) >= 2) {  
      sample_size <- min(sample(8:16, 1), length(remaining_children))
      child_group <- sample(remaining_children, sample_size)
      remaining_children <- setdiff(remaining_children, child_group)
      # Assign 2-3 supervising adults
      num_supervisors <- min(sample(2:3, 1), length(remaining_adults))
      adult_group <- sample(remaining_adults, num_supervisors)
      remaining_adults <- setdiff(remaining_adults, adult_group)  # Remove selected adults
      # Combine children and supervising adults into one group
      gathered_agents <- c(child_group, adult_group)
      # Check for infection in the group
      infected_agents_in_group <- agents %>% filter(row_id %in% gathered_agents & state == "I")
      if (nrow(infected_agents_in_group) > 0) {
        for (i in gathered_agents) {
          if (agents$state[i] == "S") {
            agents$state[i] <- ifelse(rbinom(1, 1, beta_between) == 1, "E", agents$state[i])
          }
        }
      }
    }
  }
  # 3. Between-household transmission: School for 5-17 age group
  all_agents_5_17 <- agents %>% filter(age_group == "5-17")
  #  all_agents_18_plus <- agents %>% filter(age_group == "18+")
  if (nrow(all_agents_5_17) >= 25 && length(remaining_adults) >= 1) {  
    remaining_children_1 <- all_agents_5_17$row_id  # List of available children
    #    remaining_adults_1 <- all_agents_18_plus$row_id  # List of available adults (teachers)
    #I made this change since available adults can't be all, some are already in daycares
    remaining_adults_1 <- remaining_adults  # List of available adults (teachers)
    while (length(remaining_children_1) > 25 && length(remaining_adults_1) >= 1) {  
      # Ensure all children are assigned
      sample_size <- min(sample(15:25, 1), length(remaining_children_1))
      child_group <- sample(remaining_children_1, sample_size)
      remaining_children_1 <- setdiff(remaining_children_1, child_group)
      # Assign 1 teacher per classroom
      adult_group <- sample(remaining_adults_1, 1)
      remaining_adults_1 <- setdiff(remaining_adults_1, adult_group)
      # Combine children and teacher into one group
      gathered_agents <- c(child_group, adult_group)
      # Check for infection in the group
      infected_agents_in_group_1 <- agents %>% filter(row_id %in% gathered_agents & state == "I")
      if (nrow(infected_agents_in_group_1) > 0) {
        for (i in gathered_agents) {
          if (agents$state[i] == "S") {
            agents$state[i] <- ifelse(rbinom(1, 1, beta_between) == 1, "E", agents$state[i])
          }
        }
      }
    }
  }
  
  # SEIR Transitions
  agents <- agents %>%
    mutate(
      state = case_when(
        state == "E" & days_in_state >= rexp(n(), 1 / incubation_mean) ~ "I",
        state == "I" & days_in_state >= rexp(n(), 1 / infectious_mean) ~ "R",
        TRUE ~ state
      ),
      days_in_state = days_in_state + 1
    )
  
  return(agents)
}
##
# This block may not change too much
# Define household sizes and age groups
household_sizes <- c("one_person", "two_person", "three_person", "four_person", "five_person", "six_person", "seven_plus")
prob_SVIGroup4 <- c(0.25738387,0.31382916,0.16402815,0.14270570,0.07521527,0.02938847,0.01744939) #This is for TX
age_groups <- c("0-4", "5-17", "18+")
age_group_probs <- c(0.06580155,0.18689912,0.74729933) #This is for TX

SAR=0.99 # Secondary Attack Rate # Double from initial
beta_within <- -log(1-SAR)/infectious_mean # Assuming all household members interact equally

# Disease transmission parameters (Adjusted for Measles)
beta_within <- 0.9  # Higher household transmission
beta_between <- beta_within/2  # Higher community transmission
incubation_mean <- 11  # Measles incubation period (days)
infectious_mean <- 8  # Measles infectious period (days)

#set.seed(123)  # For reproducibility
household_size_map <- c("one_person" = 1, "two_person" = 2, "three_person" = 3,
                        "four_person" = 4, "five_person" = 5, "six_person" = 6, "seven_plus" = 7)
#

# Set up initial parameters
n_households <- 1000  # Number of households to simulate
n_days <- 100  # Number of days for simulation

vaccination_coverage <- 0  # X% vaccinated

# Generate households
households <- data.frame(
  household_id = 1:n_households,
  size_household = sample(household_sizes, n_households, replace = TRUE, prob = prob_SVIGroup4)
)

# Generate agents with vaccination
agents <- households %>%
  rowwise() %>%
  mutate(agents = list({
    household_size <- household_size_map[size_household]
    # Ensure at least one 18+ individual
    if (household_size == 1) {
      assigned_ages <- c("18+")  # If household size is 1, force it to be 18+
    } else {
      assigned_ages <- c("18+", sample(age_groups, household_size - 1, replace = TRUE, prob = age_group_probs))}
    data.frame(
      household_id = household_id,
      age_group = sample(assigned_ages, household_size, replace = FALSE),  # Shuffle assignments
      state = ifelse(runif(household_size) < vaccination_coverage, "V", "S"),  # Assign V for vaccinated
      days_in_state = 0)})) %>%
  unnest(agents, names_repair = "unique") %>%
  mutate(row_id = row_number()) %>%
  rename(household_id = household_id...1) %>%
  select(-"household_id...3")

# Initialize one unvaccinated child as exposed
susceptible_kids <- agents %>% filter(age_group %in% c("0-4","5-17") & state == "S")
#Previous line updated: 0-4 and 5-17 can be initially infected

initial_infecteds<-1 #Adding this to have multiple initial infected children
if (nrow(susceptible_kids) > 0) {
  child_index <- susceptible_kids %>% slice_sample(n = initial_infecteds) %>% pull(row_id)
  agents$state[agents$row_id == child_index] <- "E"
}

# Store time series data
state_counts <- data.frame(day = 1:n_days, S = 0, E = 0, I = 0, R = 0, V = 0)

for (day in 1:n_days) {
  agents <- simulate_day(agents)
  state_counts[day, 2:6] <- table(factor(agents$state, levels = c("S", "E", "I", "R", "V")))
}

# Plot time series of states
ggplot(state_counts, aes(x = day)) +
  #geom_line(aes(y = S, color = "S")) +
  geom_line(aes(y = E, color = "E")) +
  geom_line(aes(y = I, color = "I")) +
  geom_line(aes(y = R, color = "R")) +
#  geom_line(aes(y = V, color = "V")) +
  labs(title = "Time Series of SEIRV States", x = "Day", y = "Number of Individuals", color = "State") +
  theme_minimal()

#Houses with children
agents %>% filter(age_group %in% c("0-4","5-17"),state!="V") %>%
  select(-size_household,-age_group,-days_in_state,-row_id,-household_id) %>% 
  group_by(state) %>% count() %>% mutate(prop_inf=n/93)

agents %>% filter(age_group %in% c("0-4","5-17"),state=="V") %>% pull(state) %>% length()
