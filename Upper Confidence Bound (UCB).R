# The Multi-armed Bandit
# - We have d arms. For example, arms are ads that we display to users each time they connect to this web page.
# - Each time a user connects to this web page, that make a round.
# - At each round n, we choose one ad to display to the users.
# - At each round n, ad i gives reward Ri(n) belongs to {0, 1}: Ri(n) = 1, if the user clicked on the ad i, 
# 0 if the user didnt.
# - Our goal is to maximize the total reward we get over many rounds.

# Steps for Upper Confidence Bound;
# - Step 1 : At each round n, we consider two number for each ad i:
#       - Ni(n) - the number of time the ad i was selected up to round n,
#       - Ri(n) - the sum of rewards of the ad i up to round n.
# - Step 2 : Form these two number we compute:
#       - The average reward of ad i up to round n
#             r(n) = the average reward of slot machine = R(n) / N(n).
#       - The confidence intervel [Ri(n) - Δi(n), Ri(n) + Δi(n)] at round n with
#             Δi(n) = √(3/2)(log(n)/Ni(n))
# - Step 3 : We select thr ad i that has the maximum UCB [ Ri(n) + Δi(n) ]

# ----------------------------------------------------- Importing Data -------------------------------------------- #

dataset = read.csv('Ads_CTR_Optimisation.csv')

# --------------------------------------------------- Implementating UCB ------------------------------------------ #

# Total Number of rounds to show User
N = 10000

# Number of ads
d = 10

# Full list of rounds
ads_selected = integer(0)

numbers_of_selections = integer(d)
sums_of_rewards = integer(d)
total_reward = 0

for (n in 1:N) {
  ad = 0
  max_upper_bound = 0
  for (i in 1:d) {
    if (numbers_of_selections[i] > 0) {
      average_reward = sums_of_rewards[i] / numbers_of_selections[i]
      delta_i = sqrt(3/2 * log(n) / numbers_of_selections[i])
      upper_bound = average_reward + delta_i
    } else {
      upper_bound = 1e400
    }
    if (upper_bound > max_upper_bound) {
      max_upper_bound = upper_bound
      ad = i
    }
  }
  ads_selected = append(ads_selected, ad)
  numbers_of_selections[ad] = numbers_of_selections[ad] + 1
  reward = dataset[n, ad]
  sums_of_rewards[ad] = sums_of_rewards[ad] + reward
  total_reward = total_reward + reward
}

# ---------------------------------------------- Visualising the Result ------------------------------------------- #

hist(ads_selected,
     col = 'blue',
     main = 'Histogram of Ads Selections',
     xlab = 'Ads',
     ylab = 'Number of times each ad was selected')
