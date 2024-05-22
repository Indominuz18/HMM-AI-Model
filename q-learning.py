import numpy as np
import pandas as pd
# Define the state space, actions, and initial Q-table
state_space = range(1, 101)
actions = ['Stocks', 'Real_Estate', 'Commodities', 'Cryptocurrencies', 'Forex']
q_table = np.zeros((len(state_space), len(actions)))

# Define hyperparameters
alpha = 0.1  # Learning rate
gamma = 0.9  # Discount factor

# Define the environment and profit function
def profit(investment):
    return np.floor(2 * np.random.rand() * investment)
def market_env(state, action):
    budget = state
    investment_amounts = {'Stocks': 5, 'Real_Estate': 17, 'Commodities': 11, 'Cryptocurrencies': 9, 'Forex': 7}
    investment = investment_amounts[action]
    expected_profit = profit(investment)
    next_state = budget - investment + expected_profit
    reward = np.log(max(1, budget - investment + expected_profit)) if budget - investment > 0 else budget - investment + expected_profit
    return next_state, reward

def sample_experience(N, env, states, actions):
    data = []
    for _ in range(N):
        state = np.random.choice(states)
        action = np.random.choice(actions)
        next_state, reward = env(state, action)
        data.append((state, actions.index(action), reward, next_state))
    return data
# Sample data
training_data = sample_experience(1000, market_env, state_space, actions)
# Train the Q-learning agent
for epoch in range(100):
    for state, action, reward, next_state in training_data:
        # Subtract 1 to adjust for zero-based indexing
        state_index = int(state) - 1
        next_state_index = int(next_state) - 1
        
        # Ensure that the next_state index is within the valid range
        if next_state_index < 0 or next_state_index >= len(state_space):
            continue
        
        current_q = q_table[state_index, action]
        max_future_q = np.max(q_table[next_state_index, :])
        new_q = (1 - alpha) * current_q + alpha * (reward + gamma * max_future_q)
        q_table[state_index, action] = new_q


# Compute the policy
policy = np.argmax(q_table, axis=1)
# Apply the model to unseen data
unseen_states = np.arange(15, 46)  # Example range of unseen states
optimal_actions = [actions[policy[state - 1]] for state in unseen_states]
# Print results
print("Q-Table:")
print(q_table)
print("\nPolicy:")

print(policy)

print("\nOptimal Actions for Unseen States:")
for state, action in zip(unseen_states, optimal_actions):
    print(f"State {state}: {action}")
