# High-Frequency Trading Strategy Optimization

## Overview
This project focuses on hyperparameter optimization for trading strategies using Python and R. The goal is to identify optimal parameters for strategies applied to high-frequency financial data. Python is used for orchestration and optimization, while R scripts handle strategy execution and analysis. This project uses **Optuna**, a framework for hyperparameter optimization.

---

## Directory Structure

```

HFT_Strategy_Optimization/
├── Dataset1
│   ├── data1_2022_Q1.RData
│   ├── ...
├── Dataset2
│   ├── data2_2022_Q1.RData
│   ├── ...
├── README.md
├── helpers
│   ├── plots.R
│   ├── strategies.R
│   └── utils.R
├── results
│   ├── Moving_Average_Strategy
│   │   ├── best_params_AUD.csv
│   │   ├── ...
│   │   ├── last_params.csv
│   │   └── results.csv
│   ├── Volatility_Breakout_Double_Strategy
│   │   ├── best_params_SP.csv
│   │   ├── ...
│   │   ├── last_params.csv
│   │   └── results.csv
│   ├── Volatility_Breakout_Strategy
│   │   ├── best_params_AUD.csv
│   │   ├── ...
│   │   ├── last_params.csv
│   │   └── results.csv
│   └── optuna_results.db
├── run_hp_tuning.py
└── scripts
    ├── run_strat_2vb.R
    ├── run_strat_ma.R
    └── run_strat_vb.R

```

---

## How to Run the Project

### 1. Preprocess Datasets
- Save cleaned datasets in `Dataset1/` or `Dataset2/`.
- Use helper scripts like `utils.R` for preprocessing.

### 2. Run Hyperparameter Optimization
- Execute the main Python script `run_hp_tuning.py`:
  ```bash
  python run_hp_tuning.py
  ```
  - This script:
    1. Loads data from `Dataset1/` or `Dataset2/`.
    2. Calls R scripts (e.g., `run_strat_ma.R`) for strategy execution.
    3. Optimizes hyperparameters using Optuna.
    4. Saves results in (e.g. `results/ma/`).
---

## Key Components

### **Datasets**
- The `Dataset1/` and `Dataset2/` folders store the high-frequency financial datasets used for the optimization and strategy analysis. Files are stored in `.RData` format for efficient handling in R.

### **Helpers**
- The `helpers/` folder contains utility functions and scripts for strategy implementation, plotting, and general data handling:
  - `plots.R`: Functions for visualizing results.
  - `strategies.R`: Definitions of trading strategies.
  - `utils.R`: General utility functions used throughout the project.

### **Results**
- The `results/` folder stores outputs from optimization runs and strategy backtests:
  - Each strategy has its own folder (e.g., `Moving_Average_Strategy`, `Volatility_Breakout_Strategy`).
  - Key outputs include:
    - `best_params_*.csv`: Stores the best hyperparameters identified for each asset.
    - `last_params.csv`: Logs the most recent parameters used in the optimization.
    - `results.csv`: Contains detailed results of backtests or simulations.
  - `optuna_results.db`: SQLite database containing Optuna study results for reproducibility.

### **Scripts**
- The `scripts/` folder contains R scripts for running specific trading strategies:
  - `run_strat_ma.R`: Executes the Moving Average strategy.
  - `run_strat_vb.R`: Executes the Volatility Breakout strategy.
  - `run_strat_2vb.R`: Executes the Double Volatility Breakout strategy.

### **Python Script**
- `run_hp_tuning.py`: Main Python script for orchestrating the hyperparameter optimization process using Optuna. This script:
  - Defines the search space for hyperparameters.
  - Interfaces with R scripts for executing strategies.
  - Logs results in the `results/` folder and `optuna_results.db`.

---
