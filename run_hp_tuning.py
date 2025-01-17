import subprocess
import time
import csv
import optuna
import pandas as pd

PARAMS_PATH = 'results/ma/ma_params.csv'
RESULTS_PATH = 'results/ma/ma_results.csv'

params = ['slow_ma', 'fast_ma']

def add_params(slow_ma, fast_ma):
    with open(PARAMS_PATH, mode='w', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(params)
        writer.writerow([slow_ma, fast_ma])

def read_scores(slow_ma, fast_ma):
    tmp = pd.read_csv(RESULTS_PATH).query(f"slow_ma == {slow_ma} and fast_ma == {fast_ma}")
    while tmp.shape[0] < 1:
        tmp = pd.read_csv(RESULTS_PATH).query(f"slow_ma == {slow_ma} and fast_ma == {fast_ma}")
        time.sleep(0.1)
    return tmp['stats_mm'].mean()

def objective(trial):

    slow_ma = trial.suggest_int('slow_ma', 10, 100)
    fast_ma = trial.suggest_int('fast_ma', 1, 50)

    add_params(slow_ma, fast_ma)
    subprocess.check_call(["Rscript","run_strat_ma.R"],shell=False,stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    score = read_scores(slow_ma, fast_ma)
    return score

study = optuna.create_study(direction='maximize')
study.optimize(objective, n_trials=10, show_progress_bar=True)

print(study.best_params)