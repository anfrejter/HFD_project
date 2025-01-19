"""
This script performs hyperparameter optimization for different trading strategies using the Optuna framework.
It defines the search space for each strategy, saves the parameters, runs an R script to evaluate the strategy,

Modules:
    logging: Provides a way to configure and use loggers.
    subprocess: Allows you to spawn new processes, connect to their input/output/error pipes, and obtain their return codes.
    sleep: Suspends the execution of the current thread for a specified period.
    os: Provides a way of using operating system-dependent functionality.
    csv: Implements classes to read and write tabular data in CSV format.
    optuna: An automatic hyperparameter optimization software framework.
    pandas: Provides data structures and data analysis tools.

Functions:
    ensure_directory(path):
        Ensure that a directory exists at the specified path. If the directory does not exist, it will be created.

    save_params(params, path, file="last_params"):
        Save hyperparameters to a CSV file.

    load_scores(params, results_path, stat_name='stats_fl'):
        Load the scores from the results CSV file.

    optimize_strategy(n_trials, param_space_func, study_name, r_script_path, instrument_name, group_data, stat_name='stats_fl', db_path="sqlite:///results/optuna_results.db"):
        Perform hyperparameter optimization for a given strategy using the Optuna framework.

    param_space_ma(trial):
        Define the hyperparameter search space for the Moving Average strategy.

    param_space_vb(trial):
        Define the hyperparameter search space for the Volatility Breakout Strategy.

    param_space_2vb(trial):
        Define the hyperparameter search space for the Double Volatility Breakout Strategy.

Constants:
    ASSET_GROUPS: A dictionary containing asset groups and their respective parameters.

Usage:
    Run the script to perform hyperparameter optimization for different trading strategies.
"""
import logging
import subprocess
from time import sleep
import os
import csv
import optuna
import pandas as pd
import numpy as np

ASSET_GROUPS = {
    1: {
        "SP": {"p_val": 50, "tr_cost": 12},
        "NQ": {"p_val": 20, "tr_cost": 12},
    },
    2: {
        "CAD": {"p_val": 100000, "tr_cost": 10},
        "AUD": {"p_val": 100000, "tr_cost": 10},
        "XAU": {"p_val": 100, "tr_cost": 15},
        "XAG": {"p_val": 5000, "tr_cost": 10},
    },
}
logger = logging.getLogger()

def ensure_directory(path):
    """
    Ensure that a directory exists at the specified path.
    If the directory does not exist, it will be created.
    """
    if not os.path.isdir(path):
        os.makedirs(path, exist_ok=True)
        logger.info(f"Directory created: {path}")


def save_params(params, path, file="last_params"):
    """
    Save hyperparameters to a CSV file.
    """
    ensure_directory(path)
    with open(path+"/"+file+".csv", mode='w', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(params.keys())
        writer.writerow(params.values())


def load_scores(params, results_path, stats_name = "stats_fl"):
    """
    Load the scores from the results CSV file.
    """
    query_conditions = []

    for key, value in params.items():
        if isinstance(value, int):
            query_conditions.append(f"{key} == {value}")
        elif isinstance(value, float):
            query_conditions.append(f"{key} == {round(value, 5)}")
        elif isinstance(value, str):
            query_conditions.append(f"{key} == '{value}'")

    query = " and ".join(query_conditions)
    file_path = results_path + "/results.csv"

    while True:
        tmp = pd.read_csv(file_path).tail(20).query(query)
        if not tmp.empty:
            if not tmp[stats_name].empty:
                return tmp[stats_name].fillna(0).replace(np.inf,-1).mean() - tmp[stats_name].fillna(0).replace(np.inf,-1).std()/2
            else:
                logger.error("Empty %s column in %s", stats_name, file_path)
        sleep(0.1)

def optimize_strategy(n_trials,
                      param_space_func,
                      study_name,
                      r_script_path,
                      instrument_name,
                      group_data,
                      stat_name = 'stats_fl',
                      db_path="sqlite:///results/optuna_results.db"):
    """
    This function performs hyperparameter optimization for a given strategy using the Optuna framework.
    It defines an objective function that evaluates the performance of the strategy based on the parameters
    sampled by Optuna. The function saves the parameters, runs an R script to evaluate the strategy,
    and retrieves the performance score. The optimization process is repeated for a specified number of trials.
    """

    logger.info("Optimizing %s for %s", study_name, instrument_name)
    params_path = f"results/{study_name}"
    results_path = f"results/{study_name}"

    params_dataset = {
        "instrument_name": instrument_name,
        "group_data": group_data,
        **ASSET_GROUPS[group_data][instrument_name]}

    def objective(trial):

        params_strat = param_space_func(trial)

        logger.info("Params: %s", params_strat)
        params_strat.update(params_dataset)
        save_params(params_strat, params_path)
        subprocess.check_call(["Rscript", r_script_path], shell=False,
                              stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL
                              )
        score = load_scores(params_strat, results_path, stat_name)
        return score

    study = optuna.create_study(direction='maximize',
                                study_name=study_name + "_" +
                                params_dataset['instrument_name'],
                                storage=db_path, load_if_exists=True)
    study.optimize(objective, n_trials=n_trials)
    best_param = study.best_params
    best_param.update(params_dataset)
    logger.info("Best params: %s", best_param)
    save_params(best_param, results_path, "best_params_" +
                params_dataset["instrument_name"])
    return best_param

def param_space_ma(trial):
    """
    Define the hyperparameter search space for the Moving Average strategy.
    """
    return {
        "fast_ma": trial.suggest_int("fast_ma", 1, 60),
        "ma_diff": trial.suggest_int("ma_diff", 1, 60),
        "signal_estimator": trial.suggest_categorical("signal_estimator", ["mean", "median"]),
        "window_regime": trial.suggest_int("window_regime", 2, 100),
        "treshold_regime": trial.suggest_float("treshold_regime", 0, 0.8, step=1e-05)
    }

def param_space_vb(trial):
    """
    Define the hyperparameter search space for the Volatility Breakout Strategy.
    """
    return {
        "fast_ma": trial.suggest_int("fast_ma", 1, 60),
        "ma_diff": trial.suggest_int("ma_diff", 1, 60),
        "signal_estimator": trial.suggest_categorical("signal_estimator", ["mean", "median"]),
        "window_regime": trial.suggest_int("window_regime", 2, 100),
        "treshold_regime": trial.suggest_float("treshold_regime", 0, 0.8, step=1e-05),
        "volat_param": trial.suggest_int("volat_param", 2, 100),
        "m_": trial.suggest_float("treshold_vol", 0.1, 3, step=1e-05)
    }

def param_space_2vb(trial):
    """
    Define the hyperparameter search space for the Double Volatility Breakout Strategy.
    """
    return {
        "fast_ma": trial.suggest_int("fast_ma", 1, 60),
        "ma_diff": trial.suggest_int("ma_diff", 1, 50),
        "signal_estimator": trial.suggest_categorical("signal_estimator", ["mean", "median"]),
        "window_regime": trial.suggest_int("window_regime", 2, 100),
        "treshold_regime": trial.suggest_float("treshold_regime", 0, 0.8, step=1e-05),
        "volat_param": trial.suggest_int("volat_param", 1, 100),
        "m_exit": trial.suggest_float("m_exit", 0.1, 3, step=1e-05),
        "m_diff": trial.suggest_float("m_diff", 0.1, 1, step=1e-05),

    }

if __name__ == "__main__":

    while True:
        N_TRIALS = 50
        study_dict = {
            'Volatility_Breakout_Double_Strategy': ["scripts/run_strat_2vb.R", param_space_2vb],
            'Volatility_Breakout_Strategy': ["scripts/run_strat_vb.R", param_space_vb],
            # 'Moving_Average_Strategy': ["scripts/run_strat_ma.R", param_space_ma],
        }

        for STUDY_NAME, [R_SCRIPT_PATH, PARAM_SCPACE_FUNC] in study_dict.items():
            for group, assets in ASSET_GROUPS.items():
                for asset in assets:
                    best_params = optimize_strategy(n_trials=N_TRIALS,
                                                    param_space_func=PARAM_SCPACE_FUNC,
                                                    study_name=STUDY_NAME,
                                                    r_script_path=R_SCRIPT_PATH,
                                                    instrument_name=asset,
                                                    stat_name="stats_fl",
                                                    group_data=group,
                                                    )
