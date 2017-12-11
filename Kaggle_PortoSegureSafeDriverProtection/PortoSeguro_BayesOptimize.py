'''
Kaggle competition : https://www.kaggle.com/c/porto-seguro-safe-driver-prediction
Kaggle team name : aml_jimit_ritesh
File : XGBoost for classification task
Algorithm used : Bayesian Optimization
Authors : Jimit Shah (jimshah@iu.edu), Ritesh Tawde (rtawde@iu.edu)
References : https://www.kaggle.com/tilii7/bayesian-optimization-of-xgboost-parameters
'''

import numpy as np
import pandas as pd
from sklearn.model_selection import StratifiedKFold
import xgboost as xgb
from bayes_opt import BayesianOptimization
import warnings

# xgb cross validation function
def xgb_cv(max_depth, gamma, min_child_weight, max_delta_step, subsample, colsample_bytree):
    global best_auc
    global best_iteration

    # xgboost init params
    params = {'booster': 'gbtree',
              'max_depth': int(max_depth),
              'gamma': gamma,
              'eta': 0.1,
              'objective': 'binary:logistic',
              'silent': True,
              'eval_metric': 'auc',
              'subsample': max(min(subsample, 1), 0),
              'colsample_bytree': max(min(colsample_bytree, 1), 0),
              'min_child_weight': min_child_weight,
              'max_delta_step': int(max_delta_step),
              }
    k_folds = 5
    cv_score = 0

    xgbcv_model = xgb.cv(params,
                         xgb_train,
                         num_boost_round=20000,
                         stratified=True,
                         nfold=k_folds,
                         verbose_eval=10,
                         early_stopping_rounds=100,
                         metrics='auc',
                         show_stdv=True
                         )
    valid_score = xgbcv_model['test-auc-mean'].iloc[-1]
    train_score = xgbcv_model['train-auc-mean'].iloc[-1]
    print('val score : ', valid_score, ', train score : ', train_score)
    if (valid_score > best_auc):
        best_auc = valid_score
        best_iteration = len(xgbcv_model)
    return (valid_score * 2) - 1


best_auc = -1.
best_iteration = 0

# load data
train = pd.read_csv('train.csv', na_values=-1)
test = pd.read_csv('test.csv', na_values=-1)

# training data
train_ids = train.id.values
train_labels = train.target.values
train = train.drop(['id', 'target'], axis=1)

# test data
test_id = test.id.values
test = test.drop('id', axis=1)

# convert to xgb
xgb_train = xgb.DMatrix(train, label=train_labels)

# calling starts here

# parameters to optimize with ranges
xgb_bayes = BayesianOptimization(xgb_cv, {'max_depth': (2, 12),
                                          'gamma': (0.001, 10.0),
                                          'min_child_weight': (0, 20),
                                          'max_delta_step': (0, 10),
                                          'subsample': (0.4, 1.0),
                                          'colsample_bytree': (0.4, 1.0)
                                          })

# explore from range of values to try
xgb_bayes.explore({'max_depth': [3, 8, 3, 8, 8, 3, 8, 3],
                   'gamma': [0.5, 8, 0.2, 9, 0.5, 8, 0.2, 9],
                   'min_child_weight': [0.2, 0.2, 0.2, 0.2, 12, 12, 12, 12],
                   'max_delta_step': [1, 2, 2, 1, 2, 1, 1, 2],
                   'subsample': [0.6, 0.8, 0.6, 0.8, 0.6, 0.8, 0.6, 0.8],
                   'colsample_bytree': [0.6, 0.8, 0.6, 0.8, 0.6, 0.8, 0.6, 0.8]
                   })

with warnings.catch_warnings():
    xgb_bayes.maximize(init_points=18, n_iter=35, acq='ei', xi=0.01)

df_params = pd.DataFrame(xgb_bayes.res['all']['params'])
df_values = pd.DataFrame(xgb_bayes.res['all']['values'])
df_all = pd.concat((df_params, df_values), axis=1)
df_all.rename(columns={0: 'gini'}, inplace=True)
df_all['AUC'] = (df_all['gini'] + 1) / 2
df_all.to_csv('baysian_xgb_auc_kfold.csv')