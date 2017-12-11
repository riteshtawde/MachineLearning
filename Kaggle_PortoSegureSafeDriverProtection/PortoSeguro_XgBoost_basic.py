'''
Kaggle competition : https://www.kaggle.com/c/porto-seguro-safe-driver-prediction
Kaggle team name : aml_jimit_ritesh
File : XGBoost for classification task
Algorithm used : XGBoost
Techniques used : Target Encoding, Stratified K-Fold Cross Validation
Authors : Jimit Shah (jimshah@iu.edu), Ritesh Tawde (rtawde@iu.edu)
References : https://www.kaggle.com/ogrellier/python-target-encoding-for-categorical-features
             https://kaggle2.blob.core.windows.net/forum-message-attachments/225952/7441/high%20cardinality%20categoricals.pdf
             https://www.kaggle.com/sudosudoohio/stratified-kfold-xgboost-eda-tutorial-0-281
'''
import pandas as pd
import numpy as np
import xgboost as xgb
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix
from sklearn.metrics import f1_score
from sklearn.model_selection import StratifiedKFold

# load training and testing data
train_data = pd.read_csv("train.csv",na_values="-1")
test_data = pd.read_csv("test.csv",na_values="-1")

# get target column from training data and id column from test data
X = train_data.drop(['target', 'id'], axis=1)
Y = train_data['target']

# drop id, target columns from train and id from test
test_sub_id = test_data['id'].values
test_data = test_data.drop(['id'], axis=1)


# target encoding
def add_noise(series, noise_level):
    return series * (1 + noise_level * np.random.randn(len(series)))


def target_encode(train_data=None,
                  test_data=None,
                  target=None,
                  min_samples_leaf=1,
                  smoothing=1,
                  noise_level=0):
    assert len(train_data) == len(target)
    assert train_data.name == test_data.name
    temp = pd.concat([train_data, target], axis=1)
    averages = temp.groupby(by=train_data.name)[target.name].agg(["mean", "count"])
    smoothing = 1 / (1 + np.exp(-(averages["count"] - min_samples_leaf) / smoothing))
    prior = target.mean()

    averages[target.name] = prior * (1 - smoothing) + averages["mean"] * smoothing
    averages.drop(["mean", "count"], axis=1, inplace=True)

    merged_train_data = pd.merge(
        train_data.to_frame(train_data.name),
        averages.reset_index().rename(columns={'index': target.name, target.name: 'average'}),
        on=train_data.name,
        how='left')['average'].rename(train_data.name + '_mean').fillna(prior)
    merged_train_data.index = train_data.index

    merged_test_data = pd.merge(
        test_data.to_frame(test_data.name),
        averages.reset_index().rename(columns={'index': target.name, target.name: 'average'}),
        on=test_data.name,
        how='left')['average'].rename(train_data.name + '_mean').fillna(prior)
    merged_test_data.index = test_data.index
    return add_noise(merged_train_data, noise_level), add_noise(merged_test_data, noise_level)


# extract categorical features to be target encoded
f_cats = [f for f in X.columns if "_cat" in f]
# perform target encoding of categorical features
for f in f_cats:
    X[f + "_avg"], test_data[f + "_avg"] = target_encode(train_data=X[f],
                                                         test_data=test_data[f],
                                                         target=Y,
                                                         min_samples_leaf=200,
                                                         smoothing=10,
                                                         noise_level=0)

# get numpy arrays
X = X.values
Y = Y.values

# XGBoost boosting parameters
params = {'objective': 'binary:logistic',
          'eta': 0.02,
          'silent': True,
          'max_depth': 5,
          'subsample': 0.8,
          'colsample_bytree': 0.8,
          'eval_metric': 'auc'
          }

# result frame
result = pd.DataFrame()
result['id'] = test_sub_id
result['target'] = np.zeros_like(test_sub_id)

strat_k_fold = 5
skf = StratifiedKFold(n_splits=strat_k_fold, random_state=1234)

for i, (train_idx, test_idx) in enumerate(skf.split(X, Y)):
    x_train, x_valid = X[train_idx], X[test_idx]
    y_train, y_valid = Y[train_idx], Y[test_idx]

    xg_train = xgb.DMatrix(x_train, y_train)
    xg_valid = xgb.DMatrix(x_valid, y_valid)
    xg_test = xgb.DMatrix(test_data.values)

    watchlist = [(xg_valid, 'valid'), (xg_train, 'train')]
    xg_model = xgb.train(params, xg_train, 1000, watchlist, early_stopping_rounds=100, maximize=True, verbose_eval=50)
    xg_predict = xg_model.predict(xg_test, ntree_limit=xg_model.best_ntree_limit)
    result['target'] += (xg_predict / strat_k_fold)

result.to_csv('Porto_Seguro_Simple_XGB_With_Kfold_ntree_limit.csv', index=False, float_format='%.5f')