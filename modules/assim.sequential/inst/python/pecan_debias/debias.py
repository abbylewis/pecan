# debias.py
import numpy as np
from sklearn.model_selection import GridSearchCV
from sklearn.neighbors import KNeighborsRegressor
from sklearn.ensemble import ExtraTreesRegressor
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import mean_squared_error

_models = {}  # var_name -> (knn, tree, w)

def _fit_one(X, y):
    pipe = make_pipeline(StandardScaler(), KNeighborsRegressor())
    grid = GridSearchCV(pipe,
                        {'kneighborsregressor__n_neighbors': list(range(1, 31))},
                        cv=5, scoring='neg_root_mean_squared_error', n_jobs=-1)
    grid.fit(X, y)
    k = grid.best_params_['kneighborsregressor__n_neighbors']
    knn = make_pipeline(StandardScaler(), KNeighborsRegressor(n_neighbors=k))
    knn.fit(X, y)

    tree = ExtraTreesRegressor(n_estimators=300, max_depth=30,
                               max_features='sqrt', min_samples_leaf=1,
                               min_samples_split=2)
    tree.fit(X, y)

    knn_pred  = knn.predict(X)
    tree_pred = tree.predict(X)
    weights = np.linspace(0, 1, 101)
    best_w, best_rmse = 0.0, np.inf
    for w in weights:
        rmse = np.sqrt(mean_squared_error(y, w*knn_pred + (1-w)*tree_pred))
        if rmse < best_rmse:
            best_rmse, best_w = rmse, w
    return knn, tree, best_w

def train_full_model(name, X, y):
    knn, tree, w = _fit_one(X, y)
    _models[name] = (knn, tree, w)

def has_model(name):
    return name in _models

def get_model_weights(name):
    m = _models.get(str(name))
    return None if m is None else float(m[2])

def predict_residual(name, X):
    if name not in _models:
        return np.zeros(X.shape[0])
    knn, tree, w = _models[name]
    return w*knn.predict(X) + (1-w)*tree.predict(X)
