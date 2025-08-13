### Define a function to write infos of fitted model to csv file:

def info_fitted_model_to_csv(fitted_model, target_var, train_features, test_features, train_targets, test_targets):
    import os
    import pandas as pd
    from datetime import datetime
    import builtins
    from sklearn.metrics import mean_squared_error
    from sklearn.metrics import r2_score
    from sklearn.metrics import mean_absolute_percentage_error
    from sklearn.metrics import mean_absolute_error
    from sklearn.ensemble import GradientBoostingRegressor
    from sklearn.model_selection import GridSearchCV, RandomizedSearchCV
    #from skopt import BayesSearchCV

    # Ask for prior target variable transformation:
    print("Did you transform the target variables? (yes/no)")
    trans = builtins.input("Type yes/no:")
    if trans == 'yes':
        print("Which function was used to  transform the targets? box-cox, log?")
        trans_func = builtins.input('Transformation function:')
        path = f'../../../output_data/ML_analysis/hyperparameter_settings/overview_{target_var}_{trans_func}_parametersets.csv'
        path_excel = f'../../../output_data/ML_analysis/hyperparameter_settings/overview_{target_var}_{trans_func}_parametersets.xlsx'
        print(trans_func)
    else: 
        trans = 'no'
        trans_func = 'NA'
        path = f'../../../../../output_data/ML_analysis/hyperparameter_settings/overview_{target_var}_parametersets.csv'
        path_excel = f'../../../../../output_data/ML_analysis/hyperparameter_settings/overview_{target_var}_parametersets.xlsx'

    print('Add a comment about the fitted model if you like? Optimized model? Optimization algorithm?')
    comment = builtins.input('Type in any comment:')

    # evaluate model with test data:
    pred_test = fitted_model.predict(test_features)

    # pred on training data: 
    pred_train = fitted_model.predict(train_features)
    
    # calculate R2 score training set:
    r2_train = r2_score(train_targets, pred_train)


    # calculate R2 score test set:
    r2_test = r2_score(test_targets, pred_test)

    # mean absolute percentage error  test data
    MAPE = mean_absolute_percentage_error(test_targets, pred_test)
    
    # mean squared error: 
    mse = mean_squared_error(test_targets, fitted_model.predict(test_features))
   
    # mean absolute error: 
    mae = mean_absolute_error(test_targets, fitted_model.predict(test_features))

    # current datetime: 
    date = datetime.now().strftime("%d-%m-%Y %H:%M:%S")


    # number of estimators:
    if isinstance(fitted_model, GradientBoostingRegressor)==True:
        n_estimators = fitted_model.n_estimators_
    else:
        n_estimators = 'NA'

    # if GridSearchCV or RandomSearchCV() or BayesSearchCV(): Mean cross-validated score of the best_estimator:
    
    if isinstance(fitted_model, (GridSearchCV, RandomizedSearchCV))==True: # BayesSearchCV
        r2_cross_v = fitted_model.best_score_

        parameters = fitted_model.best_params_
    else:
        r2_cross_v = 'NA'
        


        # get parameters of fitted model
        params = fitted_model.get_params
        params_str = str(params)
    
        # extract parameters between brackets:
        start_idx = params_str.find('(')
        end_idx = params_str.rfind(')')
        
        if start_idx != -1 and end_idx != -1 and start_idx < end_idx:
            parameters = params_str[start_idx + 1:end_idx]

    # put all results together 
    results = {'estimator' : [str(fitted_model)],
               'target': [str(target_var)],
               'date': [str(date)],
               'parameters':[parameters],
               'n_estimators':[n_estimators],
               'R2_cross_val':[r2_cross_v],
               'R2_train':[r2_train],
               'R2_test':[r2_test],
               'MAPE': [MAPE],
               'mse':[mse],
               'mae':[mae],
               'transformation':[trans],
               'trans_func':[trans_func],
               'comments':[comment]
              }
    # convert results to data frame
    table_results = pd.DataFrame(results)


    # check if csv-file already exists: 
    isdir = os.path.isfile(path)
    print(isdir)
    # if not save df to new csv: 
    if isdir == False:
        table_results.to_csv(path, index = False)
        table_results.to_excel(path_excel, index = False)
    # if already exists, concat new results: 
    else:
        already_exist = pd.read_csv(path)
        data_table =pd.concat([already_exist, table_results], ignore_index = True)
        data_table.to_csv(path, index = False)
        data_table.to_excel(path_excel, index = False)
        
        