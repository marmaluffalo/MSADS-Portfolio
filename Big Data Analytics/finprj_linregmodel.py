# -*- coding: utf-8 -*-
"""
Created on Sat Sep 18 23:34:20 2021

@author: Patrick
"""

from sklearn import linear_model
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.model_selection import train_test_split
import pandas as pd

##############################################################################

mergedf = pd.read_csv('mergedf.csv')


X = mergedf

#from sklearn.impute import SimpleImputer
#imputer = SimpleImputer(missing_values = np.nan, strategy = 'mean',verbose=0)
#imputer = imputer.fit(X.iloc[:, 1:83])
#X.iloc[:, 1:83] = imputer.transform(X.iloc[:, 1:83]) 


X =  X.drop(columns=['alpha', 'iota', 'delta','mu', 'gamma','beta', 'lambda', 'eta', 'kappa', 'Population' ])

newdf = X


corrMatrix = newdf.corr()
print(corrMatrix)


deaths = newdf[['new_deaths_per_million_31','facial_coverings', 'stringency_index', 'people_fully_vaccinated_per_hundred']]
deaths =  deaths.dropna(axis='rows')


# Use only one feature
deaths_X = deaths[['facial_coverings', 'stringency_index','people_fully_vaccinated_per_hundred']]
deaths_y = deaths['new_deaths_per_million_31']



# define variables from the dataset

X_train, X_test, y_train, y_test = train_test_split(deaths_X, deaths_y, test_size=0.33, random_state=42)


# Create linear regression object
regr = linear_model.LinearRegression()

# Train the model using the training sets
regr.fit(X_train, y_train)

# Make predictions using the testing set
deaths_y_pred = regr.predict(X_test)

# The coefficients
print('Coefficients: \n', regr.coef_)
# The mean squared error
print('Mean squared error: %.2f'
      % mean_squared_error(y_test, deaths_y_pred))
# The coefficient of determination: 1 is perfect prediction
print('Coefficient of determination: %.2f'
      % r2_score(y_test, deaths_y_pred))

# Plot outputs
X_test=np.arange(0,len(X_test),1)
plt.scatter(X_test, y_test, color='black')
plt.plot(X_test, deaths_y_pred, color='blue', linewidth=2)

plt.xticks(())
plt.yticks(())

plt.show()