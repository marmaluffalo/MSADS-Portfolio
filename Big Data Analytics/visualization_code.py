import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sb
import numpy as np
import matplotlib.font_manager as fm

covidData = pd.read_csv('mergedf3.csv')

covidData = covidData.iloc[:, 1:]
covidData.dropna()
covidData.columns

# New deaths per million

corrMatrix = covidData.corr()
print(corrMatrix)

# ICU patients per million and total vaccinated per hundred
# Population density and ICU patients per million
# weekly hospital admissions and extreme poverty
# Human development index and


# import modules


# plotting correlation heatmap
dataplot = sb.heatmap(covidData[['facial_coverings', 'stringency_index_y', 'extreme_poverty', 'human_development_index',
                                 'total_vaccinations_per_hundred', 'weekly_hosp_admissions',
                                 'population_density']].corr(), label='small', cmap="YlGnBu", annot=True)
plt.subplots_adjust(bottom=0.50, left=.35)
plt.figure(figsize=(12, 8))
# displaying heatmap
plt.show()

usa = covidData[covidData['CountryCode'].str.contains("USA")]
usa['LagDate'] = usa['Date'].shift(14)
usa['StringencyLag'] = usa['stringency_index_y'].shift(60)

covidData.columns

# Drop last 14 rows
usa = usa[:-60]
# Drop first 14
usa = usa[30:]
corrMatrixUSA = usa.corr()
print(corrMatrixUSA)


# As stringency goes up so too do deaths and excess mortality


def addLag(lagTime):
    # New array with all unique country codes
    covidDF = covidData
    dfCountries = covidDF['CountryCode']
    dfCountries = np.unique(dfCountries)
    lagCovidData = pd.DataFrame()
    # For each country code create new subset from covid data matching the code
    for country in dfCountries:
        # and shift target variables down by "x" number of days and cut the extra rows at bottom
        countryData = covidData[covidData['CountryCode'].str.contains(country)]
        countryData['LagDate'] = countryData['Date'].shift(lagTime)
        # countryData['StringencyLag'] = countryData['stringency_index_y'].shift(lagTime)
        countryData['LagCasesPerMillion'] = countryData['new_cases_per_million'].shift(lagTime)
        countryData['LagPositiveRate'] = countryData['positive_rate'].shift(lagTime)
        countryData['LagNewCases'] = countryData['new_cases'].shift(lagTime)
        countryData['LagDeaths'] = countryData['new_deaths_per_million'].shift(lagTime)
        countryData['LagHospitalPerMillion'] = countryData['hosp_patients_per_million'].shift(lagTime)
        countryData['LagICUPerMillion'] = countryData['icu_patients_per_million'].shift(lagTime)
        # Drop last x number of rows lagged to remove empty rows
        # if (lagTime > 196):
        countryData = countryData[:lagTime]
        # else:
        #     countryData = countryData[:-196]
        # Drop first x number of rows lagged to remove nan values
        countryData = countryData[-lagTime:]
        # Append to new dataframe
        lagCovidData = lagCovidData.append(countryData)
    return lagCovidData


lagData = addLag(-60)
# Brazil, UK
# -.33 correlation between stringency and new cases per million lagged and -.28 between stringency and new deaths per million for top 5 most stringent countries
countryList = ['VEN', 'HND', 'ARG', 'CHL', 'LBY']
# -.06 between stringency and new cases per million lagged and .02 for new deaths per million for bottom 5 most stringent countries
countryList = ['NIC', 'TZA', 'BDI', 'VUT', 'NER']
# countryList = ['USA']
focusedLagData = covidData[covidData['CountryCode'].isin(countryList)]
# focusedLagData = covidData[covidData['CountryCode'].isin(countryList)][['stringency_index','new_cases_per_million_31']]
corrMatrix = focusedLagData.corr()
print(corrMatrix)

plt.matshow(focusedLagData.corr())
plt.show()

sb.regplot(x="new_cases_per_million_31", y="stringency_index", data=focusedLagData)
plt.show()

sb.regplot(x="people_fully_vaccinated_per_hundred", y="icu_patients_per_million", data=covidData)
plt.show()

# highest average stringency level by country
def meanValue(value):
    # New array with all unique country codes
    covidDF = covidData
    dfCountries = covidDF['CountryCode']
    dfCountries = np.unique(dfCountries)
    lagCovidData = pd.DataFrame()
    # For each country code create new subset from covid data matching the code
    for country in dfCountries:
        # and shift target variables down by "x" number of days and cut the extra rows at bottom
        countryData = covidData[covidData['CountryCode'].str.contains(country)]
        countryData = countryData[['CountryCode', value]]
        countryData.dropna()
        valueMean = countryData[value].mean()
        # Drop last x number of rows lagged to remove empty rows
        # if (lagTime > 196):
        # else:
        #     countryData = countryData[:-196]
        # Drop first x number of rows lagged to remove nan values
        # Append to new dataframe
        df2 = {'Country': country, 'Mean': valueMean}
        lagCovidData = lagCovidData.append(df2, ignore_index=True)
    return lagCovidData


avgStringency = meanValue('stringency_index')

# Top 10 most stringent countries
## Visualizations

dpi = 96

title_font = fm.FontProperties(family='DejaVu Sans', style='normal', size=16, weight='bold', stretch='normal')
label_font = fm.FontProperties(family='DejaVu Sans', style='normal', size=14, weight='normal', stretch='normal')

df_plot = covidData.groupby('CountryCode')['stringency_index'].mean()
df_plot = df_plot.sort_values(ascending=False)[:10]
df_plot = df_plot / 1000

divs = [covidData[covidData['CountryCode'] == label].iloc[0]['CountryCode'] for label in df_plot.index]
colors = ['#cc0000', '#0033cc']

ax = df_plot.plot(kind='bar', figsize=[10, 7], color=colors, width=1, alpha=0.5, ec='w')
ax.yaxis.grid(True)
ax.set_xticklabels(df_plot.index, rotation=45, rotation_mode='anchor', ha='right')
ax.set_xlabel('Country', fontproperties=label_font)
ax.set_ylabel('Mean stringency', fontproperties=label_font)
ax.set_title('Top 10 Mean Stringency Levels by Country', fontproperties=title_font, y=1.01)

plt.savefig('stringencytop10.png', dpi=dpi, bbox_inches='tight', pad_inches=0.1)
plt.show()

# Bottom 10 most stringent countries
dpi = 96

title_font = fm.FontProperties(family='DejaVu Sans', style='normal', size=16, weight='bold', stretch='normal')
label_font = fm.FontProperties(family='DejaVu Sans', style='normal', size=14, weight='normal', stretch='normal')

df_plot = covidData.groupby('CountryCode')['stringency_index'].mean()
df_plot.dropna(inplace=True)
df_plot = df_plot.sort_values(ascending=True)[:10]
df_plot = df_plot / 1000

divs = [covidData[covidData['CountryCode'] == label].iloc[0]['CountryCode'] for label in df_plot.index]
colors = ['#cc0000', '#0033cc']

ax = df_plot.plot(kind='bar', figsize=[10, 7], color=colors, width=1, alpha=0.5, ec='w')
ax.yaxis.grid(True)
ax.set_xticklabels(df_plot.index, rotation=45, rotation_mode='anchor', ha='right')
ax.set_xlabel('Country', fontproperties=label_font)
ax.set_ylabel('Mean stringency', fontproperties=label_font)
ax.set_title('Bottom 10 Mean Stringency Levels by Country', fontproperties=title_font, y=1.01)

plt.savefig('stringencybottom10.png', dpi=dpi, bbox_inches='tight', pad_inches=0.1)
plt.show()

countryList = ['VEN', 'HND', 'ARG', 'CHL', 'LBY', 'NIC', 'TZA', 'BDI', 'VUT', 'NER']
# countryList = ['USA']
focusedData = covidData[covidData['CountryCode'].isin(countryList)][['CountryCode', 'new_deaths_per_million']]
focusedData = focusedData.groupby('CountryCode')['new_deaths_per_million'].mean()
# focusedLagData = covidData[covidData['CountryCode'].isin(countryList)][['stringency_index','new_cases_per_million_31']]


#k means
kmeansDF = covidData.groupby('CountryCode')[['Population', 'stringency_index']].mean()
kmeansDF['total_deaths_per_million'] = covidData.groupby('CountryCode')['total_deaths_per_million'].max()
kmeansDF.dropna(inplace=True)
kmeansDF = kmeansDF[kmeansDF['Population'] > 10000000]
kmeansDF = kmeansDF[['stringency_index', 'total_deaths_per_million']]
from sklearn.cluster import KMeans
import numpy as np
X = np.array(kmeansDF)


sb.regplot(x="stringency_index", y="total_deaths_per_million", data=kmeansDF)
plt.show()

#k means
kmeansDF = covidData.groupby('CountryCode')[['Population', 'people_fully_vaccinated_per_hundred']].max()
kmeansDF['icu_patients_per_million_31'] = covidData.groupby('CountryCode')['icu_patients_per_million_31'].mean()
kmeansDF.dropna(inplace=True)
kmeansDF = kmeansDF[kmeansDF['Population'] > 1000000]
kmeansDF = kmeansDF[['people_fully_vaccinated_per_hundred', 'icu_patients_per_million_31']]
from sklearn.cluster import KMeans
import numpy as np
X = np.array(kmeansDF)


sb.regplot(x="people_fully_vaccinated_per_hundred", y="icu_patients_per_million_31", data=covidData)
plt.show()

kmeanscorr = kmeansDF.corr()
print(kmeanscorr)

# Nicuragua, Tanzania, Burundi, Nyger
