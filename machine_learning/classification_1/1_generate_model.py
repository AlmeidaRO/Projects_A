#----- Load libraries -----
import streamlit as st

import pandas as pd
import numpy as np
import os, re, zipfile, pickle, shutil

from sklearn.compose import make_column_selector
from sklearn.model_selection import StratifiedShuffleSplit
from sklearn.preprocessing import LabelEncoder

from sklearn.linear_model import LogisticRegression
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.svm import LinearSVC
from sklearn.neighbors import KNeighborsClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.neural_network import MLPClassifier

from sklearn.model_selection import cross_val_score, StratifiedKFold

from sklearn.metrics import matthews_corrcoef, accuracy_score, precision_score, recall_score, f1_score


#----- Streamlit config -----
st.set_page_config(
    page_title = 'ML Classification - Modeling',
    layout = 'wide'
)

hide_streamlit_style = """
<style>
MainMenu {visibility: hidden;}
footer {visibility: hidden;}
header {visibility: hidden;}
</style>
"""
st.markdown(hide_streamlit_style, unsafe_allow_html = True)

z = zipfile.ZipFile('results.zip', 'w', zipfile.ZIP_DEFLATED)

DF = pd.DataFrame()
if ('perf_train_default' not in st.session_state):
    st.session_state['perf_train_default'] = pd.DataFrame()
if ('perf_test_default' not in st.session_state):
    st.session_state['perf_test_default'] = pd.DataFrame()
if 'switch_on_off' not in st.session_state:
    st.session_state['switch_on_off'] = True

#----- Metrics -----
ml_metrics_A = ['roc_auc', 'balanced_accuracy', 'precision_macro', 'recall_macro', 'f1_macro']

#============

#----- Options 1 -----
with st.container():
    st.write('---')
    col1, col2 = st.columns([1,1])
    
    # Load file
    with col1:
        st.header(':blue[Load csv file]')
        
        uploaded_file = st.file_uploader('', label_visibility = 'collapsed')
        if uploaded_file is not None:
            DF = pd.read_csv(uploaded_file)
            
    # Target feature
    target_feature = 'sp'
    
    
    if DF.shape[0] != 0:
        with col2:
            st.header(':blue[Size of teste dataset (%)]')
            
            perc_test = st.slider(label = '%', min_value = 5, max_value = 50, value = 20, step = 5,  label_visibility = 'hidden')

    # Mapping index 
    target_feature_i = None
    for i in range(0, len(list(DF.columns))):
        if list(DF.columns)[i] == target_feature:
            target_feature_i = i

    
    cat_selector = make_column_selector(dtype_include = object)
    cat_features = []
    for i in range(0, len(cat_selector(DF))):
        if cat_selector(DF)[i] != target_feature:
            cat_features.append(cat_selector(DF)[i])
    
    num_selector = make_column_selector(dtype_include = np.number)
    num_features = []
    for i in range(0, len(num_selector(DF))):
        if num_selector(DF)[i] != target_feature:
            num_features.append(num_selector(DF)[i])
    
    if cat_features != None:
        cat_features_i = []
        for i in range(0, len(list(DF.columns))):
            for j in range(0, len(cat_features)):
                if list(DF.columns)[i] == cat_features[j]:
                    cat_features_i.append(i)
    
    if num_features != None:
        num_features_i = []
        for i in range(0, len(list(DF.columns))):
            for j in range(0, len(num_features)):
                if list(DF.columns)[i] == num_features[j]:
                    num_features_i.append(i)

# ----- Train/Test dataset -----
if DF.shape[0] != 0:
    
    # Raw arrays
    array = DF.values
    X = array[:, cat_features_i + num_features_i]
    y = array[:, target_feature_i]
    
    sss = StratifiedShuffleSplit(n_splits = 1, test_size = perc_test/100, random_state = 10)
    sss.get_n_splits(X, y)
    split_i = []
    for i in enumerate(sss.split(X, y)):
        split_i.append(i)
    
    
    # dataset
    Train_dataset = DF.iloc[split_i[0][1][0], : ]
    Test_dataset = DF.iloc[split_i[0][1][1], : ]
    
    # Code categ
    Code_df = pd.DataFrame()
    
    # Train arrays
    for i in range(0, len(cat_features_i)):
        sub_Train_dataset = Train_dataset.iloc[:, cat_features_i[i]]
        tf = list(set(list(sub_Train_dataset)))
        le = LabelEncoder()
        le.fit(tf)
        cf = list(set(le.transform(tf)))
        Train_dataset.iloc[:, cat_features_i[i]] = list(le.transform(Train_dataset.iloc[:, cat_features_i[i]]))
        Code_df = pd.concat([Code_df, pd.DataFrame({'Feature':cat_features[i], 'Input':tf, 'Code':cf})], axis = 0)
    
    array = Train_dataset.values
    X_train = array[:, cat_features_i + num_features_i]
    y_train = array[:, target_feature_i]

    # Test arrays
    for i in cat_features_i:
        sub_Test_dataset = Test_dataset.iloc[:, i]
        tf = list(set(list(sub_Test_dataset)))
        le = LabelEncoder()
        le.fit(tf)
        cf = list(set(le.transform(tf)))
        Test_dataset.iloc[:, i] = list(le.transform(Test_dataset.iloc[:, i]))
    
    array = Test_dataset.values
    X_test = array[:, cat_features_i + num_features_i]
    y_test = array[:, target_feature_i]

#----- Save Train/Test dataset -----
if DF.shape[0] != 0:
    Train_dataset.to_csv('X_Train_dataset.csv', index = False)
    Test_dataset.to_csv('X_Test_dataset.csv', index = False)
    Code_df.to_csv('X_Code_Categ_Feat.csv', index = False)
    z.write('X_Train_dataset.csv')
    z.write('X_Test_dataset.csv')
    z.write('X_Code_Categ_Feat.csv')
    os.unlink('X_Train_dataset.csv')
    os.unlink('X_Test_dataset.csv')
    os.unlink('X_Code_Categ_Feat.csv')
else:
    try:
        os.unlink('X_Train_dataset.csv')
        os.unlink('X_Test_dataset.csv')
        os.unlink('X_Code_Categ_Feat.csv')
    except:
        pass

#----- EDA -----
if DF.shape[0] != 0 and Train_dataset.shape[0] != 0 and Test_dataset.shape[0] != 0:
    with st.container():
        col1, col2 = st.columns(2)

        # EDA1
        with col1:
            st.header(':green[EDA1]')
            if Train_dataset.shape[0] != 0 and Test_dataset.shape[0] != 0:
                try:
                    EDA1_a = pd.DataFrame(DF.groupby([target_feature]).size())
                    EDA1_b = pd.DataFrame(Train_dataset.groupby([target_feature]).size())
                    EDA1_c = pd.DataFrame(Test_dataset.groupby([target_feature]).size())

                    EDA1 = pd.concat([EDA1_a, EDA1_b, EDA1_c], axis = 1)
                    EDA1.index.name = 'Class'
                    EDA1.columns = ['Raw', 'Train', 'Test']
                    
                    st.dataframe(EDA1, width = 500 , height = 150)
                    
                    
                except:
                    st.error('Target feature is not a string!', icon = '❌')
                

        # EDA2
            with col2:
                st.header(':green[EDA2]')
                if Train_dataset.shape[0] != 0 and Test_dataset.shape[0] != 0:
                    try:
                        EDA2 = pd.DataFrame({'Dataset': ['Raw'] * X.shape[1] + ['Train'] * X.shape[1] + ['Test'] * X.shape[1]})
                        EDA2_a = DF.drop(target_feature, axis = 1).describe().transpose()
                        EDA2_b = Train_dataset.drop(target_feature, axis = 1).describe().transpose()
                        EDA2_c = Test_dataset.drop(target_feature, axis = 1).describe().transpose()

                        EDA2_e = pd.concat([EDA2_a, EDA2_b, EDA2_c], axis = 0)
                        EDA2_e = EDA2_e.reset_index()
                        EDA2_e = EDA2_e.rename({'index' : 'Features'}, axis = 1)
                        EDA2 = pd.concat([EDA2, EDA2_e], axis = 1)

                        count = []
                        for i in range(0, EDA2.shape[0]):
                            count.append(int(EDA2['count'][i]))
                        EDA2['count'] = count
                        
                        st.dataframe(EDA2, width = 500 , height = 150)
                    except:
                        st.warning('Categorical features in X data. Run EDA analysis manually.', icon = '⚠️')
                        
#----- Save EDA -----
if DF.shape[0] != 0:
    try:
        EDA1.to_csv('EDA1.csv', index = False)
        z.write('EDA1.csv')
        os.unlink('EDA1.csv')
    except:
        pass
    try:
        EDA2.to_csv('EDA2.csv', index = False)
        z.write('EDA2.csv')
        os.unlink('EDA2.csv')
    except:
        pass
else:
    try:
        os.unlink('EDA1.csv')
        os.unlink('EDA2.csv')
    except:
        pass

#----- Modeling -----
if DF.shape[0] != 0 and Train_dataset.shape[0] != 0 and Test_dataset.shape[0] != 0:
    
    with st.container():
        st.header(':blue[Create models]')
        
        algorithm_d = st.multiselect(
            label = 'x',
            options = tuple([
                'Logistic Regression', 'Linear Discriminant Analysis', 'SVM Linear Kernel',
                'Nearest Neighbors', 'Gaussian Naive Bayes', 'CART',
                'Random Forest', 'Multilayer Perceptron 1 Layer', 
            ]),
            key = 'default algorithms',
            max_selections = None, label_visibility = 'collapsed'
        )

        # mounting models
        models = []
        for alg in algorithm_d:
            # Linear
            if alg == 'Logistic Regression':
                model = LogisticRegression(multi_class = 'auto')
                models.append(('Logistic Regression', model))
            if alg == 'Linear Discriminant Analysis':
                model = LinearDiscriminantAnalysis()
                models.append(('Linear Discriminant Analysis', model))
            
            # Support Vector Machine
            if alg == 'SVM Linear Kernel':
                model = LinearSVC()
                models.append(('SVM Linear Kernel', model))
            
            # KNeighbors
            if alg == 'Nearest Neighbors':
                model = KNeighborsClassifier()
                models.append(('Nearest Neighbors', model))
            
            # Naive Bayes
            if alg == 'Gaussian Naive Bayes':
                model = GaussianNB()
                models.append(('Gaussian Naive Bayes', model))
            
            # Decision Tree
            if alg == 'CART':
                model = DecisionTreeClassifier()
                models.append(('CART', model))

            # Ensemble
            if alg == 'Random Forest':
                model = RandomForestClassifier()
                models.append(('Random Forest', model))
            
            # Artificial Neural Network
            if alg == 'Multilayer Perceptron 1 Layer':
                model = MLPClassifier(hidden_layer_sizes = (100,))
                models.append(('Multilayer Perceptron with 1 Layer', model))
                    
        if len(algorithm_d) != 0:
            if st.button('Run', key = 'default run button'):
                count = 1
                prog_bar = st.progress(0)

                try:
                    os.unlink('*.pkl')
                except:
                    pass
                
                mcc_a = []; acc_a = []; prec_a = []; rec_a = []; f1_a = []; names_a = []
                mcc_b = []; acc_b = []; prec_b = []; rec_b = []; f1_b = []; names_b = []
                for name, model in models:
                    
                    # evaluate each model (Train)
                    kfold = StratifiedKFold(n_splits = 10, random_state = 15, shuffle = True)
                    for m in ml_metrics_A:
                        if m == 'roc_auc':
                            try:
                                cv_results = cross_val_score(model, X_train, y_train, cv = kfold, scoring = 'matthews_corrcoef', n_jobs = -1)
                                mcc_a.append(cv_results.mean())
                            except:
                                mcc_a.append(0)
                        if m == 'balanced_accuracy':
                            try:
                                cv_results = cross_val_score(model, X_train, y_train, cv = kfold, scoring = m, n_jobs = -1)
                                acc_a.append(cv_results.mean())
                            except:
                                acc_a.append(0)
                        if m == 'precision_macro':
                            try:
                                cv_results = cross_val_score(model, X_train, y_train, cv = kfold, scoring = m, n_jobs = -1)
                                prec_a.append(cv_results.mean())
                            except:
                                prec_a.append(0)
                        if m == 'recall_macro':
                            try:
                                cv_results = cross_val_score(model, X_train, y_train, cv = kfold, scoring = m, n_jobs = -1)
                                rec_a.append(cv_results.mean())
                            except:
                                rec_a.append(0)
                        if m == 'f1_macro':
                            try:
                                cv_results = cross_val_score(model, X_train, y_train, cv = kfold, scoring = m, n_jobs = -1)
                                f1_a.append(cv_results.mean())
                            except:
                                f1_a.append(0)
                    names_a.append(name)

                    # evaluate each model (Test)
                    model_fitted = model.fit(X_train, y_train)
                    pickle.dump(model_fitted, open(name + '.pkl', 'wb'))
                    z.write(name + '.pkl')
                    os.unlink(name + '.pkl')
                            
                    test_prediction = model.predict(X_test)
                    
                    mcc_b.append(matthews_corrcoef(y_test, test_prediction))
                    acc_b.append(accuracy_score(y_test, test_prediction, normalize = True))
                    prec_b.append(precision_score(y_test, test_prediction, average = 'macro', zero_division = 1))
                    rec_b.append(recall_score(y_test, test_prediction, average = 'macro', zero_division = 1))
                    f1_b.append(f1_score(y_test, test_prediction, average = 'macro'))
                    
                    st.session_state['switch_on_off'] = False
                    
                    percent_complete = count/(len(algorithm_d))
                    prog_bar.progress(percent_complete)
                    count += 1
                
                with st.container():
                    st.write('---')
                    col1, col2 = st.columns(2)
                    
                    with col1:
                    
                        # train results
                        perf_train_default = pd.DataFrame({
                            'Model':names_a,
                            'MCC':mcc_a, 'Accuracy':acc_a, 'Precision':prec_a,'Recall':rec_a,'F1':f1_a
                        })
                        perf_train_default = perf_train_default.sort_values(by = ['MCC', 'F1', 'Precision', 'Accuracy'], ascending = False)
                        st.session_state['perf_train_default'] = perf_train_default
                        
                        st.subheader('Train results')
                        st.dataframe(perf_train_default, width = 600 )
                
                    with col2:
                        # test results
                        perf_test_default = pd.DataFrame({
                            'Model':names_a,
                            'MCC':mcc_b, 'Accuracy':acc_b, 'Precision':prec_b,'Recall':rec_b,'F1':f1_b
                        })
                        perf_test_default = perf_test_default.sort_values(by = ['MCC', 'F1', 'Precision', 'Accuracy'], ascending = False)
                        st.session_state['perf_test_default'] = perf_test_default
                        
                        st.subheader('Test results')
                        st.dataframe(perf_test_default, width = 600 )
                

        else:
            if st.button('Run'):
                st.error('Select at least one algorithm to generate a model!', icon = '❌')


#----- Save metrics comparison -----
if DF.shape[0] != 0 and st.session_state['perf_train_default'].shape[0] != 0:
    perf_train_default = st.session_state['perf_train_default']
    perf_train_default.to_csv('1_Compare_All_Default_Models.csv', index = False) 
    z.write('1_Compare_All_Default_Models.csv') 
    os.unlink('1_Compare_All_Default_Models.csv')
else:
    try:
        os.unlink('1_Compare_All_Default_Models.csv')
    except:
        pass


#----- 
if DF.shape[0] != 0:
    with st.container():
        #st.write('---')
        col1, col2, col3, col4, col5 = st.columns(5)
        
        with col3:
            z.close()
            with open('results.zip', 'rb') as fp:
                st.download_button(
                    label = 'Download Results',
                    data = fp,
                    file_name = 'results.zip',
                    mime = 'application/zip',
                    disabled = st.session_state['switch_on_off']
                )
                
            st.session_state['switch_on_off'] = True
            z = zipfile.ZipFile('results.zip', 'w', zipfile.ZIP_DEFLATED)
                
            





