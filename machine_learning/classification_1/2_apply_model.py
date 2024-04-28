#----- Load libraries -----
import streamlit as st

import pandas as pd
import numpy as np

import joblib

from sklearn.linear_model import LogisticRegression
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.svm import LinearSVC
from sklearn.neighbors import KNeighborsClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.neural_network import MLPClassifier

#----- Streamlit config -----
st.set_page_config(
    page_title = 'ML Classification - Apply model',
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

#----- Options -----
with st.container():
            st.write('---')
            col1, col2, col3, col4 = st.columns([2, 2, 2, 2])
            
            with col1:
                st.header(':blue[Load model file]')
                
                uploaded_file = st.file_uploader('', label_visibility = 'collapsed')
                if uploaded_file is not None:
                    model = joblib.load(uploaded_file)
            
            with col2:
                st.header(':green[body_depth]')
                
                body_depth = st.slider(label = 'body_depth', min_value = 6.5, max_value = 21.5, value = 10.0, step = 0.5, label_visibility = 'hidden')
            
            with col3:
                st.header(':green[carapace width]')
                
                carapace_width = st.slider(label = 'carapace_width', min_value = 17.5, max_value = 54.5, value = 25.0, step = 0.5, label_visibility = 'hidden')
            
            with col4:
                st.header(':green[carapace length]')
                
                carapace_length = st.slider(label = 'carapace_length', min_value = 15.0, max_value = 47.5, value = 20.0, step = 0.5, label_visibility = 'hidden')
                
with st.container():
            st.write('---')
            col1, col2, col3 = st.columns([2, 2, 1])

            with col1:
                st.header(':green[frontal lobe]')
                
                frontal_lobe = st.slider(label = 'frontal_lobe', min_value = 7.5, max_value = 23.0, value = 10.0, step = 0.5, label_visibility = 'hidden')
            
            with col2:
                st.header(':green[rear width]')
                
                rear_width = st.slider(label = 'rear_width', min_value = 6.5, max_value = 20.0, value = 10.0, step = 0.5, label_visibility = 'hidden')
            
            with col3:
                st.header(':green[sex]')
                
                sex = st.radio(
                'sex', options = ['Female', 'Male'], horizontal = True, label_visibility = 'hidden')
                

if sex == 'Female':
    sex = 0
if sex == 'Male':
    sex = 1

DF = pd.DataFrame({
    'sex':[sex],
    'Frontal_Lobe':[frontal_lobe],
    'rear_width':[rear_width],
    'carapace_length':[carapace_length],
    'carapace_width':[carapace_width],
    'body_depth':[body_depth]
})

st.write('---')
if uploaded_file is not None:
    result = model.predict(DF)
    st.header(':blue[Results]')
    st.subheader('Species: **:orange[' + str(list(result)[0]) + ']**')

