# -*- coding: utf-8 -*-
"""
Created on Thu Nov  3 17:30:46 2022

@author: HP
"""
#Machine Learning Desktop App

##F#rontend implementation

#Import tkinter library
import tkinter as tk
from tkinter import Menu

# Define window object and set geometry
win_ml = tk.Tk()
win_ml.title("MLAPP (Machine Learning App)")
win_ml.geometry("500x300")

#Add menu bar
menubar = Menu(win_ml)
win_ml.config(menu=menubar)

#Create menu to be added to the menu bar
menu_file = Menu(menubar,tearoff=0)
menu_preprocess =Menu(menubar,tearoff=0)
menu_partition =Menu(menubar,tearoff=0)
menu_resample =Menu(menubar,tearoff=0)
menu_train =Menu(menubar,tearoff=0)
menu_evaluate =Menu(menubar,tearoff=0)
menu_test =Menu(menubar,tearoff=0)
menu_result =Menu(menubar,tearoff=0)
menu_help =Menu(menubar,tearoff=0)


#Add items to the menu_file
menu_file.add_command(label="New")
menu_file.add_command(label="Open")
menu_file.add_command(label="Save as")
menu_file.add_command(label="Save")
menu_file.add_separator()

#Add exit item to the menu_file
menu_file.add_command(label="Exit", command=win_ml.destroy)

#Add items to the menu_preprocess
menu_preprocess.add_command(label="Normalize")
menu_preprocess.add_command(label="Standardize")
menu_preprocess.add_command(label="Scale")
menu_preprocess.add_command(label="Encode")
menu_preprocess.add_command(label="Reduce Dimension")

#Add items to the menu_partition
menu_partition.add_command(label="Random Sample")
menu_partition.add_command(label="Stratified Sample")

#Add items to the menu_resample
menu_resample.add_command(label="K-Fold CV")
menu_resample.add_command(label="Bootstrap")

#Add items to the menu_train
menu_train.add_command(label="Multi Adaptive Regression Spline")
menu_train.add_command(label="K Nearest Neighbour")
menu_train.add_command(label="Decision Tree")
menu_train.add_command(label="Support Vector Machine")
menu_train.add_command(label="Deep Learning Neural Network")

#Add submenu to the menu_train
submenu_linear = Menu(menu_train, tearoff=0)
submenu_ensemble = Menu(menu_train, tearoff=0)

#Addd items to the submenu_linear
submenu_linear.add_command(label="Ordinary Least Squares")
submenu_linear.add_command(label="Partial Least squares")
submenu_linear.add_command(label="Principal Component Regression")
submenu_linear.add_command(label="Logistic Regression")
submenu_linear.add_command(label="Regularized Regression")

#Addd items to the submenu_ensemble
submenu_ensemble.add_command(label="Bootstrap Aggregation")
submenu_ensemble.add_command(label="Random Forest")
submenu_ensemble.add_command(label="Gradient Boosting Machine")
submenu_ensemble.add_command(label="Extreme Gradient Boosting Machine")

#Add submenu to the menu_evaluate
submenu_regress = Menu(menu_evaluate, tearoff=0)
submenu_classify = Menu(menu_train, tearoff=0)

#Addd items to the submenu_linear
submenu_regress.add_command(label="MSE")
submenu_regress.add_command(label="RMSE")
submenu_regress.add_command(label="MAE")
submenu_regress.add_command(label="RSQ")

#Addd items to the submenu_classify
submenu_classify.add_command(label="Accuracy")
submenu_classify.add_command(label="Sensitivity")
submenu_classify.add_command(label="Specificity")
submenu_classify.add_command(label="ROC")
submenu_classify.add_command(label="Gini Index")
submenu_classify.add_command(label="Cross Entropy")

#Add items to the menu_test
menu_test.add_command(label="Test")
menu_test.add_command(label="Predict")

#Add items to the menu_result
menu_result.add_command(label="Table")

#Add submenu to the menu_result
submenu_plot = Menu(menu_result, tearoff=0)

#Addd items to the submenu_plot
submenu_plot.add_command(label="VIP")
submenu_plot.add_command(label="PDP")
submenu_plot.add_command(label="ROC")
submenu_plot.add_command(label="ANN")
submenu_plot.add_command(label="Tree")

#Add items to the menu_help
menu_help.add_command(label="MLAPP Documentation")
menu_help.add_command(label="Tutorial")
menu_help.add_command(label="Shortcut Summary")

#Add menu to the menu bar
menubar.add_cascade(label="File",menu=menu_file)
menubar.add_cascade(label="Preprocess",menu=menu_preprocess)
menubar.add_cascade(label="Partition",menu=menu_partition)
menubar.add_cascade(label="Resample",menu=menu_resample)
menubar.add_cascade(label="Train",menu=menu_train)
menubar.add_cascade(label="Evaluate",menu=menu_evaluate)
menubar.add_cascade(label="Test",menu=menu_test)
menubar.add_cascade(label="Result",menu=menu_result)
menubar.add_cascade(label="Help",menu=menu_help)

#Add submenu to the menus
menu_train.add_cascade(label="Linear Models",menu=submenu_linear)
menu_train.add_cascade(label="Ensemble Models",menu=submenu_ensemble)
menu_evaluate.add_cascade(label="Regression",menu=submenu_regress)
menu_evaluate.add_cascade(label="Classification",menu=submenu_classify)
menu_result.add_cascade(label="Plot",menu=submenu_plot)


#Define event loop
tk.mainloop()
