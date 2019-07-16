# Titanic Survival

* NB: the information below is extracted from the Overview in Part 1 of the project.

## Goals

1. Participate in the [Titanic Competition](https://www.kaggle.com/c/titanic) in Kaggle;
2. Perform **Exploratory Data Analysis (EDA)** and **Data Visualizations (Data Viz)** in **R**;
3. Create **Reproducible RMarkdown** files and **PDF or HTML reports**, weaving code and comments using **RStudio** and **knitr**;
4. Predict survival in the Titanic, evaluating various **machine-learning** algorithms using **Python**;
5. Emulate **production-ready code**, treating the test set as "future data";
6. Clean, model, and predict using a **single pipeline approach** using **Jupyter Notebook**;
7. Share the process in **Rpubs** and **GitHub** and get feedback.

## Audience and Scope

This project is intended to anyone willing to read any portion of it, for whatever reason. People that might benefit from the detailed code and comments are those learning R or Python, data analysis or machine learning, or how to participate in a Kaggle competition. For those willing to interact and suggest improvements, I welcome pull requests. 

The **scope** of this project is limited: however detailed, I am just capturing a slice in an iterative, continuous process. An often used terminology, the "data science pipeline" is a bit of a misnomer. Each phase of this so-called pipeline is an epicycle within a larger cycle, and all phases are connected. It is helpful to separate the entire effort into discrete phases:  

1. [re]defining a question
2. business understanding 
3. exploratory data pre-processing and analysis
4. model building and evaluation
5. model deployment

Yet each of these parts depends on and is enlightened by the other parts. With deployment comes better business understanding and more data, which can redefine the original question, generating new explorations and models, and so forth. After a first wave of EDA, the second wave is enlightened by the first. After EDA, a much better question and business understanding might ensue. 

In short, publishing results of an exploratory data analysis simply captures a frame in an ongoing movie. 


## Titanic Competition

**Kaggle** is a platform for **data science**, hosting data, competitions and more. The Titanic Competition is designed as an introduction to Kaggle competitions, machine-learning, or both. For an introduction to the competition, visit [Kaggle](https://www.kaggle.com/c/titanic).

In the Titanic disaster, 1502 people died and 722 survived. The baseline survival rate therefore is 32.46%. In the competition, we are given a training dataset with labels (survived or not) and a test dataset to predict on, with the same features (attributes) but no label. The hypothesis is that passenger characteristics carry information that has predictive power for the outcome. This hypothesis is valid, yet only to a certain degree (more on this below).

Since the test set has 418 rows, we are not being asked to predict every passenger's outcome, but just the outcome of 418 passengers, or rather, we predict the survival of about 136 people. One simple way to predict survival is to note attributes in which a majority survived, such as 'women', 'children', and the 'upper class' (i.e. 1st class passengers, or those with high fares), and predict these individuals to survive. This could be thought of as a baseline model whose performance we can improve on with machine learning.

In this project, I will explore the data, create informal hypotheses to help modeling, build machine-learning models of various complexities, evaluating them on their accuracy and generalization, and implement a production-ready pipeline to deploy the the most accurate and generalizable model.


## Exploratory Data Analysis and R

Through summaries and plots, we explore the various distributions and potential associations of variables in the data, so we can better understand it and build better machine-learning models.

I decided to use R and RStudio because they lend themselves well for this kind of work. The `plot()` function in R will take almost any type of data structure and try to plot it. R has an interactive console so one can quickly type one liners or copy/paste a few lines and get immediate results. R was written by statisticians and much of the necessary functionality for statistics is built into it. 

In RStudio, we can create an RMarkdown report and weave text and code using the `knitr` package. Output formats include html (this page) and pdf. 

In this report, I go a bit beyond simple plots and use the `ggplot2` package, but I am not aiming for the most polished plots since this is just data exploration. 


## Machine Learning and Python

This project is also my response to Exercise 3 at the end of Chapter 3 of Aurelien Geron's book [Hands-On Machine Learning with Scikit-Learn and TensorFlow](https://github.com/ageron/handson-ml), an excellent resource to anyone delving into the world of machine learning and data science. The book concentrates on implementations of machine learning using Python, whose modules are a self-contained universe for data science, able to easily implement production-ready coding pipeline that clean, model, and predict.

In the Titanic Competition, the goal is to predict who survived the tragedy in the test data. Kaggle split the data for competitors to ensure comparable scores. The ML algorithm is built and evaluated on the train set, which has labels (the outcome variable - Survived or Not), and we submit the labels for the test set. Kaggle evaluates these predictions based on their *accuracy*.

* Classification and Accuracy

Predicting a binary outcome is a classification task. Classifiers (models that classify) are evaluated using some kind of performance metric which stems from a way to partition the [confusion matrix](https://en.wikipedia.org/wiki/Confusion_matrix). One can use the entire matrix itself or consider the problem space of all matrices (PR and ROC curves), but that goes beyond the scope of the titanic competition. 

*Accuracy* is defined as the number of times we predict the correct outcome, whether positive or negative (survived, or not) over all the predictions we made. 


## Making sense of the Titanic Competition

This competition is purely for learning and based on an honor code of not looking up results in the historical record, which many participants fail to observe as there are numerous perfect prediction scores. Perfect prediction is not possible, there is an amount of **irreducible error** in the data, meaning, there is a limit to the amount of information we can extract from the data, in order to predict survivability. This limit is debatable and the subject of many discussions online.

Driving for the **highest accuracy** is not necessarily a worthy goal. Many ML models that consistently win competitions and produce the highest scores (gbm, xgboost, and the like) are also well-known for **overfitting** the data. This, coupled with the ability to submit many predictions for the same test set, lead to overfitting the test set, in other words, the machine algorithms are tailored to predict a particular test set and not new data.

Since the value of machine learning algorithms lies in predicting on new data, my approach will be to use this  competition to understand the data, and aim toward building a robust and generalizable machine-learning algorithm, emulating a production environment of sorts, with pipelines in Python that clean, model, and predict all at once.


## Project Parts

In **Part 1** of the Titanic Survival project I conduct **Exploratory Data Analisys (EDA)** of the [Kaggle Titanic train dataset](https://www.kaggle.com/c/titanic/data) in R, creating an **RMarkdown report** with RStudio and the `knitr` package, with summary tables and visualizations, performing minor pre-processing as needed. This report can be found in [Rpubs](http://rpubs.com/BigBangData/512981) or as an html file in this repo. 

In **Part 2** of the project I perform all the necessary pre-processing steps for **machine learning models**, conduct **model evaluation and regularization**, and run predictions using Python in a **Jupyter Notebook**. Given a final model, I run a **single pipeline** for **pre-processing and modeling** that emulates a **production environment**, where the Titanic test set is used as if it were future data never before seen.


---

Marcelo Sanches - July 16, 2019
