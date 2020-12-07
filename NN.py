import copy
import numpy as np
import pandas as pd
from sklearn.model_selection import KFold
from sklearn.model_selection import train_test_split
import torch
import torch.nn as nn
import torch.nn.functional as F
import torch.optim as optim
# from torchsample.modules import ModuleTrainer

class VanillaNN(nn.Module):
    def __init__(self,n_input, n_hidden, n_output):
        super().__init__()
        self.hidden = nn.Linear(n_input,n_hidden)
        self.predict = nn.Linear(n_hidden,n_output)

    def forward(self,x):
        x = F.relu(self.hidden(x))
        x = self.predict(x)
        return x


# define the trainable object
class TrainableVanillaNN():
    def __init__(self,model):

        self.model = model
        self.iteration = 100



    def set_training_iteration(self,iter):
        self.iteration = iter




    def train(self,X_train, Y_train, X_validation, Y_validation):


        X_train, Y_train = torch.tensor(X_train).float(), torch.tensor(Y_train).float()
        X_validation, Y_validation = torch.tensor(X_validation).float(), torch.tensor(Y_validation).float()


        optimizer = optim.Adam(self.model.parameters())

        for t in range(self.iteration):
            pred = self.model(X_train)  # input x and predict based on x
            criterion = nn.MSELoss()
            loss = torch.sqrt(criterion(pred, Y_train))
            optimizer.zero_grad()  # clear gradients for next train
            loss.backward()  # backpropagation, compute gradients
            optimizer.step()  # apply gradients

        print("the rmse on the training set is %.4f" % loss.data.cpu().numpy())


        # use validation set to see the model's performance on validation set
        pred = self.model(X_validation)
        criterion = nn.MSELoss()
        loss = torch.sqrt(criterion(pred, Y_validation))

        print("the rmse on the validation set is %.4f" % loss.data.cpu().numpy())

        # return the loss on validation set
        return loss.data.cpu().numpy()

    # evaluate on the test set
    def evaluate(self, X_test, Y_test):
        with torch.no_grad():
            X_test, Y_test = torch.tensor(X_test).float(), torch.tensor(Y_test).float()

            pred = self.model(X_test)
            # print(pred)
            criterion = nn.MSELoss()
            loss = torch.sqrt(criterion(pred, Y_test))
            print("the rmse on the testing set is %.4f" % loss.data.cpu().numpy())


    # cross-validation
    # split the data in X and Y to n_folds, 1 fold is testing set, and the other n-1 folds are training sets
    def cross_validation(self,X, Y, n_folds):

        kf = KFold(n_splits=n_folds)
        kf.get_n_splits(X)

        res = 0
        baseline = 0

        cloned_model = copy.deepcopy(self.model)

        for train_index, test_index in kf.split(X):

            self.model = copy.deepcopy(cloned_model)

            X_train, X_test = X[train_index], X[test_index]

            Y_train, Y_test = Y[train_index], Y[test_index]

            res += self.train(X_train,Y_train,X_test,Y_test)

            pred = np.mean(Y_train.ravel())
            baseline += np.sqrt(np.mean((pred - Y_test.ravel()) ** 2))


        res /= n_folds
        baseline /= n_folds
        self.model = copy.deepcopy(cloned_model)

        print("After cross validation with %d folds" % n_folds)
        print("Using the VanillaNN, the average rmse is %.4f" % res)
        print("Using the mean estimator, the average rmse is %.4f" % baseline)

    def tuning_iteration_steps(self, X_train,Y_train,candidates):

        for i in candidates:
            print("----------- number of iterations is %d -----------" % i)
            self.set_training_iteration(i)
            self.cross_validation(X_train,Y_train,2)









if __name__ == "__main__":

    data = pd.read_spss('1.sav')
    print(data.dtypes)

    # note that there are missing values in the data

    # delete the observations that has missing values in USA_DD_Score
    data = data[~pd.isna(data['USA_DD_Score_Cleaned'])]

    # impute median for the continuous variable
    # impute 'Unknown' for the categorical variable
    for i in range(data.shape[1]):
        if data.iloc[:,i].dtype == np.float64:
            data.iloc[pd.isna(data.iloc[:,i]),i] = np.nanmedian(data.iloc[:,i])
        else:
            # convert categorical to string
            data.iloc[:,i] = data.iloc[:,i].astype(str)
            data.iloc[pd.isna(data.iloc[:, i]), i] = "Unknown"

    # one hot encodings for categorical variables
    # if we include the country variable, it will substantially increase the dimension of the data, so decide not to include it
    data.drop(data.columns[[1,2]],axis=1, inplace=True)

    data_dummy = pd.get_dummies(data)
    print(data_dummy.shape)



    # response varialbe we want to predict
    Y = data_dummy['USA_DD_Score_Cleaned']
    X = data_dummy.drop('USA_DD_Score_Cleaned',axis = 1)


    # load the data
    Y = Y.values
    X = X.values
    # add a dimension to Y
    Y = Y.reshape(len(Y),1)


    # ---------- tune parameter process, uncomment them if you want to retune the parameters ----------

    # # baseline rmse using the naive estimator Y_bar
    # X_train, X_test, Y_train, Y_test = train_test_split(
    #     X, Y, test_size=0.2)
    # pred = np.mean(Y_train.ravel())
    # baseline_rmse = np.sqrt(np.mean((pred - Y_test.ravel()) ** 2))
    # print("Using the mean estimator, the rmse is %.4f" % baseline_rmse)
    #
    #
    #
    #
    # # use one-hidden layer vanilla network with hidden size = 50 to predict Y
    # model = VanillaNN(n_input=X.shape[1], n_hidden=50, n_output=1)
    # trainable_model = TrainableVanillaNN(model)
    #
    #
    # # tune the hyperparameters: 1. number of hidden layers; 2.number of training iteration
    # iteration_candidates = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
    # trainable_model.tuning_iteration_steps(X_train,Y_train,iteration_candidates)
    #
    #
    # # use the best iteration, iteration = 80, choose best hidden_size
    # hidden_layer_candidates = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
    # for i in hidden_layer_candidates:
    #     print("----------- hidden size is %d -----------" % i)
    #     model = VanillaNN(n_input=X.shape[1], n_hidden=i, n_output=1)
    #     trainable_model = TrainableVanillaNN(model)
    #     trainable_model.set_training_iteration(80)
    #     trainable_model.cross_validation(X_train, Y_train, 2)
    #
    #
    #
    #
    #
    #
    # # use hidden_size = 40, iteration = 80
    #
    # model = VanillaNN(n_input=X.shape[1], n_hidden=40, n_output=1)
    # trainable_model = TrainableVanillaNN(model)
    # trainable_model.set_training_iteration(80)
    # trainable_model.train(X_train,Y_train,X_test,Y_test)



    # use cv on the whole dataset, use iteration = 80, hidden size = 40 by fine-tuning
    model = VanillaNN(n_input=X.shape[1], n_hidden=40, n_output=1)
    trainable_model = TrainableVanillaNN(model)
    trainable_model.set_training_iteration(80)
    trainable_model.cross_validation(X,Y,5)



