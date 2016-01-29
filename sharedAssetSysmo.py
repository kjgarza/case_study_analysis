import pandas as pd


class Logtable:
    ## Add Permissions to a Log table
    def __init__(self):
        self.data = []
    def make(self, x):
        self.data.append(x)

    def grab(self, x):
        """


        :type x: pd.DataFrame
        :param x: is a dataframe from a logtable 
        """
        assert isinstance(x, pd.DataFrame)
        self.data.append(x)



log = Logtable()
log.make("dsdsds")
