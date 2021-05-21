"""
All data processing scripts for the thesis
"""

import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import seaborn as sns



def plot_lwc(path, vals="a"):
    data = pd.read_csv(path, sep=";")
    
    val_selection = set()
    if "a" in vals:
        val_selection = ["lines", "words", "chars"]
    else:
        if "l" in vals:
            val_selection.add("lines")
        if "w" in vals:
            val_selection.add("words")
        if "c" in vals:
            val_selection.add("chars")
    
    filtered = data[data["file_name"].map(get_category) == "total"]
    melted = filtered.melt( value_vars=list(val_selection)
                        , var_name="counted"
                        , id_vars=["file_name"]
                        , value_name="count" )
    parsed_names = melted
    parsed_names["project_name"]  = melted["file_name"].map(get_project)
    parsed_names["category_name"] = melted["file_name"].map(get_category)
    print(parsed_names)
    
    sns.set()
    sns.barplot( x="project_name"
               , y="count"
               , hue="counted"
               , palette="mako"
               , edgecolor=".1"
               , data=parsed_names )
    plt.show()


def plot_lwc_density(path):
    data = pd.read_csv(path, sep=";")
    data["words/lines"] = data.apply(lambda row: row["words"] / row["lines"], axis=1)
    data["chars/lines"] = data.apply(lambda row: row["chars"] / row["lines"], axis=1)
    data["chars/words"] = data.apply(lambda row: row["chars"] / row["words"], axis=1)
    
    # filtered = data[data["file_name"].map(get_category) == "total"]
    filtered = data
    melted = filtered.melt( value_vars=["words/lines", "chars/lines", "chars/words"]
                        , var_name="stats"
                        , id_vars=["file_name"]
                        , value_name="ratio" )
    parsed_names = melted
    parsed_names["project_name"] = melted["file_name"].map(get_project)
    # parsed_names["category_name"] = melted["file_name"].map(get_category)
    print(parsed_names)
    
    sns.set()
    sns.barplot( x="project_name"
               , y="ratio"
               , hue="stats"
               , palette="mako"
               , edgecolor=".1"
               , data=parsed_names )
    plt.show()


def get_project(string):
    return string.split("/")[0]

def get_category(string):
    return "/".join(string.split("/")[1:])


if __name__ == "__main__":
    plot_lwc("wc-stats.csv", "l")
    # plot_lwc_density("wc-stats.csv")


