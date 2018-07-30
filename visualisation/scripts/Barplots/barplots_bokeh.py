#!/usr/bin/env python
# coding: utf8

from bokeh.io import show, output_file
from bokeh.io import export_png

from bokeh.models import ColumnDataSource, FactorRange
from bokeh.models import LinearAxis, Range1d
from bokeh.models import Legend
from bokeh.models.mappers import CategoricalColorMapper

from bokeh.palettes import brewer
from bokeh.plotting import figure
from bokeh.transform import factor_cmap

import pandas as pd
import argparse, copy
from math import pi
import logging
logging.basicConfig()

def makeValues(df, start, list_sp):
    """ store values of a line in a dictionary containing counts, frequencies and pvalues for each species
    (keys=lines index). Read on every two lines

    Args:
        - df : a pandas dataFrame
        - start (int) : which lines to store (0: counts, 1:frequencies, 3: pvalues)
        - list_sp (list of str) : the species names

    Return:
        - dic (dict)
    """
    dic = {}
    for i in range(start, len(df), 3):
        dic[list_sp[i/3]] = list(df.iloc[i])
    return dic

def make_barplot(species, xaxis, dic_yaxis, dic_yaxis_pvalues, what, x_range, width, end_title):
    colors = ["#75968f", "#dfccce", "#550b1d"]
    #colors=brewer['RdYlBu'][3]
    title = '{} on {} {}'.format(what, species, end_title)

    pvalues_factor = copy.deepcopy(dic_yaxis_pvalues[species])

    # Transform pvalues in categorical data for color range
    for i in range(0,len(dic_yaxis_pvalues[species])):
        if dic_yaxis_pvalues[species][i] < 0.05:
            pvalues_factor[i] = 'lower'
        elif dic_yaxis_pvalues[species][i] > 0.95:
            pvalues_factor[i] = 'higher'
        else :
            pvalues_factor[i] = 'unsignificant'

    # Color range mapper
    mapper = CategoricalColorMapper(palette=colors, factors=['lower', 'unsignificant', 'higher'])

    p = figure(x_range=x_range[end_title], 
        plot_width=width, 
        plot_height=350, 
        toolbar_location=None, 
        title=title)

    if end_title == 'codons':
        p.xaxis.major_label_orientation = pi / 3

    source = ColumnDataSource(data=dict(what=xaxis, counts=dic_yaxis[species], label=pvalues_factor))

    r1 = p.vbar(x='what', 
        top='counts', 
        width=0.9, 
        source=source, 
        line_color='white', 
        color={'field': 'label', 'transform':mapper},
        legend='label')    
    
    p.legend.orientation = 'horizontal'
    p.legend.location = 'top_right'

    export_png(p, filename='{}.png'.format(title.replace(" ","_")))

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("file", help="Input csv file")
    parser.add_argument("represent", choices=("counts", "frequencies", "both"), help="choose what to plot")
    args = parser.parse_args()

    df = pd.read_csv(args.file, sep=",", index_col=0)   

    # 1 - species (do not sort !)    
    list_sp = [str.split(sps, "_")[0] for sps in list(df.index.values)[0::3]]

    # 2 - codons/aa/aatypes names
    xaxis = list(df)
    if len(xaxis) == 4:
        end_title = "amino-acids-types"
        width = 500
    if len(xaxis) == 20:
        end_title = "amino-acids"
        width = 650
    if len(xaxis) == 61:
        end_title = "codons"
        width = 900

    # 3 - store values
    dic_yaxis_counts = makeValues(df, 0, list_sp)
    dic_yaxis_freqs = makeValues(df, 1, list_sp)
    dic_yaxis_pvalues = makeValues(df, 2, list_sp)

    # For ordering xaxis according to amino-acids types
    x_range = {'codons' : ['ttt','ttc','tgg','tat','tac','gat','gac','gaa','gag','cat',
                            'cac','aaa','aag','cgt','cgc','cga','cgg','aga','agg','tgt',
                            'tgc','aat','aac','cct','cca','ccg','ccc','caa','cag','tct',
                            'tcc','tca','tcg','agt','agc','act','acc','aca','acg','gct',
                            'gcc','gca','gcg','ggt','ggc','gga','ggg','att','atc','ata',
                            'tta','ttg','ctt','ctc','cta','ctg','atg','gtt','gtc','gta','gtg'],
                'amino-acids' : ['F','W','Y','D','E','H','K','R','C','N','P','Q','S','T','A','G','I','L','M','V'],
                'amino-acids-types' : ['aromatics','charged','polar','unpolar']}
    
    for species in list_sp:
        if args.represent in ['counts', 'both']:
            make_barplot(species, xaxis, dic_yaxis_counts, dic_yaxis_pvalues, 'Counts', x_range, width, end_title)
        if args.represent in ['frequencies', 'both'] :
            make_barplot(species, xaxis, dic_yaxis_freqs, dic_yaxis_pvalues, 'Frequencies', x_range, width, end_title)

if __name__ == "__main__":
    main()
