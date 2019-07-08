# Create nice plots for Latex
# http://blog.mdda.net/oss/2015/11/22/matplotlib-for-latex

import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns
import matplotlib
import math

SPINE_COLOR = 'gray'

def savefig(filename):
    plt.savefig('latex_figures/{}.pgf'.format(filename))
    #plt.savefig('figure_{}.pdf'.format(filename))
 
def setup(latex, width=16, height=10, style="darkgrid"):
    """
    Setup the figures.

    Parameters
    ----------
    latex       : Make the plots ready for latex
    width       : optional, if not latexify, set the figure width (inch)
    height      : optional, if not latexify, set the figure height (inch)
    """
    sns.set(style=style)
    if latex:
        latexify()
    
    if not latex:
        
        fontsize = 24
        params = {
        'backend': 'ps',
        #'text.latex.preamble': ['\usepackage{gensymb}'],
        'axes.labelsize':     fontsize, # fontsize for x and y labels (was 10)
        'axes.titlesize':     fontsize,
        'font.size':          fontsize, # was 10
        'legend.fontsize':    fontsize, # was 10
        'xtick.labelsize':    fontsize,
        'ytick.labelsize':    fontsize,
        'text.usetex':        True,
        'figure.figsize':     [width,height],
        'font.family':        'serif'
        }

        matplotlib.rcParams.update(params)
        plt.tight_layout()
        #fig.set_size_inches(width, height)
    fig, ax = plt.subplots() 
    return fig, ax

def latexify(nrows=1, ncols=1.0):
    """Set up matplotlib's RC params for LaTeX plotting.
    Call this before plotting a figure.

    Parameters
    ----------
    ncols       : float, optional, fraction of textwidth
    nrows       : float, optional, multiplier for height
    """

    # code adapted from http://www.scipy.org/Cookbook/Matplotlib/LaTeX_Examples

    # Width and max height in inches for IEEE journals taken from
    # computer.org/cms/Computer.org/Journal%20templates/transactions_art_guide.pdf

    fig_width_pt = 395.45181 * ncols   # Get this from LaTeX using \the\textwidth
    fig_width = 5.47293*ncols        # From \printinunitsof{in}\prntlen{\textwidth}
    inches_per_pt = 1/72.27  # Convert pt to inch
    # fig_width = fig_width_pt * inches_per_pt

    golden_mean = (math.sqrt(5)-1.0)/2.0    # Aesthetic ratio
    fig_height = fig_width*golden_mean * nrows # height in inches

    MAX_HEIGHT_INCHES = 8.0
    if fig_height > MAX_HEIGHT_INCHES:
        print("WARNING: fig_height too large: {} would you consider {} inches?".format(fig_height, MAX_HEIGHT_INCHES))
        # fig_height = MAX_HEIGHT_INCHES
    fontsize = 8 # int(8/ncols)
    params = {
      'backend': 'ps',
      #'text.latex.preamble': ['\usepackage{gensymb}'],
      'axes.labelsize':     fontsize, # fontsize for x and y labels (was 10)
      'axes.titlesize':     fontsize,
      'font.size':          fontsize, # was 10
      'legend.fontsize':    fontsize, # was 10
      'xtick.labelsize':    fontsize,
      'ytick.labelsize':    fontsize,
      'text.usetex':        True,
      'figure.figsize':     [fig_width,fig_height],
      'font.family':        'serif'
    }
    print("Setting figsize to {} x {}".format(fig_width, fig_height))
    matplotlib.rcParams.update(params)
    plt.tight_layout()

def format_axes(ax):
    for spine in ['top', 'right']:
        ax.spines[spine].set_visible(False)

    for spine in ['left', 'bottom']:
        ax.spines[spine].set_color(SPINE_COLOR)
        ax.spines[spine].set_linewidth(0.5)

    ax.xaxis.set_ticks_position('bottom')
    ax.yaxis.set_ticks_position('left')

    for axis in [ax.xaxis, ax.yaxis]:
        axis.set_tick_params(direction='out', color=SPINE_COLOR)

    return ax