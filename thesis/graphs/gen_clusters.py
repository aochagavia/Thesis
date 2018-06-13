# Turn a list of ids into a list of frequencies

import matplotlib.pyplot as plt
import sys
import time

from clusters import get_cluster_sizes

def gen_clusters(kind):
    for i in range(0, 8):
        sizes = get_cluster_sizes(kind, i + 1)

        with plt.style.context('seaborn-notebook'):
            fig = plt.figure(figsize=(10,10))
            ax = fig.add_subplot(111)
            ax.set_position([0, 0, 1, 1])
            ax.pie(sizes)
            ax.axis('equal')
            fig.savefig(f'cluster-{kind}-{i + 1}.png')

gen_clusters('baseline')
gen_clusters('aggressive')
