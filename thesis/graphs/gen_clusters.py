# Turn a list of ids into a list of frequencies

import matplotlib.pyplot as plt
import sys
import time

def gen_clusters(data_path, kind):
    for i in range(0, 8):
        sizes = []
        cluster_size = 0
        for line in open(f'{data_path}/{kind}/exercise{i + 1}').readlines():
            if line.strip() == '':
                sizes.append(cluster_size)
                #print(cluster_size)
                cluster_size = 0
            else:
                cluster_size += 1

        with plt.style.context('seaborn-notebook'):
            fig = plt.figure(figsize=(10,10))
            ax = fig.add_subplot(111)
            ax.set_position([0, 0, 1, 1])
            ax.pie(sizes)
            ax.axis('equal')
            fig.savefig(f'cluster-{kind}-{i + 1}.png')

gen_clusters('../../data', 'baseline')
gen_clusters('../../data', 'aggressive')
