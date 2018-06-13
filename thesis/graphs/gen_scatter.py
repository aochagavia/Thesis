import matplotlib
import matplotlib.pyplot as plt
import numpy as np

from clusters import get_cluster_sizes

# Transforms a list of sizes into a list of coverage
# i.e. 73, 10, 5 => 73, 83, 88
def size_to_coverage(sizes):
    carry = 0
    for i in range(0, len(sizes)):
        add = sizes[i]
        sizes[i] += carry
        carry += add

def gen_scatter():
    for i in range(0, 8):
        baseline = get_cluster_sizes('baseline', i + 1)
        aggressive = get_cluster_sizes('aggressive', i + 1)
        total_progs = sum(baseline)

        # Sanity check
        assert total_progs == sum(aggressive)

        # We want coverage, not cluster size
        size_to_coverage(baseline)
        size_to_coverage(aggressive)

        # Plot the biggest 5 clusters
        max_sols = 5
        with plt.style.context('seaborn-notebook'):
            fig = plt.figure(figsize=(6,6), dpi=200)
            ax = fig.add_subplot(111)
            ax.set_position([0.15, 0.1, 0.8, 0.85])

            for j in range(0, max_sols):
                y_baseline = baseline[j] / total_progs * 100

                # Some baseline clusters don't have an aggressive counterpart
                if len(aggressive) > j:
                    y_aggressive = aggressive[j] / total_progs * 100
                    ax.annotate(xy = [j + 1.2, y_aggressive], s = f'{y_aggressive:.1f}%')
                    ax.plot([j + 1, j + 1], [y_baseline, y_aggressive], 'C3', zorder=1, lw=1)
                    ax.scatter(j + 1, y_aggressive, c='b', marker='X', zorder=2)

                ax.annotate(xy = [j + 1.2, y_baseline], s = f'{y_baseline:.1f}%')
                ax.scatter(j + 1, y_baseline, c='b', zorder=2)

            # Save the figure
            ax.set_xticks(np.arange(0, max_sols + 1, step=1))
            ax.set_ylabel("Recognized submissions")
            ax.set_xlabel("Model solutions")
            ax.set_ylim(0, 105)
            ax.set_xlim(0, max_sols + 1)
            fig.savefig(f'coverage-{i + 1}.png')

gen_scatter()
