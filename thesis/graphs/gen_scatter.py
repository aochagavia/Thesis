import matplotlib
import matplotlib.pyplot as plt
import numpy as np

# Exercise 1

xs_before = [85.5, 36,   54.4, 61.1, 45.9, 7.9,  14.3, 23.2]
xs_after  = [99.1, 80.2, 85.4, 90.7, 96.3, 86.1, 75.5, 87.9]
ys_before = [3, 5, 4, 5, 4, 4, 5, 5]
ys_after  = [1, 5, 3, 3, 2, 2, 5, 5]

#xs = [xs_before, xs_after]
#ys = [ys_before, ys_after]

# matplotlib.rcParams.update({'font.size': 22})

for i in range(0, 8):
    xs = [xs_before[i], xs_after[i]]
    ys = [ys_before[i], ys_after[i]]

    with plt.style.context('seaborn-notebook'):
        fig = plt.figure(figsize=(6,3), dpi=200)
        ax = fig.add_subplot(111)
        ax.set_position([0.1, 0.2, 0.85, 0.7])
        # ax.axis('equal')

        for j in range(0, 2):
            ax.annotate(xy = [xs[j] + 1, ys[j] + 0.2], s = f'{xs[j]}%')

        ax.plot(xs, ys, 'C3', zorder=1, lw=1)
        ax.scatter(xs[0], ys[0], zorder=2)
        ax.scatter(xs[1], ys[1], zorder=2)
        ax.set_yticks(np.arange(0, 6, step=1))
        ax.set_xlabel("Recognized submissions")
        ax.set_ylabel("Model solutions")
        # ax.legend(loc=2)
        # ax.show()
        ax.set_xlim(0, 110)
        ax.set_ylim(0, 6)
        fig.savefig(f'coverage-{i + 1}.png')

# Markers
# Move annotations

#plt.show()
