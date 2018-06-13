# Returns the sizes of the clusters and the total amount of programs
def get_cluster_sizes(kind, exercise):
    sizes = []
    cluster_size = 0
    for line in open(f'../../data/{kind}/exercise{exercise}').readlines():
        if line.strip() == '':
            sizes.append(cluster_size)
            #print(cluster_size)
            cluster_size = 0
        else:
            cluster_size += 1
    return sizes

def clusters_per_exercise():
    for i in range(0, 8):
        baseline = len(get_cluster_sizes('baseline', i + 1))
        aggressive = len(get_cluster_sizes('aggressive', i + 1))
        print(f'{i + 1} & {baseline} & {aggressive}')
