from os import listdir
from os.path import isfile, join
mypath = "."
onlyfiles = [ f for f in listdir(mypath) if isfile(join(mypath,f)) and f.find("txt") != -1]

gammas = [1e-3,1e-4,1e-5,1e-6,3e-3,3e-4,3e-5,3e-6,5e-3,5e-4,5e-5,5e-6,7e-3,7e-4,7e-5,7e-6,9e-3,9e-4,9e-5,9e-6]
gammas.sort()
gammaIndexMap = {}

for i in range(len(gammas)):
    gammaIndexMap[gammas[i]] = i
import pandas
from numpy import linalg as LA
values = [[0 for j in range(20)] for i in range(279)]
for f in onlyfiles:
    print(f)
    with open(f) as input:
        mat = pandas.read_csv(input, sep=" ", index_col=False, header=None).as_matrix()
        w, v = LA.eig(mat)
        tokens = f.split("_")
        pivot = int(tokens[1])
        gamma = float(tokens[2][:-4])
        print(pivot - 1, gamma, gammaIndexMap[gamma], max(w.real))
        values[pivot - 1][gammaIndexMap[gamma]] = max(w.real)

pivots = [i for i in range(279)]
gs = [i for i in range(20)]

xs = []
ys = []
zs = []

for p in pivots:
    for g in gs:
        xs.append(p)
        ys.append(g)
        zs.append(values[p][g])

import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
ax.scatter(xs, ys, zs)
ax.set_xlabel("pivot")
ax.set_ylabel("gamma")
ax.set_zlabel("Largest EV")
fig.savefig('temp.png', dpi=fig.dpi)
plt.show()
