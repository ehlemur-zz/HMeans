import matplotlib.pyplot as plt

def process(filename):
    xs = []
    ss = []
    ts = []
    with open(filename) as f:
        for line in f:
            line = line.split()
            xs.append(int(line[0]))
            ss.append(100 - float(line[3]))
            ts.append(float(line[1]))
            if line[2] == "ms":
                ts[-1] /= 1000
    return xs, ss, ts


HKxs, HKss, HKts = process("resultsHMeansKMeans2")
Kxs, Kss, Kts = process("resultsKMeans2")
HHxs, HHss, HHts = process("resultsHMeansHierarchical2")
Hxs, Hss, Hts = process("resultsHierachical2")

"""
fig = plt.figure()

plt.plot(HKxs, HKts, label="HMeans - Kmeans")
plt.plot(Kxs, Kts, label="Kmeans")

plt.legend()

fig.suptitle('Running time (s)')
plt.xlabel('# of points')
plt.ylabel('time (s)')

fig.savefig('KMeansTime.jpg')
"""

"""
fig = plt.figure()

plt.plot(HHxs, HHts, label="HMeans - Hierarchical")
plt.plot(Hxs, Hts, label="Hierarchical")

plt.legend()

fig.suptitle('Running time (s)')
plt.xlabel('# of points')
plt.ylabel('Running time (s)')

fig.savefig('HierarchicalTime.jpg')
"""

"""
fig = plt.figure()

plt.plot(HHxs, HHss, label="HMeans - Hierarchical")
plt.plot(Hxs, Hss, label="Hierarchical")

plt.legend()

fig.suptitle('Accuracy (%)')
plt.xlabel('# of points')
plt.ylabel('Accuracy (%)')
plt.ylim((0, 100))

fig.savefig('HierarchicalAccuracy.jpg')
"""
"""
fig = plt.figure()

plt.plot(HKxs, HKss, label="HMeans - KMeans")
plt.plot(Kxs, Kss, label="KMeans")

plt.legend()

fig.suptitle('Accuracy (%)')
plt.xlabel('# of points')
plt.ylabel('Accuracy (%)')
plt.ylim((0, 100))

fig.savefig('KMeansAccuracy.jpg')

plt.show()
"""
