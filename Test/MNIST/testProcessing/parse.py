import matplotlib.pyplot as plt

def process(filename):
    xs = []
    ss = []
    ts = []
    with open(filename) as f:
        lines = f.readlines()
        for l1, l2, l3 in zip(lines[0::3], lines[1::3], lines[2::3]):
            l1 = l1.split()
            l2 = l2.split()
            l3 = l3.split()
            xs.append(int(l1[3]))
            ss.append(100 - float(l2[-1][1:-2]))
            ts.append(float(l3[0]))
            if l3[-1] == "ms":
                ts[-1] /= 1000
    return xs, ss, ts

HKxs, HKss, HKts = process("resultsHMeansKMeans")
Kxs, Kss, Kts = process("resultsKMeans")
HHxs, HHss, HHts = process("resultsHMeansHierarchical")
Hxs, Hss, Hts = process("resultsHierarchical")

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
