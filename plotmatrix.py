import matplotlib.pyplot as pyplot
import numpy
from pprint import pprint

import argparse
parser = argparse.ArgumentParser()
parser.add_argument('filename')
parser.add_argument('skiplines', type=int)
parser.add_argument('width', type=int)
parser.add_argument('height', type=int)
args = parser.parse_args()

u = numpy.loadtxt(args.filename,
        skiprows=args.skiplines).reshape(args.width, args.height)
u = numpy.transpose(u)
pprint(u)
pyplot.contour(u)
pyplot.show()
