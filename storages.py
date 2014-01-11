import sys
from subprocess import Popen, PIPE, call, CREATE_NEW_CONSOLE
import time

def spawnNode(name, initial):
	return Popen(['erl',
			 '-pa', './data_storage/ebin',
			 '-i', './data_storage/include',
			 '-setcookie', 'moje_ciastko',
			 '-sname', name,
			 '-s', 'node', 'start', initial
			 ], creationflags=CREATE_NEW_CONSOLE)

def getName(num):
	return 'ds'+str(num).zfill(3)

localHost = 'PROFIT-PC'
def getFullName(num):
	return getName(num)+'@'+localHost

nodes = []
def start(num):
	global nodes
	nodes = []
	root = getFullName(1)
	nodes.append(spawnNode(getName(1), root))

	# give root node some time to initialize itself
	time.sleep(1)

	for i in xrange(2, num+1):
		nodes.append(spawnNode(getName(i), root))

def stop():
	for node in nodes:
		try:
			node.kill()
		except:
			pass
