all: test

test:
	prove

guile:
	guile --debug -s scm/demo/rock.guile-1.8.scm

node:
	node js/server.js

rock: guile
