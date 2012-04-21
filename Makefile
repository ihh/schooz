all: test

test:
	prove

guile:
	guile --debug -s rock.guile-1.8.scm

node:
	node server.js

rock: guile
