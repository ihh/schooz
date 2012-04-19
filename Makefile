all: test

test:
	prove

guile:
	guile -s schooz.guile-1.8.scm

node:
	node server.js

rock:
	guile --debug -s rock.guile-1.8.scm
