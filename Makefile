all: node

guile:
	guile -s guile-1.8-schooz.scm

node:
	node server.js

rock:
	guile --debug -s guile-1.8-rock.scm
