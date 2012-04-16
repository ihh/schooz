all: node

node:
	node server.js

guile:
	guile -s guile-1.8-schooz.scm
