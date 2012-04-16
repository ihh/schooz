all: node

guile:
	guile -s guile-1.8-schooz.scm

node:
	node server.js
