all: test

test:
	prove

rock-guile:
	guile --debug -s scm/demo/rock.guile-1.8.scm

rock-node:
	node js/server.js

rock: guile

redpill-guile:
	guile --debug -l scm/demo/redpill.guile-1.8.scm

redpill: redpill-guile
