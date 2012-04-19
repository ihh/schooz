#!/usr/local/bin/guile
-s
!#

;; Runs the adventure in rock.scm on a terminal interface, using guile.

(load "./terminal.scm")
(load "./guile-1.8-schooz.scm")
(load "./schooz.scm")
(load "./api.scm")

(load "./rock.scm")

(main-loop)
