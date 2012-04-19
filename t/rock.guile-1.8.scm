#!/usr/local/bin/guile
-s
!#

;; Runs the adventure in t/rock.scm on a terminal interface, using guile.

(load "../schooz.guile-1.8.scm")
(load "../schooz.scm")
(load "../terminal.scm")
(load "../api.scm")

(load "rock.scm")

(main-loop)
