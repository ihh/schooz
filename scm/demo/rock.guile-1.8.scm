#!/usr/local/bin/guile
-s
!#

;; Runs the adventure in rock.scm on a terminal interface, using guile.

(load "../core/schooz.guile-1.8.scm")
(load "../core/schooz.scm")
(load "../core/machines.scm")
(load "../ui/terminal.scm")

(load "rock.scm")

(main-loop)
