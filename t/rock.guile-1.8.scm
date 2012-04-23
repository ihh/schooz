#!/usr/local/bin/guile
-s
!#

;; Runs the adventure in t/rock.scm on a terminal interface, using guile.

(load "../scm/core/schooz.guile-1.8.scm")
(load "../scm/core/schooz.scm")
(load "../scm/core/machines.scm")
(load "../scm/ui/terminal.scm")

(load "rock.scm")

(main-loop)
