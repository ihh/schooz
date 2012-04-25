#!/usr/local/bin/guile
-s
!#

;; Runs the adventure in redpill.scm on a terminal interface, using guile.

(load "../core/schooz.guile-1.8.scm")
(load "../core/schooz.scm")
(load "../core/machines.scm")
(load "../core/once.scm")
(load "../ui/terminal.scm")

(load "redpill.scm")

(main-loop)
