#!/usr/local/bin/guile
-s
!#

;; An ultra-simple (3-room) test adventure. (Now with four rooms, plus an interactive object!)
;; Illustrates various ways of forming simple (goto...) and (gosub...) links.

;; Load the interface
(load "./terminal.scm")

;; Define the story graph
(story
 "start"
 (lambda ()
   (list
    "You are "
    (menu "standing"
	  (list
	   (choice "Stand my ground." "Yup. Just standing around.")
	   (choice-gosub "sleep" "Go to sleep." "You fall asleep.")))
    " between a "
    (link-goto "rock" "rock" "Go to the rock." "As you command.")
    " and a "
    (link-goto "hardplace" "hard place" "Go to the hard place." "OK, if you say so.")
    ".")))

(story
 "rock"
 (lambda ()
   (list
    "You are standing on a "
    (menu "rock" (list
		  (choice "I like this rock. I'm staying here." "OK.")
		  (choice-return "I take a swan-dive off the rock." "You plummet to your doom.")
		  (choice "I need to get away from this rock. It's not safe." (lambda () (begin (goto "start") "I agree.")))))
    ".")))

(story
 "hardplace"
 (lambda ()
   (list
    "You are in a very hard place. "
    (describe "mattress")
    (explicit-menu
     (list
      (choice-goto (chapter) "This place may be hard, but there are no better alternatives." "You're quite possibly correct...")  ;; (chapter) returns the current chapter, so (goto (chapter)) does nothing
      (choice-goto "start" "Let's get out of here. I don't like hard places." "No problem."))))))

;; This chapter can be entered via a gosub from two different places...
(story
 "sleep"
 (lambda ()
   (list
    "You are sound asleep."
    (explicit-menu
     (list
      (choice "Dream about elephants." "You have an entertaining dream about flying elephants.")
      (choice "Dream about roosters." "You have a terrifying dream about giant roosters.")
      (choice-return "Time to wake up." "You shake yourself awake."))))))

;; An interactive object!
(description
 "mattress"
 "start"
 (lambda ()
   (list
    "A "
    (link "mattress"
	  "Lay the mattress flat on the ground."
	  (lambda () (begin (now "mattress" "end") "You place the mattress on the ground.")))
    " is propped against the wall. ")))

(description
 "mattress"
 "end"
 (lambda ()
   (list
    "A "
    (link-gosub "sleep" "mattress" "Time for a little nap!" "You quickly drift off to sleep.")
    " lies invitingly on the floor.")))

;; And.... run.
(main-loop)
