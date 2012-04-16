#!/usr/local/bin/guile
-s
!#

;; An ultra-simple (3-room) test adventure. (Now with four rooms!)
;; Illustrates various ways of forming simple (goto...) and (gosub...) links.

;; Load the interface
(load "./terminal.scm")

;; Define the story graph
(story
 "start"
 (lambda ()
   (list
    "You are "
    (link "standing" "Stand my ground." "Yup. Just standing around.")
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
    "You are in a very hard place."
    (explicit-menu (list
	     (choice-goto (chapter) "It may be hard, but there are no better alternatives." "You're quite possibly correct...")
	     (choice-gosub "sleep" "Not too hard for a nap, I hope!" "Indeed not. You quickly drift off to sleep.")
	     (choice-goto "start" "Let's get out of here. I don't like hard places." "No problem."))))))

(story
 "sleep"
 (lambda ()
   (list
    "You are sound asleep."
    (explicit-menu (list
	     (choice "Dream about elephants." "You have an entertaining dream about flying elephants.")
	     (choice "Dream about roosters." "You have a frightening dream about giant roosters.")
	     (choice-return "Time to wake up." "You shake yourself awake."))))))

;; And.... run.
(main-loop)
