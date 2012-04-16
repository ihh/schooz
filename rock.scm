#!/usr/local/bin/guile
-s
!#

(load "./terminal.scm")

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
		  (choice "I need to get away from this rock. It's not safe." (lambda () (begin (goto "start") "I agree.")))))
    ".")))

(story
 "hardplace"
 (lambda ()
   (list
    "You are in a very hard place."
    (explicit-menu (list
	     (choice-goto (chapter) "It may be hard, but there are no better alternatives." "You're quite possibly correct...")
	     (choice-goto "start" "Let's get out of here. I don't like hard places." "No problem."))))))

(main-loop)
