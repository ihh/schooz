;; An ultra-simple (3-room) test adventure. (Now with four rooms, plus an interactive object!)
;; Illustrates various ways of forming simple (goto...) and (gosub...) links.

;; Uncomment to disable SXML, rendering everything as plain text
;; (schooz:output-plain)

;; Comment out to disable automatic "\n" (look) "\n" at start of game & after every choice
(schooz:newline-after-every-action)
(schooz:look-after-every-action)
(schooz:newline-after-every-action)

;; Define the story graph
(story
 "start"
 `("You are "
   ,(popup "standing"
	  (choice "Stand my ground." "Yup. Just standing around.")
	  (choice-gosub "sleep" "Go to sleep." "You close your eyes. Just for a moment..."))
   " between a "
   ,(link-goto "rock" "rock" "Go to the rock." "As you command.")
   " and a "
   ,(link-goto "hard place" "hardplace" "Go to the hard place." "OK, if you say so.")
   "."))

(story*  ;; just for variety and to illustrate the basic form, use (story*...) instead of (story...)
 "rock"
 (lambda ()
   `("You are standing on a "
     ,(popup "rock"
	    (choice "I like this rock. I'm staying here." "OK.")
	    (choice-return "I take a swan-dive off the rock." "You plummet to your doom.")
	    (choice* "I need to get away from this rock. It's not safe." (lambda () (begin (goto "start") "I agree."))))  ;; just to illustrate (choice*...) as an alternative to (choice-goto...)
     ".")))

(story
 "hardplace"
 `("You are in a very hard place. "
   ,@(describe "mattress")
   ,(menu
     (choice-goto (chapter) "This place may be hard, but there are no better alternatives." "You're quite possibly correct...")  ;; (chapter) returns the current chapter, so (choice-goto (chapter)...) does nothing... could have just used (choice...) here
     (choice-goto "start" "Let's get out of here. I don't like hard places." "No problem."))))

;; This chapter can be entered via a gosub from two different places...
(story
 "sleep"
 `("You are sound asleep."
   ,(menu
     (choice "Dream about elephants." "You have an entertaining dream about flying elephants.")
     (choice "Dream about roosters." "You have a terrifying dream about giant roosters.")
     (choice-return "Time to wake up." "You shake yourself awake."))))

;; An interactive object!
(description
 "mattress"
 "upright"
 `("A "
   ,(link "mattress"
	  "Lay the mattress flat on the ground."
	  (begin (now "mattress" "flat") "You place the mattress on the ground."))
   " is propped against the wall. "))

(description
 "mattress"
 "flat"
 `("A "
   ,(link-gosub "mattress" "sleep" "Time for a little nap!" "Place, hard; mattress, soft. You quickly drift off.")
   " lies invitingly on the floor."))
