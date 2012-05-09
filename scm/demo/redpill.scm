;; This is a simple story machine-driven vignette
;; First, wrap string actions with <p class="paragraph">...</p>
(define (p-wrap x)
  (cond ((string? x) (list (p x)))
	(else x)))
(schooz:wrap-string-actions p-wrap)

;; by default, if no other actions, followup with a "next-look"
(schooz:after-every-terminal-action
 (lambda () `(,(p (next-look)))))

;; first action is "look"
(schooz:set-initial-action look)

;; shortcuts
(define map schooz:map)
(define grep schooz:grep)

;; grep -v
(define (grep-not-equal x list)
  (grep (lambda (y) (not (equal? x y))) list))

;; Extract random element from list
(define random-element
  (lambda list
    (let ((len (length list)))
      (list-ref list (random-integer len)))))

;; Emily Short-style "One of..." alias for random-element
(define one-of random-element)

;; HTML helpers
(define h1 (lambda args `("h1" ,@args)))
(define h2 (lambda args `("h2" ,@args)))

;; Strings
(define morpheus "Morph33n")

;; Helper functions (move these to core libraries?)
;; Expandable objects
(define (expandable-machine name link-text post-link-text action-text expanded-description)
  (description
   name
   #f
   `(,(link link-text action-text (begin (now name #t) (look))) ,post-link-text))

  (description
   name
   #t
   `(,(span link-text post-link-text expanded-description))))

;; Simple one-way switches
(define (one-way-switch name link-text action-text)
  (expandable-machine name link-text "" action-text ""))

;; Consistent "Next" links
(define
  (next-goto state)
  `(,(p (explicit-menu (choice-goto state "Next" "")))))

(define
  (next-action action)
  `(,(p (explicit-menu (choice* "Next" action)))))

(define
  (next-look)
  (next-action look))

(define
  (next-return)
  (next-action (lambda () (return) (look))))

;; Simple buttons
(define
  (simple-button* text action)
  `(,(p (explicit-menu (choice* text action)))))

(define
  (simple-button text action-body)
  (explicit-menu (choice text action-body)))

;; Self-propelled state machines (generic)
(define (auto-machine name next-state-function descriptor-list)
  (let ((states (length descriptor-list)))
    (map
     (lambda (s)
       (let ((descriptor (list-ref descriptor-list s)))
	 (description*
	  name
	  s
	  (lambda ()
	    (let ((next-state (next-state-function s)))
	      (now name next-state)
	      ((schooz:as-function descriptor)))))))
     (iota states))))

;; Cyclic increment function
(define (inc-modulo modulus)
  (lambda (x) (% (+ x 1) modulus)))

;; Cycler
(define (cyclic-machine name descriptor-list)
  (auto-machine name (inc-modulo (length descriptor-list)) descriptor-list))

;; Random cycler (never repeats)
(define (random-machine name descriptor-list)
  (let* ((states (length descriptor-list))
	 (mutator-function
	  (lambda (x)
	    (let ((other-states (grep-not-equal x (iota states))))
	      (apply random-element other-states)))))
    (auto-machine name mutator-function descriptor-list)))

;; Timer (delay fuse)
(define (fuse-machine name descriptor-list)
  (let* ((states (length descriptor-list))
	 (inc-function (lambda (x) (if (< x (- states 1)) (+ x 1) x))))
  (auto-machine name inc-function descriptor-list)))


;; Machines whose state the player can select
(define 
  selectable-machine
  (lambda args
    (let* ((name (car args))
	   (state-info-list (cdr args))
	   (states (length state-info-list))
	   (state-list (iota states))
	   (state-action-text (map
			       (lambda (state-info)
				 (car state-info))
			       state-info-list)))
      (map
       (lambda (s)
	 (let* ((state-info (list-ref state-info-list s))
		(action-text (car state-info))
		(pre-text (cadr state-info))
		(link-text (caddr state-info))
		(post-text (cadddr state-info))
		(next-state-list (if (>= (length state-info) 5)  ;; optional argument
				     (caddddr state-info)
				     (grep-not-equal s state-list))))
	   (description*
	    name
	    s
	    (lambda ()
	      (p pre-text
		 " "
		 (if (equal? next-state-list '())
		     link-text
		     (apply menu
			    (cons
			     link-text
			     (map (lambda (d)
				    (choice*
				     (list-ref state-action-text d)
				     (lambda () (now name d) (look))))
				  next-state-list))))
		 " "
		 post-text)))))
       state-list))))

;; Wrapper for simple "Choose Your Own" actions with some text and a menu
;; (cyoa text (text1 choice1) (text2 choice2) ...)
(define cyoa
  (lambda args
    (let ((text (car args))
	  (choice-list (cdr args)))
      `(,text ,(apply explicit-menu choice-list)))))

;; Wrapper for simple "Choose Your Own" actions that uses the story-machine mechanism
(define cyo
  (lambda args
    (let ((state (unique-narrative-state))
	  (text (car args))
	  (choice-list (cdr args)))
      (story
       state
       `(,text ,(apply explicit-menu choice-list)))  ;; putting declaration inside action is a sucky hack, to prevent symbol binding errors by delaying evaluation of choice-list. Note this may give unexpected behavior if the state is re-entered (e.g. embedded initialization code will be re-run every time)
      (goto state)
      (look))))

;; Helpers for the cyo storymachine wrapper
(define (make-unique-id hash clue)
  (if (eq? (hashtable-ref hash clue #f) #f)
      clue
      (make-unique-id hash (+ clue 1))))

(define (unique-state machine)
  (let ((desc (hashtable-ref schooz:desc machine #f)))
    (number->string (make-unique-id desc (hashtable-size desc)))))

(define (unique-narrative-state) (unique-state schooz:narrative))

;; The main narrative

;; Synopsis: Morpheus offers you a choice of pills.
;; Outcomes:
;; (1) You escape without taking the pills.
;; (2) You take the red or the blue pill.

;; Club music
(random-machine
 "club-music"  ;; you are a geeky teenager, you hate this
 `("The music is disco, or something, although the kick-drum sounds distorted and the rhythm disrupted."
   "A snare-drum and cymbal rap out a martial duet. How do people even think this is music? It does sound kind of digital, though, which is cool."
   "A sampled vocal shrieks: \"My twisted love affair, m-my twisted love affair...\""
   "Over on the dancefloor strobe lights flicker over vacant faces, jumbled arms and legs."
   "A gut-wobbling bassline rolls in a maddeningly repetitive loop."
   "A tortured techno riff pounds from the speaker stacks, killing your ears."
   "On the dancefloor, you can glimpse gurning faces, grinding teeth."
   "Sweat and dry ice. And a stifling humidity."
   "A spiralling bass whoop rattles glasses on the tables."
   "The music is, like, really hypnotic American disco or something. Very repetitive vocals."))

;; Club ambience
(define (h1-club-front) `(,(h1 "Nightclub") ,(h2 "London, 1985") ,(p `("i" ,(describe "club-music"))) ,(p)))
(define (h1-club) `(,(h1 "Back of the club") ,(p `("i" ,(describe "club-music"))) ,(p)))

;; Opening scene (prelude)
(story
 "nightclub"
 `(,(h1-club-front)
   ,(p
     "Reason #4,096 to hate nightclubs: No sooner have you entered than you are "
     (link-goto "accosted" "meet-morpheus" "I object to being hustled like this!" "Despite your weak objections, you are easily hustled to the back of the club.")
     " by two punks.")
   ,(p "Well, the girl is more Gothic, maybe. They look a bit like "
     (link-goto "Siouxsie" "meet-morpheus" "Examine 'Siouxsie'" "You stare at 'Siouxsie'. She smiles, then pushes you in the chest hard. You fall back a step, then all of a sudden she and her friend are hustling you gently, but irresistibly, to the back part of the club.")
     " and "
     (link-goto "Billy Idol" "meet-morpheus" "Examine 'Billy Idol'" "The punk catches you staring, saunters over and begins preparing a mouthful of spit. Horrified, you turn and run in the direction they evidently want you to go.")
   ", actually.")
   ,(p "\"The computer hacker! "
       (link-goto
	"Anarchist computer hacker." "meet-morpheus" "Examine myself"
	`(,(p "It's true what the lady said. You are a teenage hacker, 17 years old. Too young to prosecute...")
	  ,(p "It has to be said, clubs like this wouldn't normally let you in. But your chatline contact promised you'd be welcome, and here you are.")
	  ,(p "Best do what the Goth asks, I suppose.")
	  ,(p "You proceed to the back of the club, the aggressive punk unclipping a velvet rope for you. VIP treatment for the l33t hacker. Nice.")))
       " We like anarchy, don't we?\" the one like Siouxsie says.")
   ,(p "\"I have a "
     (link-goto "colleague" "meet-morpheus" "OK, let's meet this mysterious 'colleague'." "'Yeah, cool!' you burble, as if you have any say in it. The tall gothic lady ushers you to the back of the club, while her other half growls at bad dancers.")
     " in need of your talents,\" she continues, tilting her head towards a back room.")
   ,(p "Well, this is something. Recognition, at last!")
   ,(simple-button* "First-time player? Click here!" (lambda () (gosub "tutorial") (look)))))

(story
 "tutorial"
 `(,(h1 "Help!")
   ,(p "This is a dynamic hypertext story. Basically, just hypertext with menus.")
   ,(p "Move the mouse pointer over any of the "
       `("i" "pop-up links")
       ", i.e. " (describe "gray boxes")
       ", to see a choice (or a menu of choices) of action(s) that you can perform in the story.")
   ,(p "Click on any of the choice buttons to advance the story, or mouseover a different "
       (describe "pop-up link"))
   ,(describe "UI tips")
   ,(p "You can also click on any buttons you see in the text. Click on this 'Next' button to go back to the story:")
   ,(next-return)))

(expandable-machine
 "gray boxes"
 "gray boxes" ""
 "Click on this button!"
 `("b" " just like the one you clicked on in this sentence"))

(expandable-machine
 "pop-up link"
 "pop-up link" " to see other options."
 "Click on this button!"
 `(,(span "...." `("b" "like you just did."))))

(define click-for-more-tips `("Click for more tips." ""))
(selectable-machine
 "UI tips"
 `("What about undo?" "There is no undo feature in this game. Carefully lawn-mowering options is no way to approach a story. Live for the future, and have no regrets!" ,@click-for-more-tips)
 `("What about save?" "There is no save feature in this game. You have to live for the moment! (Lemons, lemonade, etc.)" ,@click-for-more-tips)
 `("It's annoying when the choices disappear!" "If you want a choice menu to stick around (rather than disappearing when you move the mouse away), click on the parent pop-up link, rather than just mousing over it." ,@click-for-more-tips))

;; Start of main scene
(story
 "meet-morpheus"
 `(,(h1-club)
   ,(p
     "In an armchair"
     (first ", orbited by various flavors of weirdo, sits" (string-append " sits " morpheus ", orbited by weirdos,"))
     " an obvious " (describe "bigshot") " of this scene. Siouxsie gave him the faintest bow when you arrived"
     (once " (almost imperceptible, but you could tell) and Billy Idol snarls at everyone BUT him") ".")
   ,(if (state "bigshot")
	`(,(p
	  "Why 'bigshot'? Well, even loafing in a big comfy chair, he is more than a little intimidating. It doesn't hurt that he's physically domineering, and looks intelligent (you can't see his eyes behind those mirrorshades, but he's got poise).")
	  ,(p "But that's not even the point. This is " morpheus ". " morpheus "! The legendary conqueror of Minitel. Though his dress sense is awful - leather jackets that heavy went out with " (describe "the-who"))))
   ,(once
     (p "'You know well who I am,' says the man in the chair solemnly; perhaps rather undermining this claim by holding up a business card, on which is written " `("b" "'" ,morpheus "'" ".")))
   ,(apply p (cons (first "He" morpheus)
		   (describe "morpheus-intro-gaze")))))

(expandable-machine
   "the-who"
   "The Who" "."
   "The Who?"
   (p "(The Who were a British rock band of the 60's and 70's. Their followers - 'mods' - did often wear long leather coats. Why are you even thinking about this now?)"))
;; Note that flipping the switch will use up a "turn".

(one-way-switch
   "bigshot"
   "bigshot"
   "Bigshot? Why call him that?")

(define (morpheus-intro-choice pre-text stare-text intervening-text speak-text post-text)
  `(,pre-text " "
    ,(link-goto stare-text "conversation" (string-append "Stare at " morpheus)
		`("You stare at each other for a while. Then " ,morpheus " clears his throat."))
    " " ,intervening-text " "
    ,(link-goto speak-text "conversation" "Try to say something cool"
		`("'Speak up, then,' you say, feigning boredom. 'You asked me here, after all.'" ,(p morpheus " grins.")))
    ,post-text))

(fuse-machine
 "morpheus-intro-gaze"
 (list
  (lambda () (morpheus-intro-choice "" "gazes" "at you" "calmly" "."))
  (lambda () (morpheus-intro-choice "" "gazes" "at you, his fingers" "drumming" " on the armrest."))
  (lambda () (morpheus-intro-choice ", now visibly impatient, is still" "waiting" "for you to speak, his fingers" "tapping" " like crazy."))))

;; Instead of going directly to "choice", there should be a whole conversation with Morpheus here.
;; Morpheus begins by making some pompous reference to Alice in Wonderland, which you can't help but destroy.
;; Somewhat irritated, Morpheus compliments you on your natural hacking ability and talents.
;; This can lead into a long side discussion of your many exploits (and optionally force Morpheus to kick you out?)
;; or you can, politely, acknowledge that he has a reputation too (and/or flatter him by exaggerating it).
;; Morpheus then gets a bit philosophical, discusses concepts of truth, illusion, enlightenment.
;; This quickly progresses to a discussion of the "Other Plane" [footnote: Vernor Vinge's predecessor to The Matrix],
;; which Morpheus asserts is a giant virtual machine that is running every instant of time,
;; "every stack frame, every function call..."
;; but from which he has an "Escape Continuation", in the form of a pill.
;; The choice that Morpheus offers you is not between truth and forgetfulness, but two different versions of the truth.
;; ("It's not possible for me to guide your choice; but the red pill offers a familiar truth, the blue an alien one")
;; If pressed, he will reluctantly describe a conspiracy involving aliens who are growing us for energy.
;; You can then engage him in some quite serious debate, poking logical holes in this theory.
;; (Humans are massive net consumers of energy, quantum mechanics is hard to fake, etc.)
;; The conspiracy won't be exactly the same as The Matrix, and will have some fragments of the "true" conspiracy,
;; along with a fair bit of bull.
;; At some point, buried deep in the conversation graph, he may acknowledge that he possibly made it all up,
;; after JR Bob Dobbs appeared to him in a vision one time while he was playing a Jeff Minter light synth.


;; Morpheus conversation minigame:
;; Morpheus makes a pseudo-philosophically mathematicaly proposition or allusion
;; (Alice, Subgenius, Matrix, the continuum, the nature of reality, etc)
;; You can treat it as a conversational gambit, and respond politely.
;; Or you can respond by disputing it, increasing Morpheus' annoyance level.

(define morpheus-annoyance 0)
(define morpheus-annoyed-text  ;; give feedback on his escalating annoyance level
  `((,morpheus " blinks slightly.")
    (,morpheus " frowns slightly.")
    (,morpheus " glares at you, angrily.")
    (,morpheus " looks furious.")
    (,morpheus " seethes with rage.")))
(define (annoy-morpheus)
  (let ((text (list-ref morpheus-annoyed-text morpheus-annoyance)))
    (if (< morpheus-annoyance (- (length morpheus-annoyed-text) 1))
	(set! morpheus-annoyance (+ morpheus-annoyance 1)))
    (append text (list " "))))

(define (agree-choice statement result dest)
  (choice statement `(,(p "'" statement "', you say.")
		      ,(p (one-of
			   (span morpheus " nods, smiling.")
			   (span morpheus " claps his hands.")
			   (span "'Exactly!' says " morpheus "."))
			  result
			  (next-action dest)))))

(define (annoy-choice statement result dest)
  (choice statement `(,(p "'" statement "', you say.")
		      ,(apply p (append (annoy-morpheus) (list result)))
		      ,(next-action dest))))

(define (text-func t f) (lambda () `(,(if (string? t) (p t) (apply p t)) ,(f))))

(define (morpheus-alice-gambit)
  (cyo
   (p "'I bet you feel like the White Rabbit right now,' says " morpheus ". 'Running down a hole. Lost in time.'")
   (agree-choice "I suppose so." ""
		 morpheus-alice-gambit)
   (annoy-choice "Not really. The White Rabbit knew where he was going. Maybe you meant Alice?"
		 "'I meant... following the rabbit, of course.'"
		 morpheus-reality-gambit)))

(define (morpheus-reality-gambit)
  (cyo
   (p "'Have you ever wondered about the nature of reality?' " morpheus " asks.")
   (agree-choice "I suppose so." ""
		 morpheus-exploits-gambit)
   (annoy-choice "No. That's a dumb question." ""
		 morpheus-exploits-gambit)))

(define (morpheus-exploits-gambit)
  (cyo
   (p "'You are an impressive hacker. Your exploits have come to our attention.'")
   (agree-choice "I'm flattered." ""
		 morpheus-own-exploits-gambit)
   (annoy-choice "You haven't heard the least of my exploits." ""
		 morpheus-own-exploits-gambit)))

(define (morpheus-own-exploits-gambit)
  (cyo
   (p "'Yes. Well, naturally you have heard of my own modest achievments.'")
   (agree-choice "Are you kidding? You're a hacking legend!" ""
		 show-choice)
   (annoy-choice "Nope. Never heard of you." ""
		 show-choice)))

(define (show-choice) (goto "choice") (look))

;(now schooz:narrative "conversation")  ;; debug
(story*
 "conversation"
 morpheus-alice-gambit)

;; Other ideas:
;; Maybe Morpheus' mirrorshades are actually Google Goggles, and he will let you try them.
;; On looking through them, you see haloes of information around everyone, annoyingly peppered with product placement.
;; Returning the glasses to Morpheus, you notice that his eyes are somewhat bloodshot.
;; One way to escape the club (i.e. get thrown out) is to repeatedly insult these glasses.


;; Eventually, if Morpheus doesn't tire of you, the conversation brings you here
(story
 "choice"
 `(,(h1-club)
   ,(p "\"What I am trying to say,\" says " morpheus ", \"is that there is a reality of which you are completely, completely unaware. And I can show it to you. If you take a blue pill, you can forget your troubles and I am totally cool with that; but if you want to open your mind - and I'm telling you now that we're talking a higher level of consciousness, here - then you are going to want to try the red pill.\"")
   ,(p morpheus " offers you a choice between a "
       (link-goto "red pill" "pill" "Take the red pill."
	       (begin (now "pill" "red") "Silently, he hands over the red pill."))
       " and a "
       (link-goto "blue pill." "pill" "Take the blue pill."
		  (begin (now "pill" "blue") "Silently, he hands over the blue pill.")))))

;; Other options:
;; Try to politely decline the offer, but Morpheus insists that there 
;; Try to leave at this point and be physically restrained by Siouxsie or Billy

(story
 "pill"
 `(,(p
   "You crunch the pill between your teeth. Bitter.")
   ,(p
   morpheus " nods approvingly. 'You will soon see the truth,' he says. 'And now, if you wouldn't mind, that will be "
   (link-goto "fifty pounds" "payment"
	       "Fifty pounds. That's a bit expensive, isn't it?"
	       `(,(p "'Fifty pounds,' you remark. 'Truth is expensive, then?'") ,(p "'The path to insight is not without material externalities,' he agrees.")))
   " for the "
   (link-goto "pill.'" "payment"
	      "I thought this pill was supposed to be free, anyway."
	      `(,(p "'I thought this pill was supposed to be free, anyway. You're basically a common dealer, aren't you?'") ,(p "'A dealer in truth, perhaps,' he says. 'Removal of scales from eyes. You know?'"))))))

(story
 "payment"
 `(,(p "You feel strongly motivated to debate this topic further, but are now keenly aware that the attentions of 'Billy Idol' are sharply focused upon you, along with several other members of the crew.")
   ,(p
   "The bottom line, and you now kick yourself for realizing this, is that you have eaten a pill that an unknown stranger has given you, in a nightclub, under an implied contract of payment which - while unstated - is hardly unusual either.")
   ,(goto "payment2")))

(story
 "payment2"
 `("True, you could argue that you were intimidated into going along with it. Perhaps you can find a law enforcement official to help you out... meh."
   ,(p
   "Until then, the etiquette of this particular mugging appears pretty clear.")
   ,(p
   "Grudgingly, you fork over payment. You hope that whatever 'insight' this pill provides is worth it. Judging by the quality of dancing in the club, you are not optimistic.")
   ("h2" "The End")
   ,(p "That's the end of this demo so far. Reload the page to play it a different way.")))

;; Vignette ends here. Pill is a complete dud.
;; For added lulz, make occasional later references, wondering if it worked.
;; Maybe you can dance a bit, hopefully. When you look over again, the crew has disappeared.
;; Then cut to the next vignette.
;; Or, have a couple of different exit options, e.g.
;; - attempt to chat-up other clubgoers (epic fail)
;; - talk shop with an old hacker friend

;; Possibly the choice of pill has some effect, but it's a mostly irrelevant side-effect
;; (e.g. red gives you nausea, blue gives you a headache).
;; It can even unlock different scenes (that's good!) but major plot branches should be rare.

;; This illustrates the intended style of chapters in this book.
;; Almost everything is a red herring. Most choices do nothing.
;; Some paths reveal fragments of an overall conspiracy, but with many inconsistencies and false leads.

;; Significant top-level choices are telegraphed as choices between characters whose stories you want to hear.
;; These choices direct you to vignettes from those characters' pov.
;; (It would be awesome to solicit writers to cover different gender, race, age, etc., perspectives.)

;; The top-level choices that are presented to you are determined by a very simple "Drama Manager" (DM),
;; which simply ensures that the sequence and pacing of vignettes is appropriate.
;; Broadly speaking, the DM leads you through a sequence of
;; - introductory vignettes (high school pranks, etc)
;; - escalating investigation/pursuit by authorities (running from FBI, etc)
;; - finding out about the conspiracy (several ways to do this)
;; - tense action scene(s) (being chased by nasty people, etc)
;; - defeating the conspiracy
;; - epilogue scene(s)

;; Within a vignette, choices can determine...
;; (a) different dramatic endings to the scene, from a limited set;
;; (b) different details within the scene;
;; (c) later consequences, such as availability of minigames/detours.
;; It should be impossible to lawnmower ALL details in one reading, though some can be expanded in parallel
;; (c.f. "The Who" in this example).
