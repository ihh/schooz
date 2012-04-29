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
(define (random-element list)
  (let ((len (length list)))
    (list-ref list (random-integer len))))

;; Emily Short-style "One of..." alias for random-element
(define one-of random-element)

;; HTML helpers
(define h1 (lambda args `("h1" ,@args)))

;; Strings
(define morpheus "Morphetikus")

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
   `("span" ,link-text ,post-link-text ,expanded-description)))

;; Simple one-way switches
(define (one-way-switch name link-text action-text)
  (expandable-machine name link-text "" action-text ""))

;; Consistent "Next" links
(define
  (next-goto state)
  `(,(p (explicit-menu ((choice-goto state "Next" ""))))))

(define
  (next-look)
  `(,(p (explicit-menu ((choice* "Next" look))))))

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
	      (random-element other-states)))))
    (auto-machine name mutator-function descriptor-list)))

;; Timer (delay fuse)
(define (fuse-machine name descriptor-list)
  (let* ((states (length descriptor-list))
	 (inc-function (lambda (x) (if (< x (- states 1)) (+ x 1) x))))
  (auto-machine name inc-function descriptor-list)))



;; The main narrative

;; Synopsis: Morpheus offers you a choice of pills.
;; Outcomes:
;; (1) You escape without taking the pills.
;; (2) You take the red or the blue pill.

;; Club music
(random-machine
 "club-music"
 `("A distorted kick-drum pounds a disrupted rhythm."
   "A snare-drum and hi-hat rap out a martial duet."
   "A sampled vocal shrieks: \"My twisted love affair, m-my twisted love affair...\""
   "Over on the dancefloor strobe lights flicker over vacant faces, jumbled arms and legs."
   "This is actually a classic acid house track the DJ put on just now. And you are a trainspotter for noticing."
   "Phuture, Acid Trax: a classic tune."
   "A tortured techno riff pounds from the speaker stacks."
   "On the dancefloor, you can glimpse gurning faces, grinding teeth."
   "Sweat and dry ice. And a stifling humidity."
   "A swooping dubstep bass rattles glasses on the tables."))

;; Club ambience
(define (h1-club-front) `(,(h1 "Nightclub") ,(p `("i" ,(describe "club-music"))) ,(p)))
(define (h1-club) `(,(h1 "Back of the club") ,(p `("i" ,(describe "club-music"))) ,(p)))

;; Opening scene
(story
 "nightclub"
 `(,(h1-club-front)
   ,(p
     "In the front area of the club, you are "
     (link-goto "accosted" "meet-morpheus" "I object to being hustled like this!" "The more strenuously you object, the more attention you seem to draw from other clubgoers; until without quite realizing how, you are surrounded by four or five people, walking you irresistibly towards the back of the club.")
     " by two of the strangest-looking ravers you have seen. "
     (link-goto "Goth Catwoman" "meet-morpheus" "Examine Goth Catwoman" "You stare at Goth Catwoman. She smiles, then pushes you in the chest hard. You fall back a step, then all of a sudden she and her friend are hustling you gently, but irresistibly, to the back part of the club.")
     " and "
     (link-goto "Punk Dwarf" "meet-morpheus" "Examine Punk Dwarf" "The punk catches you staring, saunters over and spits at your feet. Catwoman grabs your arm and hauls you to the back.")
   ", you mentally categorize them as.")
   ,(p
     "The tall one invites you to "
     (link-goto "meet her colleague" "meet-morpheus" "OK, let's meet this mysterious 'colleague'." "The tall catlady ushers you to the back of the club, while her grumpier half growls at bad dancers.")
     " in a back room of the nightclub.")))

;; Should be able to examine your assailants in this scene

;; Should be an option to avoid this entirely and either back out of the club,
;; or hang out on the dancefloor while these two pester you, then leave.

(story
 "meet-morpheus"
 `(,(h1-club)
   ,(p
     "In an armchair"
     (first ", orbited by various flavors of weirdo, sits" (string-append " sits " morpheus ", orbited by weirdos,"))
     " an obvious " (describe "bigshot") " of this scene. Catwoman gave him the faintest bow when you arrived"
     (once " (almost imperceptible, but you could tell) and Punk Dwarf snarls at everyone BUT him") ".")
   ,(if (state "bigshot")
	`(,(p
	  "Why 'bigshot'? Well, even loafing in a big comfy chair, he is more than a little intimidating. It doesn't hurt that he's physically domineering, and looks intelligent (you can't see his eyes behind those mirrorshades, but he's got poise).")
	  ,(p
	  "Still, you wonder how leather jackets that heavy survived the heyday of " (describe "the-who"))))
   ,(once
     (p "The man introduces himself as " `("b" ,morpheus) " - a name he clearly thought you knew already, and probably expects you to remember how to spell."))
   ,(apply p (cons (first "He" morpheus)
		   (describe "morpheus-intro-gaze")))))

(define (morpheus-intro-choice pre-text stare-text intervening-text speak-text post-text)
  `(,pre-text " "
    ,(link-goto stare-text "choice" "Fine, you want to stare?"
		`("You stare at each other for a while. Then " ,morpheus " clears his throat."))
    " " ,intervening-text " "
    ,(link-goto speak-text "choice" "This guy is extremely irritating."
		`("'Speak up, then,' you say, irritated. 'You asked me here, after all.'" ,(p morpheus " grins.")))
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


;; General format of minigame:
;; Morpheus makes a philosophical proposition or allusion.
;; You can treat it as a conversational gambit, and respond politely.
;; Or you can respond by 

;; Maybe Morpheus' mirrorshades are actually Google Goggles, and he will let you try them.
;; On looking through them, you see haloes of information around everyone, annoyingly peppered with product placement.
;; Returning the glasses to Morpheus, you notice that his eyes are somewhat bloodshot.
;; One way to escape the club (i.e. get thrown out) is to repeatedly insult these glasses.

;; During the conversation there will be options to examine Morpheus.
;; Some of them are simple switches that can be flipped to reveal background info, e.g.:
(expandable-machine
   "the-who"
   "The Who" "."
   "The Who?"
   (p "(The Who were a British rock band of the 60's and 70's. Their followers - 'mods' - did often wear long leather coats. Probably to stash big bags of pills in, now you come to think of it.)"))
;; Note that flipping the switch will use up a "turn".

;; Even simpler expandable switch, for when the expanded text is somewhere other than the switch link
(one-way-switch
   "bigshot"
   "bigshot"
   "Bigshot? Why call him that?")

;; Other options in the conversation can include a gosub to a "Checking out Morpheus" scene
;; that branches a bit, describing him, but with an increasing awareness that you are eyeing him up and down

;; Eventually, if Morpheus doesn't tire of you, the conversation brings you here
(story
 "choice"
 `(,(h1-club)
   ;; This could be a separately-defined function; it will probably be offered as an exit-point from several chapters
   ,morpheus " offers you a choice between a "
   ,(link-goto "red pill" "pill" "Take the red pill."
	       (begin (now "pill" "red") "Silently, he hands over the red pill."))
   " and a "
   ,(link-goto "blue pill." "pill" "Take the blue pill."
	       (begin (now "pill" "blue") "Silently, he hands over the blue pill."))))

;; Other options:
;; Try to politely decline the offer, but Morpheus insists that there 
;; Try to leave at this point and be physically restrained by Catwoman or PD

(story
 "pill"
 `(,(p
   "You crunch the pill between your teeth. Bitter.")
   ,(p
   morpheus " nods approvingly. 'You will soon see the truth,' he says. 'And now, if you wouldn't mind, that will be "
   (link-goto "twenty dollars" "payment"
	       "Twenty dollars. That's a bit expensive, isn't it?"
	       `(,(p "'Twenty dollars,' you remark. 'Truth is expensive, then?'") ,(p "'The path to insight is not without material externalities,' he agrees.")))
   " for the "
   (link-goto "pill.'" "payment"
	      "I thought this pill was supposed to be free, anyway."
	      `(,(p "'I thought this pill was supposed to be free, anyway. You're basically a common dealer, aren't you?'") ,(p "'A dealer in truth, perhaps,' he says. 'Removal of scales from eyes. You know?'"))))))

(story
 "payment"
 `(,(p "You feel strongly motivated to debate this topic further, but are now keenly aware that the attentions of Punk Dwarf are sharply focused upon you, along with several other members of the crew.")
   ,(p
   "The bottom line, and you now kick yourself for realizing this, is that you have eaten a pill that an unknown stranger has given you, in a nightclub, under an implied contract of payment which - while unstated - is hardly unusual either.")
   ,(goto "payment2")))

(story
 "payment2"
 `("True, you could argue that you were intimidated into going along with it. Perhaps you can find a law enforcement official to help you out... meh."
   ,(p
   "Until then, the etiquette of this particular mugging appears pretty clear.")
   ,(p
   "Grudgingly, you fork over payment. You hope that whatever 'insight' this pill provides is worth it. Judging by the quality of dancing in the club, you are not optimistic.")))

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
