;; This is a simple story machine-driven vignette
(schooz:interpret-string-actions-as-functions)
(schooz:p-element-after-every-action)
(schooz:look-after-every-action)
(schooz:p-element-after-every-action)

;; HTML helpers
(define p `("p"))
(define (h1 text) `("h1" ,text))

;; Strings
(define morpheus "Morphetikus")

;; Helper functions (move these to core libraries?)
(define (expandable-object name link-text action-text expanded-description)
  (description
   name
   "hidden"
   `(,(link link-text action-text (now name "shown"))))

  (description
   name
   "shown"
   `(,link-text ,expanded-description)))

(define
  (next state)
  `(,p ,(link-goto "Next" state "Next..." "")))


;; Synopsis: Morpheus offers you a choice of pills.
;; Outcomes:
;; (1) You escape without taking the pills.
;; (2) You take the red or the blue pill.

;; Opening scene
(story
 "nightclub"
 `(,(h1 "Nightclub")
   "In the front area of the club, you are accosted by two of the strangest-looking ravers you have seen. Goth Catwoman and Punk Dwarf, you mentally categorize them as."
   ,p
   "The tall one invites you to "
   ,(link-goto "meet her colleague" "meet-morpheus" "OK, let's meet this mysterious 'colleague'." "The tall catlady ushers you to the back of the club, while her darker half growls at bad dancers.")
   " in a back room of the nightclub."))

;; Should be able to examine your assailants in this scene

;; Should be an option to avoid this entirely and either back out of the club,
;; or hang out on the dancefloor while these two pester you, then leave.

(define h1-club (h1 "Back of the club"))

(story
 "meet-morpheus"
 `(,h1-club
   "In an armchair sits an obvious bigshot of this scene. Catwoman gave him the faintest bow when you arrived (almost imperceptible, but you could tell) and Punk Dwarf snarls at everyone BUT him."
   ,p
   "The guy in the chair is more than a little intimidating. It doesn't hurt that he's physically domineering, and looks intelligent (you can't see his eyes behind those mirrorshades, but he's got poise)."
   ,p
   "Still, you wonder how leather jackets that heavy survived the heyday of " ,@(describe "The Who")
   ,p
   ,@(first
      `("The man introduces himself as " ("b" ,morpheus) " - a name he clearly expected you to know already, and now expects you to remember how to spell." ,p "He")
      `(,morpheus))
   " "
   ,(link-goto "gazes" "choice" "Fine, you want to stare?"
	       `("You stare at each other for a while. Then " ,morpheus " clears his throat."))
   " at you, "
   ,(link-goto "calmly." "choice" "This guy is extremely irritating."
	       `("'Speak up, then,' you say, pointedly. 'You asked me here, after all.'" ,p ,morpheus " grins."))))

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

;; Maybe Morpheus' mirrorshades are actually Google Goggles, and he will let you try them.
;; On looking through them, you see haloes of information around everyone, annoyingly peppered with product placement.
;; Returning the glasses to Morpheus, you notice that his eyes are somewhat bloodshot.
;; One way to escape the club (i.e. get thrown out) is to repeatedly insult these glasses.

;; During the conversation there will be options to examine Morpheus.
;; Some of them are simple switches that can be flipped to reveal background info, e.g.:
(expandable-object
   "The Who"
   "The Who."
   "The Who?"
   `(,p "(The Who were a British rock band of the 60's and 70's. Their followers - 'mods' - did often wear long leather coats. Probably to stash big bags of pills in, now you come to think of it.)"))
;; Note that flipping the switch will use up a "turn".

;; Other options in the conversation can include a gosub to a "Checking out Morpheus" scene
;; that branches a bit, describing him, but with an increasing awareness that you are eyeing him up and down

;; Eventually, if Morpheus doesn't tire of you, the conversation brings you here
(story
 "choice"
 `(,h1-club
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
 `(,p
   "You crunch the pill between your teeth. Bitter. Good sign."
   ,p
   ,morpheus " nods approvingly. 'You will soon see the truth,' he says. 'And now, if you wouldn't mind, that will be "
   ,(link-goto "twenty dollars" "payment"
	       "Twenty dollars. That's a bit expensive, isn't it?"
	       `("'Twenty dollars,' you remark. 'Truth is expensive, then?'" ,p "'The path to insight is not without material externalities,' he agrees."))
   " for the "
   ,(link-goto "pill.'" "payment"
	       "I thought this pill was supposed to be free, anyway."
	       `("'I thought this pill was supposed to be free, anyway. You're basically a common dealer, aren't you?'" ,p "'A dealer in truth, perhaps,' he says. 'Removal of scales from eyes. You know?'"))))

(story
 "payment"
 `("You feel strongly motivated to debate this topic further, but are now keenly aware that the attentions of Punk Dwarf are sharply focused upon you, along with several other members of the coterie."
   ,p
   "The bottom line, and you now kick yourself for realizing this, is that you have eaten a pill that an unknown stranger has given you, in a nightclub, under an implied contract of payment which - while unstated - is hardly unusual."
   ,(next "payment2")))

(story
 "payment2"
 `("True, you could argue that you were intimidated into going along with it. Perhaps you can find a law enforcement official to help you out... heh."
   ,p
   "Until then, the etiquette of this particular mugging appears pretty clear."
   ,p
   "Grudgingly, you fork over payment. You hope that whatever 'insight' this pill provides is worth it. Judging by the quality of dancing in the club, you are not optimistic."))

;; Vignette ends here. Pill is a complete dud.
;; For added lulz, make occasional later references, wondering if it worked.
;; Maybe you can dance a bit, hopefully. When you look over again, the crew has disappeared.
;; Then cut to the next vignette.

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
