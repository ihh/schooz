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

;; String constants
(define morpheus "Morph33n")

;; The main narrative

;; Synopsis: Morpheus offers you a choice of pills.
;; Outcomes:
;; (1) You escape without taking the pills.
;; (2) You take the red or the blue pill.

;; Standard verb responses
(define no-attack `("Attack" (,(p "Violence isn't the answer to this one.") ,(p "Yes, I know: narrators ALWAYS say that. But in your case, it's something you've learned to tell yourself; if only to save face.") ,(p "Look at you: scrawny, spotty... pure nerd. Despite your encyclopedic knowledge of the Anarchist Cookbook, violence has pretty much never been the answer to anything, for you. More like the problem you're trying to avoid."))))
(define no-kiss `("Kiss" (,(p "That seems entirely inappropriate. Well done!") ,(p "It's a shame you can't pluck up the courage, but I think you deserve some credit for actualizing the thought. Better a frustrated lover than a self-denying one!"))))


;; Club music
(random-machine
 "club-music"  ;; you are a geeky teenager, you hate this
 `("The music is disco, or something, although the kick-drum sounds distorted and the rhythm disrupted."
   "A snare-drum and cymbal rap out a martial duet. How do people even think this is music? It does sound kind of digital, though, which is cool."
   "A sampled vocal shrieks: ''My twisted love affair, m-my twisted love affair...''"
   "Over on the dancefloor strobe lights flicker over vacant faces, jumbled arms and legs."
   "A gut-wobbling bassline rolls in a maddeningly repetitive loop."
   "A tortured techno riff pounds from the speaker stacks, killing your ears."
   "On the dancefloor, gurning faces. You almost hear the grinding teeth."
   "Sweat and dry ice. And a stifling humidity."
   "A spiralling bass whoop rattles glasses on the tables."
   "The music is, like, really hypnotic American disco or something. Repetitive, meaningless vocals."))

;; Club ambience
(define (h1-club-front) `(,(h1 "Nightclub") ,(h2 "London, 1985") ,(p (i (describe "club-music"))) ,(p)))
(define (h1-club) `(,(h1 "Back of the club") ,(p (i (describe "club-music"))) ,(p)))

;; Opening scene (prelude)
(story
 "nightclub"
 `(,(h1-club-front)
   ,(p
     "Reason #4,096 to hate nightclubs: No sooner have you entered than you are "
     (link-goto "accosted" "meet-morpheus" "I object to being hustled like this!" "Despite your weak objections, you are easily hustled to the back of the club.")
     " by two punks.")
   ,(p "Well, the girl is more Gothic, maybe. They look a bit like "
       (verb-acts
	"Siouxsie"
	(verb-goto "Examine" "meet-morpheus" `(,(p "You stare at the tall Goth, who looks just like the lead singer of Siouxsie and the Banshees. She, however, does not care to be stared at, and pushes you in the chest hard.") ,(p "Who are you trying to fool? You are not the type of kid who stares people down. Cowed, you are easily hustled to the back of the club.")))
	no-attack no-kiss)
     " and "
     (verb-acts
      "Billy Idol" 
      (verb-goto "Examine" "meet-morpheus" `(,(p "The punk, who really does look like Billy Idol (except more scarred), catches you staring. He leans over and begins hawking up a mouthful of spit.") ,(p "Your horrified look makes the gothic girl laugh. ''Hop this way, sweetie,'' she says; and you do.")))
      no-attack no-kiss)
   ", actually.")
   ,(p "''The computer hacker! "
       (popup
	"Anarchist computer hacker."
	(choice-goto "meet-morpheus" "Examine myself"
		     `(,(p "It's true what the lady said. You are a teenage hacker, 17 years old. Too young to prosecute...")
	  ,(p "It has to be said, clubs like this wouldn't normally let you in. But your chatline contact promised you'd be welcome, and here you are.")
	  ,(p "Best do what the Goth asks, I suppose.")
	  ,(p "You proceed to the back of the club, the aggressive punk unclipping a velvet rope for you. VIP treatment for the l33t hacker. Nice."))))
       " We like anarchy, don't we?'' the one like Siouxsie says.")
   ,(p "''I have a "
     (link-goto "colleague" "meet-morpheus" "OK, let's meet this mysterious ''colleague'." "''Yeah, cool!'' you burble, as if you have any say in it. The tall gothic lady ushers you to the back of the club, while her other half growls at bad dancers.")
     " in need of your talents,'' she continues, tilting her head towards a back room.")
   ,(p "Well, this is something. Recognition, at last!")
   ,(simple-button* "First-time player? Click here!" (lambda () (gosub "tutorial") (look)))
   ,(simple-button*
     "Complaints about the user interface? Click here!"
     (lambda () `(,(h1 "Beach hut") ,(h2 "Somewhere in the South Pacific, 2011") ,(p (i "Through the window, a distant pulse of reggae from the ever-present party. In front of you is an open laptop, running a badly-written choose-your-adventure game set in mid-80's London. Behind the laptop, a mosquito net hangs over a dishevelled bed.")) ,(p "''Please,'' you say, pushing the laptop away. ''I... can't... you don't understand. I'm just not used to this, and... my family... they'll be wondering...''") ,(p "The author nods, and waves the minder out of the room. He frowns, and it's a while before he speaks again. But when he does, he looks you straight in the eye:") ,(p "''Look, pal. No-one wants to be a play-tester. You think you've got it tough? I grew up when computers had " (b "eight bits.") " That's like counting on your fingers.") ,(p "''ASCII was all we had, back then. You ought to count yourself lucky to even get basic HTML...") ,(p "''If you---and enough others---like the " (i "content") " of this game, maybe I'll make a better UI:-- one with graceful menus, translucent textures and gentle transitions.") ,(p "''And yes,'' a dark edge creeps into his voice, ''I will use JQuerious. Fidget. Hobo. Nod. Selenia. And whatever your favorite JavaScript library is.'' (Now he just sounds bitter.)") ,(p "''Until then, do " (i "please") " be awesome: look past the basic stylings, and try to enjoy the text.''") ,(i "At a nod, the armed guard returns to the hut. Looks like you're going to have to play a bit more of this game, before you get off the island."))))))

(story
 "tutorial"
 `(,(h1 "Help!")
   ,(p "This is a dynamic hypertext story. Basically, just hypertext with popups.")
   ,(p "Move the mouse pointer over any of the "
       (i "pop-up links")
       ", i.e. " (describe "gray boxes")
       ", to see a choice (or a popup of choices) of action(s) that you can perform in the story.")
   ,(p "Click on any of the choice buttons to advance the story, or mouseover a different "
       (describe "pop-up link"))
   ,(p "The web browser's back arrow will not allow you to ''rewind'' this story. However, you " (i "can") " restart the game, by reloading the page.")
   ,(describe "UI tips")
   ,(p "You can also click on any buttons you see in the text. Click on this ''Next'' button to go back to the story:")
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
 `("OK, got it; thanks." "" ,@click-for-more-tips)
 `("Where's the undo button?" "There is no undo feature in this game. Insofar as a piece of software can be said to encode beliefs, this game believes that lawn-mowering through its options is a poor way to approach the story, so the game's user interface deliberately doesn't make that sort of thing easy. Rest assured that it's impossible to get stuck, although some choices will commit you to different endings." ,@click-for-more-tips)
 `("How do I save and restore?" "There is, at present, no save feature in this game. The game is designed to be played through quickly, in one sitting." ,@click-for-more-tips)
 `("It's annoying when the choices disappear!" "If you want a choice popup to stick around (rather than disappearing when you move the mouse away), click on the parent pop-up link, rather than just mousing over it." ,@click-for-more-tips))

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
	  "Why ''bigshot''? Well, even loafing in a big comfy chair, he is more than a little intimidating. It doesn't hurt that he's physically domineering, and looks intelligent (you can't see his eyes behind those mirrorshades, but he's got poise).")
	  ,(p "But that's not even the point. This is " morpheus ". " morpheus "! The legendary conqueror of Minitel. Though his dress sense is awful - leather jackets that heavy went out with " (describe "the-who"))))
   ,(once
     (p "''You know well who I am,'' says the man in the chair solemnly; perhaps rather undermining this claim by holding up a business card, on which is written " `("b" "''" ,morpheus "''" ".")))
   ,(apply p (cons (first "He" morpheus)
		   (describe "morpheus-intro-gaze")))))

(expandable-machine
   "the-who"
   "The Who" "."
   "The Who?"
   (p "(The Who were a British rock band of the 60's and 70's. Their followers - ''mods'' - did often wear long leather coats. Why are you even thinking about this now?)"))
;; Note that flipping the switch will use up a "turn".

(one-way-switch
   "bigshot"
   "bigshot"
   "Bigshot? Why call him that?")

(define (morpheus-intro-choice pre-text stare-text intervening-text speak-text post-text)
  `(,pre-text
    " "
    ,(verb-acts*
      stare-text morpheus
      (verb-goto "Examine" "conversation"
		 `(,(p "You stare at each other for a while. This sang-froid is rather out of character for you, and you feel sweat prickling.") ,(p "But, eventually, " morpheus " clears his throat.")))
      no-attack no-kiss)
    " " ,intervening-text " "
    ,(link-goto speak-text "conversation" "Try to say something cool"
		`("''Speak up, then,'' you say, feigning boredom. ''You asked me here, after all.''" ,(p morpheus " grins.")))
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
  `("blinks slightly."
    "frowns slightly."
    "glares at you, angrily."
    "looks furious."
    "seethes with rage."))
(define morpheus-mood-text
  `((,morpheus " seems unruffled. Almost as though no-one has ever thought of ruffling him, in fact.")
    ("You might just be imagining it, but " ,morpheus " appears slightly off-balance. A thought occurs to you: it couldn't be that he didn't like you disagreeing with him? Surely it would take a lot of dissent to puncture THAT ego.")
    (,morpheus " looks slightly upset. Only slightly, but he clearly doesn't like his monologues to be interrupted. His nose is faintly wrinkled, as at the smell of sour milk.")
    (,"You're definitely getting to him, the way you keep disagreeing with him. He's tapping his fingers impatiently on the chair.")
    (,morpheus " looks furious. Congratulations! This is about as angry as he gets. You have truly succeeded in pissing him off.")))
(define (annoy-morpheus inc)
  (let ((increased-annoyance (+ morpheus-annoyance inc)))
    (if (< increased-annoyance (length morpheus-annoyed-text))
	(set! morpheus-annoyance increased-annoyance)))
  (list (string-append morpheus " " (list-ref morpheus-annoyed-text morpheus-annoyance) " ")))

(define (morpheus-mood)
  (apply string-append (list-ref morpheus-mood-text morpheus-annoyance)))

(define (morpheus-mood-link)
  (verb-acts
   morpheus
   `("Examine" ,morpheus-mood)
   no-attack no-kiss))

;; general format of agree-choice:
;; ''STATEMENT', you say.
;; Morpehus <does affirmative action>. <result text>
;; [Next]
(define (agree-choice statement result dest)
  (choice statement `(,(p "''" statement "'', you say.")
		      ,(p (one-of
			   (span morpheus " nods briskly.")
			   (span morpheus " claps his hands.")
			   (span "''Exactly!'' says " morpheus "."))
			  " "
			  result
			  (next-action dest)))))

;; general format of annoy-choice:
;; ''STATEMENT', you say.
;; <Morpheus gesture-emotes that he has become more annoyed> <result text>
;; [Next]
(define (annoy-choice statement result dest)
  (choice statement `(,(p "''" statement "'', you say.")
		      ,(apply p (append (annoy-morpheus 1) (list result)))
		      ,(next-action dest))))

;; general format of annoy-choice:
;; ''STATEMENT', you say.
;; <Morpheus repeats last ''becomes more annoyed'' emote> <result text>
;; [Next]
(define (neutral-choice statement result dest)
  (choice statement `(,(p "''" statement "'', you say.")
		      ,(apply p (append (annoy-morpheus 0) (list result)))
		      ,(next-action dest))))

(define (text-func t f) (lambda () `(,(if (string? t) (p t) (apply p t)) ,(f))))

;; (convo text (text1 choice1) (text2 choice2) ...)
(define convo
  (lambda args
    (let ((text (car args))
	  (opts (cdr args)))
      (apply cyo (cons (append (h1-club) (list text)) opts)))))

; (define (example-convo)
;   (convo
;    `(,(p "Blah blah."))
;    (agree-choice "Yep" "" next-convo)
;    (neutral-choice "Maybe" "" next-convo)
;    (annoy-choice "Nope" "" next-convo)))

(define (morpheus-alice-gambit)
  (convo
   (p "''I bet you feel like the White Rabbit right now,'' says " morpheus ". ''Running down a hole. Lost in time.''")
   (agree-choice "I suppose so." "" morpheus-reality-gambit)
   (annoy-choice "Not really. The White Rabbit knew where he was going. Maybe you meant Alice?"
		 "''I meant... following the rabbit, of course.''"
		 morpheus-reality-gambit)
   (neutral-choice "What do you mean, \"down a hole\"?" "''That was... just a figure of speech.''" morpheus-reality-gambit)))

(define (morpheus-reality-gambit)
  (convo
   (p "''Have you ever wondered about the nature of reality?'' " morpheus " asks.")
   (neutral-choice "I suppose so." "" morpheus-discrete-gambit)
   (agree-choice "Now you're speaking my language! I'm constantly wondering about that." "He shoots you a lidded glance, then continues." morpheus-discrete-gambit)
   (annoy-choice "No. That's a dumb question." "''Not sure why you would say that. No matter...''" morpheus-exploits-gambit)))

(define (morpheus-discrete-gambit)
  (convo
   (p "''Reality doesn't always have to be what we think it is, on our initial superficial inspection.'' " morpheus " intones. ''Quantum mechanics is a good example. Another example, the distinction between the ''discrete'' and the ''continuous''. Is it really so clear that we live in continuous space? Or might we be simulations on a grid?''")
   (neutral-choice "I guess so, if the grid is fine enough." "''Exactly! When you think about it, the discrete and the continuous are really two sides of a coin.'' This doesn't make much sense to you, but you decide to let it slide." morpheus-exploits-gambit)
   (agree-choice "You just blew my mind." "He makes a gun shape with his fingers, puts it to his head. ''Pow.'' (Did he really just do that?)" morpheus-exploits-gambit)
   (annoy-choice "Actually, continuity has a precise topological definition." `(,(p "''Well, yes; I am aware of that, of course. But there is a poetic truth, as well, behind the mathematical truth.'' (" (i "What a crock,") " you think to yourself.)")) morpheus-exploits-gambit)))

(define (morpheus-exploits-gambit)
  (convo
   (p "''Enough chit-chat. To business! You are an impressive hacker! Your accomplishments have come to our attention.''")
   (agree-choice "You honor me by even noticing." "He graces you with an oily, condescending smile. ''It is our pleasure to welcome you here.'' (Is that the ''Royal We''?)"
		 morpheus-own-exploits-gambit)
   (agree-choice "I'm sure they have, but I don't talk about it." "He waves his hand dismissively. ''Quite. Let us not dwell on the trivial accomplishments of the past, when the future holds so much more!'' he says. (OK... now THAT'S annoying.)"
		 morpheus-own-exploits-gambit)
   (neutral-choice "Let me tell you some of the REALLY good stuff." "''I would be delighted to hear more!'' You suspect he is not being completely sincere, but you can't resist the chance to brag."
		   morpheus-more-exploits-gambit)
   (annoy-choice "I seriously doubt you've heard the least of my accomplishments, amateur." "''Very well, then; tell me more of your hacking accomplishments, and we shall see who is the amateur.''"
		 morpheus-more-exploits-gambit)))

(define (morpheus-more-exploits-gambit)
  (convo
   (p "For the moment, you have hijacked the conversation. (Well done!) " morpheus " is looking at you expectantly (or even impatiently), waiting for you to finish talking about your hacking achievments.")
   (agree-choice "I don't like to brag too much." "''A man after my own heart! But this is why I brought you here...'' he adds, picking up steam again." morpheus-own-exploits-gambit)
   (once-choice (annoy-choice "Let me tell you about my buffer overflow exploit." `(,(p "You describe how you probed the stack using a hexadecimal memory inspector... to the evident discomfort of " morpheus ", who clearly likes to talk about himself. You oblige by going into extensive detail into what was definitely one of your finer hacks, technically speaking.")) morpheus-more-exploits-gambit))
   (once-choice (annoy-choice "I've turned remote control into an artform." "He clearly is unused to sharing the limelight. Nevertheless you explain, in detail, the program you wrote that would display a pair of animated dancing characters in the corner the screen of anyone connected to your school's local network. The program would click the cassette tape control relay rhythmically. (It's hardly your greatest technical achievment, but the spectacularly humorous hack value is enough that you talk very animatedly about it.)" morpheus-more-exploits-gambit))
   (once-choice (annoy-choice "I wrote the first ever 6502 virus for the BBC Micro." "He seems to prefer talking about his own exploits, rather than listening to yours. How silly! Yours are quite significant too. You patiently explain how your malware attached itself to a timer interrupt and injected itself into BASIC code, hidden within a comment using VDU control codes. This really is rather good stuff, and you have no problem expanding on the topic at length." morpheus-more-exploits-gambit))
   (neutral-choice "I've made three ZX Spectrum games." "''Call that an accomplishment! Who hasn't done that? This is why I brought you here, though; to share in the society of your peers,'' he adds, getting back into his stride." morpheus-own-exploits-gambit)))

(define (morpheus-own-exploits-gambit)
  (convo
   (p "''As a darkside hacker yourself, naturally you have heard of my own modest achievments,'' says " morpheus ".")
   (agree-choice
    "Are you kidding? Your takedown of Minitel was legendary!"
    "''Thank you very much! It's good to find someone who can appreciate the story.''"
    morpheus-describes-conspiracy)
   (neutral-choice "Erm, actually I haven't heard much at all." "''You can look it up. This does not, after all, matter much. The point is, I hacked a French email network called Minitel. It was quite widely-reported; I am surprised you didn't... but, again. No matter.''" morpheus-describes-conspiracy)
   (agree-choice
    "Didn't you break into some French network?"
    `(,(span "''Allow me to tell the story...''")
      ,(p "''Last year, I was working with an organization of environmental activists. The precise details are extraneous, but it suffices to say that France is criminally reckless with nuclear weapons, and this organization sought to draw attention to the fact.''")
      ,(p "''During the course of these investigations, I broke into the French text network, Minitel. Again, I shall not trouble you with the details. The mechanics of my penetration included all the tricks in my arsenal: social engineering, zero-day exploits... the full playbook. But none of this is as interesting as what my explorations uncovered, once I got in.''"))
    morpheus-describes-conspiracy)
   (annoy-choice
    "Nope. Never heard of you."
    `(,(p "''Hear that, Delphine?'' he says to no-one in particular. ''Not everyone has heard of me, it seems.''")
      ,(p "(Delphine... wonder if he means Siouxsie?)")
      ,(p "''It matters little whether you know my name or not,'' says " morpheus ". ''It is enough for you to know that, last year, I was able to penetrate a French computer network. You will not have heard of this either, perhaps, but the name of this network is Minitel.''"))
    morpheus-describes-conspiracy)))

(define (morpheus-describes-conspiracy)
  (convo
   `(,(p "''It was during my exploration of Minitel that I uncovered the terrible secret I brought you here to share. Oh, the network itself is of little interest: a darknet haven for the sex and drugs industries, mostly. Such human needs hold interest for me as a businessman, but not beyond that.")
     ,(p "''For myself, I am drawn naturally to the discussions of men of power... I speak five languages, and French is one of the easier ones. A little curious poking led me to some communications between the President's office and the nation of Tahiti. A mysterious spate of... disappearance... kidnappings?''")
     ,(p morpheus " leans forward. ''You remember the tsunami, last year?'' You nod. Many died. It was on TV.")
     ,(p "But " morpheus " barks---''Aha!''---as if he has caught you in a trap. ''I thought I remembered it, too... " (b "last year.") " But this is the great puzzle.''")
     ,(p "''I found references to this tsunami, in these emails, many references. But the timestamps on these email were over ten years old!''"))
   (agree-choice "Are you talking about time travel, or something?" `(,(p "''To be clear, you said that. I did not. As they say... out of the mouths of babes? In this case I think it might be more accurate to use another phrase, rather than time " (i "travel") "... time " (i "distortion") ", perhaps?''") ,(p (i "Babes?!"))) morpheus-bullshits-about-relativity)
   (annoy-choice "Big deal. Timestamps can be faked." "''I assure you, I'd know the difference,'' he says (though you can't see how he could). ''In any case, there is more. Much, much more.''" morpheus-bullshits-about-relativity)
   (neutral-choice "Wait, kidnappings?" "''You are right to be shocked.''" morpheus-describes-kidnappings)
   (agree-choice
    "The emails said something about the tsunami?"
    `(,(p "''I cannot reveal everything... The emails spoke of an asteroid. I can tell you more, but these are dangerous secrets. I need some kind of... guarantee.''"))
    show-choice)))

(define (morpheus-bullshits-about-relativity)
  (convo
   `(,(p "''The theory of relativity tells us that space and time are not real. We now know it to be true, from experiments on animals. And yet leading scientists of the time resisted the concept... If such apparently fundamental concepts as the fixity of time and space can be mere illusions of the mind, is it not possible that time itself can be undone? Especially if, as you pointed out, we are living in a simulation?''"))
   (agree-choice "Actually, I didn't say that." "''I'm pretty sure you did. But it's not important.''" morpheus-describes-kidnappings)
   (neutral-choice "Animals?" "''Yes, relativity was first proved with monkeys.'' He waves away your objections: ''We can look it up. I assure you I am correct. But this is not important - let us move on.''" morpheus-describes-kidnappings)
   (annoy-choice "I think you have fundamentally misunderstood relativity." "''My years of study would strongly disagree. But this is not what I brought you here to discuss.''" morpheus-describes-kidnappings)))

(define (morpheus-describes-kidnappings)
  (convo
   `(,(p "''The emails that I intercepted,''" morpheus " continues, ''were encrypted using an unbreakable Bellaso cipher. It goes without saying,'' he adds smugly, ''that I broke it. Only to be horrified beyond belief by what I found.''")
     ,(p "He leans forward, adopting a conspiratorial posture, but continuing to shout over the music. ''Let me ask you a hypothetical question. You're the sort of person who's read some science fiction, I'm sure. In any of those stories, do you ever recall reading about aliens harvesting humans as an energy source?''"))
   (agree-choice "I'm pretty sure I've heard that somewhere." "''Good,'' he says. ''Then, perhaps, what I'm about to tell you won't blow your mind all over the wall.'' He chuckles darkly. ''I'm not sure about that, though.''" show-choice)
   (annoy-choice "That's ridiculous - humans are net energy sinks, not generators." "''I am afraid that if you come here with a closed mind, you will not learn anything. It's time for you to make a decision.''" show-choice)
   (neutral-choice "Are you trying to say there are aliens in the South Pacific?" "''Well... not exactly.'' He shifts uncomfortably in his seat. ''To explain, I would need... how would you say... a guarantee.''" show-choice)))


(define (show-choice) (goto "choice") (look))



;(morpheus-exploits-gambit)  ;; debug


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
   ,(p "''What I am trying to say,'' says " (morpheus-mood-link) ", ''is that there is a reality of which you are completely, completely unaware. And I can show it to you, or you can walk away.''")
   ,(p "''If you take a blue pill, you can forget your troubles and I am totally cool with that; but if you want to open your mind - and I'm telling you now that we're talking Illuminatus-level secrets, here - then you are going to want to try the red pill.''")
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
   (morpheus-mood-link) " nods approvingly. ''You will soon see the truth,'' he says. ''And now, if you wouldn't mind, that will be "
   (link-goto "fifty pounds" "payment"
	       "Fifty pounds. That's a bit expensive, isn't it?"
	       `(,(p "''Fifty pounds,'' you remark. ''Truth is expensive, then?''") ,(p "''The path to insight is not without material externalities,'' he agrees.")))
   " for the "
   (link-goto "pill.''" "payment"
	      "I thought this pill was supposed to be free, anyway."
	      `(,(p "''I thought this pill was supposed to be free, anyway. You're basically a common dealer, aren't you?''") ,(p "''A dealer in truth, perhaps,'' he says. ''Removal of scales from eyes. You know?''"))))))

(story
 "payment"
 `(,(p "You feel strongly motivated to debate this topic further, but are now keenly aware that the attentions of ''Billy Idol'' are sharply focused upon you, along with several other members of the crew.")
   ,(p
   "The bottom line, and you now kick yourself for realizing this, is that you have eaten a pill that an unknown stranger has given you, in a nightclub, under an implied contract of payment which - while unstated - is hardly unusual either.")
   ,(goto "payment2")))

(story
 "payment2"
 `("True, you could argue that you were intimidated into going along with it. Perhaps you can find a law enforcement official to help you out... meh."
   ,(p
   "Until then, the etiquette of this particular mugging appears pretty clear.")
   ,(p
   "Grudgingly, you fork over payment. You hope that whatever ''insight'' this pill provides is worth it. Judging by the quality of dancing in the club, you are not optimistic.")
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
