; dateline 1989

(schooz:interpret-string-actions-as-functions)
(schooz:p-element-after-every-action)
(schooz:look-after-every-action)
(schooz:p-element-after-every-action)

(define para "p")
(define (p) `(,para))

(define morpheus "Morphetikus")

(story
 "nightclub-lobby"
 `(("h1" "Nightclub lobby")
   "In the lobby of the nightclub, you are accosted by two of the strangest-looking ravers you have seen. Goth Catwoman and Punk Dwarf, you mentally categorize them as."
   ,(p)
   "The tall one invites you to "
   ,(link-goto "meet her colleague" "meet-morpheus" "OK, let's meet this mysterious 'colleague'." "The tall catlady ushers you to the back of the club, while her darker half growls at bad dancers.")
   " in a back room of the nightclub."))

(story
 "meet-morpheus"
 `(("h1" "Back of the club")
   "In an armchair sits an obvious bigshot of this scene. Catwoman gave him the faintest bow when you arrived (almost imperceptible, but you can tell) and Punk Dwarf snarls at everyone BUT him."
   ,(p)
   "It doesn't hurt that he's physically domineering, with a keen intelligent eye; but you wonder how anyone can think leather jackets that heavy survived the heyday of "
   ,@(describe "The Who")
   ,(p)
   ,@(first
      `("The man introduces himself as " ,morpheus " - a name he clearly expected you to know already, and now expects you to remember how to spell." ,(p) "He ")
      `(,morpheus))
   " offers you a choice of a "
   ,(link-goto "red pill" "pill" "Take the red pill." "Silently, he hands over the red pill.")
   " or a "
   ,(link-goto "blue pill." "pill" "Take the blue pill." "Silently, he hands over the blue pill.")))

(description
 "The Who"
 "hidden"
 `(,(link "The Who." "The Who?" (now "The Who" "shown"))))

(description
 "The Who"
 "shown"
 `("The Who." ,(p) "(The Who were a British rock band of the 60's and 70's. Their followers - 'mods' - did often wear long leather coats. Probably to stash big bags of pills in, now you come to think of it.)"))

(story
 "pill"
 `(,(p)
   "You crunch the pill between your teeth. Bitter. Good sign."
   ,(p)
   ,morpheus " nods approvingly. 'You will soon see the truth,' he says. 'And now, if you wouldn't mind, that will be "
   ,(link-goto "twenty dollars" "payment"
	       "Twenty dollars. That's a bit expensive, isn't it?"
	       `("'Twenty dollars,' you remark. 'Truth is expensive, then?'" ,(p) "'The path to insight is not without material externalities,' he agrees."))
   " for the "
   ,(link-goto "pill.'" "payment"
	       "I thought this pill was supposed to be free, anyway."
	       `("'I thought this pill was supposed to be free, anyway. You're basically a common dealer, aren't you?'" ,(p) "'A dealer in truth, perhaps,' he says. 'Removal of scales from eyes. You know?'"))))

(story
 "payment"
 `("You feel strongly motivated to debate this topic further, but are now keenly aware that the attentions of Punk Dwarf are sharply focused upon you, along with several other members of the coterie."
   ,(p)
   "The bottom line, and you now kick yourself for realizing this, is that you have eaten a pill that an unknown stranger has given you, in a nightclub, under an implicit contract of payment which - while unstated - is hardly unusual."
   ,(p)
   "Grudgingly, you fork over payment. You hope that whatever 'insight' this pill provides is worth it. Judging by the quality of dancing in the club, you are not optimistic."))


