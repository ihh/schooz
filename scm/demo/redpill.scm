; dateline 1989

(schooz:interpret-string-actions-as-functions)
(schooz:p-element-after-every-action)
(schooz:look-after-every-action)
(schooz:p-element-after-every-action)

(define (p) '("p"))
(define morpheus "Morpheus")

(story
 "nightclub-lobby"
 `("In the lobby of the nightclub, you are accosted by two of the strangest-looking ravers you have seen. Goth Catwoman and Punk Dwarf, you mentally categorize them. The tall one invites you to "
   ,(link-goto "meet her companion" "meet-morpheus" "Let's meet this mysterious companion." "The tall catlady ushers you to the back of the club, while her darker half growls at bad dancers.")
   " in a back room of the nightclub."))

(story
 "meet-morpheus"
 `("In an armchair sits an obvious bigshot of this scene. Catwoman gave him the faintest bow when you arrived (almost imperceptible, but you can tell) and Punk Dwarf snarls at everyone BUT him. It doesn't hurt that he's physically domineering, with a keen intelligent eye; but you wonder how anyone can think leather jackets that heavy survived the heyday of The Who. "
   (p)
   (once `("The man introduces himself as " ,morpheus " - a name he clearly expects you to have heard of." (p)))
   "He offers you a choice of a "
   ,(link-goto "red pill" "redpill" "Take the red pill." "Silently, he hands over the red pill.")
   " or a "
   ,(link-goto "blue pill" "bluepill" "Take the blue pill." "Silently, he hands over the blue pill.")))
