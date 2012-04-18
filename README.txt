Minimal scheme-based CYOA framework.
Dependencies: node+biwascheme, or guile.
Files:
 schooz-api.scm - core API (R6RS Scheme)
 schooz.scm - implementation of core API
 server.js - simple biwascheme server wrapper (using node)
 index.html - simple biwascheme client wrapper
 guile-1.8-schooz.scm - server wrapper for guile 1.8 (R5RS Scheme)
 Makefile - runs server wrappers
 biwascheme/ - bundled biwascheme (primarily for client wrapper)


Core API functions:

[now X STATE]  ... places object X in state STATE
[state X]  ... returns the current state (typically a string) of object named X

[description X STATE FUNC]  ... set object X's descriptor function for state STATE to FUNC
[describe X]  ... looks up the descriptor function for current state of object X, calls it

[push X STATE]  ... pushes X's current state onto X's stack, places X into state STATE
[pop X]  ... pops state off X's stack, places X into popped state


The state machine named "narrative" determines the overall story graph:
[story STATE FUNC]   ... shortcut for [description 'narrative STATE FUNC]
[look]  ... shortcut for [describe 'narrative]
[goto STATE]   ... shortcut for [now 'narrative STATE]
[gosub STATE]   ... shortcut for [push 'narrative STATE]
[return]   ... shortcut for [pop 'narrative]
[chapter]  ... shortcut for [state 'narrative]

Helper functions to form simple (goto...), (gosub...) and (return) hyperlinks:
[link-goto LINK-TEXT STATE ACTION-TEXT RESULT-TEXT]
[link-gosub LINK-TEXT STATE ACTION-TEXT RESULT-TEXT]
[link-return LINK-TEXT ACTION-TEXT RESULT-TEXT]

Helper functions to form simple (goto...), (gosub...) and (return) menu choices:
[choice-goto STATE ACTION-TEXT RESULT-TEXT]
[choice-gosub STATE ACTION-TEXT RESULT-TEXT]
[choice-return ACTION-TEXT RESULT-TEXT]


Functions implemented by user interface:

[link* TEXT ACTIONTEXT FUNC]  ... returns a hyperlink with text TEXT that calls FUNC, with mouseover text ACTION
[menu TEXT [[ACTIONTEXT1 FUNC1] [ACTIONTEXT2 FUNC2] ...]]  ... returns text hyperlinked to a popup menu
[explicit-menu [[ACTIONTEXT1 FUNC1] [ACTIONTEXT2 FUNC2] ...]]  ... returns a menu (rendered as a list)

[ask X PROMPT]  ... queues a popup text input box with prompt PROMPT, that sets state of X directly
