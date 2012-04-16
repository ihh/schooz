Minimal scheme-based CYOA framework.
Dependencies: node, biwascheme.
Files:
 schooz.scm - main API (R6RS Scheme)
 sketch.scm - sketch of minimal command-line implementation
 server.js - simple biwascheme wrapper
 Makefile - runs wrapper using node


API functions:

[now X STATE]  ... places object X in state STATE
[state X]  ... returns the current state (typically a string) of object named X

[describe X STATE FUNC]  ... set object X's descriptor function for state STATE to FUNC
[tell X]  ... looks up the descriptor function for current state of object X, calls it

[push X STATE]  ... pushes X's current state onto X's stack, places X into state STATE
[pop X]  ... pops state off X's stack, places X into popped state

[story STATE FUNC]   ... shortcut for [describe 'narrative STATE FUNC]
[look]  ... shortcut for [tell 'narrative]
[goto STATE]   ... shortcut for [now 'narrative STATE]
[gosub STATE]   ... shortcut for [push 'narrative STATE]
[return]   ... shortcut for [pop 'narrative]
[chapter]  ... shortcut for [state 'narrative]


Functions to be defined by user interface:

[link TEXT ACTIONTEXT FUNC]  ... returns a hyperlink with text TEXT that calls FUNC, with mouseover text ACTION
[menu TEXT [[ACTIONTEXT1 FUNC1] [ACTIONTEXT2 FUNC2] ...]]  ... returns text hyperlinked to a popup menu
[choice [[ACTIONTEXT1 FUNC1] [ACTIONTEXT2 FUNC2] ...]]  ... returns a menu (rendered as a list)

[ask X PROMPT]  ... queues a popup text input box with prompt PROMPT, that sets state of X directly


Global objects:

Object->state hashtable
Object->state->descriptor hashtable
Object->stack hashtable


Vague future plans:

Simple text substitution swaps (...) with [...]
Wraps all text with quotes.
