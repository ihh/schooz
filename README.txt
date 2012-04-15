Minimal scheme-based CYOA framework.

Simple text substitution swaps (...) with [...]
Wraps all text with quotes & call to [print... ]

Main functions:
[now X STATE]  ... places object X in state STATE
[state X]  ... returns the current state (typically a string) of object named X, where X is an atom

[describe X STATE FUNC]  ... set object X's descriptor function for state STATE to FUNC
[tell X ARGS]  ... looks up the descriptor function for current state of object X, calls it with ARGS

[push X STATE]  ... pushes the current state of X onto X's stack, places X into state STATE
[pop X]  ... pops state off X's state stack, places X into popped state

[story STATE FUNC]   ... shortcut for [describe 'story STATE FUNC]
[goto STATE]   ... shortcut for [now 'story STATE]
[gosub STATE]   ... shortcut for [push 'story STATE]
[return]   ... shortcut for [pop 'story]
[chapter]  ... shortcut for [state 'story]

[link TEXT ACTION FUNC]  ... returns a hyperlink with text TEXT that calls FUNC, with mouseover text ACTION
[menu TEXT [[ACTIONTEXT1 FUNC1] [ACTIONTEXT2 FUNC2] ...]]  ... returns text hyperlinked to a popup menu
[choice [[ACTIONTEXT1 FUNC1] [ACTIONTEXT2 FUNC2] ...]]  ... returns a menu (rendered as a list)

[say TEXT]  ... queues TEXT for output


Global objects:
Text output queue (for [say...])
Object->state hashtable
Object->state->descriptor hashtable
Object->stack hashtable

