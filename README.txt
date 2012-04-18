Minimal scheme-based Choose Your Own Adventure-style framework for interactive fiction.
Dependencies: node+biwascheme, or guile.
Files:
 api.scm - core API (R6RS Scheme)
 schooz.scm - implementation of core API
 terminal.scm - terminal-based user interface
 server.js - toy biwascheme server wrapper (using node)
 index.html - toy biwascheme client wrapper
 guile-1.8-schooz.scm - toy server wrapper for guile 1.8 (R5RS Scheme)
 Makefile - runs server wrappers
 biwascheme/ - bundled biwascheme (primarily for client wrapper)
 rock.scm - a tiny demo


Basic concepts:

An action is a function that returns hyperlinked SXML.
Each hyperlink corresponds to a possible next action.
Hyperlinks are made using (link...), (menu...), and variants.

The core API also provides state machines with stacks (pushdown automata).
Each state has an associated descriptor function, that generates hyperlinked SXML.

A distinguished state machine called "narrative" models the overall story graph.
Special functions are provided for working with this narrative automaton
(for example, "look" always maps to the current narrative descriptor function).

Actions and state machines can be coupled, so "look" is appended to every action.
A common action is to change the narrative state; this amounts to a simple CYOA game.
