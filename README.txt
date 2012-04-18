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
