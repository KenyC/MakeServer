Make Server
==================================

A tiny local server. Launch it from a directory with a Makefile. Any request for a file will be preceded by a call to `make` for that file.

**Use case in mind:** You write your document in e.g. Pandoc Markdown. You have a Makefile to compile the md document to html. With the local server, a single page refresh on your browser is sufficient to see the recompiled output html page.