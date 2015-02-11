% PandocAgda Example


Pandoc Markdown basics
======================

Headers
-------

A setext-style header is a line of text "underlined" with a row of `=` signs (for a level one header) or `-` signs (for a level two header):

    A level-one header
    ==================

    A level-two header
    ------------------

For more options on headers look at
[the relevant section in Pandoc User's Guide](http://johnmacfarlane.net/pandoc/README.html#headers).


Lists
-----

A simple bullet list can be made with

    * one
    * two
    * three

Result:

* one
* two
* three

For more options on lists look at
[the relevant section in Pandoc User's Guide](http://johnmacfarlane.net/pandoc/README.html#lists).


Inline formatting
-----------------

To emphasize some text, surround it with `*`s or `_`, like this:

    This text is _emphasized with underscores_, and this
    is *emphasized with asterisks*.
    This is `verbatim text`.

Result:

This text is _emphasized with underscores_, and this
is *emphasized with asterisks*.
This is `verbatim text`.

For more options on inline formatting look at
[the relevant section in Pandoc User's Guide](http://johnmacfarlane.net/pandoc/README.html#inline-formatting).


Links
-----

An inline link consists of the link text in square brackets, followed by the URL in parentheses. (Optionally, the URL can be followed by a link title, in quotes.)

    This is an [inline link](/url), and here's [one with
    a title](http://fsf.org "click here for a good time!").

For more options on links look at
[the relevant section in Pandoc User's Guide](http://johnmacfarlane.net/pandoc/README.html#links).


PandocAgda Specific Markup
==========================

Agda code blocks
----------------

Agda code is surrounded by `\begin{code}` and `\end{code}`
(usual literate Agda syntax).

Result:

\begin{code}
data Bool : Set where
  true  : Bool
  false : Bool
\end{code}


Hyperlinked identifiers
-----------------------

Identifiers are automatically hyperlinked (click on `Bool` below):

\begin{code}
not : Bool → Bool
not true  = false
not false = true
\end{code}


External modules
----------------

External modules are automatically loaded and built.
You will notice this when you click on a hyperlinked identifier which is
imported from an external module.


Hiding Agda code
----------------

To hide Agda code, surround the code block by `<!--` and `-->`.

Result:

\begin{code}
_∧_   : Bool → Bool → Bool  -- the definition is hidden
\end{code}

<!--
\begin{code}
true  ∧ x = x
false ∧ _ = false
\end{code}
-->


Compilation
===========

Simple invocation of `agdapandoc`:

    agdapandoc --html Simple.lagda

If external libraries are used and the source code is in a separate directory,
invoke `agdapandoc` like

    agdapandoc -i $(AGDALIB)/src/ -i src --html --css Agda.css src/Index.lagda





