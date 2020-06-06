#!/bin/env sh

HEAD="\input texinfo    @c -*- texinfo -*-
@c %**start of header
@setfilename WritingWithInk.info
@settitle Writing with ink
@documentencoding UTF-8
@documentlanguage en
@c %**end of header

@copying
@quotation
Copyright (c) 2017 inkle Ltd.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
\"Software\"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

@end quotation
@end copying

@dircategory Emacs
@direntry
* Writing with ink.       Using the ink interactive fiction language.
@end direntry

@finalout
@titlepage
@title Writing with ink
@author inkle Ltd.
@page
@vskip 0pt plus 1filll
@insertcopying
@end titlepage

@contents

@ifnottex
@node Top
@top Writing with Ink

@strong{ink} is a scripting language built around the idea of marking
up pure-text with flow in order to produce interactive scripts.

At its most basic, it can be used to write a Choose Your Own-style
story, or a branching dialogue tree. But its real strength is in
writing dialogues with lots of options and lots of recombination of
the flow.

@strong{ink} offers several features to enable non-technical writers
to branch often, and play out the consequences of those branches, in
both minor and major ways, without fuss.

The script aims to be clean and logically ordered, so branching
dialogue can be tested \"by eye\". The flow is described in a
declarative fashion where possible.

It's also designed with redrafting in mind; so editing a flow should
be fast.

@quotation
Copyright (c) 2017 inkle Ltd.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
\"Software\"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

@end quotation
@end ifnottex"

# echo $HEAD

pandoc -o WritingWithInk.texi WritingWithInk.md -f gfm -t texinfo

BODY=$(cat WritingWithInk.texi)
printf '%s\n' "$HEAD" > WritingWithInk.texi
printf '%s\n' "$BODY" | tail -n +3 >> WritingWithInk.texi

makeinfo WritingWithInk.texi --no-split
