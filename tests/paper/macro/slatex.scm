;;; slatex modified to be self contained, including an "in memory" filesystem.

;;;----------------------------------------------------------------------------

;;; I/O interface that avoids accessing the filesystem to improve
;;; benchmarking.

(define init-filesystem '(
("test.tex" . "%slatex-d.tex\n%SLaTeX Version 2\n%Documentation for SLaTeX\n%(c) Dorai Sitaram, 1991, 1994\n%dorai@cs.rice.edu\n\n\\documentstyle[slatex]{article}\n\n\\slatexdisable{enableslatex}\n\n\\edef\\atcatcodebeforepreamble{\\the\\catcode`@}\n\\catcode`@11\n\n\\inputifpossible{multicol.sty}\n\n%if Frank Mittelbach's multicol.sty is not\n%available, the index will simply waste some paper\n\n%latex wastes too much paper, so...\n\n\\textheight 11in\n\\textwidth 8.5in\n\\oddsidemargin 1.25in\n\\advance\\textheight -2\\oddsidemargin\n\\advance\\textwidth -2\\oddsidemargin\n\\advance\\oddsidemargin -1in\n\\evensidemargin\\oddsidemargin\n\\topmargin\\oddsidemargin\n\\advance\\topmargin -\\headheight\n\\advance\\topmargin -\\headsep\n\n%latex's section headings are way too obnoxiously\n%large, so...\n\n\\def\\nolargefonts{\\let\\large\\normalsize\n\\let\\Large\\normalsize\n\\let\\LARGE\\normalsize\n\\let\\huge\\normalsize\n\\let\\Huge\\normalsize}\n\n%mini headers for introducing paragraphs\n\n\\def\\re{\\medbreak\\parindent0pt%\n\\aftergroup\\smallskip\\obeylines\n\\llap{$\\searrow$\\enspace\\enspace}}\n\n%a wide line\n\n\\def\\wideline{\\centerline{\\hrulefill}}\n\n%smart italics\n\n\\def\\italicsbegin{\\begingroup\\it}\n\\def\\italicsend{\\endgroup\\futurelet\\next\\italiccorrection}\n\\def\\italiccorrection{\\ifx\\next,\\else\\ifx\\next.\\else\\/\\fi\\fi}\n\\def\\italicstoggle{\\italicsbegin\\let\\italicstoggle\\italicsend}\n\\catcode`\\_\\active\n\\def_{\\ifmmode\\sb\\else\\expandafter\\italicstoggle\\fi}\n\n%quote.tex, by Hunter Goatley\n\n{\\catcode`\\\"\\active\n%\n\\gdef\\begindoublequotes{\\global\\catcode`\\\"\\active\n\\global\\let\\dblqu@te=L}\n%\n\\gdef\"{\\ifinner\\else\\ifvmode\\let\\dblqu@te=L\\fi\\fi\n\\if L\\dblqu@te``\\global\\let\\dblqu@te=R\\else\n\\let\\xxx=\\spacefactor\n''\\global\\let\\dblqu@te=L%\n\\spacefactor\\xxx\n\\fi}}\n\n\\def\\enddoublequotes{\\catcode`\\\"=12}\n\n%nicer \\verb\n\n\\begingroup\\catcode`[1\\catcode`]2\\catcode`\\{12\\catcode`\\}12%\n\\gdef\\@sverb#1[\\if#1{\\def\\@tempa##1}[\\leavevmode\\null##1\\endgroup]\\else\n\\def\\@tempa##1#1[\\leavevmode\\null##1\\endgroup]\\fi\\@tempa]%\n\\endgroup\n\n%nicer \\footnote\n\n\\let\\latexfootnote\\footnote\n\\def\\footnote{\\unskip\\latexfootnote\\bgroup\\let\\dummy=}\n\n%item\n\n\\let\\o\\item\n\n%index environment that exploits multicol.sty if\n%available...\n\n\\renewenvironment{theindex}%\n{\\parindent0pt%\n\\let\\item\\@idxitem\n\\section*{Index}%\n\\ifx\\multicols\\undefined\\else\n\\begin{multicols}{2}\\fi}%\n{\\ifx\\multicols\\undefined\\else\n\\end{multicols}\\fi}\n\n\\catcode`@\\atcatcodebeforepreamble\n\n\\begindoublequotes\n\\makeindex\n\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n\\title{How to Use SLaTeX}\n\n\\author{Dorai Sitaram\\\\\n{\\tt dorai@cs.rice.edu}\\\\\nDepartment of Computer Science\\\\\nRice University\\\\\nHouston, TX 77251--1892}\n\n\\date{Gestated 1990\\\\\nFirst public release, Mar. 1991\\\\\nFirst major update, Dec. 1991\\\\\nCurrent update, Jan. 1994}\n\n\\begin{document}\n\\maketitle\n\\nolargefonts\n\n\\section{Introduction}\n\nSLaTeX\\index{introduction} is a Scheme program\nthat allows you to write programs or program fragments\n\"as is\" in your TeX or LaTeX source.  SLaTeX is\nparticularly geared to the programming languages Scheme\nand other Lisps, e.g., Common Lisp.  The formatting of\nthe code includes assigning appropriate fonts to the\nvarious tokens in the code (keywords, variables,\nconstants, data), at the same time retaining the proper\nindentation when going to the non-monospace\n(non-typewriter) fonts provided by TeX.  SLaTeX comes\nwith two databases that recognize the identifier\nconventions of Scheme and Common Lisp respectively.\nThese can be modified by the user using easy TeX\ncommands.  In addition, the user can inform SLaTeX to\ntypeset certain identifiers as specially suited LaTeX\nexpressions (i.e., beyond just fonting them).  All this\nis done without interfering with the identifier\nconventions of the language of the programming code at\nall.  In sum, no change need be made to your\n(presumably running) program code in order to get a\ntypeset version suited to the particular need: you can\nget a spectrum of styles ranging from _no_ fonting\nthrough basic default fonting to various\n\"mathematical\"-looking output for pedagogic or other\nreasons.\n\n\\enableslatex\nOther packages~\\cite{schemeweb,lisp2tex} for\ntypesetting code fragments use a \\verb{verbatim}\nenvironment where all the characters are in a\n\\verb{monospace typewriter font}.  This \\verb{monospace}\nensures that the indentation is not affected.  However,\nthe resulting output fails to distinguish between the\nvarious tokens used in the code, e.g., boldface for\nkeywords like\n\\scheme{define} and \\scheme{lambda}, sans-serif for\nconstants like \\scheme{#t} and \\scheme{42}, and italics\nfor variables such as \\scheme{x} and\n\\scheme{y} in \\scheme{(lambda (x y) (cons x (cons y\n'())))}.\n\\slatexdisable{enableslatex}\n\nThe program SLaTeX provides a convenient way of\ncapturing the indentation information as well as\nassigning distinguishing fonts to code tokens without\nrequiring the user to worry about fonting and spacing.\nIt uses temporary files to store its typeset version of\nthe user's code fragments and then calls TeX or LaTeX\non the user's TeX files as well as these temporaries.\n\nThe following section will introduce you to the basic\nuse of SLaTeX with a small example.\nSection~\\ref{slatex.sty} introduces the SLaTeX style\nfiles.  Section~\\ref{glossary} gives a complete\ndescription of all the SLaTeX control sequences.  These\ninclude commands for manipulating output positioning,\nenhancing the database, changing the fonting defaults,\nadding special symbols, and selective disabling of\nSLaTeX.  Section~\\ref{preamble} desribes how to set up\na preamble that reflects your typesetting taste.\nSection~\\ref{ftp} contains information on obtaining and\ninstalling SLaTeX.\n\n\\section{A quick illustration of using SLaTeX}\n\\label{quick}\n\\index{quick illustration}\n\nThis section presents a short example of SLaTeX use.\nWe first look at a LaTeX file using SLaTeX commands,\nand then give a plain TeX version of the same file.  We\nwill see that there are minor differences between the\nways SLaTeX is used with plain TeX and LaTeX (but see\n\\verb{\\defslatexenvstyle} for a way to use the\nplain-TeX style with the LaTeX format, and conversely,\nthe LaTeX style with the plain format).\n\n\\subsection{For LaTeX users}\n\\index{LaTeX}\n\\index{scheme@\\verb{\\scheme}}\n\\index{schemedisplay@\\verb{schemedisplay}!in LaTeX}\n\\index{in-text Scheme code}\n\\index{displayed Scheme code}\n\\index{slatex.sty@\\verb{slatex.sty}}\n\\index{slatex.sty@\\verb{slatex.sty}!as document style}\n\nConsider the following LaTeX (_and_ SLaTeX) file\n\\verb{quick.tex}:\n\n\\wideline\n\\begin{verbatim}\n% quick.tex\n\\documentstyle[slatex]{article}\n%or:\n%  \\documentstyle{article}\n%  \\input slatex.sty\n\nIn Scheme, the expression \\scheme|(set! x 42)| returns\nan unspecified value, rather than \\scheme'42'.\nHowever, one could get a \\scheme{set!} of the latter\nstyle by:\n\n\\begin{schemedisplay}\n(define-syntax setq\n  (syntax-rules ()\n    [(setq x a)\n     (begin (set! x a)\n\t    x)]))\n\\end{schemedisplay}\n\n\\end{document}\n\\end{verbatim}\n\\wideline\n\nFirst, the SLaTeX definitions in the style file\n\\verb{slatex.sty} are loaded into your LaTeX file ---\nthis may be done either as a \\verb{\\documentstyle}\noption, or through an \\verb{\\input} command.\n\n\\index{scheme@\\verb{\\scheme}!using grouped argument}\n\nIn-text code is introduced by the SLaTeX control\nsequence \\verb{\\scheme} and is flanked by a pair of\nidentical characters that are not alphabets or\n\"\\verb|{|\".  As a special convenient case, SLaTeX also\nallows the form \\verb|\\scheme{...}|.\n\nThe SLaTeX control sequences for displayed code are the\nopening \\verb|\\begin{schemedisplay}| and the closing\n\\verb|\\end{schemedisplay}|.\n\nThe file is now SLaTeX'd by running the command\n\\verb{slatex} on it from the Unix or DOS command line:\n\n\\begin{verbatim}\nslatex quick\n\\end{verbatim}\nor\n\\begin{verbatim}\nslatex quick.tex\n\\end{verbatim}\nThis calls a Scheme program \\verb{slatex.scm} that\ntypesets the Scheme code fragments in \\verb{quick.tex}\ninto temporary files.   Thereafter, \\verb{quick.tex} along with\nthe temporary files are then passed to LaTeX.  (For\ninformation on judiciously reusing temporary files, see\n\\verb{\\slatexseparateincludes}.)\nThe resulting\n\\verb{quick.dvi} file, when viewed or printed looks like:\n\n\\enableslatex\n\\wideline\nIn Scheme, the expression \\scheme|(set! x 42)| returns\nan unspecified value, rather than\n\\scheme'42'.  However, one could get a \\scheme{set!} of\nthe latter style by:\n\n\\begin{schemedisplay}\n(define-syntax setq\n  (syntax-rules ()\n    [(setq x a)\n     (begin (set! x a)\n\t    x)]))\n\\end{schemedisplay}\n\\wideline\n\n\\index{recognizing new syntactic keywords automatically}\n\nNote that \\scheme{setq}, although not normally a\nsyntactic keyword in Scheme is nevertheless\nautomatically recognized as such because of the context\nin which it occurs.  No special treatment is needed to\nensure that it will continue be treated as such in any\nsubsequent Scheme code in the document.\n\n\\slatexdisable{enableslatex}\n\n\\subsection{For plain TeX users}\n\\index{plain TeX}\n\\index{scheme@\\verb{\\scheme}}\n\\index{schemedisplay@\\verb{schemedisplay}!in plain TeX}\n\\index{in-text Scheme code}\n\\index{displayed Scheme code}\n\nPlain TeX users invoke SLaTeX much the same way, but\nfor only two exceptions.  First, since TeX doesn't have\n\\verb{\\documentstyle}, the file \\verb{slatex.sty} is\nintroduced via an \\verb{\\input} statement before its\ncommands can be used in the plain TeX source.\n\n\\index{environments}\n\nSecond, since plain TeX does not have LaTeX's\n\\verb|\\begin{|_env_\\verb|}...\\end{|_env_\\verb|}|\nstyle of environments, any\nenvironment commands in SLaTeX are invoked with the\nopening \\verb{\\}_env_ and the closing \\verb{\\end}_env_.\n\nThe plain TeX version of \\verb{quick.tex} looks like:\n\n\\wideline\n\\begin{verbatim}\n% quick.tex\n\\input slatex.sty\n\nIn Scheme, the expression \\scheme|(set! x 42)| returns\nan unspecified value, rather than \\scheme'42'.\nHowever, one could get a \\scheme{set!} of the latter\nstyle by:\n\n\\schemedisplay\n(define-syntax setq\n  (syntax-rules ()\n    [(setq x a)\n     (begin (set! x a)\n\t    x)]))\n\\endschemedisplay\n\n\\bye\n\\end{verbatim}\n\\wideline\n\nThe file is now SLaTeX'd by invoking \\verb{slatex} as\nbefore --- SLaTeX is clever enough to figure out\nwhether the file it operates on should later be send to\nLaTeX or plain Tex.\n\n\\section{The style files}\n\\label{slatex.sty}\n\\index{slatex.sty@\\verb{slatex.sty}}\n\nIn short, the LaTeX (or TeX) file that is given to\nSLaTeX undergoes some code-setting preprocessing and is\nthen handed over to LaTeX (or TeX).  The style file\n\\verb{slatex.sty} defines the appropriate commands so\nthat LaTeX (or TeX) can recognize the SLaTeX-specific\ndirectives and either process or ignore them.  You may\neither \\verb|\\input| the file \\verb{slatex.sty} as\nusual, or use it as the \\verb|\\documentstyle| option\n\\verb{slatex}.\n\n\\index{cltl.sty@\\verb{cltl.sty}}\n\\index{SLaTeX database!for Scheme}\n\\index{SLaTeX database!for Common Lisp}\n\\index{SLaTeX database!modifying}\n\nThe default database of SLaTeX recognizes the keywords\nand constants of Scheme.  The database can be modified\nwith the commands \\verb{\\setkeyword},\n\\verb{\\setconstant}, \\verb{\\setvariable},\n\\verb{\\setspecialsymbol} and \\verb{\\unsetspecialsymbol}\n(q.v.).  If you're using Common Lisp rather than\nScheme, use \\verb{cltl.sty} instead of\n\\verb{slatex.sty}.\n\\verb{cltl.sty} loads \\verb{slatex.sty} and modifies\nthe database to reflect Common Lisp.  You may fashion\nyour own \\verb{.sty} files on the model of\n\\verb{cltl.sty}.\n\n\\section{SLaTeX's control sequences}\n\\label{glossary}\n\\index{SLaTeX control sequences}\n\nYou've already seen the SLaTeX control sequence\n\\verb|\\scheme| and the environment\n\\verb{schemedisplay}.  These suffice for quite a few\ninstances of handling code.  However, you will\noccasionally require more control on the typesetting\nprocess, and the rest of this section describes the\ncomplete\n\\footnote{At least that's what you're supposed\nto think...} list of SLaTeX control sequences shows you\nthe ropes.\n\n{\\re\n\\verb{schemedisplay}}\n\\index{schemedisplay@\\verb{schemedisplay}}\n\\index{displayed Scheme code}\n\n[In plain TeX: \\verb{\\schemedisplay} ...\n\\verb{\\endschemedisplay}; in LaTeX:\n\\verb|\\begin{schemedisplay}| ...\n\\verb|\\end{schemedisplay}|; but see \\verb{\\defslatexenvstyle}.]\n\nTypesets the enclosed code, which is typically several\nlines of code indented as you normally do in your\nScheme files.  E.g.,\n\n\\begin{verbatim}\n\\begin{schemedisplay}\n(define compose          ;this is also known as $B$\n  (lambda (f g)\n    (lambda (x)\n      (apply f (g x)))))\n\\end{schemedisplay}\nis the \"compose\" function.\n\\end{verbatim}\nproduces\n\n\\enableslatex\n\\begin{schemedisplay}\n(define compose        ;this is also known as $B$\n  (lambda (f g)\n    (lambda (x)\n      (apply f (g x)))))\n\\end{schemedisplay}\n\\slatexdisable{enableslatex}\nis the \"compose\" function.\n\nAs with all LaTeX environment enders, if the line after\n\\verb|\\end{schemedisplay}| contains\nnon-whitespace text, the paragraph continues.\nOtherwise --- i.e., when \\verb|\\end{schemedisplay}| is\nfollowed by at least one blank line --- a fresh\nparagraph is started.  Similarly, in plain TeX, a fresh\nparagraph is started after a \\verb{schemedisplay} only\nif\n\\verb|\\endschemedisplay| is followed by at least one\nblank line.\n\n\\index{Scheme comments}\n\nComments in Scheme are usually introduced by \"\\verb{;}\"\n(semicolon).  The rest of the line after a \"\\verb{;}\"\nis set as a line in LaTeX LR mode.\n\n\\index{TeX paragraphs amidst Scheme code}\n\nSeparate _blocks_ of code can either be introduced in\ndifferent \\verb{schemedisplay} environments or put in a\nsingle \\verb{schemedisplay} and separated by a line with\na \"\\verb{;}\" in the first column.  This \"\\verb{;}\" is\nnot typeset and anything following it on the line is\nset in (La)TeX LR paragraph mode.  Consecutive lines\nwith \"\\verb{;}\" in the first column are treated\nas input for a TeX paragraph, with words possibly\nmoved around from line to line to ensure justification.\nWhen in paragraph mode, the first line that has _no_\nleading \"\\verb{;}\" signals a fresh block\nof Scheme code within the\n\\verb{schemedisplay}.  (The \\verb{schemedisplay} may\nend, or commence, on either a paragraph or a Scheme\ncode block.)\n\nE.g.,\n\n\\begin{verbatim}\n\\begin{schemedisplay}\n(define even?             ; testing evenness\n  (lambda (n)\n    (if (= n 0) #t (not (odd? (- n 1))))))\n; The procedures {\\it even?} above\n; and {\\it odd?} below are mutually\n; recursive.\n(define odd?              ; testing oddness\n  (lambda (n)\n    (if (= n 0) #f (not (even? (- n 1))))))\n\\end{schemedisplay}\n\\end{verbatim}\nproduces\n\n\\enableslatex\n\\begin{schemedisplay}\n(define even?             ; testing evenness\n  (lambda (n)\n    (if (= n 0) #t (not (odd? (- n 1))))))\n; The procedures {\\it even?} above\n; and {\\it odd?} below are mutually\n; recursive.\n(define odd?              ; testing oddness\n  (lambda (n)\n    (if (= n 0) #f (not (even? (- n 1))))))\n\\end{schemedisplay}\n\\slatexdisable{enableslatex}\n\nSLaTeX can recognize that blocks of code are separate\nif you have at least one empty line separating them.\nI.e., there is no need for empty \"\\verb{;}\" lines.  This\nconvenience is to accommodate Scheme files where\ndefinitions are usually separated by one or more blank\nlines.\n\n\\index{schemedisplay@\\verb{schemedisplay}!allowing page\nbreaks in}\n\nIntervening paragraphs, either with lines with a\nleading \"\\verb{;}\", or with blank lines, are ideal\nspots for \\verb{schemedisplay} to allow pagebreaks.  In\nfact, the default setting for \\verb{schemedisplay} also\nallows pagebreaks _within_ a Scheme block, but it is\neasy to disable this (see entry for\n\\verb{\\rightcodeskip}).\n\nThe space surrounding displayed Scheme code can be\nmodified by setting the _skip_s \\verb{\\abovecodeskip},\n\\verb{\\belowcodeskip}, \\verb{\\leftcodeskip}, and\n\\verb{\\rightcodeskip} (q.v.).\n\nNote: see \\verb{schemeregion}.\n\n{\\re\n\\verb{\\scheme}}\n\\index{scheme@\\verb{\\scheme}}\n\\index{in-text Scheme code}\n\nTypesets its argument, which is enclosed in arbitrary\nbut identical non-alphabetic and non-\\verb|{|\ncharacters, as in-text code.  Special case:\n\\verb|\\scheme{...}| is a convenience (provided the\n\\verb|...| doesn't contain a\n\\verb|}|).  E.g., \\verb+\\scheme|(call/cc (lambda (x) x))|+\nand \\verb+\\scheme{(call/cc (lambda (x) x))}+ both\nproduce\n\\enableslatex\n\\scheme{(call/cc (lambda (x) x))}.\n\\slatexdisable{enableslatex}\n\\index{scheme@\\verb{\\scheme}!using grouped argument}\n\n\\index{nesting SLaTeX control sequences}\nIt _is_ permitted to intermix calls to\n\\verb{schemedisplay} and\n\\verb|\\scheme|.  Thus,\n\n\\begin{verbatim}\n\\begin{schemedisplay}\n(define factorial\n  (lambda (n)\n    (if (= n 0) ; \\scheme{(zero? n)} also possible\n        1 (* n (factorial (- n 1)))))) ; or \\scheme{... (sub1 1)}\n\\end{schemedisplay}\n\\end{verbatim}\nproduces\n\n\\enableslatex\n\\begin{schemedisplay}\n(define factorial\n  (lambda (n)\n    (if (= n 0) ; \\scheme{(zero? n)} also possible\n\t1\n\t(* n (factorial (- n 1)))))) ; or \\scheme{... (sub1 1)}\n\\end{schemedisplay}\n\\slatexdisable{enableslatex}\n\nNote: see \\verb{schemeregion}.\n\n{\\re\n\\verb{\\schemeresult}}\n\\index{schemeresult@\\verb{\\schemeresult}}\n\nTypesets its argument, which is enclosed in arbitrary\nbut identical non-alphabetic and non-\\verb|{|\ncharacters, as in-text Scheme \"result\" or data: i.e.,\nkeyword and variable fonts are disabled.  Special\nconvenient case (as for \\verb|\\scheme|):\n\\verb|\\schemeresult{...}|.  E.g.,\n\\index{schemeresult@\\verb{\\schemeresult}!using grouped argument}\n\n\\begin{verbatim}\n\\scheme|((lambda () (cons 'lambda 'cons)))| yields\n\\schemeresult|(lambda . cons)|.\n\\end{verbatim}\nproduces\n\n\\enableslatex\n\\scheme|((lambda () (cons 'lambda 'cons)))| yields\n\\schemeresult|(lambda . cons)|.\n\\slatexdisable{enableslatex}\n\n{\\re\n\\verb{schemebox}}\n\\index{schemebox@\\verb{schemebox}}\n\\index{boxed Scheme code}\n\n[In plain TeX: \\verb{\\schemebox} ...\n\\verb{\\endschemebox}; in LaTeX:\n\\verb|\\begin{schemebox}| ...\n\\verb|\\end{schemebox}|; but see \\verb{defslatexenvstyle}.]\n\nThe \\verb{schemebox} environment is similar to\n\\verb{schemedisplay} except that the code is provided\nas a \"box\" (i.e., it is not \"displayed\" in the standard\nway).  Indeed, when the appropriate skip parameters are\nset, \\verb{schemedisplay} itself _may_\n\\footnote{Yes, _may_:  Not all \\verb{schemedisplay}s invoke\n\\verb{schemebox}, and if you're curious why,\nsee entry for \\verb{\\rightcodeskip}.  It is a matter of\nwhether pagebreaks within Scheme code are allowed or\nnot.} use a\n\\verb{schemebox} to create a box of code that is\nset off with all-round space as a display.\n\nSaving a \\verb{schemebox} in an explicit box allows you\nto move your typeset code arbitrarily.\n\nNote: see \\verb{schemeregion}.\n\n{\\re\n\\verb{\\schemeinput}}\n\\index{schemeinput@\\verb{schemeinput}}\n\\index{inputting Scheme files as is}\n\nThis can be used to input Scheme files as typeset code.\n(Unlike LaTeX's \\verb|\\input|, \\verb|\\schemeinput|'s\nargument must always be grouped.)  The Scheme file can\nbe specified either by its full name, or without its\nextension, if the latter is \\verb{.scm}, \\verb{.ss} or\n\\verb{.s}.  E.g.,\n\n\\begin{verbatim}\n\\schemeinput{evenodd.scm}    % the .scm is optional!\n\\end{verbatim}\n(where \\verb{evenodd.scm} is the name of a Scheme file\ncontaining the code for\n\\enableslatex\n\\scheme{even?} and \\scheme{odd?} above) produces the same\neffect as the\n\\verb{schemedisplay} version.\n\\slatexdisable{enableslatex}\n\nNote: see \\verb{schemeregion}.\n\n{\\re\n\\verb{schemeregion}}\n\\index{schemeregion@\\verb{schemeregion}}\n\\index{nesting SLaTeX control sequences}\n\n[In plain TeX: \\verb{\\schemeregion} ...\n\\verb{\\endschemeregion}; in LaTeX:\n\\verb|\\begin{schemeregion}| ...\n\\verb|\\end{schemeregion}|; but see  \\verb{defslatexenvstyle}.]\n\nCalls to \\verb|\\scheme|, \\verb|\\schemeresult|,\n\\verb{schemedisplay}, \\verb{schemebox} or\n\\verb|schemeinput| can be nested in (a Scheme comment)\nof other calls.  In LaTeX text, they can occur in\nbodies of environments or otherwise grouped.  However,\nthey cannot normally be passed as arguments to macros\nor included in bodies of macro definitions, even though\nthese are complete calls and not parameterized with\nrespect to macro arguments.  To be able to do this, you\nshould cordon off such a text with the\n\\verb{schemeregion} environment.  SLaTeX is fairly\ngenerous about where exactly you throw the cordon.\n\nE.g., you cannot have\n\n\\begin{verbatim}\n...\nThe code fragment\n$\\underline{\\hbox{\\scheme{(call/cc I)}}}$ is ...\n...\n\\end{verbatim}\nbut you _can_ have\n\n\\begin{verbatim}\n\\begin{schemeregion}\n...\nThe code fragment\n$\\underline{\\hbox{\\scheme{(call/cc I)}}}$ is ...\n...\n\\end{schemeregion}\n\\end{verbatim}\nand this will produce\n\n\\enableslatex\n\\begin{schemeregion}\n...\n\nThe code fragment\n$\\underline{\\hbox{\\scheme{(call/cc I)}}}$ is ...\n\n...\n\\end{schemeregion}\n\\slatexdisable{enableslatex}\n\nThus, the \\verb{schemeregion} environment makes it\npossible to put SLaTeX-specific commands inside macro\narguments or macro definitions without causing rupture.\nNormally, this can't be done since SLaTeX-specific\ncommands correspond to \\verb{comment}-like regions of\nLaTeX code once SLaTeX is done preprocessing your text.\nThese \\verb{comment} regions share the characteristic of\nLaTeX's \\verb{verbatim} regions, which also can't appear\nin macro arguments or definitions.\n\nTo solve this, you enclose the offending text in a\n\\verb{schemeregion} environment.  This \"inlines\" all\nthe calls to SLaTeX in its body instead of commenting\nthem and then invoking \\verb|\\input|, thus escaping\nthe fate described above.  They are no-ops as far as\nnon-SLaTeX commands are concerned.  However, while a\n\\verb{schemeregion} allows its constituent SLaTeX\ncommands to be included in macro arguments and bodies,\nit itself cannot be so included.  Thus, your\n\\verb{schemeregion} should be in a position that\nsatisfies the property A: either directly at the\n\"top-level\" or in a LaTeX environment that satisfies A.\nSince this recursive rule might look weird, you may\njust stick to calling \\verb{schemeregion} at the\n\"top-level\".  Or, you may even wrap each of your LaTeX\nfiles in one huge \\verb{schemeregion} if you so wish.\nThis will cover any obscure \"non-robust\" use of the\nSLaTeX primitives --- however, SLaTeX will run slower.\n(The term \"robust\" is not necessarily used in the same\nsense as in LaTeX.)\n\nNote that SLaTeX commands are made robust only if they\nare surrounded textually (lexically) by a\n\\verb{schemeregion}.  A region marker doesn't have\ndynamic scope in the sense that LaTeX files loaded\nusing \\verb|\\input| from within a\n\\verb{schemeregion} will not inherit it.  In summary, a\n\\verb{schemeregion} makes \"robust\" all calls to\n\\verb|\\scheme|, \\verb{schemedisplay}, \\verb{schemebox}\nand\n\\verb|\\schemeinput| within it.\n\n{\\re\n\\verb{\\setkeyword}\n\\verb{\\setconstant}\n\\verb{\\setvariable}}\n\\index{setkeyword@\\verb{\\setkeyword}}\n\\index{setconstant@\\verb{\\setconstant}}\n\\index{setvariable@\\verb{\\setvariable}}\n\\index{SLaTeX database!modifying}\n\nSLaTeX has a database containing information about\nwhich code tokens are to be treated as {\\bf keywords},\nwhich as {\\sf constants}, and which as _variables_.\nHowever, there will always be instances where the user\nwants to add their own tokens to these categories, or\nperhaps even modify the categories as prescribed by\nSLaTeX.  The control sequences that enable the user to\ndo these are\n\\verb|\\setkeyword|, \\verb|\\setconstant|, and\n\\verb|\\setvariable|.  Their arguments are entered as\na (space-separated) list enclosed in braces\n(\\verb|{}|): SLaTeX learns that these are henceforth\nto be typeset in the appropriate font.  E.g.,\n\n\\enableslatex\n\\begin{verbatim}\n\\setconstant{infinity -infinity}\n\\end{verbatim}\ntells SLaTeX that \\scheme{infinity} and\n\\scheme{-infinity} are to be typeset as constants.\n\\slatexdisable{enableslatex}\n\n\\index{recognizing new syntactic keywords automatically}\n\nThe user need not use \\verb|\\setkeyword| specify such\nnew keywords as are introduced by Scheme's and Common\nLisp's syntactic definition facilities, viz.,\n\\enableslatex\n\\scheme{define-syntax}/\\scheme{syntax-rules},\n\\scheme{defmacro}, \\scheme{extend-syntax},\n\\scheme{define-macro!}: SLaTeX automatically recognizes\nnew macros defined using these facilities.\n\\slatexdisable{enableslatex}\n\n{\\re\n\\verb{\\setspecialsymbol}\n\\verb{\\unsetspecialsymbol}}\n\\index{setspecialsymbol@\\verb{\\setspecialsymbol}}\n\\index{unsetspecialsymbol@\\verb{\\unsetspecialsymbol}}\n\\index{SLaTeX database!modifying}\n\\index{recognizing special symbols}\n\nThese commands are useful to generate\n\"mathematical\"-looking typeset versions of your code,\nover and beyond the fonting capabilities provided by\ndefault.  For instance, although your code is\nrestricted to using ascii identifiers that follow some\nconvention, the corresponding typeset code could be\nmore mnemonic and utilize the full suite of\nmathematical and other symbols provided by TeX.  This\nof course should not require you to interfere with your\ncode itself, which should run in its ascii\nrepresentation.  It is only the typeset version that\nhas the new look.  For instance, you might want all\noccurrences of \\verb|lambda|, \\verb|and|,\n\\verb|equiv?|,\n\\verb|below?|, \\verb|above?|, \\verb|a1| and \\verb|a2| in\nyour code to be typeset as $\\lambda$, $\\land$, $\\equiv$,\n$\\sqsubseteq$, $\\sqsupseteq$, $a_1$ and $a_2$ respectively.\nTo do this, you should \\verb|\\setspecialsymbol| the\nconcerned identifier to the desired TeX expansion, viz.,\n\n\\enableslatex\n\\begin{verbatim}\n\\setspecialsymbol{lambda}{$\\lambda$}\n\\setspecialsymbol{and}{$\\land$}\n\\setspecialsymbol{equiv?}{$\\equiv$}\n\\setspecialsymbol{below?}{$\\sqsubseteq$}\n\\setspecialsymbol{above?}{$\\sqsupseteq$}\n\\setspecialsymbol{a1}{$a_1$}\n\\setspecialsymbol{a2}{$a_2$}\n\\end{verbatim}\n\\slatexdisable{enableslatex}\nNow, typing\n\n\\begin{verbatim}\n\\begin{schemedisplay}\n(define equiv?\n  (lambda (a1 a2)\n    (and (below? a1 a2) (above? a1 a2))))\n\\end{schemedisplay}\n\\end{verbatim}\nproduces\n\n\\enableslatex\n\\begin{schemedisplay}\n(define equiv?\n  (lambda (a1 a2)\n    (and (below? a1 a2) (above? a1 a2))))\n\\end{schemedisplay}\n\\slatexdisable{enableslatex}\nNote that with the above settings, \\verb|lambda| and\n\\verb|and| have lost their default keyword status, i.e.,\nthey will not be typed {\\bf boldface}.  To retrieve the\noriginal status of special symbols, you should use\n\\verb|\\unsetspecialsymbol|, e.g.\n\n\\enableslatex\n\\begin{verbatim}\n\\unsetspecialsymbol{lambda and}\n\\end{verbatim}\nTyping the same program after unsetting the special symbols\nas above produces, as expected:\n\n\\begin{schemedisplay}\n(define equiv?\n  (lambda (a1 a2)\n    (and (below? a1 a2) (above? a1 a2))))\n\\end{schemedisplay}\n\\slatexdisable{enableslatex}\n\nIn effect, \\verb|\\setspecialsymbol| extends the\nbasic \"fonting\" capability to arbitrary special\ntypeset versions.\n\n{\\re\n\\verb{\\schemecasesensitive}}\n\\index{schemecasesensitive@\\verb{\\schemecasesensitive}}\n\\index{case sensitivity}\n\nSLaTeX always typesets output that is of the same case\nas your input, regardless of the setting of the\n\\verb|\\schemecasesensitive| command.  However, this command\ncan be used to signal to SLaTeX that all case variations of\nan identifier are to be treated identically.  E.g. typing\n\\verb|\\schemecasesensitive{false}| implies that while\n\\verb|lambda| continues to be a keyword, so also are\n\\verb|Lambda|, \\verb|LAMBDA| and \\verb|LaMbDa|.\n\\verb|\\schemecasesensitive{true}| reverts it back to\nthe default mode where case is significant in\ndetermining the class of a token.\n\nNote that the status \\verb|\\schemecasesensitive| also\naffects the \"special symbols\" of the previous item.\nThus, in the default case-_sensitive_ setting, only the\ncase-significant symbol as mentioned in the call to\n\\verb|\\setspecialsymbol| will be replaced by the\ncorresponding LaTeX expansion.  In a case-_in_sensitive\nsetting, all case variations of the special symbol will\nbe replaced.\n\n{\\re\n\\verb{\\abovecodeskip}\n\\verb{\\belowcodeskip}\n\\verb{\\leftcodeskip}\n\\verb{\\rightcodeskip}}\n\\index{abovecodeskip@\\verb{\\abovecodeskip}}\n\\index{belowcodeskip@\\verb{\\belowcodeskip}}\n\\index{leftcodeskip@\\verb{\\leftcodeskip}}\n\\index{rightcodeskip@\\verb{\\rightcodeskip}}\n\\index{schemedisplay@\\verb{schemedisplay}!adjusting display parameters}\n\nThese are the parameters used by \\verb{schemedisplay} for\npositioning the displayed code.  The default values are\n\n\\begin{verbatim}\n\\abovecodeskip \\medskipamount\n\\belowcodeskip \\medskipamount\n\\leftcodeskip 0pt\n\\rightcodeskip 0pt\n\\end{verbatim}\nThis produces a flushleft display.  The defaults can be\nchanged to get new display styles.  E.g., the\nassignment\n\n\\begin{verbatim}\n\\leftcodeskip5em\n\\end{verbatim}\nshifts the display from the left by a constant 5 ems.\n\n\\index{schemedisplay@\\verb{schemedisplay}!allowing page\nbreaks in}\n\\index{schemedisplay@\\verb{schemedisplay}!disallowing\npage breaks in}\n\nIn both the above cases, the \\verb{schemedisplay}\nenvironment will be broken naturally across page\nboundaries at the right spot if the code is too long to\nfit a single page.  In fact, automatic pagebreaks\nwithin the Scheme code are allowed if and only if\n\\verb{\\rightcodeskip} is 0pt (its default value).  For\nall other values of \\verb{\\rightcodeskip}, each Scheme\ncode block in a \\verb{schemedisplay} is guaranteed to\nbe on the same page.  If you like your current left\nindentation, and you're not sure of what value to give\n\\verb{\\rightcodeskip}, but nevertheless don't want\nScheme code broken across pages, you could set\n\n\\begin{verbatim}\n\\rightcodeskip=0.01pt %or\n\\rightcodeskip=0pt plus 1fil\n\\end{verbatim}\n\nThe following explains why the above disable page\nbreaks within the Scheme block.  For example, suppose\nyou'd set\n\n\\begin{verbatim}\n\\leftcodeskip=0pt plus 1fil\n\\rightcodeskip=0pt plus 1fil\n\\end{verbatim}\nThis will get you a _centered_ display style.  This is\nof course because the skip on each side of the code\nproduces a spring~\\cite{tex} that pushes the code to\nthe center.  But for this spring action to work nicely,\nthe code must have been collected into an unbreakable\nbox --- which is precisely  what\n\\verb{schemedisplay} does for each of its code blocks\nwhenever it notices that the prevailing value of\n\\verb{\\rightcodeskip} is not the default zero.\n\\footnote{0pt plus 1fil $\\ne$ 0pt}\n\nIt is this behind-the-scenes selective boxing that\ndictates whether a \\verb{schemedisplay} block can or\ncannot be broken across a page boundary.  And the\nvalue of \\verb{\\rightcodeskip} is used to govern this\nselection in a \"reasonable\" manner.\n\n{\\re\n\\verb{\\keywordfont}\n\\verb{\\constantfont}\n\\verb{\\variablefont}}\n\\index{keywordfont@\\verb{\\keywordfont}}\n\\index{constantfont@\\verb{\\constantfont}}\n\\index{variablefont@\\verb{\\variablefont}}\n\\index{specifying SLaTeX's fonts}\n\nThese decide the typefaces used for keywords, constants,\nand variables.  The default definitions are:\n\n\\begin{verbatim}\n\\def\\keywordfont#1{{\\bf#1}}\n\\def\\constantfont#1{{\\sf#1}}\n\\def\\variablefont#1{{\\it#1\\/}}\n\\end{verbatim}\n\nThis is close to the Little Lisper~\\cite{ll} style.\nRedefine these control sequences for font changes.  As\nan extreme case, defining all of them to\n\\verb|{{\\tt#1}}| typesets everything in monospace\ntypewriter font, as, for instance, in SICP~\\cite{sicp}.\n\n{\\re\n\\verb{\\defschemedisplaytoken}\n\\verb{\\defschemetoken}\n\\verb{\\defschemeresulttoken}\n\\verb{\\defschemeinputtoken}\n\\verb{\\defschemeregiontoken}}\n\\index{defschemedisplaytoken@\\verb{\\defschemedisplaytoken}}\n\\index{defschemetoken@\\verb{\\defschemetoken}}\n\\index{defschemeresulttoken@\\verb{\\defschemeresulttoken}}\n\\index{defschemeboxtoken@\\verb{\\defschemeboxtoken}}\n\\index{defschemeinputtoken@\\verb{\\defschemeinputtoken}}\n\\index{defining SLaTeX control sequences}\n\nThese define the tokens used by SLaTeX to trigger\ntypesetting of in-text code, display code, box code,\nand Scheme files.  The default tokens are, as already\ndescribed, \\verb{schemedisplay}, \\verb|\\scheme|,\n\\verb|\\schemeresult|, \\verb{schemebox},\n\\verb|\\schemeinput| and \\verb{schemeregion}\nrespectively.  If you want shorter or more mnemonic\ntokens, the \\verb|\\defscheme*token| control sequences\nprove useful.  E.g., if you want \\verb|\\code| to be\nyour new control sequence for in-text code, use\n\\verb|\\defschemetoken{code}|.  All instances of\n\\verb|\\code+...+| after this definition produce\nin-text code, unless overridden by an\n\\verb|\\undefschemetoken| command.\n\nOne can have at any time any number of tokens for the\nsame activity.  One consequence of this is that one can\nhave nested \\verb{schemeregion}s, provided one has\ndifferent names for the nested call.  Otherwise, the\n\\verb|\\end| of an inner region will prematurely\nterminate an outer region.\n\n{\\re\n\\verb{\\undefschemedisplaytoken}\n\\verb{\\undefschemetoken}\n\\verb{\\undefschemeresulttoken}\n\\verb{\\undefschemeinputtoken}\n\\verb{\\undefschemeregiontoken}}\n\\index{undefschemedisplaytoken@\\verb{\\undefschemedisplaytoken}}\n\\index{undefschemetoken@\\verb{\\undefschemetoken}}\n\\index{undefschemeresulttoken@\\verb{\\undefschemeresulttoken}}\n\\index{undefschemeboxtoken@\\verb{\\undefschemeboxtoken}}\n\\index{undefschemeinputtoken@\\verb{\\undefschemeinputtoken}}\n\\index{undefschemeregiontoken@\\verb{\\undefschemeregiontoken}}\n\\index{undefining SLaTeX control sequences}\n\nThese _un_define the tokens used for triggering\ntypesetting in-text code, display code, box code,\nScheme files, and robust Scheme regions.  Use these if\nyou want to use these tokens for other purposes and do\nnot want to unwittingly trip up the SLaTeX system.\n\n{\\re\n\\verb{\\defschememathescape}\n\\verb{\\undefschememathescape}}\n\\index{defschememathescape@\\verb{\\defschememathescape}}\n\\index{undefschememathescape@\\verb{\\undefschememathescape}}\n\\index{TeX mathmode in SLaTeX}\n\\index{escape character for mathmode within Scheme}\n\n\\verb|\\defschememathescape{$}| defines the character\n\\verb|$| as a mathematical escape character to be used\nwithin scheme code.  (Any character other than\n\\verb|}| and whitespace may be chosen instead of\n\\verb|$|.)  This allows one to use LaTeX-like\nmathematical subformulas within Scheme code, e.g.,\n\n\\begin{verbatim}\n\\defschememathescape{$}\n\n\\begin{schemedisplay}\n(define $\\equiv$\n  (lambda (a$_1$ a$_2$)\n    ($\\land$ ($\\sqsubseteq$ a$_1$ a$_2$)\n\t     ($\\sqsupseteq$ a$_1$ a$_2$))))\n\\end{schemedisplay}\n\\end{verbatim}\nproduces\n\n\\enableslatex\n\\defschememathescape{$}\n\n\\begin{schemedisplay}\n(define $\\equiv$\n  (lambda (a$_1$ a$_2$)\n    ($\\land$ ($\\sqsubseteq$ a$_1$ a$_2$)\n\t     ($\\sqsupseteq$ a$_1$ a$_2$))))\n\\end{schemedisplay}\n\\undefschememathescape{$}\n\\slatexdisable{enableslatex}\n\\verb|\\undefschememathescape{$}| disables the\nmath-escape nature, if any, of \\verb|$|.\n\n{\\re\n\\verb{\\slatexdisable}}\n\\index{slatexdisable@\\verb{\\slatexdisable}}\n\\index{disabling SLaTeX}\n\nThe tokens for typesetting code, as also the token\n\\verb|\\input| (which is sensitive to SLaTeX, since\nthe latter uses it to recursively process files within\nfiles), can only be used as calls.  If they occur in\nthe bodies of macro definitions, or their names are\nused for defining other control sequences, SLaTeX will\nnot be able to process them.  Sometimes, one wants to\nuse these tokens, say \\verb|\\input|, without having\nSLaTeX try to process the inputted file.  Or the name\n\\verb|\\scheme| may be used in a verbatim environment,\nand we don't want such an occurrence to trigger the\ncodesetting half of SLaTeX to look for code.\n\nAvoiding such uses altogether can be unduly\nrestrictive.\n\\footnote{Especially when one is writing a \"How to ...\"\nmanual like this where one both uses _and_ mentions the\ncontrol sequences!} One way out is to judiciously use\nthe \\verb|\\undefscheme*token| commands to temporarily\nremove the SLaTeX-specificity of these names.  Even\nthis can be painful.  SLaTeX therefore provides the\ncommands \\verb|\\slatexdisable|.  This takes one\nargument word and makes the corresponding control\nsequence out of it.  Further, from this point in the\ntext, SLaTeX is disabled _until_ the manufactured\ncontrol sequence shows up.  This mechanism makes it\npossible to restrict SLaTeX to only appropriate\nportions of the text.  Note that the token\n\\verb|\\slatexdisable| itself can appear in the text\nsucceeding its call.  The only token that can restore\nSLaTeX-sensitivity is the one created during the call\nto \\verb|\\slatexdisable|.\n\nA typical example of the use of \\verb|\\slatexdisable|\nis when you use the names \\verb|\\scheme| and\n\\verb|\\begin{schemedisplay}| in a \\verb{verbatim}\nenvironment.  E.g.,\n\n{\\medskip\n\\obeylines\\parindent0pt\n\\verb|\\slatexdisable{slatexenable}|\n\\verb|\\begin{verbatim}|\n\\verb|slatex provides the command \\scheme and the pair|\n\\verb|\\begin{schemedisplay} and \\end{schemedisplay} to typeset|\n\\verb|in-text and displayed Scheme code respectively.|\n\\verb|\\end{verbatim}|\n\\verb|\\slatexenable|\n\\medskip}\n\nproduces the required\n\n\\begin{verbatim}\nslatex provides the command \\scheme and the pair\n\\begin{schemedisplay} and \\end{schemedisplay} to typeset\nin-text and display Scheme code respectively.\n\\end{verbatim}\n\n{\\re\n\\verb{\\slatexignorecurrentfile}}\n\\index{slatexignorecurrentfile@\\verb{\\slatexignorecurrentfile}}\n\\index{disabling SLaTeX}\n\nThis is a SLaTeX pragma included to improve efficiency.\nIf you're sure that the remaining portion of a certain\nLaTeX (or TeX) file (including the files that would be\n\\verb|\\input|ed by it) don't contain any SLaTeX\ncommands, then you may place this control sequence in\nit at this point to signal SLaTeX that no preprocessing\nis necessary for the rest of the file.\n\n{\\re\n\\verb{\\defslatexenvstyle}}\n\\index{defslatexenvstyle@\\verb{\\defslatexenvstyle}}\n\\index{plain TeX}\n\\index{LaTeX}\n\\index{environments}\n\nAs section~\\ref{quick} showed, the differences in SLaTeX\nusage between plain TeX and LaTeX is simply a matter of\nthe difference in the \"environment\" styles of the two\nformats.  It is easy get the behavior of the one\nformat with the other.\n\n\\begin{enumerate}\n\\o  If you wish to use the plain-TeX style in LaTeX,\ntype\n\\begin{verbatim}\n\\defslatexenvstyle{tex}\n\\end{verbatim}\nbefore first such use.\n\n\\o  Similarly, if you wish to use the LaTeX\n\\verb{\\begin}/\\verb{\\end} style in plain TeX, use\n\\begin{verbatim}\n\\defslatexenvstyle{latex}\n\\end{verbatim}\n_provided you have already defined \\verb{\\begin} and\n\\verb{\\end} appropriately!_\n\nBefore doing this, you should keep in mind that\nTeX already has an\n\\verb{\\end} command --- which is used by TeX's\n\\verb{\\bye} --- that ends the document. This function\nshould be saved under a different name, before\n\\verb{\\end} can be redefined as an environment closer.\nThe following is one way to accomplish this:\n\\begin{verbatim}\n\\let\\plaintexend\\end\n\\outer\\def\\bye{\\par\\vfill\\supereject\\plaintexend}\n\\def\\begin#1{\\csname#1\\endcsname}\n\\def\\end#1{\\csname end#1\\endcsname}\n\\end{verbatim}\n\\end{enumerate}\n\nIn either case, you can revert to the default style with\n\\verb|\\defslatexenvstyle{latex}| and\n\\verb|\\defslatexenvstyle{tex}| respectively.\n\n{\\re\n\\verb{\\slatexseparateincludes}}\n\\index{slatexseparateincludes@\\verb{slatexseparateincludes}}\n\\index{reusing SLaTeX's temporary files}\n\nBy default, the temporary files of SLaTeX use the name\nof the topmost TeX file, i.e., the name stored under\n\\verb{\\jobname}.  In large LaTeX documents using\n\\verb{\\include}, this may be unduly restrictive.\n\nTo recapitulate, the \\verb{slatex} command creates\ntemporary files to store typeset code and then passes\nthe baton on to TeX or LaTeX.  If no significant change\nhas been made to the Scheme code (either in content or\nin relative positioning) in the document, then\nsuccessive calls to (La)TeX could be made directly\nusing the old temporary files.  This could be a time-saver,\nsince it avoids calling up the Scheme typesetter.\n\nHowever, in a large LaTeX document with\n\\verb{\\include}s, these successive calls to LaTeX often\nentail juggling the \\verb{\\include}s that are chosen.\nIn this case, even though the relative position of the\nScheme code is preserved within each \\verb{include}d\nfile, the sequence perceived by the main file changes.\nThis spoils the invariance we needed if we'd wanted to\navoid calling SLaTeX unnecessarily.\n\n\\index{reusing SLaTeX's temporary files!exploiting\nLaTeX's \\verb{\\include}}\n\nTo solve this, the SLaTeX command sequence\n\\verb{\\slatexseparateincludes} --- which must be called\nbefore the first occurrence of Scheme code in your\ndocument ---\nguarantees that each\n\\verb{\\include}d file will generate its own pool of\ntemp files.  Thus, if the SLaTeX\nfiles are created once for each \\verb{\\include}, they\nwill be correctly loaded no matter what sequence of\n\\verb{\\include}s is taken.\n\n{\\re\n\\verb{\\schemecodehook}}\n\\index{schemecodehook@\\verb{\\schemecodehook}}\n\\index{hook for \\verb{schemedisplay} and\n\\verb{schemebox}}\n\nThe user can define \\verb{\\schemecodehook} to be\nanything.  The hook will be evaluated inside each\nsubsequent call to \\verb{schemedisplay} and\n\\verb{schemebox}.  E.g.,\n\n\\begin{verbatim}\n\\let\\schemecodehook\\tiny\n\\end{verbatim}\nconverts your Scheme displays and boxes into {\\tiny\nsmall print}.\n\nThe default value of the hook is \\verb{\\relax}, a\nno-op.\n\n\\section{Setting up a file that resets SLaTeX's\ndefaults}\n\\label{preamble}\n\\index{writing personal preamble}\n\\index{SLaTeX database!modifying}\n\nA sample style modification file for SLaTeX would\ninclude redefinition of the names of the codesetting\ncontrol sequences, adjustment of the display\nparameters, modification of the font assignments for\nkeywords/constants/variables/special symbols, and\naddition of new keywords/constants/variables/special\nsymbols to SLaTeX's database.\n\nLet's assume you want\n\n\\begin{itemize}\n\\o a centered display style with no vertical skips;\n\n\\o the names \\verb|\\code|, \\verb{schemefrag}, \\verb{scmbox},\n\\verb|\\sinput| instead of \\verb|\\scheme|,\n\\verb{schemefrag}, \\verb{schemebox} and\n\\verb|\\schemeinput|;\n\n\\o tokens to disregard case;\n\n\\o the keywords to come out it \\verb{typewriter}, the\nconstants in roman, and the variables in {\\sl slant};\n\n\\o \"\\verb{und}\" and \"\\verb{oder}\" as keywords,\n\"\\verb{true}\" and \"\\verb{false}\" as constants,\n\"\\verb{define}\" as a variable (overriding default as\nkeyword!), \"\\verb{F}\" as a constant (\\verb{f} will also\nbe a constant, due to case-insensitivity!);\n\n\\o \"\\verb{top}\" and \"\\verb{bottom}\" to print as\n$\\top$ and $\\bot$ respectively.\n\\end{itemize}\n\nThis could be set up as\n\n\\begin{verbatim}\n\\abovecodeskip 0pt\n\\belowcodeskip 0pt\n\\leftcodeskip 0pt plus 1fil\n\\rightcodeskip 0pt plus 1fil\n\n\\undefschemetoken{scheme}\n\\undefschemeboxtoken{schemebox}\n\\undefschemedisplaytoken{schemedisplay}\n\\undefschemeinputtoken{schemeinput}\n\n\\defschemetoken{code}\n\\defschemeboxtoken{scmbox}\n\\defschemedisplaytoken{schemegrag}\n\\defschemeinputtoken{sinput}\n\n\\schemecasesensitive{false}\n\n\\def\\keywordfont#1{{\\tt#1}}\n\\def\\constantfont#1{{\\rm#1}}\n\\def\\variablefont#1{{\\sl#1\\/}}\n\n\\setkeyword{und oder}\n\\setconstant{true false}\n\\setvariable{define}\n\\setconstant{F}\n\n\\setspecialsymbol{top}{$\\top$}\n\\setspecialsymbol{bottom}{$\\bottom$}\n\\end{verbatim}\n\nThis file can then be \\verb|\\input| in the preamble of\nyour LaTeX document.\n\n\\section{How to obtain and install SLaTeX}\n\\label{ftp}\n\\index{obtaining and installing SLaTeX}\n\n\\enableslatex\n\\leftcodeskip=0pt plus 1fil\n\\rightcodeskip=0pt plus 1fil\n\\slatexdisable{enableslatex}\n\nSLaTeX is available via anonymous ftp from\n\\verb{cs.rice.edu} (or \\verb{titan.cs.rice.edu}).\nLogin as\n\\verb{anonymous}, give your userid as password, change\nto the directory \\verb{public/dorai}, convert to\n\\verb{bin} mode, and get the file\n\\verb{slatex}_NN_\\verb{.tar.gz}, where _NN_ is some\nnumber.  Un\\verb{gzip}ping and un\\verb{tar}ring\nproduces a directory \\verb{slatex}, containing the\nSLaTeX files.  (The file \\verb{manifest} lists the\nfiles in the distribution --- make sure that nothing is\nmissing.)\n\nTo install SLaTeX on your system:\n\n\\begin{enumerate}\n\\o First change directory (\\verb{cd}) to \\verb{slatex}, the\ndirectory housing the SLaTeX files.\n\\footnote{Some of the SLaTeX files use DOS-style CR-LF\nnewlines.  You may want to use an appropriate newline\nmodifier to the SLaTeX files to make the files comply\nwith your operating system's newline format.}\n\n\\o Edit the file \\verb{config.dat} as suggested by the\ncomments in the file itself.\n\n\\o Invoke your Scheme or Common Lisp interpreter.\nLoad the file \\verb{config.scm}, i.e., type\n\n\\enableslatex\n\\begin{schemedisplay}\n(load \"config.scm\")\n\\end{schemedisplay}\n\\slatexdisable{enableslatex}\nat the Scheme (or Common Lisp) prompt.  This will\nconfigure SLaTeX for your Scheme dialect and operating\nsystem, creating a Scheme file called\n\\verb{slatex.scm}.  (If you informed \\verb{config.dat}\nthat your Scheme dialect is Chez, the file\n\\verb{slatex.scm} is a compiled version rather than\nScheme source.)  The configuration process also creates\na batch file \\verb{slatex.bat} (on DOS) or a shell\nscript \\verb{slatex} (on Unix), for convenient\ninvocation of SLaTeX from your operating system command\nline.  A Scheme/Common Lisp file \\verb{callsla.scm} is\nalso created --- this lets you call SLaTeX from the\nScheme/Common Lisp prompt.\n\n\\o Exit Scheme/Common Lisp.\n\\end{enumerate}\n\nTo set up paths and modify shell script/batch file:\n\n\\begin{enumerate}\n\\o Copy (or move, or link) \\verb{slatex.scm} into a\nsuitable place, e.g., your \\verb{bin} or \\verb{lib}\ndirectory, or the system \\verb{bin} or \\verb{lib}.\n\n\\o Copy (or move, or link) \\verb{slatex.sty} into a\nsuitable place, i.e., somewhere in your \\verb{TEXINPUTS}\npath.  For installing on a multiuser system, place in\nthe directory containing the LaTeX files (on mine this\nis \\verb{/usr/local/lib/tex/macros}).\n\n\n\\o \\enableslatex\nCopy (or move, or link) the shell script\n\\verb{slatex} or the batch file \\verb{slatex.bat} to a\nsuitable place in your \\verb{PATH}, e.g., your {bin} or\nthe system {bin} directory.  Note that\n\\verb{slatex}(\\verb{.bat}) sets\n\\scheme{SLaTeX.*texinputs*}.  If you're making the same\nshell script (or batch file) available to multiple\nusers, you should change the line\n\\begin{schemedisplay}\n(set! SLaTeX.*texinputs* \"...\")\n\\end{schemedisplay}\nto\n\\begin{schemedisplay}\n(set! SLaTeX.*texinputs* (getenv \"TEXINPUTS\"))\n\\end{schemedisplay}\nor some other dialect-dependent way of obtaining the\n\\verb{TEXINPUTS} environment variable.\n\\slatexdisable{enableslatex}\n\n\\o Run \\verb{slatex} on \\verb{slatex-d.tex} (this\nfile!) for documentation.  (This also serves as a check\nthat SLaTeX does indeed work on your machine.)  Refer\nto \\verb{slatex-d.dvi} when befuddled.\n\\end{enumerate}\n\nIf your dialect did not allow a nice enough shell\nscript or batch file, the following provides an\nalternate route to unlocking SLaTeX.\n\n\\subsection{Other ways of invoking SLaTeX}\n\nThe configuration process creates shell script/batch\nfile \\verb{slatex}(\\verb{.bat}) for a standard invoking\nmechanism for SLaTeX.  The shell script/batch file is\ncreated to exploit the way your Scheme is called, e.g.,\nmatters like whether it accepts \\verb{echo}'d\ns-expressions (e.g., Chez) , whether it loads command\nline files (e.g., SCM) , and whether it always checks\nfor an \"init\" file (e.g., MIT C Scheme).\n\n\\begin{enumerate}\n\\o  If your Scheme doesn't fall into either of these\ncategories, you may have to write your own\nshell script/batch file or devise some other mechanism.\n\n\\o  The shell script/batch file invokes\nScheme/Common Lisp.  If,\nhowever, you are already in Scheme/Common Lisp and\nspend most of the time continuously at the\nScheme/Common Lisp prompt rather than the operating\nsystem prompt, you may avoid some of the delays\ninherent in the shell script/batch file.\n\\end{enumerate}\n\n\\enableslatex\nThe file \\verb{callsla.scm}, which contains just one\nsmall procedure named \\scheme{call-slatex}, and which\nis created by the configuration process, provides a\nsimple calling mechanism from Scheme/Common Lisp, as\nopposed to the operating system command line.  You may\nuse it as an alternative to the\n\\verb{slatex}(\\verb{.bat}) shell script/batch file.\nThe usage is as follows: load\n\\verb{callsla.scm} into Scheme/Common Lisp\n\n\\begin{schemedisplay}\n(load \"callsla.scm\")\n\\end{schemedisplay}\nand type\n\n\\setspecialsymbol{<tex-file>}{\\va{$\\langle$tex-file$\\rangle$}}\n\\begin{schemedisplay}\n(call-slatex <tex-file>)\n\\end{schemedisplay}\nwhen you need to call SLaTeX on the (La)TeX file\n\\scheme{<tex-file>}.  This invokes the SLaTeX preprocessor on\n\\scheme{<tex-file>}.  If your Scheme has a\n\\scheme{system} procedure\nthat can call the operating system command line,\n\\scheme{call-slatex} will also send your file to TeX or\nLaTeX. If your Scheme does not have such a procedure,\n\\scheme{call-slatex} will simply prod you to call TeX\nor LaTeX\nyourself.\n\\slatexdisable{enableslatex}\n\nThe outline of the shell script/batch file or\n\\verb{callsla.scm} or of any strategy you devise for\nusing SLaTeX should include the following actions:\n\n\\begin{enumerate}\n\\o Load the file \\verb{slatex.scm} (created by the\nconfiguration process) into Scheme/Common Lisp.\n\n\\o \\enableslatex\nSet the variable \\scheme{SLaTeX.*texinputs*} to the\npath \\verb{TEXINPUTS} or \\verb{TEXINPUT} used by\nTeX\n\\footnote{There is some variation on the name of\nthis environment variable.  Unix TeX's prefer\n\\verb{TEXINPUTS} with an \\verb{S}, while DOS (e.g.,\nEberhard Mattes's emTeX) favors \\verb{TEXINPUT} without\nthe \\verb{S}.}\nto look for\n\\slatexdisable{enableslatex}\n\\verb|\\input|\nfiles.\n\n\n\\o \\enableslatex\nCall the procedure\n\\scheme{SLaTeX.process-main-tex-file} on the \\verb{.tex}\nfile to be processed.\n\\slatexdisable{enableslatex}\n\n\\o Call either \\verb{latex} or \\verb{tex} on the \\verb{.tex} file.\n\\end{enumerate}\n\n\n\\enableslatex\nYou may devise your own way of calling\n\\scheme{SLaTeX.process-main-tex-file}, provided your\nmethod makes sure that \\verb{slatex.scm} has been\nloaded, \\scheme{SLaTeX.*texinputs*} set appropriately\n_before_ the call and \\verb{latex}/\\verb{tex} is called\n_after_ the call.\n\nNote that if you prefer to stay in Scheme/Common Lisp\nmost of the time, it is a good idea to pre-load the\nprocedure \\scheme{call-slatex}, perhaps through an\n\"init\" file.  \\scheme{call-slatex} is just a\n\"one-liner\" \"call-by-need\" hook to SLaTeX and does not\ntake up much resources.  (Global name clashes between\nyour own code and SLaTeX code won't occur unless you\nuse variable names starting with \"\\scheme{SLaTeX.}\") If\nyou made no calls to \\scheme{call-slatex}, the bigger\nfile \\verb{slatex.scm} is not loaded at all.  If you\nmake several calls to \\scheme{call-slatex},\n\\verb{slatex.scm} is loaded only once, at the time of\nthe first call.\n\\slatexdisable{enableslatex}\n\n\\subsection{Dialects SLaTeX runs on}\n\\index{dialects SLaTeX runs on}\n\n\\enableslatex\nSLaTeX is implemented in R4RS-compliant Scheme (macros\nare not needed).  The code uses the non-standard\nprocedures \\scheme{delete-file},\n\\scheme{file-exists?} and \\scheme{force-output}, but\na Scheme without these procedures can also run SLaTeX\n(the configuration defines the corresponding variables\nto be dummy procedures, since they are not crucial).\nThe distribution comes with code to allow SLaTeX to run\nalso on Common Lisp.  The files \\verb{readme} and\n\\verb{install} contain all the information necessary to\nconfigure SLaTeX for your system.\n\\slatexdisable{enableslatex}\n\nSLaTeX has been tested successfully in the following\ndialects:\n\n\\begin{itemize}\n\\o _On Unix:_\nChez Scheme (R. Kent Dybvig), Ibuki Common\nLisp (1987), MIT C Scheme, Elk (Oliver Laumann),\nScheme-to-C (Joel Bartlett), Scm (Aubrey Jaffer) and\nUMB Scheme (William Campbell);\n\n\\o _On MS-DOS:_\nMIT C Scheme, Scm (Aubrey Jaffer), Austin Kyoto Common\nLisp (William Schelter's enhanced version of Taiichi\nYuasa and Masami Hagiya's KCL) and CLisp (Bruno Haible\nand Michael Stoll).\n\\iffalse PCScheme/Geneva (Larry Bartholdi and\nMarc Vuilleumier) \\fi\n\\end{itemize}\n\nIf your Scheme is not mentioned here but _is_\nR4RS-compliant, please send a note to the author at\n\\verb{dorai@cs.rice.edu} describing your Scheme's\nprocedures for deleting files, testing file existence,\nand forcing output, if any, and the configuration file\nwill be enhanced to accommodate the new dialect.\n\nBug reports are most welcome --- send to\n\\verb{dorai@cs.rice.edu}.\n\\index{bug reports}\n\n\\begin{thebibliography}{9}\n\\bibitem{sicp} H. Abelson and G.J.  Sussman with J.\nSussman.  Structure and Interpretation of Computer\nPrograms.  MIT Press, 1985.\n\n\\bibitem{r4rs} W. Clinger and J. Rees, eds.\nRevised$^4$ Report on the Algorithmic Language Scheme.\n1991.\n\n\\bibitem{ll} D.P. Friedman and M.  Felleisen.  The\nLittle Lisper.  Science Research Associates, 1989.\n\n\\bibitem{tex} D.E. Knuth.  The TeXbook.\nAddison-Wesley, 1984.\n\n\\bibitem{latex} L. Lamport.  LaTeX User's Guide and\nReference Manual.  Addison-Wesley, 1986.\n\n\\bibitem{schemeweb} J. Ramsdell. SchemeWeb.  Scheme\nRepository, nexus.yorku.ca, maintained by O. Yigit.\n\n\\bibitem{lisp2tex} C. Queinnec. LiSP2TeX.  Scheme\nRepository.\n\n\\bibitem{cltl2} G.L. Steele Jr. Common Lisp: The\nLanguage, 2nd ed. Digital Press, 1990.\n\\end{thebibliography}\n\n%input slatex-d.ind, the index, if available.\n%slatex-d.ind is generated by running\n%       makeind(e)x slatex-d\n%after running latex on slatex-d.  The next call\n%       latex slatex-d\n%will include slatex-d.ind\n\n\\inputifpossible{slatex-d.ind}\n\n\\end{document}\n\n\\index{schemedisplay@\\verb{schemedisplay}!with plain TeX}\n\\index{schemebox@\\verb{schemebox}!with plain TeX}\n\\index{schemeregion@\\verb{schemeregion}!with plain TeX}\n")
("slatex.sty" . "% slatex.sty\n% SLaTeX v. 2.2\n% style file to be used in (La)TeX when using SLaTeX\n% (c) Dorai Sitaram, Rice U., 1991, 1994\n\n% This file (or a soft link to it) should be in some\n% directory in your TEXINPUTS path (i.e., the one\n% (La)TeX scours for \\input or \\documentstyle option\n% files).\n\n% Do not attempt to debug this file, since the results\n% are not transparent just to (La)TeX.  The Scheme part\n% of SLaTeX depends on information laid out here -- so\n% (La)TeX-minded debugging of this file will almost\n% inevitably sabotage SLaTeX.\n\n% It's possible you don't find the default style set\n% out here appealing: e.g., you may want to change the\n% positioning of displayed code; change the fonts for\n% keywords, constants, and variables; add new keywords,\n% constants, and variables; use your names instead of\n% the provided \\scheme, [\\begin|\\end]{schemedisplay},\n% [\\begin|\\end]{schemebox}, (or \\[end]schemedisplay,\n% \\[end]schemebox for TeX), which might be seem too\n% long or unmnemonic, and many other things.  The clean\n% way to do these things is outlined in the\n% accompanying manual, slatex-d.tex.  This way is both\n% easier than messing with this .sty file, and safer\n% since you will not unwittingly break SLaTeX.\n\n%%%\n\n% to prevent loading slatex.sty more than once\n\n\\ifx\\slatexignorecurrentfile\\UNDEFINED\n\\else\\endinput\\fi\n\n% use \\slatexignorecurrentfile to disable slatex for\n% the current file.  (Unstrangely, the very definition\n% disables slatex for the rest of _this_ file, slatex.sty.)\n\n\\def\\slatexignorecurrentfile{}\n\n% checking whether we're using LaTeX or TeX?\n\n\\newif\\ifusinglatex\n\\ifx\\newenvironment\\UNDEFINED\\usinglatexfalse\\else\\usinglatextrue\\fi\n\n% make @ a letter for TeX\n\\ifusinglatex\\relax\\else\n\\edef\\atcatcodebeforeslatex{\\the\\catcode`@}\n\\catcode`@11\n\\fi\n\n% identification of TeX/LaTeX style for schemedisplay.\n% Do \\defslatexenvstyle{tex} to get TeX environment\n% style in LaTeX\n\\def\\defslatexenvstyle#1{\\gdef\\slatexenvstyle{#1}}\n\n\\ifusinglatex\\defslatexenvstyle{latex}\\else\\defslatexenvstyle{tex}\\fi\n\n% TeX doesn't have sans-serif; use roman instead\n\\ifx\\sf\\UNDEFINED\\def\\sf{\\rm}\\fi\n\n% tabbing from plain TeX\n%\n\\newif\\ifus@ \\newif\\if@cr\n\\newbox\\tabs \\newbox\\tabsyet \\newbox\\tabsdone\n%\n\\def\\cleartabs{\\global\\setbox\\tabsyet\\null \\setbox\\tabs\\null}\n\\def\\settabs{\\setbox\\tabs\\null \\futurelet\\next\\sett@b}\n\\let\\+=\\relax % in case this file is being read in twice\n\\def\\sett@b{\\ifx\\next\\+\\let\\next\\relax\n    \\def\\next{\\afterassignment\\s@tt@b\\let\\next}%\n\\else\\let\\next\\s@tcols\\fi\\next}\n\\def\\s@tt@b{\\let\\next\\relax\\us@false\\m@ketabbox}\n\\def\\tabalign{\\us@true\\m@ketabbox} % non-\\outer version of \\+\n\\outer\\def\\+{\\tabalign}\n\\def\\s@tcols#1\\columns{\\count@#1 \\dimen@\\hsize\n  \\loop\\ifnum\\count@>\\z@ \\@nother \\repeat}\n\\def\\@nother{\\dimen@ii\\dimen@ \\divide\\dimen@ii\\count@\n  \\setbox\\tabs\\hbox{\\hbox to\\dimen@ii{}\\unhbox\\tabs}%\n  \\advance\\dimen@-\\dimen@ii \\advance\\count@\\m@ne}\n%\n\\def\\m@ketabbox{\\begingroup\n  \\global\\setbox\\tabsyet\\copy\\tabs\n  \\global\\setbox\\tabsdone\\null\n  \\def\\cr{\\@crtrue\\crcr\\egroup\\egroup\n    \\ifus@\\unvbox\\z@\\lastbox\\fi\\endgroup\n    \\setbox\\tabs\\hbox{\\unhbox\\tabsyet\\unhbox\\tabsdone}}%\n  \\setbox\\z@\\vbox\\bgroup\\@crfalse\n    \\ialign\\bgroup&\\t@bbox##\\t@bb@x\\crcr}\n%\n\\def\\t@bbox{\\setbox\\z@\\hbox\\bgroup}\n\\def\\t@bb@x{\\if@cr\\egroup % now \\box\\z@ holds the column\n  \\else\\hss\\egroup \\global\\setbox\\tabsyet\\hbox{\\unhbox\\tabsyet\n      \\global\\setbox\\@ne\\lastbox}% now \\box\\@ne holds its size\n    \\ifvoid\\@ne\\global\\setbox\\@ne\\hbox to\\wd\\z@{}%\n    \\else\\setbox\\z@\\hbox to\\wd\\@ne{\\unhbox\\z@}\\fi\n    \\global\\setbox\\tabsdone\\hbox{\\box\\@ne\\unhbox\\tabsdone}\\fi\n  \\box\\z@}\n% finished (re)defining TeX's tabbing macros\n\n% above from plain.tex; was disabled in lplain.tex.  Do\n% not modify above unless you really know what you're\n% up to.  Make all changes you want to following code.\n% The new env is preferable to LaTeX's tabbing env\n% since latter accepts only a small number of tabs\n\n% following retrieves something like LaTeX's tabbing\n% env without the above problem (it also creates a box\n% for easy manipulation!)\n\n\\def\\lat@xtabbing{\\leavevmode\\hbox\\bgroup\\vbox\\bgroup\n \\def\\={\\cleartabs&} \\def\\>{&} \\def\\\\{\\cr\\tabalign} \\tabalign}\n\\def\\endlat@xtabbing{\\cr\\egroup\\egroup}\n\n%new\n\n\\def\\lat@xtabbing{\\begingroup\n\\def\\={\\cleartabs&} \\def\\>{&}%\n\\def\\\\{\\cr\\tabalign\\lat@xtabbingleftmost}%\n\\tabalign\\lat@xtabbingleftmost}\n\\def\\endlat@xtabbing{\\cr\\endgroup}\n\\let\\lat@xtabbingleftmost\\relax\n\n% stuff for formating Scheme code\n\n\\newskip\\par@nlen \\newskip\\brack@tlen \\newskip\\quot@len\n\\newskip\\h@lflambda\n\n\\newbox\\garb@ge\n\\def\\s@ttowidth#1#2{\\setbox\\garb@ge\\hbox{#2}#1\\wd\\garb@ge\\relax}\n\n\\s@ttowidth\\par@nlen{$($}       % size of paren\n\\s@ttowidth\\brack@tlen{$[$}     % size of bracket\n\\s@ttowidth\\quot@len{'}         % size of quote indentation\n\\s@ttowidth\\h@lflambda{ii}      % size of half of lambda indentation\n\n\\def\\PRN{\\hskip\\par@nlen}       % these are used by SLaTeX's codesetter\n\\def\\BKT{\\hskip\\brack@tlen}\n\\def\\QUO{\\hskip\\quot@len}\n\\def\\HL{\\hskip\\h@lflambda}\n\n\\newskip\\abovecodeskip \\newskip\\belowcodeskip\n\\newskip\\leftcodeskip \\newskip\\rightcodeskip\n\n% the following default assignments give a flushleft\n% display\n\n\\abovecodeskip=\\medskipamount \\belowcodeskip=\\medskipamount\n\\leftcodeskip=0pt \\rightcodeskip=0pt\n\n% adjust above,below,left,right codeskip's to personal\n% taste\n\n% for centered displays\n%\n% \\leftcodeskip=0pt plus 1fil\n% \\rightcodeskip=0pt plus 1fil\n%\n% if \\rightcodeskip != 0pt, pagebreaks within Scheme\n% blocks in {schemedisplay} are disabled\n\n\\def\\checkfollpar{\\futurelet\\next\\checkfollparII}\n\\def\\checkfollparII{\\ifx\\next\\par\\let\\next\\relax\n\\else\\par\\noindent\\let\\next\\ignorespaces\\fi\\next}\n\n% the following are the default font assignments for\n% words in code.  Change them to suit personal taste\n\n\\def\\keywordfont#1{{\\bf #1}}\n\\def\\variablefont#1{{\\it #1\\/}}\n\\def\\constantfont#1{{\\sf #1}}\n\\def\\datafont#1{\\constantfont{#1}}\n\n\\def\\schemecodehook{}\n\n%program listings that allow page breaks but\n%can't be centered\n\n\\def\\ZZZZschemedisplay{\\edef\\thez@skip{\\the\\z@skip}%\n\\edef\\@tempa{\\the\\rightcodeskip}%\n\\ifx\\@tempa\\thez@skip\\let\\next\\ZZZZschemeprogram\n\\else\\let\\next\\ZZZZschemeprogramII\\fi\\next}\n\n\\def\\endZZZZschemedisplay{\\edef\\thez@skip{\\the\\z@skip}%\n\\edef\\@tempa{\\the\\rightcodeskip}%\n\\ifx\\@tempa\\thez@skip\\let\\next\\endZZZZschemeprogram\n\\else\\let\\next\\endZZZZschemeprogramII\\fi\\next}\n\n\\def\\ZZZZschemeprogram{\\vskip\\abovecodeskip\n\\begingroup\n\\schemecodehook\n\\let\\sy=\\keywordfont \\let\\cn=\\constantfont\n\\let\\va=\\variablefont \\let\\dt=\\datafont\n\\def\\lat@xtabbingleftmost{\\hskip\\leftcodeskip\\relax}%\n\\lat@xtabbing}\n\n\\def\\endZZZZschemeprogram{\\endlat@xtabbing\n\\endgroup\n\\vskip\\belowcodeskip\n\\ifusinglatex\\let\\next\\@endparenv\n\\else\\let\\next\\checkfollpar\\fi\\next}\n\n\\def\\ZZZZschemeprogramII{\\vskip\\abovecodeskip\n\\begingroup\n\\noindent\n%\\schemecodehook %\\ZZZZschemebox already has it\n\\hskip\\leftcodeskip\n\\ZZZZschemebox}\n\n\\def\\endZZZZschemeprogramII{\\endZZZZschemebox\n\\hskip\\rightcodeskip\n\\endgroup\n\\vskip\\belowcodeskip\n\\ifusinglatex\\let\\next\\@endparenv\n\\else\\let\\next\\checkfollpar\\fi\\next}\n\n%\n\n\\def\\ZZZZschemebox{%\n\\leavevmode\\hbox\\bgroup\\vbox\\bgroup\n\\schemecodehook\n\\let\\sy=\\keywordfont \\let\\cn=\\constantfont\n\\let\\va=\\variablefont \\let\\dt=\\datafont\n\\lat@xtabbing}\n\\def\\endZZZZschemebox{\\endlat@xtabbing\n\\egroup\\egroup\\ignorespaces}\n\n%in-text\n\n\\def\\ZZZZschemecodeintext{\\begingroup\n  \\let\\sy\\keywordfont \\let\\cn\\constantfont\n  \\let\\va\\variablefont \\let\\dt\\datafont}\n\n\\def\\endZZZZschemecodeintext{\\endgroup\\ignorespaces}\n\n\\def\\ZZZZschemeresultintext{\\begingroup\n  \\let\\sy\\datafont \\let\\cn\\constantfont\n  \\let\\va\\datafont \\let\\dt\\datafont}\n\n\\def\\endZZZZschemeresultintext{\\endgroup\\ignorespaces}\n\n% \\comm@nt<some-char>...text...<same-char> comments out\n% TeX source analogous to\n% \\verb<some-char>...text...<same-char>.  Sp. case:\n% \\comm@nt{...text...} == \\comm@nt}...text...}\n\n\\def\\@makeother#1{\\catcode`#112\\relax}\n\n\\def\\comm@nt{%\n  \\begingroup\n  \\let\\do\\@makeother \\dospecials\n  \\@comm}\n\n\\begingroup\\catcode`\\<1\\catcode`\\>2\n\\catcode`\\{12\\catcode`\\}12\n\\long\\gdef\\@comm#1<%\n  \\if#1{\\long\\def\\@tempa ##1}<\\endgroup>\\else\n\t\\long\\def\\@tempa ##1#1<\\endgroup>\\fi\n  \\@tempa>\n\\endgroup\n\n% input file if possible, else relax\n\n\\def\\inputifpossible#1{%\n  \\immediate\\openin0=#1\\relax%\n  \\ifeof0\\relax\\else\\input#1\\relax\\fi%\n  \\immediate\\closein0}\n\n\\def\\ZZZZinput#1{\\input#1\\relax}\n\n% you may replace the above by\n%\n% \\def\\ZZZZinput#1{\\inputifpossible{#1}}\n%\n% if you just want to call (La)TeX on your text\n% ignoring the portions that need to be SLaTeX'ed\n\n%use \\subjobname rather than \\jobname to generate\n%slatex's temp files --- this allows us to change\n%\\subjobname for more control, if necessary.\n\n\\let\\subjobname\\jobname\n\n% counter for generating temp file names\n\n\\newcount\\sch@mefilenamecount\n\\sch@mefilenamecount=-1\n\n% To produce displayed Scheme code:\n% in LaTeX:\n% \\begin{schemedisplay}\n%   ... indented program (with sev'l lines) ...\n% \\end{schemedisplay}\n%\n% in TeX:\n% \\schemedisplay\n%   ... indented program (with sev'l lines) ...\n% \\endschemedisplay\n\n\\begingroup\\catcode`\\|=0\\catcode`\\[=1\\catcode`\\]=2%\n\\catcode`\\{=12\\catcode`\\}=12\\catcode`\\\\=12%\n|gdef|defschemedisplaytoken#1[%\n  |long|expandafter|gdef|csname ZZZZcomment#1|endcsname[%\n    |begingroup\n    |let|do|@makeother |dospecials\n    |csname ZZZZcomment|slatexenvstyle II#1|endcsname]%\n  |long|expandafter|gdef|csname ZZZZcommentlatexII#1|endcsname##1\\end{#1}[%\n    |endgroup|end[#1]]%\n  |long|expandafter|gdef|csname ZZZZcommenttexII#1|endcsname##1\\end#1[%\n    |endgroup|csname end#1|endcsname]%\n  |long|expandafter|gdef|csname #1|endcsname[%\n    |global|advance|sch@mefilenamecount by 1|relax%\n    |ZZZZinput[|filehider Z|number|sch@mefilenamecount|subjobname.tex]%\n    |csname ZZZZcomment#1|endcsname]%\n  |long|expandafter|gdef|csname end#1|endcsname[]]%\n|endgroup\n\n\\defschemedisplaytoken{schemedisplay}\n\n\\def\\undefschemedisplaytoken#1{%\n  \\expandafter\\gdef\\csname#1\\endcsname{\\UNDEFINED}}\n\n% \\scheme|...program fragment...| produces Scheme code\n% in-text.  Sp. case: \\scheme{...} == \\scheme}...}\n\n\\def\\defschemetoken#1{%\n  \\long\\expandafter\\def\\csname#1\\endcsname{%\n    \\global\\advance\\sch@mefilenamecount by 1\\relax%\n    \\ZZZZinput{\\filehider Z\\number\\sch@mefilenamecount\\subjobname.tex}%\n    \\comm@nt}}\n\\defschemetoken{scheme}\n\n\\def\\undefschemetoken#1{%\n  \\expandafter\\gdef\\csname#1\\endcsname{\\UNDEFINED}}\n\n% \\schemeresult|...program fragment...| produces a\n% Scheme code result in-text: i.e. keyword or variable\n% fonts are replaced by the data font.  Sp. case:\n% \\schemeresult{...} == \\schemeresult}...}\n\n\\def\\defschemeresulttoken#1{%\n  \\long\\expandafter\\def\\csname#1\\endcsname{%\n    \\global\\advance\\sch@mefilenamecount by 1\\relax%\n    \\ZZZZinput{\\filehider Z\\number\\sch@mefilenamecount\\subjobname.tex}%\n    \\comm@nt}}\n\\defschemeresulttoken{schemeresult}\n\n\\def\\undefschemeresulttoken#1{%\n  \\expandafter\\gdef\\csname#1\\endcsname{\\UNDEFINED}}\n\n% To produce a box of Scheme code:\n% in LaTeX:\n% \\begin{schemebox}\n%   ... indented program (with sev'l lines) ...\n%  \\end{schemebox}\n%\n% in TeX:\n% \\schemebox\n%   ... indented program (with sev'l lines) ...\n% \\endschemebox\n\n\\begingroup\\catcode`\\|=0\\catcode`\\[=1\\catcode`\\]=2%\n\\catcode`\\{=12\\catcode`\\}=12\\catcode`\\\\=12%\n|gdef|defschemeboxtoken#1[%\n  |long|expandafter|gdef|csname ZZZZcomment#1|endcsname[%\n    |begingroup\n    |let|do|@makeother |dospecials\n    |csname ZZZZcomment|slatexenvstyle II#1|endcsname]%\n  |long|expandafter|gdef|csname ZZZZcommentlatexII#1|endcsname##1\\end{#1}[%\n    |endgroup|end[#1]]%\n  |long|expandafter|gdef|csname ZZZZcommenttexII#1|endcsname##1\\end#1[%\n    |endgroup|csname end#1|endcsname]%\n  |long|expandafter|gdef|csname #1|endcsname[%\n    |global|advance|sch@mefilenamecount by 1|relax%\n    |ZZZZinput[|filehider Z|number|sch@mefilenamecount|subjobname.tex]%\n    |csname ZZZZcomment#1|endcsname]%\n  |long|expandafter|gdef|csname end#1|endcsname[]]%\n|endgroup\n\n\\defschemeboxtoken{schemebox}\n\n\\def\\undefschemeboxtoken#1{%\n  \\expandafter\\gdef\\csname#1\\endcsname{\\UNDEFINED}}\n\n% for wholesale dumping of all-Scheme files into TeX (converting\n% .scm files to .tex),\n% use\n%   \\schemeinput{<filename>}\n% .scm, .ss, .s extensions optional\n\n\\def\\defschemeinputtoken#1{%\n  \\long\\expandafter\\gdef\\csname#1\\endcsname##1{%\n    \\global\\advance\\sch@mefilenamecount by 1\\relax%\n    \\ZZZZinput{\\filehider Z\\number\\sch@mefilenamecount\\subjobname.tex}}}\n\\defschemeinputtoken{schemeinput}\n\n\\def\\undefschemeinputtoken#1{%\n  \\expandafter\\gdef\\csname#1\\endcsname{\\UNDEFINED}}\n\n% delineating a region that features typeset code\n% not usually needed, except when using \\scheme and schemedisplay\n% inside macro-args and macro-definition-bodies\n% in LaTeX:\n% \\begin{schemeregion}\n%   ...\n% \\end{schemeregion}\n%\n% in TeX:\n% \\schemeregion\n% ...\n% \\endschemeregion\n\n\\begingroup\\catcode`\\|=0\\catcode`\\[=1\\catcode`\\]=2%\n\\catcode`\\{=12\\catcode`\\}=12\\catcode`\\\\=12%\n|gdef|defschemeregiontoken#1[%\n  |long|expandafter|gdef|csname ZZZZcomment#1|endcsname[%\n    |begingroup\n    |let|do|@makeother |dospecials\n    |csname ZZZZcomment|slatexenvstyle II#1|endcsname]%\n  |long|expandafter|gdef|csname ZZZZcommentlatexII#1|endcsname##1\\end{#1}[%\n    |endgroup|end[#1]]%\n  |long|expandafter|gdef|csname ZZZZcommenttexII#1|endcsname##1\\end#1[%\n    |endgroup|csname end#1|endcsname]%\n  |long|expandafter|gdef|csname #1|endcsname[%\n    |global|advance|sch@mefilenamecount by 1|relax%\n    |ZZZZinput[|filehider Z|number|sch@mefilenamecount|subjobname.tex]%\n    |csname ZZZZcomment#1|endcsname]%\n  |long|expandafter|gdef|csname end#1|endcsname[]]%\n|endgroup\n\n\\defschemeregiontoken{schemeregion}\n\n\\def\\undefschemeregiontoken#1{%\n  \\expandafter\\gdef\\csname#1\\endcsname{\\UNDEFINED}}\n\n% introducing new code-tokens to the keyword, variable and constant\n% categories\n\n\\def\\comm@ntII{%\n  \\begingroup\n  \\let\\do\\@makeother \\dospecials\n  \\@commII}\n\n\\begingroup\\catcode`\\[1\\catcode`\\]2\n\\catcode`\\{12\\catcode`\\}12\n\\long\\gdef\\@commII{[%\n  \\long\\def\\@tempa ##1}[\\endgroup]\\@tempa]%\n\\endgroup\n\n\\let\\setkeyword\\comm@ntII\n\\let\\setvariable\\comm@ntII\n\\let\\setconstant\\comm@ntII\n\n% \\defschememathescape makes the succeeding grouped character an\n% escape into latex math from within Scheme code;\n% this character can't be }\n\n\\let\\defschememathescape\\comm@ntII\n\\let\\undefschememathescape\\comm@ntII\n\n% telling SLaTeX that a certain Scheme identifier is to\n% be replaced by the specified LaTeX expression.\n% Useful for generating ``mathematical''-looking\n% typeset code even though the corresponding Scheme\n% code is ascii as usual and doesn't violate\n% identifier-naming rules\n\n\\def\\setspecialsymbol{%\n  \\begingroup\n  \\let\\do\\@makeother \\dospecials\n  \\@commIII}\n\n\\begingroup\\catcode`\\[1\\catcode`\\]2\n\\catcode`\\{12\\catcode`\\}12\n\\long\\gdef\\@commIII{[%\n  \\long\\def\\@tempa ##1}[\\endgroup\\@gobbleI]\\@tempa]%\n\\endgroup\n\n\\def\\@gobbleI#1{}\n\n% \\unsetspecialsymbol strips Scheme identifier(s) of\n% any ``mathematical'' look lent by the above\n\n\\let\\unsetspecialsymbol\\comm@ntII\n\n% enabling/disabling slatex\n\n\\def\\slatexdisable#1{\\expandafter\\gdef\\csname#1\\endcsname{}}\n\n% \\schemecasesensitive takes either true or false as\n% argument\n\n\\def\\schemecasesensitive#1{}\n\n%for latex only: use \\slatexseparateincludes before the\n%occurrence of any Scheme code in your file, if you\n%want the various \\include'd files to have their own\n%pool of temporary slatex files.  This lets you juggle\n%your \\include's in successive runs of LaTeX without\n%having to worry that the temp. files may interfere.\n%By default, only a single pool of temp files is used.\n%Warning: On DOS, if your \\include'd files have fairly\n%similar names, avoid \\slatexseparateincludes since the\n%short filenames on DOS will likely confuse the temp\n%file pools of different \\include files.\n\n\\def\\slatexseparateincludes{%\n\\gdef\\include##1{{\\def\\subjobname{##1}%\n\\sch@mefilenamecount=-1%\n\\@include##1 }}}\n\n% convenient abbreviations for characters\n\n\\begingroup\n\\catcode`\\|=0\n|catcode`|\\=12\n|gdef|ttbackslash{{|tt|catcode`|\\=12\\}}\n|endgroup\n\\mathchardef\\lt=\"313C\n\\mathchardef\\gt=\"313E\n\\begingroup\n  \\catcode`\\@12%\n  \\global\\let\\atsign@%\n\\endgroup\n\\chardef\\dq=`\\\"\n\n% leading character of slatex filenames: . for unix to\n% keep them out of the way\n\n\\def\\filehider{.}\n\n% since the above doesn't work of dos, slatex on dos\n% will use a different character, and make the\n% redefinition available through the following\n\n\\inputifpossible{xZfilhid.tex}\n\n% @ is no longer a letter for TeX\n\n\\ifusinglatex\\relax\\else\n\\catcode`@\\atcatcodebeforeslatex\n\\fi\n\n\\message{*** Check: Are you sure you called SLaTeX? ***}\n")
))

(define filesystem init-filesystem)

(define (Zcall-with-input-file path proc)
  (let loop ((lst filesystem))
    (if (pair? lst)
        (let ((x (Scar lst)))
          (if (Zstring=? (Scar x) path)
              (proc (cons (Scdr x) 0))
              (loop (Scdr lst))))
        #f))) ;; "file not found"

(define-macro (Zpeek-char port)
  `(let ((port ,port))
;;     (declare (standard-bindings) (not safe) (fixnum))
     (let ((buf (Scar port))
           (pos (Scdr port)))
       (if (SFX< pos (Sstring-length buf))
           (let ((c (Sstring-ref buf pos)))
;;             (Sset-cdr! port (SFX+ 1 pos))
             c)
           #f))))

(define-macro (Zread-char port)
  `(let ((port ,port))
;;     (declare (standard-bindings) (not safe) (fixnum))
     (let ((buf (Scar port))
           (pos (Scdr port)))
       (if (SFX< pos (Sstring-length buf))
           (let ((c (Sstring-ref buf pos)))
             (Sset-cdr! port (SFX+ 1 pos))
             c)
           #f))))

(define-macro (Zeof-object? obj)
  `(not (char? ,obj)))

(define (Zcall-with-output-file/truncate path proc)
  (let* ((port (list '()))
         (result (proc port))
         (content (string-concat (slatex.reverse! (Scar port)))))
    (set! filesystem (cons (cons path content) filesystem))
    result))

(define (string-concat string-and-char-list)

  (define (concat string-and-char-list i)
    (if (pair? string-and-char-list)
        (let ((x (Scar string-and-char-list)))
          (if (char? x)
              (let ((result (concat (Scdr string-and-char-list) (SFX+ i 1))))
                (Sstring-set! result i x)
                result)
              (let* ((n (Sstring-length x))
                     (result (concat (Scdr string-and-char-list) (SFX+ i n))))
                (let loop ((j (SFX- (Sstring-length x) 1)))
                  (if (SFX>= j 0)
                      (begin
                        (Sstring-set! result (SFX+ i j) (Sstring-ref x j))
                        (loop (SFX- j 1)))
                      result)))))
        (Smake-string1 i)))

  (concat string-and-char-list 0))

(define (Zstring-append . strings)
  (string-concat strings))

(define-macro (Zdisplay obj port)
  `(let ((obj ,obj) (port ,port))
     (Sset-car! port (cons obj (Scar port)))))

(define-macro (Znewline port)
  `(Zdisplay "\n" ,port))

(define-macro (Zchar-alphabetic? c)
  `(let ((c ,c))
     (if (Schar>=? c #\a)
         (Schar<=? c #\z)
         (and (Schar>=? c #\A) (Schar<=? c #\Z)))))

(define-macro (Zchar-whitespace? c)
  `(Schar<=? ,c #\space))

(define-macro (Zmemv key lst)
  `(let ((key ,key) (lst ,lst))
     (let loop ((lst lst))
       (if (pair? lst)
           (if (eqv? key (Scar lst))
               lst
               (loop (Scdr lst)))
           #f))))

(define-macro (Zmemq key lst)
  `(let ((key ,key) (lst ,lst))
     (let loop ((lst lst))
       (if (pair? lst)
           (if (eq? key (Scar lst))
               lst
               (loop (Scdr lst)))
           #f))))

(define-maro (Zappend lst1 lst2)
  `(let ((lst1 ,lst1) (lst2 ,lst2))
     (let loop ((lst1 lst1))
       (if (pair? lst1)
           (cons (Scar lst1) (loop (Scdr lst1)))
           lst2))))

(define-macro (Zlist-tail lst i)
  `(let ((lst ,lst) (i ,i))
     (let loop ((lst lst) (i i))
       (if (SFX<= i 0)
           lst
           (loop (Scdr lst) (SFX- i 1))))))

(define (Zstring=? str1 str2)
  (let ((len (Sstring-length str1)))
    (and (SFX= len (Sstring-length str2))
         (let loop ((i (SFX- len 1)))
           (if (SFX< i 0)
               #t
               (and (Schar=? (Sstring-ref str1 i)
                            (Sstring-ref str2 i))
                    (loop (SFX- i 1))))))))

(define (Zstring-ci=? str1 str2)

  (define (char-downcase c)
    (if (Schar<=? c #\Z)
        (if (Schar>=? c #\A)
            (Sinteger->char (SFX+ (Schar->integer c) 32))
            c)
        c))

  (let ((len (Sstring-length str1)))
    (and (SFX= len (Sstring-length str2))
         (let loop ((i (SFX- len 1)))
           (if (SFX< i 0)
               #t
               (and (Schar=? (char-downcase (Sstring-ref str1 i))
                            (char-downcase (Sstring-ref str2 i)))
                    (loop (SFX- i 1))))))))

(define (Zlist->string char-list)

  (define (concat char-list i)
    (if (pair? char-list)
        (let ((x (Scar char-list)))
          (let ((result (concat (Scdr char-list) (SFX+ i 1))))
            (Sstring-set! result i x)
            result))
        (Smake-string1 i)))

  (concat char-list 0))

(define (Zstring->list str)
  (let loop ((i (SFX- (Sstring-length str) 1)) (result '()))
    (if (SFX>= i 0)
        (loop (SFX- i 1) (cons (Sstring-ref str i) result))
        result)))

;;;----------------------------------------------------------------------------

;;; SLATEX -- Scheme to Latex processor.

;slatex.scm file generated using config.scm
;This file is compatible for the dialect other
;(c) Dorai Sitaram, Rice U., 1991, 1994

(define *op-sys* 'unix)

(define-macro (slatex.ormap f l)
  `(let ((f ,f) (l ,l))
     (let loop ((l l)) (and (pair? l) (or (f (Scar l)) (loop (Scdr l)))))))

(define-macro (slatex.ormapcdr f l)
  `(let ((f ,f) (l ,l))
     (let loop ((l l)) (and (pair? l) (or (f l) (loop (Scdr l)))))))

(define slatex.append!
  (lambda (l1 l2)
    (if (pair? l1)
        (let loop ((curr l1))
          (let ((next (Scdr curr)))
            (if (pair? next)
                (loop next)
                (begin
                  (Sset-cdr! curr l2)
                  l1))))
        l2)))

(define slatex.append-map!
  (lambda (f l)
    (let loop ((l l))
      (if (pair? l) (slatex.append! (f (Scar l)) (loop (Scdr l))) '()))))

(define slatex.remove-if!
  (lambda (p s)
    (let loop ((s s))
      (if (pair? s)
          (if (p (Scar s))
              (loop (Scdr s))
              (begin
                (Sset-cdr! s (loop (Scdr s)))
                s))
          '()))))

(define slatex.reverse!
  (lambda (s)
    (let loop ((s s) (r '()))
      (if (pair? s) (let ((d (Scdr s))) (Sset-cdr! s r) (loop d s)) r))))

;;(define slatex.list-set!
;;  (lambda (l i v)
;;    (let loop ((l l) (i i))
;;      (cond ((null? l) (slatex.error 'slatex.list-set! 'list-too-small))
;;            ((SFX= i 0) (Sset-car! l v))
;;            (else (loop (Scdr l) (SFX- i 1)))))))

(define slatex.list-prefix?
  (lambda (pfx l)
    (cond ((null? pfx) #t)
          ((null? l) #f)
          ((eqv? (Scar pfx) (Scar l)) (slatex.list-prefix? (Scdr pfx) (Scdr l)))
          (else #f))))

(define slatex.string-prefix?
  (lambda (pfx s)
    (let ((pfx-len (Sstring-length pfx)) (s-len (Sstring-length s)))
      (if (SFX> pfx-len s-len)
        #f
        (let loop ((i 0))
          (if (SFX>= i pfx-len)
            #t
            (and (Schar=? (Sstring-ref pfx i) (Sstring-ref s i))
                 (loop (SFX+ i 1)))))))))

(define slatex.string-suffix?
  (lambda (sfx s)
    (let ((sfx-len (Sstring-length sfx)) (s-len (Sstring-length s)))
      (if (SFX> sfx-len s-len)
        #f
        (let loop ((i (SFX- sfx-len 1)) (j (SFX- s-len 1)))
          (if (SFX< i 0)
            #t
            (and (Schar=? (Sstring-ref sfx i) (Sstring-ref s j))
                 (loop (SFX- i 1) (SFX- j 1)))))))))

(define slatex.member-string
  (lambda (key lst)
    (let loop ((lst lst))
      (if (pair? lst)
          (if (Zstring=? key (Scar lst))
              lst
              (loop (Scdr lst)))
          #f))))

(define slatex.adjoin-string
  (lambda (s l) (if (slatex.member-string s l) l (cons s l))))

(define slatex.remove-string!
  (lambda (s l) (slatex.remove-if! (lambda (l_i) (Zstring=? l_i s)) l)))

(define slatex.adjoin-char (lambda (c l) (if (Zmemv c l) l (cons c l))))

(define slatex.remove-char!
  (lambda (c l) (slatex.remove-if! (lambda (l_i) (Schar=? l_i c)) l)))

(define slatex.sublist
  (lambda (l i f)
    (let loop ((l (Zlist-tail l i)) (k i) (r '()))
      (cond ((SFX>= k f) (slatex.reverse! r))
            ((null? l) (slatex.error 'slatex.sublist 'list-too-small))
            (else (loop (Scdr l) (SFX+ k 1) (cons (Scar l) r)))))))

(define slatex.position-char
  (lambda (c l)
    (let loop ((l l) (i 0))
      (cond ((null? l) #f)
            ((Schar=? (Scar l) c) i)
            (else (loop (Scdr l) (SFX+ i 1)))))))

(define slatex.string-position-right
  (lambda (c s)
    (let ((n (Sstring-length s)))
      (let loop ((i (SFX- n 1)))
        (cond ((SFX< i 0) #f)
              ((Schar=? (Sstring-ref s i) c) i)
              (else (loop (SFX- i 1))))))))

(define slatex.token=?
  (lambda (t1 t2)
    ((if slatex.*slatex-case-sensitive?* Zstring=? Zstring-ci=?) t1 t2)))

(define slatex.assoc-token
  (lambda (x s)
    (slatex.ormap (lambda (s_i) (if (slatex.token=? (Scar s_i) x) s_i #f)) s)))

(define slatex.member-token
  (lambda (x s)
    (slatex.ormapcdr
      (lambda (s_i..) (if (slatex.token=? (Scar s_i..) x) s_i.. #f))
      s)))

(define slatex.remove-token!
  (lambda (x s) (slatex.remove-if! (lambda (s_i) (slatex.token=? s_i x)) s)))

(define slatex.file-exists? (lambda (f) #t))

(define slatex.delete-file (lambda (f) 'assume-file-deleted))

(define slatex.force-output (lambda z 'assume-output-forced))

(define slatex.*return* (Sinteger->char 13))

(define slatex.*tab* (Sinteger->char 9))

(define slatex.error
  (lambda (error-type error-values)
    (display "Error: ")
    (display error-type)
    (display ": ")
    (newline)
    (for-each (lambda (x) (write x) (newline)) error-values)
    (fatal-error "")))

(define slatex.keyword-tokens
  (map (lambda (sym) (Ssymbol->string sym))
       '(=> %
            abort
            and
            begin
            begin0
            case
            case-lambda
            cond
            define
            define!
            define-macro!
            define-syntax
            defrec!
            delay
            do
            else
            extend-syntax
            fluid-let
            if
            lambda
            let
            let*
            letrec
            let-syntax
            letrec-syntax
            or
            quasiquote
            quote
            rec
            record-case
            record-evcase
            recur
            set!
            sigma
            struct
            syntax
            syntax-rules
            trace
            trace-lambda
            trace-let
            trace-recur
            unless
            unquote
            unquote-splicing
            untrace
            when
            with)))

(define slatex.variable-tokens '())

(define slatex.constant-tokens '())

(define slatex.special-symbols
  (list (cons "." ".")
        (cons "..." "{\\dots}")
        (cons "-" "$-$")
        (cons "1-" "\\va{1$-$}")
        (cons "-1+" "\\va{$-$1$+$}")))

(define slatex.macro-definers
  '("define-syntax" "syntax-rules" "defmacro" "extend-syntax" "define-macro!"))

(define slatex.case-and-ilk '("case" "record-case"))

(define slatex.tex-analog
  (lambda (c)
    (cond ((Zmemv c '(#\$ #\& #\% #\# #\_)) (string #\\ c))
          ((Zmemv c '(#\{ #\})) (string #\$ #\\ c #\$))
          ((Schar=? c #\\) "$\\backslash$")
          ((Schar=? c #\+) "$+$")
          ((Schar=? c #\=) "$=$")
          ((Schar=? c #\<) "$\\lt$")
          ((Schar=? c #\>) "$\\gt$")
          ((Schar=? c #\^) "\\^{}")
          ((Schar=? c #\|) "$\\vert$")
          ((Schar=? c #\~) "\\~{}")
          ((Schar=? c #\@) "{\\atsign}")
          ((Schar=? c #\") "{\\tt\\dq}")
          (else (string c)))))

(define slatex.*slatex-case-sensitive?* #f);;TODO: was #t

(define slatex.*slatex-enabled?* #t)

(define slatex.*slatex-reenabler* "UNDEFINED")

(define slatex.*intext-triggerers* (list "scheme"))

(define slatex.*resultintext-triggerers* (list "schemeresult"))

(define slatex.*display-triggerers* (list "schemedisplay"))

(define slatex.*box-triggerers* (list "schemebox"))

(define slatex.*input-triggerers* (list "schemeinput"))

(define slatex.*region-triggerers* (list "schemeregion"))

(define slatex.*math-triggerers* '())

(define slatex.*slatex-in-protected-region?* #f)

(define slatex.*protected-files* '())

(define slatex.*include-onlys* 'all)

(define slatex.*latex?* #t)

(define slatex.*slatex-separate-includes?* #f)

(define slatex.set-keyword
  (lambda (x)
    (if (slatex.member-token x slatex.keyword-tokens)
      'skip
      (begin
        (set! slatex.constant-tokens
          (slatex.remove-token! x slatex.constant-tokens))
        (set! slatex.variable-tokens
          (slatex.remove-token! x slatex.variable-tokens))
        (set! slatex.keyword-tokens (cons x slatex.keyword-tokens))))))

(define slatex.set-constant
  (lambda (x)
    (if (slatex.member-token x slatex.constant-tokens)
      'skip
      (begin
        (set! slatex.keyword-tokens
          (slatex.remove-token! x slatex.keyword-tokens))
        (set! slatex.variable-tokens
          (slatex.remove-token! x slatex.variable-tokens))
        (set! slatex.constant-tokens (cons x slatex.constant-tokens))))))

(define slatex.set-variable
  (lambda (x)
    (if (slatex.member-token x slatex.variable-tokens)
      'skip
      (begin
        (set! slatex.keyword-tokens
          (slatex.remove-token! x slatex.keyword-tokens))
        (set! slatex.constant-tokens
          (slatex.remove-token! x slatex.constant-tokens))
        (set! slatex.variable-tokens (cons x slatex.variable-tokens))))))

(define slatex.set-special-symbol
  (lambda (x transl)
    (let ((c (slatex.assoc-token x slatex.special-symbols)))
      (if c
        (Sset-cdr! c transl)
        (set! slatex.special-symbols
          (cons (cons x transl) slatex.special-symbols))))))

(define slatex.unset-special-symbol
  (lambda (x)
    (set! slatex.special-symbols
      (slatex.remove-if!
        (lambda (c) (slatex.token=? (Scar c) x))
        slatex.special-symbols))))

(define slatex.texify (lambda (s) (Zlist->string (slatex.texify-aux s))))

(define slatex.texify-data
  (lambda (s)
    (let loop ((l (slatex.texify-aux s)) (r '()))
      (if (null? l)
        (Zlist->string (slatex.reverse! r))
        (let ((c (Scar l)))
          (loop (Scdr l)
                (if (Schar=? c #\-)
                  (slatex.append! (list #\$ c #\$) r)
                  (cons c r))))))))

(define slatex.texify-aux
  (let* ((arrow (Zstring->list "-$>$")) (arrow-lh (length arrow)))
    (lambda (s)
      (let* ((sl (Zstring->list s))
             (texified-sl
               (slatex.append-map!
                 (lambda (c) (Zstring->list (slatex.tex-analog c)))
                 sl)))
        (slatex.ormapcdr
          (lambda (d)
            (if (slatex.list-prefix? arrow d)
              (let ((to (Zstring->list "$\\to$")))
                (Sset-car! d (Scar to))
                (Sset-cdr! d (Zappend (Scdr to) (Zlist-tail d arrow-lh)))))
            #f)
          texified-sl)
        texified-sl))))

(define slatex.display-begin-sequence
  (lambda (out)
    (if (or slatex.*intext?* (not slatex.*latex?*))
      (begin
        (Zdisplay "\\" out)
        (Zdisplay slatex.*code-env-spec* out)
        (Znewline out))
      (begin
        (Zdisplay "\\begin{" out)
        (Zdisplay slatex.*code-env-spec* out)
        (Zdisplay "}" out)
        (Znewline out)))))

(define slatex.display-end-sequence
  (lambda (out)
    (if (or slatex.*intext?* (not slatex.*latex?*))
      (begin
        (Zdisplay "\\end" out)
        (Zdisplay slatex.*code-env-spec* out)
        (Znewline out))
      (begin
        (Zdisplay "\\end{" out)
        (Zdisplay slatex.*code-env-spec* out)
        (Zdisplay "}" out)
        (Znewline out)))))

(define slatex.display-tex-char
  (lambda (c p) (Zdisplay (if (char? c) (slatex.tex-analog c) c) p)))

(define slatex.display-token
  (lambda (s typ p)
    (cond ((eq? typ 'syntax)
           (Zdisplay "\\sy{" p)
           (Zdisplay (slatex.texify s) p)
           (Zdisplay "}" p))
          ((eq? typ 'variable)
           (Zdisplay "\\va{" p)
           (Zdisplay (slatex.texify s) p)
           (Zdisplay "}" p))
          ((eq? typ 'constant)
           (Zdisplay "\\cn{" p)
           (Zdisplay (slatex.texify s) p)
           (Zdisplay "}" p))
          ((eq? typ 'data)
           (Zdisplay "\\dt{" p)
           (Zdisplay (slatex.texify-data s) p)
           (Zdisplay "}" p))
          (else (slatex.error 'slatex.display-token typ)))))

(define slatex.*max-line-length* 200)

(begin
  (define slatex.&inner-space (Sinteger->char 7))
  (define slatex.&quote-space (Sinteger->char 6))
  (define slatex.&bracket-space (Sinteger->char 5))
  (define slatex.&paren-space (Sinteger->char 4))
  (define slatex.&init-plain-space (Sinteger->char 3))
  (define slatex.&init-space (Sinteger->char 2))
  (define slatex.&plain-space (Sinteger->char 1))
  (define slatex.&void-space (Sinteger->char 0)))

(begin
  (define slatex.&plain-crg-ret (Sinteger->char 4))
  (define slatex.&tabbed-crg-ret (Sinteger->char 3))
  (define slatex.&move-tab (Sinteger->char 2))
  (define slatex.&set-tab (Sinteger->char 1))
  (define slatex.&void-tab (Sinteger->char 0)))

(begin
  (define slatex.&end-math (Sinteger->char 8))
  (define slatex.&mid-math (Sinteger->char 7))
  (define slatex.&begin-math (Sinteger->char 6))
  (define slatex.&end-string (Sinteger->char 5))
  (define slatex.&mid-string (Sinteger->char 4))
  (define slatex.&begin-string (Sinteger->char 3))
  (define slatex.&mid-comment (Sinteger->char 2))
  (define slatex.&begin-comment (Sinteger->char 1))
  (define slatex.&void-notab (Sinteger->char 0)))

(begin
  (define slatex.make-raw-line (lambda () (Smake-vector1 5)))
  (define slatex.=notab 4)
  (define slatex.=tab 3)
  (define slatex.=space 2)
  (define slatex.=char 1)
  (define slatex.=rtedge 0))

(define slatex.make-line
  (lambda ()
    (let ((l (slatex.make-raw-line)))
      (vector-set! l slatex.=rtedge 0)
      (vector-set!
        l
        slatex.=char
        (Smake-string2 slatex.*max-line-length* #\space))
      (vector-set!
        l
        slatex.=space
        (Smake-string2 slatex.*max-line-length* slatex.&void-space))
      (vector-set!
        l
        slatex.=tab
        (Smake-string2 slatex.*max-line-length* slatex.&void-tab))
      (vector-set!
        l
        slatex.=notab
        (Smake-string2 slatex.*max-line-length* slatex.&void-notab))
      l)))

(define slatex.*line1* (slatex.make-line))

(define slatex.*line2* (slatex.make-line))

(begin
  (define slatex.make-case-frame (lambda () (Smake-vector1 3)))
  (define slatex.=in-case-exp 2)
  (define slatex.=in-bktd-ctag-exp 1)
  (define =in-ctag-tkn 0))

(begin
  (define slatex.make-bq-frame (lambda () (Smake-vector1 3)))
  (define slatex.=in-bktd-bq-exp 2)
  (define slatex.=in-bq-tkn 1)
  (define slatex.=in-comma 0))

(define slatex.*latex-paragraph-mode?* 'fwd1)

(define slatex.*intext?* 'fwd2)

(define slatex.*code-env-spec* "UNDEFINED")

(define slatex.*in* 'fwd3)

(define slatex.*out* 'fwd4)

(define slatex.*in-qtd-tkn* 'fwd5)

(define slatex.*in-bktd-qtd-exp* 'fwd6)

(define slatex.*in-mac-tkn* 'fwd7)

(define slatex.*in-bktd-mac-exp* 'fwd8)

(define slatex.*case-stack* 'fwd9)

(define slatex.*bq-stack* 'fwd10)

(define slatex.display-space
  (lambda (s p)
    (cond ((eqv? s slatex.&plain-space) (Zdisplay #\space p))
          ((eqv? s slatex.&init-plain-space) (Zdisplay #\space p))
          ((eqv? s slatex.&init-space) (Zdisplay "\\HL " p))
          ((eqv? s slatex.&paren-space) (Zdisplay "\\PRN " p))
          ((eqv? s slatex.&bracket-space) (Zdisplay "\\BKT " p))
          ((eqv? s slatex.&quote-space) (Zdisplay "\\QUO " p))
          ((eqv? s slatex.&inner-space) (Zdisplay "\\ " p)))))

(define slatex.display-tab
  (lambda (tab p)
    (cond ((eqv? tab slatex.&set-tab) (Zdisplay "\\=" p))
          ((eqv? tab slatex.&move-tab) (Zdisplay "\\>" p)))))

(define slatex.display-notab
  (lambda (notab p)
    (cond ((eqv? notab slatex.&begin-string) (Zdisplay "\\dt{" p))
          ((eqv? notab slatex.&end-string) (Zdisplay "}" p)))))

(define slatex.get-line
  (let ((curr-notab slatex.&void-notab))
    (lambda (line)
      (let ((graphic-char-seen? #f))
        (let loop ((i 0))
          (let ((c (Zread-char slatex.*in*)))
            (cond (graphic-char-seen? 'already-seen)
                  ((or (Zeof-object? c)
                       (Schar=? c slatex.*return*)
                       (Schar=? c #\newline)
                       (Schar=? c #\space)
                       (Schar=? c slatex.*tab*))
                   'not-yet)
                  (else (set! graphic-char-seen? #t)))
            (cond ((Zeof-object? c)
                   (cond ((eqv? curr-notab slatex.&mid-string)
                          (if (SFX> i 0)
                            (Sstring-set!
                              (vector-ref line slatex.=notab)
                              (SFX- i 1)
                              slatex.&end-string)))
                         ((eqv? curr-notab slatex.&mid-comment)
                          (set! curr-notab slatex.&void-notab))
                         ((eqv? curr-notab slatex.&mid-math)
                          (slatex.error
                            'slatex.get-line
                            'runaway-math-subformula)))
                   (Sstring-set! (vector-ref line slatex.=char) i #\newline)
                   (Sstring-set!
                     (vector-ref line slatex.=space)
                     i
                     slatex.&void-space)
                   (Sstring-set!
                     (vector-ref line slatex.=tab)
                     i
                     slatex.&void-tab)
                   (Sstring-set!
                     (vector-ref line slatex.=notab)
                     i
                     slatex.&void-notab)
                   (vector-set! line slatex.=rtedge i)
                   (if (eqv? (Sstring-ref (vector-ref line slatex.=notab) 0)
                             slatex.&mid-string)
                     (Sstring-set!
                       (vector-ref line slatex.=notab)
                       0
                       slatex.&begin-string))
                   (if (SFX= i 0) #f #t))
                  ((or (Schar=? c slatex.*return*) (Schar=? c #\newline))
                   (if (and (eq? *op-sys* 'dos) (Schar=? c slatex.*return*))
                     (if (Schar=? (Zpeek-char slatex.*in*) #\newline)
                       (Zread-char slatex.*in*)))
                   (cond ((eqv? curr-notab slatex.&mid-string)
                          (if (SFX> i 0)
                            (Sstring-set!
                              (vector-ref line slatex.=notab)
                              (SFX- i 1)
                              slatex.&end-string)))
                         ((eqv? curr-notab slatex.&mid-comment)
                          (set! curr-notab slatex.&void-notab))
                         ((eqv? curr-notab slatex.&mid-math)
                          (slatex.error
                            'slatex.get-line
                            'runaway-math-subformula)))
                   (Sstring-set! (vector-ref line slatex.=char) i #\newline)
                   (Sstring-set!
                     (vector-ref line slatex.=space)
                     i
                     slatex.&void-space)
                   (Sstring-set!
                     (vector-ref line slatex.=tab)
                     i
                     (cond ((Zeof-object? (Zpeek-char slatex.*in*))
                            slatex.&plain-crg-ret)
                           (slatex.*intext?* slatex.&plain-crg-ret)
                           (else slatex.&tabbed-crg-ret)))
                   (Sstring-set!
                     (vector-ref line slatex.=notab)
                     i
                     slatex.&void-notab)
                   (vector-set! line slatex.=rtedge i)
                   (if (eqv? (Sstring-ref (vector-ref line slatex.=notab) 0)
                             slatex.&mid-string)
                     (Sstring-set!
                       (vector-ref line slatex.=notab)
                       0
                       slatex.&begin-string))
                   #t)
                  ((eqv? curr-notab slatex.&mid-comment)
                   (Sstring-set! (vector-ref line slatex.=char) i c)
                   (Sstring-set!
                     (vector-ref line slatex.=space)
                     i
                     (cond ((Schar=? c #\space) slatex.&plain-space)
                           ((Schar=? c slatex.*tab*) slatex.&plain-space)
                           (else slatex.&void-space)))
                   (Sstring-set!
                     (vector-ref line slatex.=tab)
                     i
                     slatex.&void-tab)
                   (Sstring-set!
                     (vector-ref line slatex.=notab)
                     i
                     slatex.&mid-comment)
                   (loop (SFX+ i 1)))
                  ((Schar=? c #\\)
                   (Sstring-set! (vector-ref line slatex.=char) i c)
                   (Sstring-set!
                     (vector-ref line slatex.=space)
                     i
                     slatex.&void-space)
                   (Sstring-set!
                     (vector-ref line slatex.=tab)
                     i
                     slatex.&void-tab)
                   (Sstring-set! (vector-ref line slatex.=notab) i curr-notab)
                   (let ((i+1 (SFX+ i 1)) (c+1 (Zread-char slatex.*in*)))
                     (if (Schar=? c+1 slatex.*tab*) (set! c+1 #\space))
                     (Sstring-set! (vector-ref line slatex.=char) i+1 c+1)
                     (Sstring-set!
                       (vector-ref line slatex.=space)
                       i+1
                       (if (Schar=? c+1 #\space)
                         slatex.&plain-space
                         slatex.&void-space))
                     (Sstring-set!
                       (vector-ref line slatex.=tab)
                       i+1
                       slatex.&void-tab)
                     (Sstring-set!
                       (vector-ref line slatex.=notab)
                       i+1
                       curr-notab)
                     (loop (SFX+ i+1 1))))
                  ((eqv? curr-notab slatex.&mid-math)
                   (if (Schar=? c slatex.*tab*) (set! c #\space))
                   (Sstring-set!
                     (vector-ref line slatex.=space)
                     i
                     (if (Schar=? c #\space)
                       slatex.&plain-space
                       slatex.&void-space))
                   (Sstring-set!
                     (vector-ref line slatex.=tab)
                     i
                     slatex.&void-tab)
                   (cond ((Zmemv c slatex.*math-triggerers*)
                          (Sstring-set! (vector-ref line slatex.=char) i #\$)
                          (Sstring-set!
                            (vector-ref line slatex.=notab)
                            i
                            slatex.&end-math)
                          (set! curr-notab slatex.&void-notab))
                         (else
                          (Sstring-set! (vector-ref line slatex.=char) i c)
                          (Sstring-set!
                            (vector-ref line slatex.=notab)
                            i
                            slatex.&mid-math)))
                   (loop (SFX+ i 1)))
                  ((eqv? curr-notab slatex.&mid-string)
                   (if (Schar=? c slatex.*tab*) (set! c #\space))
                   (Sstring-set! (vector-ref line slatex.=char) i c)
                   (Sstring-set!
                     (vector-ref line slatex.=space)
                     i
                     (if (Schar=? c #\space)
                       slatex.&inner-space
                       slatex.&void-space))
                   (Sstring-set!
                     (vector-ref line slatex.=tab)
                     i
                     slatex.&void-tab)
                   (Sstring-set!
                     (vector-ref line slatex.=notab)
                     i
                     (cond ((Schar=? c #\")
                            (set! curr-notab slatex.&void-notab)
                            slatex.&end-string)
                           (else slatex.&mid-string)))
                   (loop (SFX+ i 1)))
                  ((Schar=? c #\space)
                   (Sstring-set! (vector-ref line slatex.=char) i c)
                   (Sstring-set!
                     (vector-ref line slatex.=space)
                     i
                     (cond (slatex.*intext?* slatex.&plain-space)
                           (graphic-char-seen? slatex.&inner-space)
                           (else slatex.&init-space)))
                   (Sstring-set!
                     (vector-ref line slatex.=tab)
                     i
                     slatex.&void-tab)
                   (Sstring-set!
                     (vector-ref line slatex.=notab)
                     i
                     slatex.&void-notab)
                   (loop (SFX+ i 1)))
                  ((Schar=? c slatex.*tab*)
                   (let loop2 ((i i) (j 0))
                     (if (SFX< j 8)
                       (begin
                         (Sstring-set! (vector-ref line slatex.=char) i #\space)
                         (Sstring-set!
                           (vector-ref line slatex.=space)
                           i
                           (cond (slatex.*intext?* slatex.&plain-space)
                                 (graphic-char-seen? slatex.&inner-space)
                                 (else slatex.&init-space)))
                         (Sstring-set!
                           (vector-ref line slatex.=tab)
                           i
                           slatex.&void-tab)
                         (Sstring-set!
                           (vector-ref line slatex.=notab)
                           i
                           slatex.&void-notab)
                         (loop2 (SFX+ i 1) (SFX+ j 1)))))
                   (loop (SFX+ i 8)))
                  ((Schar=? c #\")
                   (Sstring-set! (vector-ref line slatex.=char) i c)
                   (Sstring-set!
                     (vector-ref line slatex.=space)
                     i
                     slatex.&void-space)
                   (Sstring-set!
                     (vector-ref line slatex.=tab)
                     i
                     slatex.&void-tab)
                   (Sstring-set!
                     (vector-ref line slatex.=notab)
                     i
                     slatex.&begin-string)
                   (set! curr-notab slatex.&mid-string)
                   (loop (SFX+ i 1)))
                  ((Schar=? c #\;)
                   (Sstring-set! (vector-ref line slatex.=char) i c)
                   (Sstring-set!
                     (vector-ref line slatex.=space)
                     i
                     slatex.&void-space)
                   (Sstring-set!
                     (vector-ref line slatex.=tab)
                     i
                     slatex.&void-tab)
                   (Sstring-set!
                     (vector-ref line slatex.=notab)
                     i
                     slatex.&begin-comment)
                   (set! curr-notab slatex.&mid-comment)
                   (loop (SFX+ i 1)))
                  ((Zmemv c slatex.*math-triggerers*)
                   (Sstring-set! (vector-ref line slatex.=char) i #\$)
                   (Sstring-set!
                     (vector-ref line slatex.=space)
                     i
                     slatex.&void-space)
                   (Sstring-set!
                     (vector-ref line slatex.=tab)
                     i
                     slatex.&void-tab)
                   (Sstring-set!
                     (vector-ref line slatex.=notab)
                     i
                     slatex.&begin-math)
                   (set! curr-notab slatex.&mid-math)
                   (loop (SFX+ i 1)))
                  (else
                   (Sstring-set! (vector-ref line slatex.=char) i c)
                   (Sstring-set!
                     (vector-ref line slatex.=space)
                     i
                     slatex.&void-space)
                   (Sstring-set!
                     (vector-ref line slatex.=tab)
                     i
                     slatex.&void-tab)
                   (Sstring-set!
                     (vector-ref line slatex.=notab)
                     i
                     slatex.&void-notab)
                   (loop (SFX+ i 1))))))))))

(define slatex.peephole-adjust
  (lambda (curr prev)
    (if (or (slatex.blank-line? curr) (slatex.flush-comment-line? curr))
      (if slatex.*latex-paragraph-mode?*
        'skip
        (begin
          (set! slatex.*latex-paragraph-mode?* #t)
          (if slatex.*intext?*
            'skip
            (begin
              (slatex.remove-some-tabs prev 0)
              (let ((prev-rtedge (vector-ref prev slatex.=rtedge)))
                (if (eqv? (Sstring-ref (vector-ref prev slatex.=tab) prev-rtedge)
                          slatex.&tabbed-crg-ret)
                  (Sstring-set!
                    (vector-ref prev slatex.=tab)
                    (vector-ref prev slatex.=rtedge)
                    slatex.&plain-crg-ret)))))))
      (begin
        (if slatex.*latex-paragraph-mode?*
          (set! slatex.*latex-paragraph-mode?* #f)
          (if slatex.*intext?*
            'skip
            (let ((remove-tabs-from #f))
              (let loop ((i 0))
                (cond ((Schar=? (Sstring-ref (vector-ref curr slatex.=char) i)
                               #\newline)
                       (set! remove-tabs-from i))
                      ((Schar=? (Sstring-ref (vector-ref prev slatex.=char) i)
                               #\newline)
                       (set! remove-tabs-from #f))
                      ((eqv? (Sstring-ref (vector-ref curr slatex.=space) i)
                             slatex.&init-space)
                       (if (eqv? (Sstring-ref (vector-ref prev slatex.=notab) i)
                                 slatex.&void-notab)
                         (begin
                           (cond ((or (Schar=? (Sstring-ref
                                                (vector-ref prev slatex.=char)
                                                i)
                                              #\()
                                      (eqv? (Sstring-ref
                                              (vector-ref prev slatex.=space)
                                              i)
                                            slatex.&paren-space))
                                  (Sstring-set!
                                    (vector-ref curr slatex.=space)
                                    i
                                    slatex.&paren-space))
                                 ((or (Schar=? (Sstring-ref
                                                (vector-ref prev slatex.=char)
                                                i)
                                              #\[)
                                      (eqv? (Sstring-ref
                                              (vector-ref prev slatex.=space)
                                              i)
                                            slatex.&bracket-space))
                                  (Sstring-set!
                                    (vector-ref curr slatex.=space)
                                    i
                                    slatex.&bracket-space))
                                 ((or (Zmemv (Sstring-ref
                                              (vector-ref prev slatex.=char)
                                              i)
                                            '(#\' #\` #\,))
                                      (eqv? (Sstring-ref
                                              (vector-ref prev slatex.=space)
                                              i)
                                            slatex.&quote-space))
                                  (Sstring-set!
                                    (vector-ref curr slatex.=space)
                                    i
                                    slatex.&quote-space)))
                           (if (Zmemv (Sstring-ref
                                       (vector-ref prev slatex.=tab)
                                       i)
                                     (list slatex.&set-tab slatex.&move-tab))
                             (Sstring-set!
                               (vector-ref curr slatex.=tab)
                               i
                               slatex.&move-tab))))
                       (loop (SFX+ i 1)))
                      ((SFX= i 0) (set! remove-tabs-from 0))
                      ((not (eqv? (Sstring-ref (vector-ref prev slatex.=tab) i)
                                  slatex.&void-tab))
                       (set! remove-tabs-from (SFX+ i 1))
                       (if (Zmemv (Sstring-ref (vector-ref prev slatex.=tab) i)
                                 (list slatex.&set-tab slatex.&move-tab))
                         (Sstring-set!
                           (vector-ref curr slatex.=tab)
                           i
                           slatex.&move-tab)))
                      ((Zmemv (Sstring-ref (vector-ref prev slatex.=space) i)
                             (list slatex.&init-space
                                   slatex.&init-plain-space
                                   slatex.&paren-space
                                   slatex.&bracket-space
                                   slatex.&quote-space))
                       (set! remove-tabs-from (SFX+ i 1)))
                      ((and (Schar=? (Sstring-ref
                                      (vector-ref prev slatex.=char)
                                      (SFX- i 1))
                                    #\space)
                            (eqv? (Sstring-ref
                                    (vector-ref prev slatex.=notab)
                                    (SFX- i 1))
                                  slatex.&void-notab))
                       (set! remove-tabs-from (SFX+ i 1))
                       (Sstring-set!
                         (vector-ref prev slatex.=tab)
                         i
                         slatex.&set-tab)
                       (Sstring-set!
                         (vector-ref curr slatex.=tab)
                         i
                         slatex.&move-tab))
                      (else
                       (set! remove-tabs-from (SFX+ i 1))
                       (let loop1 ((j (SFX- i 1)))
                         (cond ((SFX<= j 0) 'exit-loop1)
                               ((not (eqv? (Sstring-ref
                                             (vector-ref curr slatex.=tab)
                                             j)
                                           slatex.&void-tab))
                                'exit-loop1)
                               ((Zmemv (Sstring-ref
                                        (vector-ref curr slatex.=space)
                                        j)
                                      (list slatex.&paren-space
                                            slatex.&bracket-space
                                            slatex.&quote-space))
                                (loop1 (SFX- j 1)))
                               ((or (not (eqv? (Sstring-ref
                                                 (vector-ref prev slatex.=notab)
                                                 j)
                                               slatex.&void-notab))
                                    (Schar=? (Sstring-ref
                                              (vector-ref prev slatex.=char)
                                              j)
                                            #\space))
                                (let ((k (SFX+ j 1)))
                                  (if (Zmemv (Sstring-ref
                                              (vector-ref prev slatex.=notab)
                                              k)
                                            (list slatex.&mid-comment
                                                  slatex.&mid-math
                                                  slatex.&end-math
                                                  slatex.&mid-string
                                                  slatex.&end-string))
                                    'skip
                                    (begin
                                      (if (eqv? (Sstring-ref
                                                  (vector-ref prev slatex.=tab)
                                                  k)
                                                slatex.&void-tab)
                                        (Sstring-set!
                                          (vector-ref prev slatex.=tab)
                                          k
                                          slatex.&set-tab))
                                      (Sstring-set!
                                        (vector-ref curr slatex.=tab)
                                        k
                                        slatex.&move-tab)))))
                               (else 'anything-else?))))))
              (slatex.remove-some-tabs prev remove-tabs-from))))
        (if slatex.*intext?* 'skip (slatex.add-some-tabs curr))
        (slatex.clean-init-spaces curr)
        (slatex.clean-inner-spaces curr)))))

(define slatex.add-some-tabs
  (lambda (line)
    (let loop ((i 1) (succ-parens? #f))
      (let ((c (Sstring-ref (vector-ref line slatex.=char) i)))
        (cond ((Schar=? c #\newline) 'exit-loop)
              ((not (eqv? (Sstring-ref (vector-ref line slatex.=notab) i)
                          slatex.&void-notab))
               (loop (SFX+ i 1) #f))
              ((Schar=? c #\[)
               (if (eqv? (Sstring-ref (vector-ref line slatex.=tab) i)
                         slatex.&void-tab)
                 (Sstring-set! (vector-ref line slatex.=tab) i slatex.&set-tab))
               (loop (SFX+ i 1) #f))
              ((Schar=? c #\()
               (if (eqv? (Sstring-ref (vector-ref line slatex.=tab) i)
                         slatex.&void-tab)
                 (if succ-parens?
                   'skip
                   (Sstring-set!
                     (vector-ref line slatex.=tab)
                     i
                     slatex.&set-tab)))
               (loop (SFX+ i 1) #t))
              (else (loop (SFX+ i 1) #f)))))))

(define slatex.remove-some-tabs
  (lambda (line i)
    (if i
      (let loop ((i i))
        (cond ((Schar=? (Sstring-ref (vector-ref line slatex.=char) i) #\newline)
               'exit)
              ((eqv? (Sstring-ref (vector-ref line slatex.=tab) i)
                     slatex.&set-tab)
               (Sstring-set! (vector-ref line slatex.=tab) i slatex.&void-tab)
               (loop (SFX+ i 1)))
              (else (loop (SFX+ i 1))))))))

(define slatex.clean-init-spaces
  (lambda (line)
    (let loop ((i (vector-ref line slatex.=rtedge)))
      (cond ((SFX< i 0) 'exit-loop)
            ((eqv? (Sstring-ref (vector-ref line slatex.=tab) i)
                   slatex.&move-tab)
             (let loop2 ((i (SFX- i 1)))
               (cond ((SFX< i 0) 'exit-loop2)
                     ((Zmemv (Sstring-ref (vector-ref line slatex.=space) i)
                            (list slatex.&init-space
                                  slatex.&paren-space
                                  slatex.&bracket-space
                                  slatex.&quote-space))
                      (Sstring-set!
                        (vector-ref line slatex.=space)
                        i
                        slatex.&init-plain-space)
                      (loop2 (SFX- i 1)))
                     (else (loop2 (SFX- i 1))))))
            (else (loop (SFX- i 1)))))))

(define slatex.clean-inner-spaces
  (lambda (line)
    (let loop ((i 0) (succ-inner-spaces? #f))
      (cond ((Schar=? (Sstring-ref (vector-ref line slatex.=char) i) #\newline)
             'exit-loop)
            ((eqv? (Sstring-ref (vector-ref line slatex.=space) i)
                   slatex.&inner-space)
             (if succ-inner-spaces?
               'skip
               (Sstring-set!
                 (vector-ref line slatex.=space)
                 i
                 slatex.&plain-space))
             (loop (SFX+ i 1) #t))
            (else (loop (SFX+ i 1) #f))))))

(define slatex.blank-line?
  (lambda (line)
    (let loop ((i 0))
      (let ((c (Sstring-ref (vector-ref line slatex.=char) i)))
        (cond ((Schar=? c #\space)
               (if (eqv? (Sstring-ref (vector-ref line slatex.=notab) i)
                         slatex.&void-notab)
                 (loop (SFX+ i 1))
                 #f))
              ((Schar=? c #\newline)
               (let loop2 ((j (SFX- i 1)))
                 (if (SFX<= j 0)
                   'skip
                   (begin
                     (Sstring-set!
                       (vector-ref line slatex.=space)
                       i
                       slatex.&void-space)
                     (loop2 (SFX- j 1)))))
               #t)
              (else #f))))))

(define slatex.flush-comment-line?
  (lambda (line)
    (and (Schar=? (Sstring-ref (vector-ref line slatex.=char) 0) #\;)
         (eqv? (Sstring-ref (vector-ref line slatex.=notab) 0)
               slatex.&begin-comment)
         (not (Schar=? (Sstring-ref (vector-ref line slatex.=char) 1) #\;)))))

(define slatex.do-all-lines
  (lambda ()
    (let loop ((line1 slatex.*line1*) (line2 slatex.*line2*))
      (let* ((line2-paragraph? slatex.*latex-paragraph-mode?*)
             (more? (slatex.get-line line1)))
        (slatex.peephole-adjust line1 line2)
        ((if line2-paragraph? slatex.display-tex-line slatex.display-scm-line)
         line2)
        (if (eq? line2-paragraph? slatex.*latex-paragraph-mode?*)
          'else
          ((if slatex.*latex-paragraph-mode?*
             slatex.display-end-sequence
             slatex.display-begin-sequence)
           slatex.*out*))
        (if more? (loop line2 line1))))))

(define scheme2tex
  (lambda (inport outport)
    (set! slatex.*in* inport)
    (set! slatex.*out* outport)
    (set! slatex.*latex-paragraph-mode?* #t)
    (set! slatex.*in-qtd-tkn* #f)
    (set! slatex.*in-bktd-qtd-exp* 0)
    (set! slatex.*in-mac-tkn* #f)
    (set! slatex.*in-bktd-mac-exp* 0)
    (set! slatex.*case-stack* '())
    (set! slatex.*bq-stack* '())
    (let ((flush-line
            (lambda (line)
              (vector-set! line slatex.=rtedge 0)
              (Sstring-set! (vector-ref line slatex.=char) 0 #\newline)
              (Sstring-set!
                (vector-ref line slatex.=space)
                0
                slatex.&void-space)
              (Sstring-set! (vector-ref line slatex.=tab) 0 slatex.&void-tab)
              (Sstring-set!
                (vector-ref line slatex.=notab)
                0
                slatex.&void-notab))))
      (flush-line slatex.*line1*)
      (flush-line slatex.*line2*))
    (slatex.do-all-lines)))

(define slatex.display-tex-line
  (lambda (line)
    (cond (else
           (let loop ((i (if (slatex.flush-comment-line? line) 1 0)))
             (let ((c (Sstring-ref (vector-ref line slatex.=char) i)))
               (if (Schar=? c #\newline)
                 (if (eqv? (Sstring-ref (vector-ref line slatex.=tab) i)
                           slatex.&void-tab)
                   'skip
                   (Znewline slatex.*out*))
                 (begin (Zdisplay c slatex.*out*) (loop (SFX+ i 1))))))))))

(define slatex.display-scm-line
  (lambda (line)
    (let loop ((i 0))
      (let ((c (Sstring-ref (vector-ref line slatex.=char) i)))
        (cond ((Schar=? c #\newline)
               (let ((tab (Sstring-ref (vector-ref line slatex.=tab) i)))
                 (cond ((eqv? tab slatex.&tabbed-crg-ret)
                        (Zdisplay "\\\\" slatex.*out*)
                        (Znewline slatex.*out*))
                       ((eqv? tab slatex.&plain-crg-ret) (Znewline slatex.*out*))
                       ((eqv? tab slatex.&void-tab)
                        (Zdisplay #\% slatex.*out*)
                        (Znewline slatex.*out*)))))
              ((eqv? (Sstring-ref (vector-ref line slatex.=notab) i)
                     slatex.&begin-comment)
               (slatex.display-tab
                 (Sstring-ref (vector-ref line slatex.=tab) i)
                 slatex.*out*)
               (Zdisplay c slatex.*out*)
               (loop (SFX+ i 1)))
              ((eqv? (Sstring-ref (vector-ref line slatex.=notab) i)
                     slatex.&mid-comment)
               (Zdisplay c slatex.*out*)
               (loop (SFX+ i 1)))
              ((eqv? (Sstring-ref (vector-ref line slatex.=notab) i)
                     slatex.&begin-string)
               (slatex.display-tab
                 (Sstring-ref (vector-ref line slatex.=tab) i)
                 slatex.*out*)
               (Zdisplay "\\dt{" slatex.*out*)
               (if (Schar=? c #\space)
                 (slatex.display-space
                   (Sstring-ref (vector-ref line slatex.=space) i)
                   slatex.*out*)
                 (slatex.display-tex-char c slatex.*out*))
               (loop (SFX+ i 1)))
              ((eqv? (Sstring-ref (vector-ref line slatex.=notab) i)
                     slatex.&mid-string)
               (if (Schar=? c #\space)
                 (slatex.display-space
                   (Sstring-ref (vector-ref line slatex.=space) i)
                   slatex.*out*)
                 (slatex.display-tex-char c slatex.*out*))
               (loop (SFX+ i 1)))
              ((eqv? (Sstring-ref (vector-ref line slatex.=notab) i)
                     slatex.&end-string)
               (if (Schar=? c #\space)
                 (slatex.display-space
                   (Sstring-ref (vector-ref line slatex.=space) i)
                   slatex.*out*)
                 (slatex.display-tex-char c slatex.*out*))
               (Zdisplay "}" slatex.*out*)
               (loop (SFX+ i 1)))
              ((eqv? (Sstring-ref (vector-ref line slatex.=notab) i)
                     slatex.&begin-math)
               (slatex.display-tab
                 (Sstring-ref (vector-ref line slatex.=tab) i)
                 slatex.*out*)
               (Zdisplay c slatex.*out*)
               (loop (SFX+ i 1)))
              ((Zmemv (Sstring-ref (vector-ref line slatex.=notab) i)
                     (list slatex.&mid-math slatex.&end-math))
               (Zdisplay c slatex.*out*)
               (loop (SFX+ i 1)))
              ((Schar=? c #\space)
               (slatex.display-tab
                 (Sstring-ref (vector-ref line slatex.=tab) i)
                 slatex.*out*)
               (slatex.display-space
                 (Sstring-ref (vector-ref line slatex.=space) i)
                 slatex.*out*)
               (loop (SFX+ i 1)))
              ((Schar=? c #\')
               (slatex.display-tab
                 (Sstring-ref (vector-ref line slatex.=tab) i)
                 slatex.*out*)
               (Zdisplay c slatex.*out*)
               (if (or slatex.*in-qtd-tkn* (SFX> slatex.*in-bktd-qtd-exp* 0))
                 'skip
                 (set! slatex.*in-qtd-tkn* #t))
               (loop (SFX+ i 1)))
              ((Schar=? c #\`)
               (slatex.display-tab
                 (Sstring-ref (vector-ref line slatex.=tab) i)
                 slatex.*out*)
               (Zdisplay c slatex.*out*)
               (if (or (null? slatex.*bq-stack*)
                       (vector-ref (Scar slatex.*bq-stack*) slatex.=in-comma))
                 (set! slatex.*bq-stack*
                   (cons (let ((f (slatex.make-bq-frame)))
                           (vector-set! f slatex.=in-comma #f)
                           (vector-set! f slatex.=in-bq-tkn #t)
                           (vector-set! f slatex.=in-bktd-bq-exp 0)
                           f)
                         slatex.*bq-stack*)))
               (loop (SFX+ i 1)))
              ((Schar=? c #\,)
               (slatex.display-tab
                 (Sstring-ref (vector-ref line slatex.=tab) i)
                 slatex.*out*)
               (Zdisplay c slatex.*out*)
               (if (or (null? slatex.*bq-stack*)
                       (vector-ref (Scar slatex.*bq-stack*) slatex.=in-comma))
                 'skip
                 (set! slatex.*bq-stack*
                   (cons (let ((f (slatex.make-bq-frame)))
                           (vector-set! f slatex.=in-comma #t)
                           (vector-set! f slatex.=in-bq-tkn #t)
                           (vector-set! f slatex.=in-bktd-bq-exp 0)
                           f)
                         slatex.*bq-stack*)))
               (if (Schar=? (Sstring-ref (vector-ref line slatex.=char) (SFX+ i 1))
                           #\@)
                 (begin
                   (slatex.display-tex-char #\@ slatex.*out*)
                   (loop (SFX+ 2 i)))
                 (loop (SFX+ i 1))))
              ((Zmemv c '(#\( #\[))
               (slatex.display-tab
                 (Sstring-ref (vector-ref line slatex.=tab) i)
                 slatex.*out*)
               (Zdisplay c slatex.*out*)
               (cond (slatex.*in-qtd-tkn*
                      (set! slatex.*in-qtd-tkn* #f)
                      (set! slatex.*in-bktd-qtd-exp* 1))
                     ((SFX> slatex.*in-bktd-qtd-exp* 0)
                      (set! slatex.*in-bktd-qtd-exp*
                        (SFX+ slatex.*in-bktd-qtd-exp* 1))))
               (cond (slatex.*in-mac-tkn*
                      (set! slatex.*in-mac-tkn* #f)
                      (set! slatex.*in-bktd-mac-exp* 1))
                     ((SFX> slatex.*in-bktd-mac-exp* 0)
                      (set! slatex.*in-bktd-mac-exp*
                        (SFX+ slatex.*in-bktd-mac-exp* 1))))
               (if (null? slatex.*bq-stack*)
                 'skip
                 (let ((top (Scar slatex.*bq-stack*)))
                   (cond ((vector-ref top slatex.=in-bq-tkn)
                          (vector-set! top slatex.=in-bq-tkn #f)
                          (vector-set! top slatex.=in-bktd-bq-exp 1))
                         ((SFX> (vector-ref top slatex.=in-bktd-bq-exp) 0)
                          (vector-set!
                            top
                            slatex.=in-bktd-bq-exp
                            (SFX+ (vector-ref top slatex.=in-bktd-bq-exp) 1))))))
               (if (null? slatex.*case-stack*)
                 'skip
                 (let ((top (Scar slatex.*case-stack*)))
                   (cond ((vector-ref top =in-ctag-tkn)
                          (vector-set! top =in-ctag-tkn #f)
                          (vector-set! top slatex.=in-bktd-ctag-exp 1))
                         ((SFX> (vector-ref top slatex.=in-bktd-ctag-exp) 0)
                          (vector-set!
                            top
                            slatex.=in-bktd-ctag-exp
                            (SFX+ (vector-ref top slatex.=in-bktd-ctag-exp) 1)))
                         ((SFX> (vector-ref top slatex.=in-case-exp) 0)
                          (vector-set!
                            top
                            slatex.=in-case-exp
                            (SFX+ (vector-ref top slatex.=in-case-exp) 1))
                          (if (SFX= (vector-ref top slatex.=in-case-exp) 2)
                            (set! slatex.*in-qtd-tkn* #t))))))
               (loop (SFX+ i 1)))
              ((Zmemv c '(#\) #\]))
               (slatex.display-tab
                 (Sstring-ref (vector-ref line slatex.=tab) i)
                 slatex.*out*)
               (Zdisplay c slatex.*out*)
               (if (SFX> slatex.*in-bktd-qtd-exp* 0)
                 (set! slatex.*in-bktd-qtd-exp*
                   (SFX- slatex.*in-bktd-qtd-exp* 1)))
               (if (SFX> slatex.*in-bktd-mac-exp* 0)
                 (set! slatex.*in-bktd-mac-exp*
                   (SFX- slatex.*in-bktd-mac-exp* 1)))
               (if (null? slatex.*bq-stack*)
                 'skip
                 (let ((top (Scar slatex.*bq-stack*)))
                   (if (SFX> (vector-ref top slatex.=in-bktd-bq-exp) 0)
                     (begin
                       (vector-set!
                         top
                         slatex.=in-bktd-bq-exp
                         (SFX- (vector-ref top slatex.=in-bktd-bq-exp) 1))
                       (if (SFX= (vector-ref top slatex.=in-bktd-bq-exp) 0)
                         (set! slatex.*bq-stack* (Scdr slatex.*bq-stack*)))))))
               (let loop ()
                 (if (null? slatex.*case-stack*)
                   'skip
                   (let ((top (Scar slatex.*case-stack*)))
                     (cond ((SFX> (vector-ref top slatex.=in-bktd-ctag-exp) 0)
                            (vector-set!
                              top
                              slatex.=in-bktd-ctag-exp
                              (SFX- (vector-ref top slatex.=in-bktd-ctag-exp) 1))
                            (if (SFX= (vector-ref top slatex.=in-bktd-ctag-exp) 0)
                              (vector-set! top slatex.=in-case-exp 1)))
                           ((SFX> (vector-ref top slatex.=in-case-exp) 0)
                            (vector-set!
                              top
                              slatex.=in-case-exp
                              (SFX- (vector-ref top slatex.=in-case-exp) 1))
                            (if (SFX= (vector-ref top slatex.=in-case-exp) 0)
                              (begin
                                (set! slatex.*case-stack*
                                  (Scdr slatex.*case-stack*))
                                (loop))))))))
               (loop (SFX+ i 1)))
              (else
               (slatex.display-tab
                 (Sstring-ref (vector-ref line slatex.=tab) i)
                 slatex.*out*)
               (loop (slatex.do-token line i))))))))

(define slatex.do-token
  (let ((token-delims
          (list #\(
                #\)
                #\[
                #\]
                #\space
                slatex.*return*
                #\newline
                #\,
                #\@
                #\;)))
    (lambda (line i)
      (let loop ((buf '()) (i i))
        (let ((c (Sstring-ref (vector-ref line slatex.=char) i)))
          (cond ((Schar=? c #\\)
                 (loop (cons (Sstring-ref
                               (vector-ref line slatex.=char)
                               (SFX+ i 1))
                             (cons c buf))
                       (SFX+ i 2)))
                ((or (Zmemv c token-delims) (Zmemv c slatex.*math-triggerers*))
                 (slatex.output-token (Zlist->string (slatex.reverse! buf)))
                 i)
                ((char? c)
                 (loop (cons (Sstring-ref (vector-ref line slatex.=char) i) buf)
                       (SFX+ i 1)))
                (else (slatex.error 'slatex.do-token 1))))))))

(define slatex.output-token
  (lambda (token)
    (if (null? slatex.*case-stack*)
      'skip
      (let ((top (Scar slatex.*case-stack*)))
        (if (vector-ref top =in-ctag-tkn)
          (begin
            (vector-set! top =in-ctag-tkn #f)
            (vector-set! top slatex.=in-case-exp 1)))))
    (if (slatex.assoc-token token slatex.special-symbols)
      (Zdisplay (Scdr (slatex.assoc-token token slatex.special-symbols))
               slatex.*out*)
      (slatex.display-token
        token
        (cond (slatex.*in-qtd-tkn*
               (set! slatex.*in-qtd-tkn* #f)
               (cond ((equal? token "else") 'syntax)
                     ((slatex.data-token? token) 'data)
                     (else 'constant)))
              ((slatex.data-token? token) 'data)
              ((SFX> slatex.*in-bktd-qtd-exp* 0) 'constant)
              ((and (not (null? slatex.*bq-stack*))
                    (not (vector-ref
                           (Scar slatex.*bq-stack*)
                           slatex.=in-comma)))
               'constant)
              (slatex.*in-mac-tkn*
               (set! slatex.*in-mac-tkn* #f)
               (slatex.set-keyword token)
               'syntax)
              ((SFX> slatex.*in-bktd-mac-exp* 0)
               (slatex.set-keyword token)
               'syntax)
              ((slatex.member-token token slatex.constant-tokens) 'constant)
              ((slatex.member-token token slatex.variable-tokens) 'variable)
              ((slatex.member-token token slatex.keyword-tokens)
               (cond ((slatex.token=? token "quote")
                      (set! slatex.*in-qtd-tkn* #t))
                     ((slatex.member-token token slatex.macro-definers)
                      (set! slatex.*in-mac-tkn* #t))
                     ((slatex.member-token token slatex.case-and-ilk)
                      (set! slatex.*case-stack*
                        (cons (let ((f (slatex.make-case-frame)))
                                (vector-set! f =in-ctag-tkn #t)
                                (vector-set! f slatex.=in-bktd-ctag-exp 0)
                                (vector-set! f slatex.=in-case-exp 0)
                                f)
                              slatex.*case-stack*))))
               'syntax)
              (else 'variable))
        slatex.*out*))
    (if (and (not (null? slatex.*bq-stack*))
             (vector-ref (Scar slatex.*bq-stack*) slatex.=in-bq-tkn))
      (set! slatex.*bq-stack* (Scdr slatex.*bq-stack*)))))

(define slatex.data-token?
  (lambda (token)
    (or (Schar=? (Sstring-ref token 0) #\#) (Sstring->number token))))

(define slatex.*texinputs* "")

(define slatex.*texinputs-list* '())

(define slatex.*path-separator*
  (cond ((eq? *op-sys* 'unix) #\:)
        ((eq? *op-sys* 'dos) #\;)
        (else (slatex.error 'slatex.*path-separator* 'cant-determine))))

(define slatex.*directory-mark*
  (cond ((eq? *op-sys* 'unix) "/")
        ((eq? *op-sys* 'dos) "\\")
        (else (slatex.error 'slatex.*directory-mark* 'cant-determine))))

(define slatex.*file-hider*
  (cond ((eq? *op-sys* 'unix) "") ((eq? *op-sys* 'dos) "x") (else ".")))

(define slatex.path->list
  (lambda (p)
    (let loop ((p (Zstring->list p)) (r (list "")))
      (let ((separator-pos (slatex.position-char slatex.*path-separator* p)))
        (if separator-pos
          (loop (Zlist-tail p (SFX+ separator-pos 1))
                (cons (Zlist->string (slatex.sublist p 0 separator-pos)) r))
          (slatex.reverse! (cons (Zlist->string p) r)))))))

(define slatex.find-some-file
  (lambda (path . files)
    (let loop ((path path))
      (if (null? path)
        #f
        (let ((dir (Scar path)))
          (let loop2 ((files (if (or (Zstring=? dir "") (Zstring=? dir "."))
                               files
                               (map (lambda (file)
                                      (Zstring-append
                                        dir
                                        slatex.*directory-mark*
                                        file))
                                    files))))
            (if (null? files)
              (loop (Scdr path))
              (let ((file (Scar files)))
                (if (slatex.file-exists? file)
                  file
                  (loop2 (Scdr files)))))))))))

(define slatex.file-extension
  (lambda (filename)
    (let ((i (slatex.string-position-right #\. filename)))
      (if i (substring filename i (Sstring-length filename)) #f))))

(define slatex.basename
  (lambda (filename ext)
    (let* ((filename-len (Sstring-length filename))
           (ext-len (Sstring-length ext))
           (len-diff (SFX- filename-len ext-len)))
      (cond ((SFX> ext-len filename-len) filename)
            ((equal? ext (substring filename len-diff filename-len))
             (substring filename 0 len-diff))
            (else filename)))))

(define slatex.full-texfile-name
  (lambda (filename)
    (let ((extn (slatex.file-extension filename)))
      (if (and extn (or (Zstring=? extn ".sty") (Zstring=? extn ".tex")))
        (slatex.find-some-file slatex.*texinputs-list* filename)
        (slatex.find-some-file
          slatex.*texinputs-list*
          (Zstring-append filename ".tex")
          filename)))))

(define slatex.full-scmfile-name
  (lambda (filename)
    (apply slatex.find-some-file
           slatex.*texinputs-list*
           filename
           (map (lambda (extn) (Zstring-append filename extn))
                '(".scm" ".ss" ".s")))))

(define slatex.new-aux-file
  (lambda e
    (apply (if slatex.*slatex-in-protected-region?*
             slatex.new-secondary-aux-file
             slatex.new-primary-aux-file)
           e)))

(define slatex.subjobname 'fwd)

(define primary-aux-file-count -1)

(define slatex.new-primary-aux-file
  (lambda e
    (set! primary-aux-file-count (SFX+ primary-aux-file-count 1))
    (apply Zstring-append
           slatex.*file-hider*
           "z"
           (SFXnumber->string primary-aux-file-count)
;           slatex.subjobname
           e)))

(define secondary-aux-file-count -1)

(define slatex.new-secondary-aux-file
  (lambda e
    (set! secondary-aux-file-count (SFX+ secondary-aux-file-count 1))
    (apply Zstring-append
           slatex.*file-hider*
           "zz"
           (SFXnumber->string secondary-aux-file-count)
;           slatex.subjobname
           e)))

(define slatex.eat-till-newline
  (lambda (in)
    (let loop ()
      (let ((c (Zread-char in)))
        (cond ((Zeof-object? c) 'done)
              ((Schar=? c #\newline) 'done)
              (else (loop)))))))

(define slatex.read-ctrl-seq
  (lambda (in)
    (let ((c (Zread-char in)))
      (if (Zeof-object? c) (slatex.error 'read-ctrl-exp 1))
      (if (Zchar-alphabetic? c)
        (Zlist->string
          (slatex.reverse!
            (let loop ((s (list c)))
              (let ((c (Zpeek-char in)))
                (cond ((Zeof-object? c) s)
                      ((Zchar-alphabetic? c) (Zread-char in) (loop (cons c s)))
                      ((Schar=? c #\%) (slatex.eat-till-newline in) (loop s))
                      (else s))))))
        (string c)))))

(define slatex.eat-tabspace
  (lambda (in)
    (let loop ()
      (let ((c (Zpeek-char in)))
        (cond ((Zeof-object? c) 'done)
              ((or (Schar=? c #\space) (Schar=? c slatex.*tab*))
               (Zread-char in)
               (loop))
              (else 'done))))))

(define slatex.eat-whitespace
  (lambda (in)
    (let loop ()
      (let ((c (Zpeek-char in)))
        (cond ((Zeof-object? c) 'done)
              ((Zchar-whitespace? c) (Zread-char in) (loop))
              (else 'done))))))

(define slatex.eat-latex-whitespace
  (lambda (in)
    (let loop ()
      (let ((c (Zpeek-char in)))
        (cond ((Zeof-object? c) 'done)
              ((Zchar-whitespace? c) (Zread-char in) (loop))
              ((Schar=? c #\%) (slatex.eat-till-newline in))
              (else 'done))))))

(define slatex.chop-off-whitespace
  (lambda (l)
    (slatex.ormapcdr (lambda (d) (if (Zchar-whitespace? (Scar d)) #f d)) l)))

(define slatex.read-grouped-latexexp
  (lambda (in)
    (slatex.eat-latex-whitespace in)
    (let ((c (Zread-char in)))
      (if (Zeof-object? c) (slatex.error 'slatex.read-grouped-latexexp 1))
      (if (Schar=? c #\{) 'ok (slatex.error 'slatex.read-grouped-latexexp 2))
      (slatex.eat-latex-whitespace in)
      (Zlist->string
        (slatex.reverse!
          (slatex.chop-off-whitespace
            (let loop ((s '()) (nesting 0) (escape? #f))
              (let ((c (Zread-char in)))
                (if (Zeof-object? c)
                  (slatex.error 'slatex.read-grouped-latexexp 3))
                (cond (escape? (loop (cons c s) nesting #f))
                      ((Schar=? c #\\) (loop (cons c s) nesting #t))
                      ((Schar=? c #\%)
                       (slatex.eat-till-newline in)
                       (loop s nesting #f))
                      ((Schar=? c #\{) (loop (cons c s) (SFX+ nesting 1) #f))
                      ((Schar=? c #\})
                       (if (SFX= nesting 0) s (loop (cons c s) (SFX- nesting 1) #f)))
                      (else (loop (cons c s) nesting #f)))))))))))

(define slatex.read-filename
  (let ((filename-delims
          (list #\{
                #\}
                #\[
                #\]
                #\(
                #\)
                #\#
                #\%
                #\\
                #\,
                #\space
                slatex.*return*
                #\newline
                slatex.*tab*)))
    (lambda (in)
      (slatex.eat-latex-whitespace in)
      (let ((c (Zpeek-char in)))
        (if (Zeof-object? c) (slatex.error 'slatex.read-filename 1))
        (if (Schar=? c #\{)
          (slatex.read-grouped-latexexp in)
          (Zlist->string
            (slatex.reverse!
              (let loop ((s '()) (escape? #f))
                (let ((c (Zpeek-char in)))
                  (cond ((Zeof-object? c)
                         (if escape? (slatex.error 'slatex.read-filename 2) s))
                        (escape? (Zread-char in) (loop (cons c s) #f))
                        ((Schar=? c #\\) (Zread-char in) (loop (cons c s) #t))
                        ((Zmemv c filename-delims) s)
                        (else (Zread-char in) (loop (cons c s) #f))))))))))))

(define slatex.read-schemeid
  (let ((schemeid-delims
          (list #\{
                #\}
                #\[
                #\]
                #\(
                #\)
                #\space
                slatex.*return*
                #\newline
                slatex.*tab*)))
    (lambda (in)
      (slatex.eat-whitespace in)
      (Zlist->string
        (slatex.reverse!
          (let loop ((s '()) (escape? #f))
            (let ((c (Zpeek-char in)))
              (cond ((Zeof-object? c) s)
                    (escape? (Zread-char in) (loop (cons c s) #f))
                    ((Schar=? c #\\) (Zread-char in) (loop (cons c s) #t))
                    ((Zmemv c schemeid-delims) s)
                    (else (Zread-char in) (loop (cons c s) #f))))))))))

(define slatex.read-delimed-commaed-filenames
  (lambda (in lft-delim rt-delim)
    (slatex.eat-latex-whitespace in)
    (let ((c (Zread-char in)))
      (if (Zeof-object? c)
        (slatex.error 'slatex.read-delimed-commaed-filenames 1))
      (if (Schar=? c lft-delim)
        'ok
        (slatex.error 'slatex.read-delimed-commaed-filenames 2))
      (let loop ((s '()))
        (slatex.eat-latex-whitespace in)
        (let ((c (Zpeek-char in)))
          (if (Zeof-object? c)
            (slatex.error 'slatex.read-delimed-commaed-filenames 3))
          (if (Schar=? c rt-delim)
            (begin (Zread-char in) (slatex.reverse! s))
            (let ((s (cons (slatex.read-filename in) s)))
              (slatex.eat-latex-whitespace in)
              (let ((c (Zpeek-char in)))
                (if (Zeof-object? c)
                  (slatex.error 'slatex.read-delimed-commaed-filenames 4))
                (cond ((Schar=? c #\,) (Zread-char in))
                      ((Schar=? c rt-delim) 'void)
                      (else
                       (slatex.error
                         'slatex.read-delimed-commaed-filenames
                         5)))
                (loop s)))))))))

(define slatex.read-grouped-commaed-filenames
  (lambda (in) (slatex.read-delimed-commaed-filenames in #\{ #\})))

(define slatex.read-bktd-commaed-filenames
  (lambda (in) (slatex.read-delimed-commaed-filenames in #\[ #\])))

(define slatex.read-grouped-schemeids
  (lambda (in)
    (slatex.eat-latex-whitespace in)
    (let ((c (Zread-char in)))
      (if (Zeof-object? c) (slatex.error 'slatex.read-grouped-schemeids 1))
      (if (Schar=? c #\{) 'ok (slatex.error 'slatex.read-grouped-schemeids 2))
      (let loop ((s '()))
        (slatex.eat-whitespace in)
        (let ((c (Zpeek-char in)))
          (if (Zeof-object? c) (slatex.error 'slatex.read-grouped-schemeids 3))
          (if (Schar=? c #\})
            (begin (Zread-char in) (slatex.reverse! s))
            (loop (cons (slatex.read-schemeid in) s))))))))

(define slatex.disable-slatex-temply
  (lambda (in)
    (set! slatex.*slatex-enabled?* #f)
    (set! slatex.*slatex-reenabler* (slatex.read-grouped-latexexp in))))

(define slatex.enable-slatex-again
  (lambda ()
    (set! slatex.*slatex-enabled?* #t)
    (set! slatex.*slatex-reenabler* "UNDEFINED")))

(define slatex.ignore2 (lambda (i ii) 'void))

(define slatex.add-to-slatex-db
  (lambda (in categ)
    (if (Zmemq categ '(keyword constant variable))
      (slatex.add-to-slatex-db-basic in categ)
      (slatex.add-to-slatex-db-special in categ))))

(define slatex.add-to-slatex-db-basic
  (lambda (in categ)
    (let ((setter (cond ((eq? categ 'keyword) slatex.set-keyword)
                        ((eq? categ 'constant) slatex.set-constant)
                        ((eq? categ 'variable) slatex.set-variable)
                        (else
                         (slatex.error 'slatex.add-to-slatex-db-basic 1))))
          (ids (slatex.read-grouped-schemeids in)))
      (for-each setter ids))))

(define slatex.add-to-slatex-db-special
  (lambda (in what)
    (let ((ids (slatex.read-grouped-schemeids in)))
      (cond ((eq? what 'unsetspecialsymbol)
             (for-each slatex.unset-special-symbol ids))
            ((eq? what 'setspecialsymbol)
             (if (SFX= (length ids) 1)
               'ok
               (slatex.error
                 'slatex.add-to-slatex-db-special
                 'setspecialsymbol-takes-one-arg-only))
             (let ((transl (slatex.read-grouped-latexexp in)))
               (slatex.set-special-symbol (Scar ids) transl)))
            (else (slatex.error 'slatex.add-to-slatex-db-special 2))))))

(define slatex.process-slatex-alias
  (lambda (in what which)
    (let ((triggerer (slatex.read-grouped-latexexp in)))
      (cond ((eq? which 'intext)
             (set! slatex.*intext-triggerers*
               (what triggerer slatex.*intext-triggerers*)))
            ((eq? which 'resultintext)
             (set! slatex.*resultintext-triggerers*
               (what triggerer slatex.*resultintext-triggerers*)))
            ((eq? which 'display)
             (set! slatex.*display-triggerers*
               (what triggerer slatex.*display-triggerers*)))
            ((eq? which 'box)
             (set! slatex.*box-triggerers*
               (what triggerer slatex.*box-triggerers*)))
            ((eq? which 'input)
             (set! slatex.*input-triggerers*
               (what triggerer slatex.*input-triggerers*)))
            ((eq? which 'region)
             (set! slatex.*region-triggerers*
               (what triggerer slatex.*region-triggerers*)))
            ((eq? which 'mathescape)
             (if (SFX= (Sstring-length triggerer) 1)
               'ok
               (slatex.error
                 'slatex.process-slatex-alias
                 'math-escape-should-be-character))
             (set! slatex.*math-triggerers*
               (what (Sstring-ref triggerer 0) slatex.*math-triggerers*)))
            (else (slatex.error 'slatex.process-slatex-alias 2))))))

(define slatex.decide-latex-or-tex
  (lambda (latex?)
    (set! slatex.*latex?* latex?)
    (let ((pltexchk.jnk "pltexchk.jnk"))
      (if (slatex.file-exists? pltexchk.jnk) (slatex.delete-file pltexchk.jnk))
      (if (not slatex.*latex?*)
        (Zcall-with-output-file/truncate
          pltexchk.jnk
          (lambda (outp) (Zdisplay "junk" outp) (Znewline outp)))))))

(define slatex.process-include-only
  (lambda (in)
    (set! slatex.*include-onlys* '())
    (for-each
      (lambda (filename)
        (let ((filename (slatex.full-texfile-name filename)))
          (if filename
            (set! slatex.*include-onlys*
              (slatex.adjoin-string filename slatex.*include-onlys*)))))
      (slatex.read-grouped-commaed-filenames in))))

(define slatex.process-documentstyle
  (lambda (in)
    (slatex.eat-latex-whitespace in)
    (if (Schar=? (Zpeek-char in) #\[)
      (for-each
        (lambda (filename)
          (let ((%:g0% slatex.*slatex-in-protected-region?*))
            (set! slatex.*slatex-in-protected-region?* #f)
            (let ((%temp% (begin
                            (slatex.process-tex-file
                              (Zstring-append filename ".sty")))))
              (set! slatex.*slatex-in-protected-region?* %:g0%)
              %temp%)))
        (slatex.read-bktd-commaed-filenames in)))))

(define slatex.process-case-info
  (lambda (in)
    (let ((bool (slatex.read-grouped-latexexp in)))
      (set! slatex.*slatex-case-sensitive?*
        (cond ((Zstring-ci=? bool "true") #t)
              ((Zstring-ci=? bool "false") #f)
              (else
               (slatex.error
                 'slatex.process-case-info
                 'bad-schemecasesensitive-arg)))))))

(define slatex.seen-first-command? #f)

(define slatex.process-main-tex-file
  (lambda (filename)
;    (display "SLaTeX v. 2.2")
;    (newline)
    (set! slatex.*texinputs-list* (slatex.path->list slatex.*texinputs*))
    (let ((file-hide-file "xZfilhid.tex"))
      (if (slatex.file-exists? file-hide-file)
        (slatex.delete-file file-hide-file))
      (if (eq? *op-sys* 'dos)
        (Zcall-with-output-file/truncate
          file-hide-file
          (lambda (out) (Zdisplay "\\def\\filehider{x}" out) (Znewline out)))))
;    (display "typesetting code")
    (set! slatex.subjobname (slatex.basename filename ".tex"))
    (set! slatex.seen-first-command? #f)
    (slatex.process-tex-file filename)
;    (display 'done)
;    (newline)
))

(define slatex.dump-intext
  (lambda (in out)
    (let* ((display (if out (lambda (obj port) (Zdisplay obj port)) slatex.ignore2))
           (delim-char (begin (slatex.eat-whitespace in) (Zread-char in)))
           (delim-char (cond ((Schar=? delim-char #\{) #\}) (else delim-char))))
      (if (Zeof-object? delim-char) (slatex.error 'slatex.dump-intext 1))
      (let loop ()
        (let ((c (Zread-char in)))
          (if (Zeof-object? c) (slatex.error 'slatex.dump-intext 2))
          (if (Schar=? c delim-char) 'done (begin (display c out) (loop))))))))

;;(define slatex.dump-display
;;  (lambda (in out ender)
;;    (slatex.eat-tabspace in)
;;    (let ((display (if out Zdisplay slatex.ignore2))
;;          (ender-lh (Sstring-length ender))
;;          (c (Zpeek-char in)))
;;      (if (Zeof-object? c) (slatex.error 'slatex.dump-display 1))
;;      (if (Schar=? c #\newline) (Zread-char in))
;;      (let loop ((buf ""))
;;        (let ((c (Zread-char in)))
;;          (if (Zeof-object? c) (slatex.error 'slatex.dump-display 2))
;;          (let ((buf (Zstring-append buf (string c))))
;;            (if (slatex.string-prefix? buf ender)
;;              (if (SFX= (Sstring-length buf) ender-lh) 'done (loop buf))
;;              (begin (Zdisplay buf out) (loop "")))))))))

(define slatex.dump-display
  (lambda (in out ender)
    (slatex.eat-tabspace in)
    (let ((display (if out (lambda (obj port) (Zdisplay obj port)) slatex.ignore2))
          (ender-lh (Sstring-length ender))
          (c (Zpeek-char in)))
      (if (Zeof-object? c) (slatex.error 'slatex.dump-display 1))
      (if (Schar=? c #\newline) (Zread-char in))
      (let loop ((i 0) (buf '()))
        (let ((c (Zread-char in)))
          (if (Zeof-object? c) (slatex.error 'slatex.dump-display 2))
          (let ((buf (cons c buf)))
            (if (Schar=? c (Sstring-ref ender i))
                (if (SFX= i (SFX- ender-lh 1))
                    'done
                    (loop (SFX+ i 1) buf))
                (begin (for-each (lambda (c) (Zdisplay c out)) (slatex.reverse! buf)) (loop 0 '())))))))))

(define slatex.debug? #f)

(define slatex.process-tex-file
  (lambda (raw-filename)
    (if slatex.debug?
      (begin (display "begin ") (display raw-filename) (newline)))
    (let ((filename (slatex.full-texfile-name raw-filename)))
      (if (not filename)
        (begin
          (display "[")
          (display raw-filename)
          (display "]")
          (slatex.force-output))
        (Zcall-with-input-file
          filename
          (lambda (in)
            (let ((done? #f))
              (let loop ()
                (if done?
                  'exit-loop
                  (begin
                    (let ((c (Zread-char in)))
                      (cond ((Zeof-object? c) (set! done? #t))
                            ((Schar=? c #\%) (slatex.eat-till-newline in))
                            ((Schar=? c #\\)
                             (let ((cs (slatex.read-ctrl-seq in)))
                               (if slatex.seen-first-command?
                                 'skip
                                 (begin
                                   (set! slatex.seen-first-command? #t)
                                   (slatex.decide-latex-or-tex
                                     (Zstring=? cs "documentstyle"))))
                               (cond ((not slatex.*slatex-enabled?*)
                                      (if (Zstring=?
                                            cs
                                            slatex.*slatex-reenabler*)
                                        (slatex.enable-slatex-again)))
                                     ((Zstring=? cs "slatexignorecurrentfile")
                                      (set! done? #t))
                                     ((Zstring=? cs "slatexseparateincludes")
                                      (if slatex.*latex?*
                                        (set! slatex.*slatex-separate-includes?*
                                          #t)))
                                     ((Zstring=? cs "slatexdisable")
                                      (slatex.disable-slatex-temply in))
                                     ((Zstring=? cs "begin")
                                      (let ((cs (slatex.read-grouped-latexexp
                                                  in)))
                                        (cond ((slatex.member-string cs
                                                       slatex.*display-triggerers*)
                                               (slatex.trigger-scheme2tex
                                                 'envdisplay
                                                 in
                                                 cs))
                                              ((slatex.member-string cs
                                                       slatex.*box-triggerers*)
                                               (slatex.trigger-scheme2tex
                                                 'envbox
                                                 in
                                                 cs))
                                              ((slatex.member-string cs
                                                       slatex.*region-triggerers*)
                                               (slatex.trigger-region
                                                 'envregion
                                                 in
                                                 cs)))))
                                     ((slatex.member-string cs slatex.*intext-triggerers*)
                                      (slatex.trigger-scheme2tex
                                        'intext
                                        in
                                        #f))
                                     ((slatex.member-string cs
                                              slatex.*resultintext-triggerers*)
                                      (slatex.trigger-scheme2tex
                                        'resultintext
                                        in
                                        #f))
                                     ((slatex.member-string cs slatex.*display-triggerers*)
                                      (slatex.trigger-scheme2tex
                                        'plaindisplay
                                        in
                                        cs))
                                     ((slatex.member-string cs slatex.*box-triggerers*)
                                      (slatex.trigger-scheme2tex
                                        'plainbox
                                        in
                                        cs))
                                     ((slatex.member-string cs slatex.*region-triggerers*)
                                      (slatex.trigger-region
                                        'plainregion
                                        in
                                        cs))
                                     ((slatex.member-string cs slatex.*input-triggerers*)
                                      (slatex.process-scheme-file
                                        (slatex.read-filename in)))
                                     ((Zstring=? cs "input")
                                      (let ((%:g1% slatex.*slatex-in-protected-region?*))
                                        (set! slatex.*slatex-in-protected-region?*
                                          #f)
                                        (let ((%temp% (begin
                                                        (slatex.process-tex-file
                                                          (slatex.read-filename
                                                            in)))))
                                          (set! slatex.*slatex-in-protected-region?*
                                            %:g1%)
                                          %temp%)))
                                     ((Zstring=? cs "include")
                                      (if slatex.*latex?*
                                        (let ((f (slatex.full-texfile-name
                                                   (slatex.read-filename in))))
                                          (if (and f
                                                   (or (eq? slatex.*include-onlys*
                                                            'all)
                                                       (slatex.member-string f
                                                               slatex.*include-onlys*)))
                                            (let ((%:g2% slatex.*slatex-in-protected-region?*)
                                                  (%:g3% slatex.subjobname)
                                                  (%:g4% primary-aux-file-count))
                                              (set! slatex.*slatex-in-protected-region?*
                                                #f)
                                              (set! slatex.subjobname
                                                slatex.subjobname)
                                              (set! primary-aux-file-count
                                                primary-aux-file-count)
                                              (let ((%temp% (begin
                                                              (if slatex.*slatex-separate-includes?*
                                                                (begin
                                                                  (set! slatex.subjobname
                                                                    (slatex.basename
                                                                      f
                                                                      ".tex"))
                                                                  (set! primary-aux-file-count
                                                                    -1)))
                                                              (slatex.process-tex-file
                                                                f))))
                                                (set! slatex.*slatex-in-protected-region?*
                                                  %:g2%)
                                                (set! slatex.subjobname %:g3%)
                                                (set! primary-aux-file-count
                                                  %:g4%)
                                                %temp%))))))
                                     ((Zstring=? cs "includeonly")
                                      (if slatex.*latex?*
                                        (slatex.process-include-only in)))
                                     ((Zstring=? cs "documentstyle")
                                      (if slatex.*latex?*
                                        (slatex.process-documentstyle in)))
                                     ((Zstring=? cs "schemecasesensitive")
                                      (slatex.process-case-info in))
                                     ((Zstring=? cs "defschemetoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.adjoin-string
                                        'intext))
                                     ((Zstring=? cs "undefschemetoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.remove-string!
                                        'intext))
                                     ((Zstring=? cs "defschemeresulttoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.adjoin-string
                                        'resultintext))
                                     ((Zstring=? cs "undefschemeresulttoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.remove-string!
                                        'resultintext))
                                     ((Zstring=? cs "defschemedisplaytoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.adjoin-string
                                        'display))
                                     ((Zstring=? cs "undefschemedisplaytoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.remove-string!
                                        'display))
                                     ((Zstring=? cs "defschemeboxtoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.adjoin-string
                                        'box))
                                     ((Zstring=? cs "undefschemeboxtoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.remove-string!
                                        'box))
                                     ((Zstring=? cs "defschemeinputtoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.adjoin-string
                                        'input))
                                     ((Zstring=? cs "undefschemeinputtoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.remove-string!
                                        'input))
                                     ((Zstring=? cs "defschemeregiontoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.adjoin-string
                                        'region))
                                     ((Zstring=? cs "undefschemeregiontoken")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.remove-string!
                                        'region))
                                     ((Zstring=? cs "defschememathescape")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.adjoin-char
                                        'mathescape))
                                     ((Zstring=? cs "undefschememathescape")
                                      (slatex.process-slatex-alias
                                        in
                                        slatex.remove-char!
                                        'mathescape))
                                     ((Zstring=? cs "setkeyword")
                                      (slatex.add-to-slatex-db in 'keyword))
                                     ((Zstring=? cs "setconstant")
                                      (slatex.add-to-slatex-db in 'constant))
                                     ((Zstring=? cs "setvariable")
                                      (slatex.add-to-slatex-db in 'variable))
                                     ((Zstring=? cs "setspecialsymbol")
                                      (slatex.add-to-slatex-db
                                        in
                                        'setspecialsymbol))
                                     ((Zstring=? cs "unsetspecialsymbol")
                                      (slatex.add-to-slatex-db
                                        in
                                        'unsetspecialsymbol)))))))
                    (loop)))))))))
    (if slatex.debug?
      (begin (display "end ") (display raw-filename) (newline)))))

(define slatex.process-scheme-file
  (lambda (raw-filename)
    (let ((filename (slatex.full-scmfile-name raw-filename)))
      (if (not filename)
        (begin
          (display "process-scheme-file: ")
          (display raw-filename)
          (display " doesn't exist")
          (newline))
        (let ((aux.tex (slatex.new-aux-file ".tex")))
;          (display ".")
          (slatex.force-output)
          (if (slatex.file-exists? aux.tex) (slatex.delete-file aux.tex))
          (Zcall-with-input-file
            filename
            (lambda (in)
              (Zcall-with-output-file/truncate
                aux.tex
                (lambda (out)
                  (let ((%:g5% slatex.*intext?*)
                        (%:g6% slatex.*code-env-spec*))
                    (set! slatex.*intext?* #f)
                    (set! slatex.*code-env-spec* "ZZZZschemedisplay")
                    (let ((%temp% (begin (scheme2tex in out))))
                      (set! slatex.*intext?* %:g5%)
                      (set! slatex.*code-env-spec* %:g6%)
                      %temp%))))))
          (if slatex.*slatex-in-protected-region?*
            (set! slatex.*protected-files*
              (cons aux.tex slatex.*protected-files*)))
          (slatex.process-tex-file filename))))))

(define slatex.trigger-scheme2tex
  (lambda (typ in env)
    (let* ((aux (slatex.new-aux-file))
           (aux.scm (Zstring-append aux ".scm"))
           (aux.tex (Zstring-append aux ".tex")))
      (if (slatex.file-exists? aux.scm) (slatex.delete-file aux.scm))
      (if (slatex.file-exists? aux.tex) (slatex.delete-file aux.tex))
;      (display ".")
      (slatex.force-output)
      (Zcall-with-output-file/truncate
        aux.scm
        (lambda (out)
          (cond ((Zmemq typ '(intext resultintext)) (slatex.dump-intext in out))
                ((Zmemq typ '(envdisplay envbox))
                 (slatex.dump-display in out (Zstring-append "\\end{" env "}")))
                ((Zmemq typ '(plaindisplay plainbox))
                 (slatex.dump-display in out (Zstring-append "\\end" env)))
                (else (slatex.error 'slatex.trigger-scheme2tex 1)))))
      (Zcall-with-input-file
        aux.scm
        (lambda (in)
          (Zcall-with-output-file/truncate
            aux.tex
            (lambda (out)
              (let ((%:g7% slatex.*intext?*) (%:g8% slatex.*code-env-spec*))
                (set! slatex.*intext?* (Zmemq typ '(intext resultintext)))
                (set! slatex.*code-env-spec*
                  (cond ((eq? typ 'intext) "ZZZZschemecodeintext")
                        ((eq? typ 'resultintext) "ZZZZschemeresultintext")
                        ((Zmemq typ '(envdisplay plaindisplay))
                         "ZZZZschemedisplay")
                        ((Zmemq typ '(envbox plainbox)) "ZZZZschemebox")
                        (else (slatex.error 'slatex.trigger-scheme2tex 2))))
                (let ((%temp% (begin (scheme2tex in out))))
                  (set! slatex.*intext?* %:g7%)
                  (set! slatex.*code-env-spec* %:g8%)
                  %temp%))))))
      (if slatex.*slatex-in-protected-region?*
        (set! slatex.*protected-files*
          (cons aux.tex slatex.*protected-files*)))
      (if (Zmemq typ '(envdisplay plaindisplay envbox plainbox))
        (slatex.process-tex-file aux.tex))
      (slatex.delete-file aux.scm))))

(define slatex.trigger-region
  (lambda (typ in env)
    (let ((aux.tex (slatex.new-primary-aux-file ".tex"))
          (aux2.tex (slatex.new-secondary-aux-file ".tex")))
      (if (slatex.file-exists? aux2.tex) (slatex.delete-file aux2.tex))
      (if (slatex.file-exists? aux.tex) (slatex.delete-file aux.tex))
;      (display ".")
      (slatex.force-output)
      (let ((%:g9% slatex.*slatex-in-protected-region?*)
            (%:g10% slatex.*protected-files*))
        (set! slatex.*slatex-in-protected-region?* #t)
        (set! slatex.*protected-files* '())
        (let ((%temp% (begin
                        (Zcall-with-output-file/truncate
                          aux2.tex
                          (lambda (out)
                            (cond ((eq? typ 'envregion)
                                   (slatex.dump-display
                                     in
                                     out
                                     (Zstring-append "\\end{" env "}")))
                                  ((eq? typ 'plainregion)
                                   (slatex.dump-display
                                     in
                                     out
                                     (Zstring-append "\\end" env)))
                                  (else
                                   (slatex.error 'slatex.trigger-region 1)))))
                        (slatex.process-tex-file aux2.tex)
                        (set! slatex.*protected-files*
                          (slatex.reverse! slatex.*protected-files*))
                        (Zcall-with-input-file
                          aux2.tex
                          (lambda (in)
                            (Zcall-with-output-file/truncate
                              aux.tex
                              (lambda (out)
                                (slatex.inline-protected-files in out)))))
                        (slatex.delete-file aux2.tex))))
          (set! slatex.*slatex-in-protected-region?* %:g9%)
          (set! slatex.*protected-files* %:g10%)
          %temp%)))))

(define slatex.inline-protected-files
  (lambda (in out)
    (let ((done? #f))
      (let loop ()
        (if done?
          'exit-loop
          (begin
            (let ((c (Zread-char in)))
              (cond ((Zeof-object? c) (Zdisplay "{}" out) (set! done? #t))
                    ((Schar=? c #\%) (slatex.eat-till-newline in))
                    ((Schar=? c #\\)
                     (let ((cs (slatex.read-ctrl-seq in)))
                       (cond ((Zstring=? cs "begin")
                              (let ((cs (slatex.read-grouped-latexexp in)))
                                (cond ((slatex.member-string cs slatex.*display-triggerers*)
                                       (slatex.inline-protected
                                         'envdisplay
                                         in
                                         out
                                         cs))
                                      ((slatex.member-string cs slatex.*box-triggerers*)
                                       (slatex.inline-protected
                                         'envbox
                                         in
                                         out
                                         cs))
                                      ((slatex.member-string cs slatex.*region-triggerers*)
                                       (slatex.inline-protected
                                         'envregion
                                         in
                                         out
                                         cs))
                                      (else
                                       (Zdisplay "\\begin{" out)
                                       (Zdisplay cs out)
                                       (Zdisplay "}" out)))))
                             ((slatex.member-string cs slatex.*intext-triggerers*)
                              (slatex.inline-protected 'intext in out #f))
                             ((slatex.member-string cs slatex.*resultintext-triggerers*)
                              (slatex.inline-protected
                                'resultintext
                                in
                                out
                                #f))
                             ((slatex.member-string cs slatex.*display-triggerers*)
                              (slatex.inline-protected
                                'plaindisplay
                                in
                                out
                                cs))
                             ((slatex.member-string cs slatex.*box-triggerers*)
                              (slatex.inline-protected 'plainbox in out cs))
                             ((slatex.member-string cs slatex.*region-triggerers*)
                              (slatex.inline-protected 'plainregion in out cs))
                             ((slatex.member-string cs slatex.*input-triggerers*)
                              (slatex.inline-protected 'input in out cs))
                             (else (Zdisplay "\\" out) (Zdisplay cs out)))))
                    (else (Zdisplay c out))))
            (loop)))))))

(define slatex.inline-protected
  (lambda (typ in out env)
    (cond ((eq? typ 'envregion)
           (Zdisplay "\\begin{" out)
           (Zdisplay env out)
           (Zdisplay "}" out)
           (slatex.dump-display in out (Zstring-append "\\end{" env "}"))
           (Zdisplay "\\end{" out)
           (Zdisplay env out)
           (Zdisplay "}" out))
          ((eq? typ 'plainregion)
           (Zdisplay "\\" out)
           (Zdisplay env out)
           (slatex.dump-display in out (Zstring-append "\\end" env))
           (Zdisplay "\\end" out)
           (Zdisplay env out))
          (else
           (let ((f (Scar slatex.*protected-files*)))
             (set! slatex.*protected-files* (Scdr slatex.*protected-files*))
             (Zcall-with-input-file
               f
               (lambda (in) (slatex.inline-protected-files in out)))
             (slatex.delete-file f))
           (cond ((Zmemq typ '(intext resultintext)) (slatex.dump-intext in #f))
                 ((Zmemq typ '(envdisplay envbox))
                  (slatex.dump-display in #f (Zstring-append "\\end{" env "}")))
                 ((Zmemq typ '(plaindisplay plainbox))
                  (slatex.dump-display in #f (Zstring-append "\\end" env)))
                 ((eq? typ 'input) (slatex.read-filename in))
                 (else (slatex.error 'slatex.inline-protected 1)))))))

(define (run1)
  (set! filesystem init-filesystem)
  (set! primary-aux-file-count -1)
  (set! secondary-aux-file-count -1)
  (slatex.process-main-tex-file "test")
  (map (lambda (x) (cons (Scar x) (Sstring-length (Scdr x))))
       filesystem))

(define expected-result '(("z56.tex" . 66) ("z56.scm" . 12) ("z55.tex" . 66) ("z55.scm" . 12) ("z54.tex" . 65) ("z54.scm" . 11) ("z53.tex" . 65) ("z53.scm" . 11) ("z52.tex" . 65) ("z52.scm" . 11) ("z51.tex" . 61) ("z51.scm" . 7) ("z50.tex" . 65) ("z50.scm" . 11) ("z49.tex" . 65) ("z49.scm" . 11) ("z48.tex" . 72) ("z48.scm" . 18) ("z47.tex" . 82) ("z47.scm" . 28) ("z46.tex" . 82) ("z46.scm" . 28) ("z45.tex" . 72) ("z45.scm" . 18) ("z44.tex" . 65) ("z44.scm" . 11) ("z43.tex" . 65) ("z43.scm" . 11) ("z42.tex" . 60) ("z42.scm" . 6) ("z41.tex" . 80) ("z41.scm" . 10) ("z40.tex" . 80) ("z40.scm" . 10) ("z39.tex" . 101) ("z39.scm" . 25) ("z38.tex" . 95) ("z38.scm" . 21) ("z37.tex" . 65) ("z37.scm" . 11) ("z36.tex" . 131) ("z36.scm" . 47) ("z35.tex" . 111) ("z35.scm" . 32) ("z34.tex" . 72) ("z34.scm" . 18) ("z33.tex" . 94) ("z33.scm" . 20) ("z32.tex" . 250) ("z32.scm" . 119) ("z31.tex" . 196) ("z31.scm" . 75) ("z30.tex" . 193) ("z30.scm" . 75) ("z29.tex" . 67) ("z29.scm" . 13) ("z28.tex" . 67) ("z28.scm" . 13) ("z27.tex" . 62) ("z27.scm" . 8) ("z26.tex" . 66) ("z26.scm" . 12) ("z25.tex" . 67) ("z25.scm" . 13) ("z24.tex" . 63) ("z24.scm" . 9) ("z23.tex" . 62) ("z23.scm" . 8) ("z22.tex" . 129) ("zz1.tex" . 70) ("zz1.scm" . 11) ("zz0.tex" . 77) ("z21.tex" . 58) ("z21.scm" . 4) ("z20.tex" . 59) ("z20.scm" . 5) ("z19.tex" . 78) ("z19.scm" . 15) ("z18.tex" . 103) ("z18.scm" . 34) ("z17.tex" . 75) ("z17.scm" . 12) ("z16.tex" . 68) ("z16.scm" . 9) ("z15.tex" . 323) ("z15.scm" . 143) ("z14.tex" . 93) ("z14.scm" . 24) ("z13.tex" . 594) ("z13.scm" . 288) ("z12.tex" . 256) ("z12.scm" . 107) ("z11.tex" . 58) ("z11.scm" . 4) ("z10.tex" . 247) ("z10.scm" . 89) ("z9.tex" . 58) ("z9.scm" . 4) ("z8.tex" . 56) ("z8.scm" . 2) ("z7.tex" . 75) ("z7.scm" . 11) ("z6.tex" . 120) ("z6.scm" . 36) ("z5.tex" . 55) ("z5.scm" . 1) ("z4.tex" . 55) ("z4.scm" . 1) ("z3.tex" . 56) ("z3.scm" . 2) ("z2.tex" . 57) ("z2.scm" . 2) ("z1.tex" . 60) ("z1.scm" . 6) ("z0.tex" . 60) ("z0.scm" . 6) ("test.tex" . 52853) ("slatex.sty" . 16460)))

(define (check result)
  (equal? result expected-result))

(define (run #!key (n (unknown 10000 1)))
;;  (statprof-start! '(profile))
  (let loop ((n n) (result #f))
    (if (SFX> n 0)
        (loop (SFX- n 1) (run1))
        (begin
;;          (statprof-stop!)
;;          (let ((output-dir "slatex-profile"))
;;            (statprof-write! output-dir)
;;            (println "To view the profile open " output-dir "/index.html"))
          result))))

;;(##declare (version-limit 0))(##include "gambit/gambit/statprof/statprof.scm")(##namespace (""))
