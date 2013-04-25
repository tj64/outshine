Thorsten Jolitz


Table of Contents
_________________

1 outshine.el --- outline with outshine outshines outline
.. 1.1 Copyright
.. 1.2 License
.. 1.3 Credits
.. 1.4 Commentary
..... 1.4.1 About outshine
..... 1.4.2 Installation
..... 1.4.3 Emacs Version
.. 1.5 ChangeLog


1 outshine.el --- outline with outshine outshines outline
=========================================================

1.1 Copyright
~~~~~~~~~~~~~

  Copyright (C) 2013 Thorsten Jolitz

  Authors: Thorsten Jolitz, Carsten Dominik, Per Abrahamsen
  Maintainer: Thorsten Jolitz <tjolitz AT gmail DOT com>
  Version: 0.9
  Keywords:  outlines, file structuring


1.2 License
~~~~~~~~~~~

  This file is not (yet) part of GNU Emacs

  This file is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation; either version 3, or (at your option) any
  later version.

  This file is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  General Public License for more details.

  For a full copy of the GNU General Public License see
  [http://www.gnu.org/licenses/].


1.3 Credits
~~~~~~~~~~~

  This library is based on, or rather an extension of, Carsten Dominik's
  `outline-magic' ([https://github.com/tj64/outline-magic]) and my own
  `outxxtra' ([https://github.com/tj64/outxxtra]), which is itself a
  modified extension of Per Abrahamsen's `out-xtra.el'
  ([http://tinyurl.com/aql9p97]). Some ideas were taken from Fabrice
  Niessen's '`.emacs'
  ([http://www.mygooglest.com/fni/dot-emacs.html#sec-2]).


1.4 Commentary
~~~~~~~~~~~~~~

1.4.1 About outshine
--------------------

  This library merges, modifies and extends two existing
  extension-libraries for `outline' (minor) mode: `outline-magic' and
  `out-xtra'. It offers all the functionality of `outline-magic' (with
  some tiny changes) and parts of the functionality of `out-xtra',
  together with some new features and ideas.

  See `outline-magic.el' ([https://github.com/tj64/outline-magic]) for
  detailled instructions on usage of the additional outline functions
  introduced by `outline-magic'.

  Furthermore, `outshine.el' includes functions and keybindings from
  `outline-mode-easy-bindings' (http://emacswiki.org/emacs/OutlineMinorMode).
  Unfortunately, no author is given for that library, so I cannot credit the
  person who wrote it.

  Outshines main purpose is to make `outline-minor-mode' more similar to
  outline-navigation and structure-editing with (the one-and-only)
  `Org-mode'. Furthermore, as additional but quite useful features,
  correctly structured outshine-buffers enable the use of `outorg.el'
  (subtree editing in temporary Org-mode buffers) and `navi-mode.el'
  (fast navigation and remote-control via modified occur-buffers).


1.4.2 Installation
------------------

  Download `outshine.el' and copy it to a location where Emacs can find
  it, and use this in your '.emacs' to get started:

  #+begin_src emacs-lisp
   (require 'outshine)
   (add-hook  'outline-minor-mode-hook 'outshine-hook-function)
  #+end_src

  If you like the functions and keybindings for 'M -<<arrow-key>>' navigation
  and visibility cycling copied from `outline-mode-easy-bindings', you might
  want to put the following code into your Emacs init file to have the same
  functionality/keybindings available in Org-mode too, overriding the less
  frequently used commands for moving and promoting/demoting subtrees:

  #+begin_src emacs-lisp
    (add-hook 'org-mode-hook
              (lambda ()
                ;; Redefine arrow keys, since promoting/demoting and moving
                ;; subtrees up and down are less frequent tasks then
                ;; navigation and visibility cycling
                (when (require 'outshine nil 'NOERROR)
                  (org-defkey org-mode-map
                              (kbd "M-<left>") 'outline-hide-more)
                  (org-defkey org-mode-map
                              (kbd "M-<right>") 'outline-show-more)
                  (org-defkey org-mode-map
                              (kbd "M-<up>") 'outline-previous-visible-heading)
                  (org-defkey org-mode-map
                              (kbd "M-<down>") 'outline-next-visible-heading)))
              'append)
  #+end_src

  Add this to your .emacs if, e.g., you always want outshine for emacs-lisp
  buffers (recommended):

  #+begin_src emacs-lisp
   (add-hook ‘emacs-lisp-mode-hook ‘outline-minor-mode)
  #+end_src

  If you want a different prefix key for outline-minor-mode, insert
  first:

  #+begin_src emacs-lisp
   (defvar outline-minor-mode-prefix "\C-c")
  #+end_src

  or whatever. The prefix can only be changed before outline (minor)
  mode is loaded.


1.4.3 Emacs Version
-------------------

  `outshine.el' works with [GNU Emacs 24.2.1 (x86_64-unknown-linux-gnu,
  GTK+ Version 3.6.4) of 2013-01-20 on eric]. No attempts of testing
  with older versions or other types of Emacs have been made (yet).


1.5 ChangeLog
~~~~~~~~~~~~~

   date            author(s)          version 
  -------------------------------------------------
   2013-02-20 Mi   Thorsten Jolitz      0.9 
