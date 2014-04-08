- [outshine.el &#x2014; outline with outshine outshines outline](#outshine.el-&#x2014;-outline-with-outshine-outshines-outline)
  - [MetaData](#metadata)
  - [Commentary](#commentary)
    - [About outshine](#about-outshine)
    - [Installation](#installation)
    - [Emacs Version](#emacs-version)
  - [ChangeLog](#changelog)



# outshine.el &#x2014; outline with outshine outshines outline

Author: Thorsten Jolitz <tjolitz AT gmail DOT com>
Version: 1.0
URL: <https://github.com/tj64/outshine>

## MetaData

    copyright: Thorsten_Jolitz
    
    copyright-from: 2013+
    
    version: 1.0
    
    licence: GPL 2 or later (free software)
    
    licence-url: http://www.gnu.org/licenses/
    
    part-of-emacs: no
    
    authors: Thorsten_Jolitz Carsten_Dominik Per_Abrahamsen
    
    author_email: tjolitz AT gmail DOT com
    
    credits: Fabrice_Niessen Alexander_Vorobiev Jonas_Bernoulli
    
    inspiration: outline-magic outxxtra out-xtra
    
    keywords: emacs outlines file_structuring
    
    git-repo: https://github.com/tj64/outshine.git
    
    git-clone: git://github.com/tj64/outshine.git

## Commentary

### About outshine

[NOTE: For the sake of adding this library to MELPA, headlines
had to be converted back from 'Org-mode style' to 'oldschool',
and a few extra lines of required information had to be added on
top of the MetaData section - just to comply with the required
file formatting. All outshine, outorg and navi-mode functionality
still works with this file. See my
[iOrg](https://github.com/tj64/iorg) repository for examples of
Emacs-Lisp and PicoLisp files structured 'the outshine way'.]

This library merges, modifies and extends two existing
extension-libraries for \`outline' (minor) mode: \`outline-magic'
(by Carsten Dominik) and \`out-xtra' (by Per Abrahamsen). It
offers all the functionality of \`outline-magic' (with some tiny
changes) and parts of the functionality of \`out-xtra', together
with some new features and ideas.

See \`outline-magic.el' (<https://github.com/tj64/outline-magic>)
for detailled instructions on usage of the additional outline
functions introduced by \`outline-magic'.

Furthermore, \`outshine.el' includes functions and keybindings
from \`outline-mode-easy-bindings'
(<http://emacswiki.org/emacs/OutlineMinorMode>).  Unfortunately, no
author is given for that library, so I cannot credit the person
who wrote it.

Outshine's main purpose is to make \`outline-minor-mode' more
similar to outline-navigation and structure-editing with (the
one-and-only) \`Org-mode'. Furthermore, as additional but quite
useful features, correctly structured outshine-buffers enable the
use of \`outorg.el' (subtree editing in temporary Org-mode
buffers) and \`navi-mode.el' (fast navigation and remote-control
via modified occur-buffers).

### Installation

Download \`outshine.el' and copy it to a location where Emacs can
find it, and use this in your '.emacs' to get started:

    (require 'outshine)
    (add-hook 'outline-minor-mode-hook 'outshine-hook-function)

If you like the functions and keybindings for 'M -<a id="arrow-key"></a>'
navigation and visibility cycling copied from
\`outline-mode-easy-bindings', you might want to put the following
code into your Emacs init file to have the same
functionality/keybindings available in Org-mode too, overriding
the less frequently used commands for moving and
promoting/demoting subtrees:

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

Add this to your .emacs if, e.g., you always want outshine for
emacs-lisp buffers (recommended):

    (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)

If you want a different prefix key for outline-minor-mode, insert first:

    (defvar outline-minor-mode-prefix "\C-c")

or

    (defvar outline-minor-mode-prefix "\M-#")

or whatever. The prefix can only be changed before outline
(minor) mode is loaded.

### Emacs Version

\`outshine.el' works with [GNU Emacs 24.2.1
(x86\_64-unknown-linux-gnu, GTK+ Version 3.6.4) of 2013-01-20 on
eric]. No attempts of testing with older versions or other types
of Emacs have been made (yet).

## ChangeLog

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="right" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">date</th>
<th scope="col" class="left">author(s)</th>
<th scope="col" class="right">version</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left"><span class="timestamp-wrapper"><span class="timestamp">&lt;2013-05-03 Fr&gt;</span></span></td>
<td class="left">Thorsten Jolitz</td>
<td class="right">1.0</td>
</tr>


<tr>
<td class="left"><span class="timestamp-wrapper"><span class="timestamp">&lt;2013-02-20 Mi&gt;</span></span></td>
<td class="left">Thorsten Jolitz</td>
<td class="right">0.9</td>
</tr>
</tbody>
</table>
