;; * outshine.el --- outline with outshine outshines outline
;;   :PROPERTIES:
;;   :copyright: Thorsten_Jolitz
;;   :copyright-years: 2013
;;   :version:  1.0
;;   :licence:  GPL 2 or later (free software)
;;   :licence-url: http://www.gnu.org/licenses/
;;   :part-of-emacs: no
;;   :authors: Thorsten_Jolitz Carsten_Dominik Per_Abrahamsen
;;   :author_email: tjolitz AT gmail DOT com
;;   :credits:  Fabrice_Niessen Alexander_Vorobiev Jonas_Bernoulli
;;   :inspiration: outline-magic outxxtra out-xtra
;;   :keywords: emacs outlines file_structuring
;;   :git-repo: https://github.com/tj64/outshine.git
;;   :git-clone: git://github.com/tj64/outshine.git
;;   :END:

;; ** Commentary

;; *** About outshine

;; This library merges, modifies and extends two existing extension-libraries
;; for `outline' (minor) mode: `outline-magic' (by Carsten Dominik) and
;; `out-xtra' (by Per Abrahamsen). It offers all the functionality of
;; `outline-magic' (with some tiny changes) and parts of the functionality of
;; `out-xtra', together with some new features and ideas.

;; See `outline-magic.el' (https://github.com/tj64/outline-magic) for
;; detailled instructions on usage of the additional outline functions
;; introduced by `outline-magic'.

;; Furthermore, `outshine.el' includes functions and keybindings from
;; `outline-mode-easy-bindings' (http://emacswiki.org/emacs/OutlineMinorMode).
;; Unfortunately, no author is given for that library, so I cannot credit the
;; person who wrote it.

;; Outshine's main purpose is to make `outline-minor-mode' more similar to
;; outline-navigation and structure-editing with (the one-and-only)
;; `Org-mode'. Furthermore, as additional but quite useful features, correctly
;; structured outshine-buffers enable the use of `outorg.el' (subtree editing
;; in temporary Org-mode buffers) and `navi-mode.el' (fast navigation and
;; remote-control via modified occur-buffers).

;; *** Installation

;; Download `outshine.el' and copy it to a location where Emacs can find it,
;; and use this in your '.emacs' to get started:

;; # #+begin_src emacs-lisp
;; #   (require 'outshine)
;; #   (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
;; # #+end_src

;; If you like the functions and keybindings for 'M -<<arrow-key>>' navigation
;; and visibility cycling copied from `outline-mode-easy-bindings', you might
;; want to put the following code into your Emacs init file to have the same
;; functionality/keybindings available in Org-mode too, overriding the less
;; frequently used commands for moving and promoting/demoting subtrees:

;; # #+begin_src emacs-lisp
;; #   (add-hook 'org-mode-hook
;; #             (lambda ()
;; #               ;; Redefine arrow keys, since promoting/demoting and moving
;; #               ;; subtrees up and down are less frequent tasks then
;; #               ;; navigation and visibility cycling
;; #               (when (require 'outshine nil 'NOERROR)
;; #                 (org-defkey org-mode-map
;; #                             (kbd "M-<left>") 'outline-hide-more)
;; #                 (org-defkey org-mode-map
;; #                             (kbd "M-<right>") 'outline-show-more)
;; #                 (org-defkey org-mode-map
;; #                             (kbd "M-<up>") 'outline-previous-visible-heading)
;; #                 (org-defkey org-mode-map
;; #                             (kbd "M-<down>") 'outline-next-visible-heading)))
;; #             'append)
;; # #+end_src

;; Add this to your .emacs if, e.g., you always want outshine for emacs-lisp
;; buffers (recommended):

;; # #+begin_src emacs-lisp
;; #   (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
;; # #+end_src

;; If you want a different prefix key for outline-minor-mode, insert first:

;; # #+begin_src emacs-lisp
;; #  (defvar outline-minor-mode-prefix "\C-c")
;; # #+end_src

;; or whatever. The prefix can only be changed before outline (minor) mode is
;; loaded.

;; *** Emacs Version

;; `outshine.el' works with [GNU Emacs 24.2.1 (x86_64-unknown-linux-gnu, GTK+
;; Version 3.6.4) of 2013-01-20 on eric]. No attempts of testing with older
;; versions or other types of Emacs have been made (yet).


;; ** ChangeLog

;; | date            | author(s)       | version |
;; |-----------------+-----------------+---------|
;; | <2013-05-03 Fr> | Thorsten Jolitz |     1.0 |
;; | <2013-02-20 Mi> | Thorsten Jolitz |     0.9 |

;; * Requires

(require 'outline)
;; necessary before Emacs 24.3
(require 'newcomment)

;; * Variables
;; ** Consts

(defconst outshine-version "1.0"
  "outshine version number.")

;; copied from org-source.el
(defconst outshine-level-faces
  '(outshine-level-1 outshine-level-2 outshine-level-3 outshine-level-4
                     outshine-level-5 outshine-level-6 outshine-level-7
                     outshine-level-8))

(defconst outshine-outline-heading-end-regexp "\n"
  "Global default value of `outline-heading-end-regexp'.
Used to override any major-mode specific file-local settings")

(defconst outshine-oldschool-elisp-outline-regexp-base "[;]+"
  "Oldschool Emacs Lisp base for calculating the outline-regexp")

;; ** Vars

(defvar outline-minor-mode-prefix "\C-c"
  "New outline-minor-mode prefix.
Does not really take effect when set in the `outshine' library.
Instead, it must be set in your init file *before* the `outline'
library is loaded, see the installation tips in the comment
section of `outshine'.")

;; from `outline-magic'
(defvar outline-promotion-headings nil
  "A sorted list of headings used for promotion/demotion commands.
Set this to a list of headings as they are matched by `outline-regexp',
top-level heading first.  If a mode or document needs several sets of
outline headings (for example numbered and unnumbered sections), list
them set by set, separated by a nil element.  See the example for
`texinfo-mode' in the file commentary.")
(make-variable-buffer-local 'outline-promotion-headings)

(defvar outshine-delete-leading-whitespace-from-outline-regexp-base-p nil
  "If non-nil, delete leading whitespace from outline-regexp-base.")
(make-variable-buffer-local
 'outshine-delete-leading-whitespace-from-outline-regexp-base-p)

(defvar outshine-enforce-no-comment-padding-p nil
  "If non-nil, make sure no comment-padding is used in heading.")
(make-variable-buffer-local
 'outshine-enforce-no-comment-padding-p)

(defvar outshine-outline-regexp-base ""
  "Actual base for calculating the outline-regexp")

(defvar outshine-normalized-comment-start ""
  "Comment-start regexp without leading and trailing whitespace")
(make-variable-buffer-local
 'outshine-normalized-comment-start)

(defvar outshine-normalized-comment-end ""
  "Comment-end regexp without leading and trailing whitespace")
(make-variable-buffer-local
 'outshine-normalized-comment-end)

(defvar outshine-normalized-outline-regexp-base ""
  "Outline-regex-base without leading and trailing whitespace")
(make-variable-buffer-local
 'outshine-normalized-outline-regexp-base)

;; show number of hidden lines in folded subtree
(defvar outshine-show-hidden-lines-cookies-p nil
  "If non-nil, commands for hidden-lines cookies are activated.")

;; remember if hidden-lines cookies are shown or hidden
(defvar outshine-hidden-lines-cookies-on-p nil
  "If non-nil, hidden-lines cookies are shown, otherwise hidden.")


;; ** Hooks

(defvar outshine-hook nil
  "Functions to run after `outshine' is loaded.")

;; ** Faces

;; from `org-compat.el'
(defun outshine-compatible-face (inherits specs)
  "Make a compatible face specification.
If INHERITS is an existing face and if the Emacs version supports it,
just inherit the face.  If INHERITS is set and the Emacs version does
not support it, copy the face specification from the inheritance face.
If INHERITS is not given and SPECS is, use SPECS to define the face.
XEmacs and Emacs 21 do not know about the `min-colors' attribute.
For them we convert a (min-colors 8) entry to a `tty' entry and move it
to the top of the list.  The `min-colors' attribute will be removed from
any other entries, and any resulting duplicates will be removed entirely."
  (when (and inherits (facep inherits) (not specs))
    (setq specs (or specs
		    (get inherits 'saved-face)
		    (get inherits 'face-defface-spec))))
  (cond   ((and inherits (facep inherits)
	 (not (featurep 'xemacs))
	 (>= emacs-major-version 22)
	 ;; do not inherit outline faces before Emacs 23
	 (or (>= emacs-major-version 23)
	     (not (string-match "\\`outline-[0-9]+"
				(symbol-name inherits)))))
    (list (list t :inherit inherits)))
   ((or (featurep 'xemacs) (< emacs-major-version 22))
    ;; These do not understand the `min-colors' attribute.
    (let (r e a)
      (while (setq e (pop specs))
	(cond
	 ((memq (car e) '(t default)) (push e r))
	 ((setq a (member '(min-colors 8) (car e)))
	  (nconc r (list (cons (cons '(type tty) (delq (car a) (car e)))
			       (cdr e)))))
	 ((setq a (assq 'min-colors (car e)))
	  (setq e (cons (delq a (car e)) (cdr e)))
	  (or (assoc (car e) r) (push e r)))
	 (t (or (assoc (car e) r) (push e r)))))
      (nreverse r)))
   (t specs)))
(put 'outshine-compatible-face 'lisp-indent-function 1)

;; The following face definitions are from `org-faces.el'
;; originally copied from font-lock-function-name-face
(defface outshine-level-1
  (outshine-compatible-face 'outline-1
    '((((class color) (min-colors 88)
        (background light)) (:foreground "Blue1"))
      (((class color) (min-colors 88)
        (background dark)) (:foreground "LightSkyBlue"))
      (((class color) (min-colors 16)
        (background light)) (:foreground "Blue"))
      (((class color) (min-colors 16)
        (background dark)) (:foreground "LightSkyBlue"))
      (((class color) (min-colors 8)) (:foreground "blue" :bold t))
      (t (:bold t))))
  "Face used for level 1 headlines."
  :group 'outshine-faces)

;; originally copied from font-lock-variable-name-face
(defface outshine-level-2
  (outshine-compatible-face 'outline-2
    '((((class color) (min-colors 16)
        (background light)) (:foreground "DarkGoldenrod"))
      (((class color) (min-colors 16)
        (background dark))  (:foreground "LightGoldenrod"))
      (((class color) (min-colors 8)
        (background light)) (:foreground "yellow"))
      (((class color) (min-colors 8)
        (background dark))  (:foreground "yellow" :bold t))
      (t (:bold t))))
  "Face used for level 2 headlines."
  :group 'outshine-faces)

;; originally copied from font-lock-keyword-face
(defface outshine-level-3
  (outshine-compatible-face 'outline-3
    '((((class color) (min-colors 88)
        (background light)) (:foreground "Purple"))
      (((class color) (min-colors 88)
        (background dark))  (:foreground "Cyan1"))
      (((class color) (min-colors 16)
        (background light)) (:foreground "Purple"))
      (((class color) (min-colors 16)
        (background dark))  (:foreground "Cyan"))
      (((class color) (min-colors 8)
        (background light)) (:foreground "purple" :bold t))
      (((class color) (min-colors 8)
        (background dark))  (:foreground "cyan" :bold t))
      (t (:bold t))))
  "Face used for level 3 headlines."
  :group 'outshine-faces)

   ;; originally copied from font-lock-comment-face
(defface outshine-level-4
  (outshine-compatible-face 'outline-4
    '((((class color) (min-colors 88)
        (background light)) (:foreground "Firebrick"))
      (((class color) (min-colors 88)
        (background dark))  (:foreground "chocolate1"))
      (((class color) (min-colors 16)
        (background light)) (:foreground "red"))
      (((class color) (min-colors 16)
        (background dark))  (:foreground "red1"))
      (((class color) (min-colors 8)
        (background light))  (:foreground "red" :bold t))
      (((class color) (min-colors 8)
        (background dark))   (:foreground "red" :bold t))
      (t (:bold t))))
  "Face used for level 4 headlines."
  :group 'outshine-faces)

 ;; originally copied from font-lock-type-face
(defface outshine-level-5
  (outshine-compatible-face 'outline-5
    '((((class color) (min-colors 16)
        (background light)) (:foreground "ForestGreen"))
      (((class color) (min-colors 16)
        (background dark)) (:foreground "PaleGreen"))
      (((class color) (min-colors 8)) (:foreground "green"))))
  "Face used for level 5 headlines."
  :group 'outshine-faces)

 ;; originally copied from font-lock-constant-face
(defface outshine-level-6
  (outshine-compatible-face 'outline-6
    '((((class color) (min-colors 16)
        (background light)) (:foreground "CadetBlue"))
      (((class color) (min-colors 16)
        (background dark)) (:foreground "Aquamarine"))
      (((class color) (min-colors 8)) (:foreground "magenta")))) "Face used for level 6 headlines."
  :group 'outshine-faces)

 ;; originally copied from font-lock-builtin-face
(defface outshine-level-7
  (outshine-compatible-face 'outline-7
    '((((class color) (min-colors 16)
        (background light)) (:foreground "Orchid"))
      (((class color) (min-colors 16)
        (background dark)) (:foreground "LightSteelBlue"))
      (((class color) (min-colors 8)) (:foreground "blue"))))
  "Face used for level 7 headlines."
  :group 'outshine-faces)

 ;; originally copied from font-lock-string-face
(defface outshine-level-8
  (outshine-compatible-face 'outline-8
    '((((class color) (min-colors 16)
        (background light)) (:foreground "RosyBrown"))
      (((class color) (min-colors 16)
        (background dark)) (:foreground "LightSalmon"))
      (((class color) (min-colors 8)) (:foreground "green"))))
  "Face used for level 8 headlines."
  :group 'outshine-faces)

;; ** Customs
;; *** Custom Groups

(defgroup outshine nil
  "Enhanced library for outline navigation in source code buffers."
  :prefix "outshine-"
  :group 'lisp)

(defgroup outshine-faces nil
  "Faces in Outshine."
  :tag "Outshine Faces"
  :group 'outshine)


;; *** Custom Vars

;; from `org'
(defcustom outshine-fontify-whole-heading-line nil
  "Non-nil means fontify the whole line for headings.
This is useful when setting a background color for the
poutshine-level-* faces."
  :group 'outshine
  :type 'boolean)

(defcustom outshine-outline-regexp-outcommented-p t
  "Non-nil if regexp-base is outcommented to calculate outline-regexp."
  :group 'outshine
  :type 'boolean)

(defcustom outshine-outline-regexp-special-chars "[][+]"
  "Regexp for detecting (special) characters in outline-regexp.
These special chars will be stripped when the outline-regexp is
transformed into a string, e.g. when the outline-string for a
certain level is calculated. "
  :group 'outshine
  :type 'regexp)

;; from `outline-magic'
(defcustom outline-cycle-emulate-tab nil
  "Where should `outline-cycle' emulate TAB.
nil    Never
white  Only in completely white lines
t      Everywhere except in headlines"
  :group 'outlines
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Only in completely white lines" white)
		 (const :tag "Everywhere except in headlines" t)
		 ))

;; from `outline-magic'
(defcustom outline-structedit-modifiers '(meta)
  "List of modifiers for outline structure editing with the arrow keys."
  :group 'outlines
  :type '(repeat symbol))

;; startup options
(defcustom outshine-startup-folded-p nil
  "Non-nil means files will be opened with all but top level headers folded."
  :group 'outshine
  :type 'boolean)

(defcustom outshine-hidden-lines-cookie-left-delimiter "["
  "Left delimiter of cookie that shows number of hidden lines."
  :group 'outshine
  :type 'string)

(defcustom outshine-hidden-lines-cookie-right-delimiter "]"
  "Left delimiter of cookie that shows number of hidden lines."
  :group 'outshine
  :type 'string)

(defcustom outshine-hidden-lines-cookie-left-signal-char "#"
  "Left signal character of cookie that shows number of hidden lines."
  :group 'outshine
  :type 'string)

(defcustom outshine-hidden-lines-cookie-right-signal-char ""
  "Right signal character of cookie that shows number of hidden lines."
  :group 'outshine
  :type 'string)

(defcustom outshine-regexp-base-char "*"
  "Character used in outline-regexp base."
  :group 'outshine
  :type 'string)

;; old regexp: "[*]+"
(defvar outshine-default-outline-regexp-base 
  (format "[%s]+" outshine-regexp-base-char)
  "Default base for calculating the outline-regexp")

;; TODO delete this line  "\\(\\[\\)\\([[:digit:]+]\\)\\( L\\]\\)"
(defvar outshine-hidden-lines-cookie-format-regexp
  (concat
   "\\( "
   (regexp-quote outshine-hidden-lines-cookie-left-delimiter)
   (regexp-quote outshine-hidden-lines-cookie-left-signal-char)
   "\\)"
   "\\([[:digit:]]+\\)"
   "\\("
   (regexp-quote outshine-hidden-lines-cookie-right-signal-char)
   ;; FIXME robust enough?
   (format "\\%s" outshine-hidden-lines-cookie-right-delimiter)
   "\\)")
  "Matches cookies that show number of hidden lines for folded subtrees.")

;; * Defuns
;; ** Functions
;; *** Define keys with fallback

;; copied and adapted from Alexander Vorobiev
;; http://www.mail-archive.com/emacs-orgmode@gnu.org/msg70648.html
(defmacro outshine-define-key-with-fallback
  (keymap key def condition &optional mode)
  "Define key with fallback.
Binds KEY to definition DEF in keymap KEYMAP, the binding is
active when the CONDITION is true. Otherwise turns MODE off and
re-enables previous definition for KEY. If MODE is nil, tries to
recover it by stripping off \"-map\" from KEYMAP name."
  `(define-key
     ,keymap
     ,key
     (lambda ()
       (interactive)
       (if ,condition ,def
         (let* ((,(if mode mode
                    (let* ((keymap-str (symbol-name keymap))
                           (mode-name-end
                            (- (string-width keymap-str) 4)))
                      (if (string=
                           "-map"
                           (substring keymap-str mode-name-end))
                          (intern (substring keymap-str 0 mode-name-end))
                        (message
                         "Could not deduce mode name from keymap name")
                        (intern "dummy-sym"))
                      )) nil)
                (original-func (key-binding ,key)))
           (condition-case nil
               (call-interactively original-func)
             (error nil)))))))

;; *** Normalize regexps

;; from http://emacswiki.org/emacs/ElispCookbook#toc6
(defun outshine-chomp (str)
  "Chomp leading and trailing whitespace from STR."
  (save-excursion
    (save-match-data
      (while (string-match
              "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
              str)
        (setq str (replace-match "" t t str)))
      str)))

(defun outshine-set-outline-regexp-base ()
  "Return the actual outline-regexp-base."
  (if (and
       (not (outshine-modern-header-style-in-elisp-p))
       (eq major-mode 'emacs-lisp-mode))
      (progn
        (setq outshine-enforce-no-comment-padding-p t)
        (setq outshine-outline-regexp-base
              outshine-oldschool-elisp-outline-regexp-base))
    (setq outshine-enforce-no-comment-padding-p nil)
    (setq outshine-outline-regexp-base
          outshine-default-outline-regexp-base)))

(defun outshine-normalize-regexps ()
  "Chomp leading and trailing whitespace from outline regexps."
  (and comment-start
       (setq outshine-normalized-comment-start
             (outshine-chomp comment-start)))
  (and comment-end
       (setq outshine-normalized-comment-end
             (outshine-chomp comment-end)))
  (and outshine-outline-regexp-base
       (setq outshine-normalized-outline-regexp-base
             (outshine-chomp outshine-outline-regexp-base))))

;; *** Calculate outline-regexp and outline-level

;; dealing with special case of oldschool headers in elisp (;;;+)
(defun outshine-modern-header-style-in-elisp-p (&optional buffer)
  "Return nil, if there is no match for a outshine-style header.
Searches in BUFFER if given, otherwise in current buffer."
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (re-search-forward
         (format "^;; [%s]+ " outshine-regexp-base-char)
         nil 'NOERROR)))))

(defun outshine-calc-comment-region-starter ()
  "Return comment-region starter as string.
Based on `comment-start' and `comment-add'."
  (if (or (not comment-add) (eq comment-add 0))
      outshine-normalized-comment-start
    (let ((comment-add-string outshine-normalized-comment-start))
      (dotimes (i comment-add comment-add-string)
        (setq comment-add-string
              (concat comment-add-string outshine-normalized-comment-start))))))

(defun outshine-calc-comment-padding ()
  "Return comment-padding as string"
  (cond
   ;; comment-padding is nil
   ((not comment-padding) " ")
   ;; comment-padding is integer
   ((integer-or-marker-p comment-padding)
    (let ((comment-padding-string ""))
      (dotimes (i comment-padding comment-padding-string)
        (setq comment-padding-string
              (concat comment-padding-string " ")))))
   ;; comment-padding is string
   ((stringp comment-padding)
    comment-padding)
   (t (error "No valid comment-padding"))))


;; FIXME: no comment padding if oldschool elisp regexp-base
(defun outshine-calc-outline-regexp ()
  "Calculate the outline regexp for the current mode."
  (concat
   (and outshine-outline-regexp-outcommented-p
         ;; regexp-base outcommented, but no 'comment-start' defined
         (or comment-start
             (message (concat
                       "Cannot calculate outcommented outline-regexp\n"
                       "without 'comment-start' character defined!")))
         (concat
          ;; comment-start
          (outshine-calc-comment-region-starter)
          ;; comment-padding
          (if outshine-enforce-no-comment-padding-p
              ""
            (outshine-calc-comment-padding))))
   ;; regexp-base
   outshine-normalized-outline-regexp-base
   " "))

;; TODO how is this called (match-data?) 'looking-at' necessary?
(defun outshine-calc-outline-level ()
  "Calculate the right outline level for the outshine-outline-regexp"
  (save-excursion
    (save-match-data
      (and
       (looking-at (outshine-calc-outline-regexp))
       (let ((m-strg (match-string-no-properties 0)))
         (if outshine-enforce-no-comment-padding-p
             ;; deal with oldschool elisp headings (;;;+)
             (setq m-strg
                   (split-string
                    (substring m-strg 2)
                    nil
                    'OMIT-NULLS))
           ;; orgmode style elisp heading (;; *+)
           (setq m-strg
                 (split-string
                  m-strg
                  (format "%s" outshine-normalized-comment-start)
                  'OMIT-NULLS)))
         (length
          (mapconcat
           (lambda (str)
             (car
              (split-string
               str
               " "
               'OMIT-NULLS)))
           m-strg
           "")))))))

;; *** Set outline-regexp und outline-level

(defun outshine-set-local-outline-regexp-and-level
  (start-regexp &optional fun end-regexp)
   "Set `outline-regexp' locally to START-REGEXP.
Set optionally `outline-level' to FUN and
`outline-heading-end-regexp' to END-REGEXP."
	(make-local-variable 'outline-regexp)
	(setq outline-regexp start-regexp)
	(and fun
             (make-local-variable 'outline-level)
             (setq outline-level fun))
      	(and end-regexp
             (make-local-variable 'outline-heading-end-regexp)
             (setq outline-heading-end-regexp end-regexp)))

;; *** Show number of lines in hidden body

;; Calc and show line number of hidden body for all visible headlines
(defun outshine-write-hidden-lines-cookies ()
  "Show line number of hidden lines in folded headline."
  (and outshine-show-hidden-lines-cookies-p
       (save-excursion
         (goto-char (point-min))
         (and (outline-on-heading-p)
              (outshine-hidden-lines-cookie-status-changed-p)
              (outshine-set-hidden-lines-cookie))
         (while (not (eobp))
           (outline-next-visible-heading 1)
           (and (outline-on-heading-p)
                (outshine-hidden-lines-cookie-status-changed-p)
                (outshine-set-hidden-lines-cookie))))))

(defun outshine-hidden-lines-cookie-status-changed-p ()
  "Return non-nil if hidden-lines cookie needs modification."
  (save-excursion
    (save-match-data
      (or (not (outline-body-visible-p))
          (re-search-forward
           outshine-hidden-lines-cookie-format-regexp
           (line-end-position)
           'NO-ERROR)))))

(defun outshine-set-hidden-lines-cookie ()
  "Calculate and set number of hidden lines in folded headline."
  (let* ((folded-p (not (outline-body-visible-p)))
         (line-num-current-header (line-number-at-pos))
         (line-num-next-visible-header
          (save-excursion
            (outline-next-visible-heading 1)
            (line-number-at-pos)))
         (body-lines
          (1- (- line-num-next-visible-header line-num-current-header))))
    (if (re-search-forward
         outshine-hidden-lines-cookie-format-regexp
         (line-end-position)
         'NO-ERROR)
        (cond
         ((not folded-p) (replace-match ""))
         (folded-p (replace-match (format "%s" body-lines) nil nil nil 2)))
      (show-entry)
      (save-excursion
        (end-of-line)
        (insert
         (format
          " %s%s%s%s%s"
          outshine-hidden-lines-cookie-left-delimiter
          outshine-hidden-lines-cookie-left-signal-char
          body-lines
          outshine-hidden-lines-cookie-right-signal-char
          outshine-hidden-lines-cookie-right-delimiter)))
      (hide-entry))))

;; ;; FIXME
;; ;; outline-flag-region: Variable binding depth exceeds max-specpdl-size
;; (add-hook 'outline-view-change-hook
;;           'outshine-write-hidden-lines-cookies)

;; *** Return outline-string at given level

(defun outshine-calc-outline-string-at-level (level)
  "Return outline-string at level LEVEL."
  (let ((base-string (outshine-calc-outline-base-string-at-level level)))
    (if (not outshine-outline-regexp-outcommented-p)
        base-string
      (concat (outshine-calc-comment-region-starter)
              (if outshine-enforce-no-comment-padding-p
                  ""
                (outshine-calc-comment-padding))
              base-string
              " "))))

(defun outshine-calc-outline-base-string-at-level (level)
  "Return outline-base-string at level LEVEL."
  (let* ((star (outshine-transform-normalized-outline-regexp-base-to-string))
         (stars star))
       (dotimes (i (1- level) stars)
         (setq stars (concat stars star)))))

(defun outshine-transform-normalized-outline-regexp-base-to-string ()
  "Transform 'outline-regexp-base' to string by stripping off special chars."
  (replace-regexp-in-string
   outshine-outline-regexp-special-chars
   ""
   outshine-normalized-outline-regexp-base))

;; make demote/promote from `outline-magic' work
(defun outshine-make-promotion-headings-list (max-level)
  "Make a sorted list of headings used for promotion/demotion commands.
Set this to a list of MAX-LEVEL headings as they are matched by `outline-regexp',
top-level heading first."
  (let ((list-of-heading-levels
         `((,(outshine-calc-outline-string-at-level 1) . 1))))
    (dotimes (i (1- max-level) list-of-heading-levels)
            (add-to-list
             'list-of-heading-levels
             `(,(outshine-calc-outline-string-at-level (+ i 2)) . ,(+ i 2))
             'APPEND))))

;; *** Fontify the headlines

(defun outshine-fontify-headlines (outline-regexp)
  ;; (interactive)
  ;; (setq outline-regexp (tj/outline-regexp))

  ;; highlight the headings
  ;; see http://www.gnu.org/software/emacs/manual/html_node/emacs/Font-Lock.html
  ;; use `M-x customize-apropos-faces' to customize faces
  ;; to find the corresponding face for each outline level, see
  ;; `org-faces.el'

  ;; Added `\n?', after having read the following chunk of code (from org.el):
  ;; `(,(if org-fontify-whole-heading-line
  ;;        "^\\(\\**\\)\\(\\* \\)\\(.*\n?\\)"
  ;;      "^\\(\\**\\)\\(\\* \\)\\(.*\\)")

  (let ((outshine-fontify-whole-heading-line "") ; "\n?")
        (heading-1-regexp
         ;; deal with special case 'oldschool elisp headers'
         (if outshine-enforce-no-comment-padding-p
             "^;;; \\(.*\\)"
           (concat (substring outline-regexp 0 -1)
                   "\\{1\\} \\(.*" outshine-fontify-whole-heading-line "\\)")))
        (heading-2-regexp
         (if outshine-enforce-no-comment-padding-p
             "^;;;; \\(.*\\)"
           (concat (substring outline-regexp 0 -1)
                   "\\{2\\} \\(.*" outshine-fontify-whole-heading-line "\\)")))
        (heading-3-regexp
         (if outshine-enforce-no-comment-padding-p
             "^;;;;; \\(.*\\)"
           (concat (substring outline-regexp 0 -1)
                   "\\{3\\} \\(.*" outshine-fontify-whole-heading-line "\\)")))
        (heading-4-regexp
         (if outshine-enforce-no-comment-padding-p
             "^;;;;;; \\(.*\\)"
           (concat (substring outline-regexp 0 -1)
                   "\\{4,\\} \\(.*" outshine-fontify-whole-heading-line "\\)")))
        (heading-5-regexp
         (if outshine-enforce-no-comment-padding-p
             "^;;;;;;; \\(.*\\)"
           (concat (substring outline-regexp 0 -1)
                   "\\{5\\} \\(.*" outshine-fontify-whole-heading-line "\\)")))
        (heading-6-regexp
         (if outshine-enforce-no-comment-padding-p
             "^;;;;;;;; \\(.*\\)"
           (concat (substring outline-regexp 0 -1)
                   "\\{6,\\} \\(.*" outshine-fontify-whole-heading-line "\\)")))
        (heading-7-regexp
         (if outshine-enforce-no-comment-padding-p
             "^;;;;;;;;; \\(.*\\)"
           (concat (substring outline-regexp 0 -1)
                   "\\{7,\\} \\(.*" outshine-fontify-whole-heading-line "\\)")))
        (heading-8-regexp
         (if outshine-enforce-no-comment-padding-p
             "^;;;;;;;;;; \\(.*\\)"
           (concat (substring outline-regexp 0 -1)
                   "\\{8,\\} \\(.*" outshine-fontify-whole-heading-line "\\)"))))
    (font-lock-add-keywords
     nil
     `((,heading-1-regexp 1 'outshine-level-1 t)
       (,heading-2-regexp 1 'outshine-level-2 t)
       (,heading-3-regexp 1 'outshine-level-3 t)
       (,heading-4-regexp 1 'outshine-level-4 t)
       (,heading-5-regexp 1 'outshine-level-5 t)
       (,heading-6-regexp 1 'outshine-level-6 t)
       (,heading-7-regexp 1 'outshine-level-7 t)
       (,heading-8-regexp 1 'outshine-level-8 t)))))

;; *** Outshine hook-function

(defun outshine-hook-function ()
  "Add this function to outline-minor-mode-hook"
  (outshine-set-outline-regexp-base)
  (outshine-normalize-regexps)
  (let ((out-regexp (outshine-calc-outline-regexp)))
    (outshine-set-local-outline-regexp-and-level
     out-regexp
     'outshine-calc-outline-level
     outshine-outline-heading-end-regexp)
    (outshine-fontify-headlines out-regexp)
    (setq outline-promotion-headings
          (outshine-make-promotion-headings-list 8)))
  (when outshine-startup-folded-p
    (condition-case error-data
        (outline-hide-sublevels 1)
      ('error (message "No outline structure detected")))))

;; ;; add this to your .emacs
;; (add-hook 'outline-minor-mode-hook 'outshine-hook-function)

;; *** Additional outline functions
;; **** Functions from `outline-magic'

(defun outline-cycle-emulate-tab ()
  "Check if TAB should be emulated at the current position."
  ;; This is called after the check for point in a headline,
  ;; so we can assume we are not in a headline
  (if (and (eq outline-cycle-emulate-tab 'white)
	   (save-excursion
	     (beginning-of-line 1) (looking-at "[ \t]+$")))
      t
    outline-cycle-emulate-tab))

(defun outline-change-level (delta)
  "Workhorse for `outline-demote' and `outline-promote'."
  (let* ((headlist (outline-headings-list))
	 (atom (outline-headings-atom headlist))
	 (re (concat "^" outline-regexp))
	 (transmode (and transient-mark-mode mark-active))
	 beg end)

    ;; Find the boundaries for this operation
    (save-excursion
      (if transmode
	  (setq beg (min (point) (mark))
		end (max (point) (mark)))
	(outline-back-to-heading)
	(setq beg (point))
	(outline-end-of-heading)
	(outline-end-of-subtree)
	(setq end (point)))
      (setq beg (move-marker (make-marker) beg)
	    end (move-marker (make-marker) end))

      (let (head newhead level newlevel static)

	;; First a dry run to test if there is any trouble ahead.
	(goto-char beg)
	(while (re-search-forward re end t)
	  (outline-change-heading headlist delta atom 'test))

	;; Now really do replace the headings
	(goto-char beg)
	(while (re-search-forward re end t)
	  (outline-change-heading headlist delta atom))))))

(defun outline-headings-list ()
  "Return a list of relevant headings, either a user/mode defined
list, or an alist derived from scanning the buffer."
  (let (headlist)
    (cond
     (outline-promotion-headings
      ;; configured by the user or the mode
      (setq headlist outline-promotion-headings))

     ((and (eq major-mode 'outline-mode) (string= outline-regexp "[*\^L]+"))
      ;; default outline mode with original regexp
      ;; this need special treatment because of the \f in the regexp
      (setq headlist '(("*" . 1) ("**" . 2))))  ; will be extrapolated

     (t ;; Check if the buffer contains a complete set of headings
      (let ((re (concat "^" outline-regexp)) head level)
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward re nil t)
	    (save-excursion
	      (beginning-of-line 1)
	      (setq head (outline-cleanup-match (match-string 0))
		    level (funcall outline-level))
	      (add-to-list  'headlist (cons head level))))))
      ;; Check for uniqueness of levels in the list
      (let* ((hl headlist) entry level seen nonunique)
	(while (setq entry (car hl))
	  (setq hl (cdr hl)
		level (cdr entry))
	  (if (and (not (outline-static-level-p level))
		   (member level seen))
	      ;; We have two entries for the same level.
	      (add-to-list 'nonunique level))
	  (add-to-list 'seen level))
	(if nonunique
	    (error "Cannot promote/demote: non-unique headings at level %s\nYou may want to configure `outline-promotion-headings'."
		   (mapconcat 'int-to-string nonunique ","))))))
    ;; OK, return the list
    headlist))

(defun outline-change-heading (headlist delta atom &optional test)
  "Change heading just matched by `outline-regexp' by DELTA levels.
HEADLIST can be either an alist ((\"outline-match\" . level)...) or a
straight list like `outline-promotion-headings'. ATOM is a character
if all headlines are composed of a single character.
If TEST is non-nil, just prepare the change and error if there are problems.
TEST nil means, really replace old heading with new one."
  (let* ((head (outline-cleanup-match (match-string 0)))
	 (level (save-excursion
		  (beginning-of-line 1)
		  (funcall outline-level)))
	 (newhead  ; compute the new head
	  (cond
	   ((= delta 0) t)
	   ((outline-static-level-p level) t)
	   ((null headlist) nil)
	   ((consp (car headlist))
	    ;; The headlist is an association list
	    (or (car (rassoc (+ delta level) headlist))
		(and atom
		     (> (+ delta level) 0)
		     (make-string (+ delta level) atom))))
	   (t
	    ;; The headlist is a straight list - grab the correct element.
	    (let* ((l (length headlist))
		   (n1 (- l (length (member head headlist)))) ; index old
		   (n2 (+ delta n1)))                         ; index new
	      ;; Careful checking
	      (cond
	       ((= n1 l) nil)                ; head not found
	       ((< n2 0) nil)                ; newlevel too low
	       ((>= n2 l) nil)               ; newlevel too high
	       ((let* ((tail (nthcdr (min n1 n2) headlist))
		       (nilpos (- (length tail) (length (memq nil tail)))))
		  (< nilpos delta))          ; nil element between old and new
		nil)
	       (t (nth n2 headlist))))))))      ; OK, we have a match!
    (if (not newhead)
	(error "Cannot shift level %d heading \"%s\" to level %d"
	       level head (+ level delta)))
    (if (and (not test) (stringp newhead))
	(save-excursion
	  (beginning-of-line 1)
	  (or (looking-at (concat "[ \t]*\\(" (regexp-quote head) "\\)"))
	      (error "Please contact maintainer"))
	  (replace-match (outline-cleanup-match newhead) t t nil 1)))))

(defun outline-headings-atom (headlist)
  "Use the list created by `outline-headings-list' and check if all
headings are polymers of a single character, e.g. \"*\".
If yes, return this character."
  (if (consp (car headlist))
      ;; this is an alist - it makes sense to check for atomic structure
      (let ((re (concat "\\`"
			(regexp-quote (substring (car (car headlist)) 0 1))
			"+\\'")))
	(if (not (delq nil (mapcar (lambda (x) (not (string-match re (car x))))
				   headlist)))
	    (string-to-char (car (car headlist)))))))

(defun outline-cleanup-match (s)
  "Remove text properties and start/end whitespace from a string."
  (set-text-properties 1 (length s) nil s)
  (save-match-data
    (if (string-match "^[ \t]+" s) (setq s (replace-match "" t t s)))
    (if (string-match "[ \t]+$" s) (setq s (replace-match "" t t s))))
  s)

(defun outline-static-level-p (level)
  "Test if a level should not be changed by level promotion/demotion."
  (>= level 1000))

;; ** Commands
;; *** Additional outline commands
;; **** Commands from `out-xtra'

(defun outline-hide-sublevels (keep-levels)
  "Hide everything except the first KEEP-LEVEL headers."
  (interactive "p")
  (if (< keep-levels 1)
      (error "Must keep at least one level of headers"))
  (setq keep-levels (1- keep-levels))
  (save-excursion
    (goto-char (point-min))
    (hide-subtree)
    (show-children keep-levels)
    (condition-case err
      (while (outline-get-next-sibling)
	(hide-subtree)
	(show-children keep-levels))
      (error nil))))

(defun outline-hide-other ()
  "Hide everything except for the current body and the parent headings."
  (interactive)
  (outline-hide-sublevels 1)
  (let ((last (point))
	(pos (point)))
    (while (save-excursion
	     (and (re-search-backward "[\n\r]" nil t)
		  (eq (following-char) ?\r)))
      (save-excursion
	(beginning-of-line)
	(if (eq last (point))
	    (progn
	      (outline-next-heading)
	      (outline-flag-region last (point) ?\n))
	  (show-children)
	  (setq last (point)))))))

;; **** Commands from `outline-magic'

(defun outline-next-line ()
  "Forward line, but mover over invisible line ends.
Essentially a much simplified version of `next-line'."
  (interactive)
  (beginning-of-line 2)
  (while (and (not (eobp))
	      (get-char-property (1- (point)) 'invisible))
    (beginning-of-line 2)))

(defun outline-move-subtree-up (&optional arg)
  "Move the currrent subtree up past ARG headlines of the same level."
  (interactive "p")
  (outline-move-subtree-down (- arg)))

(defun outline-move-subtree-down (&optional arg)
  "Move the currrent subtree down past ARG headlines of the same level."
  (interactive "p")
  (let ((re (concat "^" outline-regexp))
	(movfunc (if (> arg 0) 'outline-get-next-sibling
		   'outline-get-last-sibling))
	(ins-point (make-marker))
	(cnt (abs arg))
	beg end txt)
    ;; Select the tree
    (outline-back-to-heading)
    (setq beg (point))
    (outline-end-of-subtree)
    (if (= (char-after) ?\n) (forward-char 1))
    (setq end (point))
    ;; Find insertion point, with error handling
    (goto-char beg)
    (while (> cnt 0)
      (or (funcall movfunc)
	  (progn (goto-char beg)
		 (error "Cannot move past superior level")))
      (setq cnt (1- cnt)))
    (if (> arg 0)
	;; Moving forward - still need to move over subtree
	(progn (outline-end-of-subtree)
	       (if (= (char-after) ?\n) (forward-char 1))))
    (move-marker ins-point (point))
    (setq txt (buffer-substring beg end))
    (delete-region beg end)
    (insert txt)
    (goto-char ins-point)
    (move-marker ins-point nil)))

(defun outline-promote (&optional arg)
  "Decrease the level of an outline-structure by ARG levels.
When the region is active in transient-mark-mode, all headlines in the
region are changed.  Otherwise the current subtree is targeted. Note that
after each application of the command the scope of \"current subtree\"
may have changed."
  (interactive "p")
  (outline-change-level (- arg)))

(defun outline-demote (&optional arg)
  "Increase the level of an outline-structure by ARG levels.
When the region is active in transient-mark-mode, all headlines in the
region are changed.  Otherwise the current subtree is targeted. Note that
after each application of the command the scope of \"current subtree\"
may have changed."
  (interactive "p")
  (outline-change-level arg))

(defun outline-cycle (&optional arg)
  "Visibility cycling for outline(-minor)-mode.

- When point is at the beginning of the buffer, or when called with a
  C-u prefix argument, rotate the entire buffer through 3 states:
  1. OVERVIEW: Show only top-level headlines.
  2. CONTENTS: Show all headlines of all levels, but no body text.
  3. SHOW ALL: Show everything.

- When point is at the beginning of a headline, rotate the subtree started
  by this line through 3 different states:
  1. FOLDED:   Only the main headline is shown.
  2. CHILDREN: The main headline and the direct children are shown.  From
               this state, you can move to one of the children and
               zoom in further.
  3. SUBTREE:  Show the entire subtree, including body text.

- When point is not at the beginning of a headline, execute
  `indent-relative', like TAB normally does."
  (interactive "P")
  (setq deactivate-mark t)
  (cond

   ((equal arg '(4))
    ; Run `outline-cycle' as if at the top of the buffer.
    (save-excursion
      (goto-char (point-min))
      (outline-cycle nil)))

   (t
    (cond
     ((bobp) ;; Beginning of buffer: Global cycling

      (cond
       ((eq last-command 'outline-cycle-overview)
	;; We just created the overview - now do table of contents
	;; This can be slow in very large buffers, so indicate action
	(message "CONTENTS...")
	(save-excursion
	  ;; Visit all headings and show their offspring
	  (goto-char (point-max))
	  (catch 'exit
	    (while (and (progn (condition-case nil
				   (outline-previous-visible-heading 1)
				 (error (goto-char (point-min))))
			       t)
			(looking-at outline-regexp))
	      (show-branches)
	      (if (bobp) (throw 'exit nil))))
	  (message "CONTENTS...done"))
	(setq this-command 'outline-cycle-toc))
       ((eq last-command 'outline-cycle-toc)
	;; We just showed the table of contents - now show everything
	(show-all)
	(message "SHOW ALL")
	(setq this-command 'outline-cycle-showall))
       (t
	;; Default action: go to overview
	(hide-sublevels 1)
	(message "OVERVIEW")
	(setq this-command 'outline-cycle-overview))))

     ((save-excursion (beginning-of-line 1) (looking-at outline-regexp))
      ;; At a heading: rotate between three different views
      (outline-back-to-heading)
      (let ((goal-column 0) beg eoh eol eos)
	;; First, some boundaries
	(save-excursion
	  (outline-back-to-heading)           (setq beg (point))
	  (save-excursion (outline-next-line) (setq eol (point)))
	  (outline-end-of-heading)            (setq eoh (point))
	  (outline-end-of-subtree)            (setq eos (point)))
	;; Find out what to do next and set `this-command'
	(cond
	 ((= eos eoh)
	  ;; Nothing is hidden behind this heading
	  (message "EMPTY ENTRY"))
	 ((>= eol eos)
	  ;; Entire subtree is hidden in one line: open it
	  (show-entry)
	  (show-children)
	  (message "CHILDREN")
	  (setq this-command 'outline-cycle-children))
	 ((eq last-command 'outline-cycle-children)
	  ;; We just showed the children, now show everything.
	  (show-subtree)
	  (message "SUBTREE"))
	 (t
	  ;; Default action: hide the subtree.
	  (hide-subtree)
	  (message "FOLDED")))))

     ;; TAB emulation
     ((outline-cycle-emulate-tab)
      (indent-relative))

     (t
      ;; Not at a headline: Do indent-relative
      (outline-back-to-heading))))))

(defun outshine-cycle-buffer ()
  "Cycle the visibility state of buffer."
  (interactive)
  (outline-cycle '(4)))

;; **** Commands from `outline-mode-easy-bindings'

;; Copied from: http://emacswiki.org/emacs/OutlineMinorMode

(defun outline-body-p ()
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (and (not (eobp))
         (progn (forward-char 1)
                (not (outline-on-heading-p))))))

(defun outline-body-visible-p ()
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (not (outline-invisible-p))))

(defun outline-subheadings-p ()
  (save-excursion
    (outline-back-to-heading)
    (let ((level (funcall outline-level)))
      (outline-next-heading)
      (and (not (eobp))
           (< level (funcall outline-level))))))

(defun outline-subheadings-visible-p ()
  (interactive)
  (save-excursion
    (outline-next-heading)
    (not (outline-invisible-p))))

(defun outline-hide-more ()
  (interactive)
  (when (outline-on-heading-p)
    (cond ((and (outline-body-p)
                (outline-body-visible-p))
           (hide-entry)
           (hide-leaves))
          (t
           (hide-subtree)))))

(defun outline-show-more ()
  (interactive)
  (when (outline-on-heading-p)
    (cond ((and (outline-subheadings-p)
                (not (outline-subheadings-visible-p)))
           (show-children))
          ((and (not (outline-subheadings-p))
                (not (outline-body-visible-p)))
           (show-subtree))
          ((and (outline-body-p)
                (not (outline-body-visible-p)))
           (show-entry))
          (t
           (show-subtree)))))

(defun outshine-show-hidden-lines-cookies ()
  "Show hidden-lines cookies for all visible and folded headlines."
  (interactive)
  (if outshine-show-hidden-lines-cookies-p
      (outshine-write-hidden-lines-cookies)
    (if (not (y-or-n-p "Activate hidden-lines cookies "))
        (message "Unable to show hidden-lines cookies - deactivated.")
      (outshine-toggle-hidden-lines-cookies-activation)
      (outshine-write-hidden-lines-cookies)))
  (setq outshine-hidden-lines-cookies-on-p 1))

(defun outshine-hide-hidden-lines-cookies ()
  "Delete all hidden-lines cookies."
  (interactive)
  (let* ((base-buf (point-marker))
         (indirect-buf-name
          (generate-new-buffer-name
           (buffer-name (marker-buffer base-buf)))))
    (unless outshine-show-hidden-lines-cookies-p
      (setq outshine-show-hidden-lines-cookies-p 1))
    (clone-indirect-buffer indirect-buf-name nil 'NORECORD)
    (save-excursion
      (switch-to-buffer indirect-buf-name)
      (show-all)
      (let ((indirect-buf (point-marker)))
        (outshine-write-hidden-lines-cookies)
        (switch-to-buffer (marker-buffer base-buf))
        (kill-buffer (marker-buffer indirect-buf))
        (set-marker indirect-buf nil))
      (set-marker base-buf nil)))
  (setq outshine-hidden-lines-cookies-on-p nil))

(defun outshine-toggle-hidden-lines-cookies-activation ()
  "Toggles activation of hidden-lines cookies."
  (interactive)
  (if outshine-show-hidden-lines-cookies-p
      (progn
        (setq outshine-show-hidden-lines-cookies-p nil)
        (setq outshine-hidden-lines-cookies-on-p nil)
        (message "hidden-lines cookies are deactivated now"))
    (setq outshine-show-hidden-lines-cookies-p 1)
    (message "hidden-lines cookies are activated now")))

(defun outshine-toggle-hidden-lines-cookies ()
  "Toggles status of hidden-lines cookies between shown and hidden."
  (interactive)
  (if outshine-hidden-lines-cookies-on-p
      (outshine-hide-hidden-lines-cookies)
    (outshine-show-hidden-lines-cookies)))

;; *** Overridden outline commands

;; overriding 'outline-insert-heading'
;; copied and adapted form outline.el, taking into account modes
;; with 'comment-end' defined (as non-empty string).
(defun outshine-insert-heading ()
  "Insert a new heading at same depth at point.
This function takes `comment-end' into account."
  (interactive)
  (let* ((head-with-prop
          (save-excursion
            (condition-case nil
                (outline-back-to-heading)
              (error (outline-next-heading)))
            (if (eobp)
                (or (caar outline-heading-alist) "")
              (match-string 0))))
         (head (substring-no-properties head-with-prop))
         (com-end-p))
    (unless (or (string-match "[ \t]\\'" head)
		(not (string-match (concat "\\`\\(?:" outline-regexp "\\)")
				   (concat head " "))))
      (setq head (concat head " ")))
    (unless (or (not comment-end) (string-equal "" comment-end))
      (setq head (concat head " " outshine-normalized-comment-end))
      (setq com-end-p t))
    (unless (bolp) (end-of-line) (newline))
    (insert head)
    (unless (eolp)
      (save-excursion (newline-and-indent)))
    (and com-end-p
         (re-search-backward outshine-normalized-comment-end)
         (forward-char -1))
    (run-hooks 'outline-insert-heading-hook)))

;; * Keybindings.
;; ** From `outline-magic'

;; keybindings like Org-mode
(outshine-define-key-with-fallback outline-minor-mode-map (kbd "TAB")
  (outline-cycle 1)(outline-on-heading-p))
(outshine-define-key-with-fallback outline-minor-mode-map (kbd "M-RET")
  (outshine-insert-heading)(outline-on-heading-p))
(define-key outline-minor-mode-map (kbd "<backtab>") 'outshine-cycle-buffer)

;; Menu entries

(define-key outline-mode-menu-bar-map [headings outline-move-subtree-down]
  '("Move subtree down" . outline-move-subtree-down))
(define-key outline-mode-menu-bar-map [headings outline-move-subtree-up]
  '("Move subtree up" . outline-move-subtree-up))
(define-key outline-mode-menu-bar-map [headings outline-demote]
  '("Demote by 1 level" . outline-demote))
(define-key outline-mode-menu-bar-map [headings outline-promote]
  '("Promote by 1 level" . outline-promote))
(define-key outline-mode-menu-bar-map [show outline-cycle]
  '("Rotate visibility" . outline-cycle))
(define-key outline-mode-menu-bar-map [hide outline-cycle]
  '("Rotate visibility" . outline-cycle))

;; ** From `out-xtra'

;; We provide bindings for all keys.
;; old stuff from `out-xtra' - still up-to-date?

(if (fboundp 'eval-after-load)
    ;; FSF Emacs 19.
    (eval-after-load "outline"
      '(let ((map (lookup-key outline-minor-mode-map
			      outline-minor-mode-prefix)))
	 (define-key map "\C-t" 'hide-body)
	 (define-key map "\C-a" 'show-all)
	 (define-key map "\C-c" 'hide-entry)
	 (define-key map "\C-e" 'show-entry)
	 (define-key map "\C-l" 'hide-leaves)
	 (define-key map "\C-k" 'show-branches)
	 (define-key map "\C-q" 'outline-hide-sublevels)
	 (define-key map "\C-o" 'outline-hide-other)
      	 (define-key map (kbd "RET") 'outshine-insert-heading)
         (define-key map "'" 'outorg-edit-as-org)

	 (define-key outline-minor-mode-map [menu-bar hide hide-sublevels]
	   '("Hide Sublevels" . outline-hide-sublevels))
	 (define-key outline-minor-mode-map [menu-bar hide hide-other]
	   '("Hide Other" . outline-hide-other))
	 (if (fboundp 'update-power-keys)
	     (update-power-keys outline-minor-mode-map))))

  (if (string-match "Lucid" emacs-version)
      (progn				;; Lucid Emacs 19
	(defconst outline-menu
	  '(["Up" outline-up-heading t]
	    ["Next" outline-next-visible-heading t]
	    ["Previous" outline-previous-visible-heading t]
	    ["Next Same Level" outline-forward-same-level t]
	    ["Previous Same Level" outline-backward-same-level t]
	    "---"
	    ["Show All" show-all t]
	    ["Show Entry" show-entry t]
	    ["Show Branches" show-branches t]
	    ["Show Children" show-children t]
	    ["Show Subtree" show-subtree t]
	    "---"
	    ["Hide Leaves" hide-leaves t]
	    ["Hide Body" hide-body t]
	    ["Hide Entry" hide-entry t]
	    ["Hide Subtree" hide-subtree t]
	    ["Hide Other" outline-hide-other t]
	    ["Hide Sublevels" outline-hide-sublevels t]))

        (defun outline-add-menu ()
	  (set-buffer-menubar (copy-sequence current-menubar))
	  (add-menu nil "Outline" outline-menu))

	(add-hook 'outline-minor-mode-hook 'outline-add-menu)
	(add-hook 'outline-mode-hook 'outline-add-menu)
	(add-hook 'outline-minor-mode-off-hook
                  (function (lambda () (delete-menu-item '("Outline")))))))

  ;; Lucid Emacs or Emacs 18.
  (require 'outln-18)
  (let ((map (lookup-key outline-minor-mode-map outline-minor-mode-prefix)))
    ;; Should add a menu here.
    (define-key map "\C-t" 'hide-body)
    (define-key map "\C-a" 'show-all)
    (define-key map "\C-c" 'hide-entry)
    (define-key map "\C-e" 'show-entry)
    (define-key map "\C-l" 'hide-leaves)
    (define-key map "\C-k" 'show-branches)
    (define-key map "\C-q" 'outline-hide-sublevels)
    (define-key map "\C-o" 'outline-hide-other)))


;; ** From `outline-mode-easy-bindings'

(let ((map outline-mode-map))
  (outshine-define-key-with-fallback
   map (kbd "M-<left>") (outline-hide-more) (outline-on-heading-p))
  (outshine-define-key-with-fallback
   map (kbd "M-<right>") (outline-show-more) (outline-on-heading-p))
  (outshine-define-key-with-fallback
   map (kbd "C-c J") (outline-hide-more) (outline-on-heading-p))
  (outshine-define-key-with-fallback
   map (kbd "C-c L") (outline-show-more) (outline-on-heading-p))
  (define-key map (kbd "M-<up>") 'outline-previous-visible-heading)
  (define-key map (kbd "M-<down>") 'outline-next-visible-heading)
  (define-key map (kbd "C-c I") 'outline-previous-visible-heading)
  (define-key map (kbd "C-c K") 'outline-next-visible-heading))

(let ((map outline-minor-mode-map))
  (outshine-define-key-with-fallback
   map (kbd "M-<left>") (outline-hide-more) (outline-on-heading-p))
  (outshine-define-key-with-fallback
   map (kbd "M-<right>") (outline-show-more) (outline-on-heading-p))
  (outshine-define-key-with-fallback
   map (kbd "C-c J") (outline-hide-more) (outline-on-heading-p))
  (outshine-define-key-with-fallback
   map (kbd "C-c L") (outline-show-more) (outline-on-heading-p))
  (define-key map (kbd "M-<up>") 'outline-previous-visible-heading)
  (define-key map (kbd "M-<down>") 'outline-next-visible-heading)
  (define-key map (kbd "C-c I") 'outline-previous-visible-heading)
  (define-key map (kbd "C-c K") 'outline-next-visible-heading))

;; * Run hooks and provide

(run-hooks 'outshine-hook)

(provide 'outshine)

;; Local Variables:
;; coding: utf-8
;; ispell-local-dictionary: "en_US"
;; End:

;; outshine.el ends here
