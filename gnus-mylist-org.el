;;; gnus-mylist-org.el --- Some org-mode integration for gnus-mylist  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Deus Max

;; Author: Deus Max <deusmax@gmx.com>
;; URL: https://github.com/deusmax/gnus-mylist
;; Version: 0.3.0
;; Package-Requires: ((emacs "25.1.0"))
;; Keywords: convenience, mail, gnus helm, org

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Keep track of your seen messages, using a helm interface.
;; Gnus-mylist provides an interface with minimum configuration.
;; It should "just work".
;; This file provides some integration with Org-mode and TODO headings.
;;

;;; Code:

(require 's)
(require 'gnus-mylist-helm)
(unless (require 'ol-gnus nil 'noerror)
  (require 'org-gnus))
(require 'org-capture)

(defvar gnus-mylist--articles-list)

(declare-function gnus-mylist--reply-article-wide-yank "gnus-mylist" (artdata))
(declare-function gnus-mylist--create-org-link "gnus-mylist" (artdata))
(declare-function gnus-mylist-kill-new-org-link "gnus-mylist" (artdata))
(declare-function gnus-mylist-split-org-link-gnus "gnus-mylist" (link))
(declare-function gnus-mylist-outgoing-message-p "gnus-mylist" (artdata))
(declare-function gnus-mylist-bbdb-display-all "gnus-mylist" (artdata))

(defgroup gnus-mylist-org nil
  "Org integration for gnus-mylist"
  :tag "Gnus Mylist Org"
  :group 'gnus-mylist)

(defcustom gnus-mylist-org-capture-key "e"
    "The key used for `gnus-mylist-org-capture-template'.
The template key should match with this value."
  :group 'gnus-mylist-org
  :type 'string)

(defcustom gnus-mylist-org-capture-template
  '("e" "Email Reply Scheduled (a)" entry
    (file+olp "~/Documents/org/notes.org" "Tasks" "Emails")
 "* RPLY %^{Description}  %^g
  SCHEDULED: %^T
  :PROPERTIES:
  :ID:       %(org-id-new)
  :END:
  EmailDate: %:date-timestamp-inactive, Group: %:group \\\\
  Added: %U \\\\
  %a \\\\
  %?" :prepend t :clock-in t :clock-resume t)
  "A template for capturing gnus emails and other articles.
A single entry for `org-capture-templates'. See its documentation
for details."
  :group 'gnus-mylist-org
  :type
  (let ((file-variants '(choice :tag "Filename       "
				(file :tag "Literal")
				(function :tag "Function")
				(variable :tag "Variable")
				(sexp :tag "Form"))))
    `(choice :value ("" "" entry (file "~/org/notes.org") "")
	     (list :tag "Multikey description"
		   (string :tag "Keys       ")
		   (string :tag "Description"))
	     (list :tag "Template entry"
		   (string :tag "Keys           ")
		   (string :tag "Description    ")
		   (choice :tag "Capture Type   " :value entry
			   (const :tag "Org entry" entry)
			   (const :tag "Plain list item" item)
			   (const :tag "Checkbox item" checkitem)
			   (const :tag "Plain text" plain)
			   (const :tag "Table line" table-line))
		   (choice :tag "Target location"
			   (list :tag "File"
			         (const :format "" file)
			         ,file-variants)
			   (list :tag "ID"
			         (const :format "" id)
			         (string :tag "  ID"))
			   (list :tag "File & Headline"
			         (const :format "" file+headline)
			         ,file-variants
			         (string :tag "  Headline"))
			   (list :tag "File & Outline path"
			         (const :format "" file+olp)
			         ,file-variants
			         (repeat :tag "Outline path" :inline t
				         (string :tag "Headline")))
			   (list :tag "File & Regexp"
			         (const :format "" file+regexp)
			         ,file-variants
			         (regexp :tag "  Regexp"))
			   (list :tag "File [ & Outline path ] & Date tree"
			         (const :format "" file+olp+datetree)
			         ,file-variants
			         (option (repeat :tag "Outline path" :inline t
					         (string :tag "Headline"))))
			   (list :tag "File & function"
			         (const :format "" file+function)
			         ,file-variants
			         (sexp :tag "  Function"))
			   (list :tag "Current clocking task"
			         (const :format "" clock))
			   (list :tag "Function"
			         (const :format "" function)
			         (sexp :tag "  Function")))
		   (choice :tag "Template       "
			   (string)
			   (list :tag "File"
			         (const :format "" file)
			         (file :tag "Template file"))
			   (list :tag "Function"
			         (const :format "" function)
			         (function :tag "Template function")))
		   (plist :inline t
		          ;; Give the most common options as checkboxes
		          :options (((const :format "%v " :prepend) (const t))
				    ((const :format "%v " :immediate-finish) (const t))
				    ((const :format "%v " :jump-to-captured) (const t))
				    ((const :format "%v " :empty-lines) (const 1))
				    ((const :format "%v " :empty-lines-before) (const 1))
				    ((const :format "%v " :empty-lines-after) (const 1))
				    ((const :format "%v " :clock-in) (const t))
				    ((const :format "%v " :clock-keep) (const t))
				    ((const :format "%v " :clock-resume) (const t))
				    ((const :format "%v " :time-prompt) (const t))
				    ((const :format "%v " :tree-type) (const week))
				    ((const :format "%v " :unnarrowed) (const t))
				    ((const :format "%v " :table-line-pos) (string))
				    ((const :format "%v " :kill-buffer) (const t))))))))

(defvar gnus-mylist-org--template-context (list gnus-mylist-org-capture-key ""
                                                '((in-mode . "article-mode")
                                                  (in-mode . "summary-mode")))
  "Context for when `gnus-mylist-org-capture-template' is available.")

(defvar gnus-mylist-org--current-org-id nil
  "Internal variable; for temporary holding the current heading org-id.")

(defvar gnus-mylist-org--current-heading-alist nil
  "Internal variable; for temporary holding the current heading info.")

(defvar gnus-mylist-org--last-window-configuration nil
  "Internal variable; for saving a window configuration.")

;; (defhydra gnus-mylist-org-trigger-actions (:color blue :columns 2)
;;   "List of actions on org-headings."
;;   ("t" gnus-mylist-org-todo "change Todo")
;;   ("n" gnus-mylist-org-note-add "add Note")
;;   ("a" gnus-mylist-org-associate  "only Associate")
;;   ("c" gnus-mylist-org-capture-child "capture to Child")
;;   ("s" gnus-mylist-org-capture-sibling "capture to Sibling")
;;   ("q" nil "Quit, don't associate"))

;; FIXME: here it is hoped the top article-crumb is the top link. Most likely true,
;; but not guaranteed. Crumbs may have been deleted. Need to check and confirm, this
;; to be the case.
(defun gnus-mylist-org-handle-mail-top ()
  "Reply to the top email message on the current org headline.
The body of the org heading must have at least one gnus link to
reply to."
  (interactive)
  (let ((art (car-safe (alist-get 'articles-crumbs gnus-mylist-org--current-heading-alist))))
    (if art
        (gnus-mylist--reply-article-wide-yank art)
      (gnus-message 5 "gnus-mylist has lost the article, revisit top article.")
      (gnus-mylist-org-clear-heading-alist))))

;; TODO: implement additional nnir engines, currently only IMAP (2020-04-013)
(defun gnus-mylist-org-handle-mail-view ()
  "Do a gnus nnir search for the gnus messages on the current org headline.
The messages will be shown in a Gnus ephemeral group using nnir.
Currently, the search is limited to nnimap groups."
  (interactive)
  ;; 1. take the org-link-gnus form the *--current-heading-alist
  ;; 2. split each link to a (group . msgid) pair
  ;; 3. filter to allow only pairs with an nnimap group (current limitation)
  (let ((nnimap-links-split
         (seq-filter
          #'(lambda (p)
              (string-match-p "^nnimap" (gnus-group-server (car p))))
          (mapcar #'gnus-mylist-split-org-link-gnus
                  (alist-get 'org-links-gnus gnus-mylist-org--current-heading-alist))))
        groups-list msgids-list)
    (if (eql 0 (length nnimap-links-split))
        (message-box "No Gnus IMAP messages found under current org heading subtree.")
      ;; separate parts and make unique
      (dolist (link nnimap-links-split)
        (cl-pushnew (cdr link) msgids-list :test #'equal)
        (cl-pushnew (car link) groups-list :test #'equal))
      (gnus-mylist-org-nnir-search (gnus-mylist-org-nnir-query-spec msgids-list)
                                   (gnus-mylist-org-nnir-group-spec groups-list)))))

(defun gnus-mylist-org-nnir-group-spec (groups)
  "Given a GROUPS list format a nnir `group-spec' list.
No duplicate groups are expected. Each group element in the list should be
unique. Check, for uniqueness, before calling this function."
  (let (server item group-spec)
    (dolist (gr groups group-spec)
      (setq server (gnus-group-server gr))
      ;; (setq item (map-elt group-spec server nil #'equal))
      (setq item (alist-get server group-spec nil nil #'equal))
      (setf (map-elt group-spec server) (list (if (eql 0 (length item))
                                                  (list gr)
                                                (push gr (car item))))))))

(defun gnus-mylist-org-nnir-query-spec (query &optional criteria)
  "Given an IMAP QUERY, format a nnir `query-spec' list.
Default query CRITERIA on article Message-ID. See
`nnir-imap-search-arguments' for available IMAP search items for
use in nnir. Currently, only IMAP search implemented and only for
Message-ID."
  (list (cons 'query (string-join query " OR "))
        (cons 'criteria (or criteria "HEADER \"Message-ID\""))))

(defun gnus-mylist-org-nnir-search (query-spec group-spec)
  "Convenience wrapper to `gnus-group-read-ephemeral-group'.
See also function `gnus-group-make-nnir-group' for details on the QUERY-SPEC and
GROUP-SPEC."
  (interactive)
  (gnus-group-read-ephemeral-group
   (concat "nnir-" (message-unique-id))
   (list 'nnir "nnir")
   nil nil nil nil
   (list
    (cons 'nnir-specs (list (cons 'nnir-query-spec query-spec)
                            (cons 'nnir-group-spec group-spec)))
    (cons 'nnir-artlist nil))))

(defun gnus-mylist-org-get-heading-alist ()
  "Get the text of a org heading and extract the info needed."
  (interactive)
  (save-excursion
    (when (eq major-mode 'org-agenda-mode)
      (org-agenda-goto))
    (org-back-to-heading t)
    (let* ((org-hd-marker (point-marker))
           (uid (org-id-get-create))
           (hd-txt (save-excursion
                     (buffer-substring-no-properties (point-at-bol 2)
                                                     (org-end-of-subtree t))))
           (org-links-gnus (gnus-mylist-org-search-string-org-links-gnus hd-txt))
           (articles-msgid (mapcar 'cdr
                                   (mapcar 'gnus-mylist-split-org-link-gnus
                                           org-links-gnus)))
           (articles (gnus-mylist-org-filter-message-ids-list articles-msgid)))
      (list
       (cons 'uid uid)
       (cons 'org-hd-marker org-hd-marker)
       (cons 'windc gnus-mylist-org--last-window-configuration)
       (cons 'entry-text hd-txt)
       (cons 'org-links-gnus org-links-gnus)
       (cons 'orgids (gnus-mylist-org-get-orgids hd-txt))
       (cons 'articles-msgid articles-msgid)
       (cons 'articles-crumbs articles)))))

(defun gnus-mylist-org-set-heading-alist ()
  "Save the heading info needed to `gnus-mylist-org--current-heading-alist'."
  (interactive)
  (setq gnus-mylist-org--current-heading-alist (gnus-mylist-org-get-heading-alist)))

(defun gnus-mylist-org-clear-heading-alist ()
  "Clear all data from variable `gnus-mylist-org--current-heading-alist'."
  (interactive)
  (setq gnus-mylist-org--current-heading-alist nil))

(defun gnus-mylist-org-message-add-hooks ()
  "Add the hooks for an outgoing message."
  (add-hook 'message-sent-hook 'gnus-mylist-org-message-sent-actions t)
  (add-hook 'message-cancel-hook 'gnus-mylist-org-message-remove-hooks))

(defun gnus-mylist-org-message-remove-hooks ()
  "Remove the hooks set by `gnus-mylist-org-message-add-hooks'."
  (remove-hook 'message-sent-hook 'gnus-mylist-org-message-sent-actions)
  (remove-hook 'message-cancel-hook 'gnus-mylist-org-message-remove-hooks))

;; FIXME: Exploratory code, need to handle cancelling and aborting.
;; FIXME: Message-send (C-c C-s) results in empty group field.
(defun gnus-mylist-org-message-sent-actions ()
  "Tidy up after an outgoing message is sent.
Add a gnus-link to the org entry as a log-note, then tidy up."
  (when gnus-mylist-org--current-heading-alist
    (let* ((artdata-out (car gnus-mylist--articles-list))
           (root-marker (alist-get 'org-hd-marker
                                   gnus-mylist-org--current-heading-alist))
           (org-link (gnus-mylist--create-org-link artdata-out)))
      ;; confirm the last item was an outgoing message
      (when (gnus-mylist-outgoing-message-p artdata-out)
        (org-with-point-at root-marker
          (org-add-log-setup 'note nil nil nil org-link))
        (gnus-mylist-org-clear-heading-alist)
        (gnus-mylist-org-message-remove-hooks)))))

;;; FIXME: use el-patch for this advice
(defun gnus-mylist-org-outshine-comment-region-advice (beg end &optional arg)
  "Check the current major mode.
BEG, END and optional ARG are the agruments of the function to be advised."
  (ignore beg end arg)                  ; keep byte compiler quiet
  (eq major-mode 'gnus-summary-mode))

;; don't allow outshine-comment-region to proceed for gnus buffers.
(eval-after-load 'outshine
  (advice-add 'outshine-comment-region
              :before-until
              #'gnus-mylist-org-outshine-comment-region-advice))

;; if needed during development.
;; (advice-remove 'outshine-comment-region #'gnus-mylist-org-outshine-comment-region-advice)

(defun gnus-mylist-org-handle-mail-crumbs ()
  "Show available gnus messages from the current org headline in helm."
  (interactive)
  (helm
   :sources (helm-build-sync-source "Heading articles"
              :keymap gnus-mylist-helm-map
              :candidates (lambda ()
                            (gnus-mylist-helm-candidates
                             (alist-get 'articles-crumbs
                                        gnus-mylist-org--current-heading-alist)))
              :filtered-candidate-transformer  'gnus-mylist-helm-candidate-transformer
              :persistent-action 'gnus-mylist-org-helm-hydra-pa
              :persistent-help "view hydra"
              :action '(("Open article"               . gnus-mylist--open-article)
                        ("Wide reply and yank"        . gnus-mylist--reply-article-wide-yank)
                        ("Show thread"                . gnus-mylist--show-article-thread)
                        ("Copy org link to kill ring" . gnus-mylist-kill-new-org-link)
                        ("Display BBDB entries"       . gnus-mylist-bbdb-display-all)))
   :buffer "*helm org heading articles*"
   :truncate-lines t))

(defun gnus-mylist-org-handle-mail ()
  "Handle mail in org heading.
First, this function sets the variable
`gnus-mylist--current-heading-alist' with the the current heading
info. Second, it activates a hook to run after sending a message,
that will take care of the org stuff. Then it calls a hydra to
select the action on the email articles."
  (interactive)
  (setq gnus-mylist-org--last-window-configuration (current-window-configuration))
  (gnus-mylist-org-set-heading-alist)
  (if (alist-get 'org-links-gnus gnus-mylist-org--current-heading-alist)
      (progn
        (gnus-mylist-org-message-add-hooks)
        (hydra-gnus-mylist-org-handle-mail/body))
    (gnus-message 5 "No gnus links found in current org entry")
    (gnus-mylist-org-clear-heading-alist)))

(defhydra hydra-gnus-mylist-org-handle-mail (:color blue :columns 2)
  "Reply to email from current heading"
  ("h" gnus-mylist-org-handle-mail-crumbs "View in helm")
  ("t" gnus-mylist-org-handle-mail-top "Reply to top")
  ("v" gnus-mylist-org-handle-mail-view "Search Gnus (imap)")
  ("q" gnus-mylist-org-clear-heading-alist "quit"))

(defun gnus-mylist-org-capture-mail ()
  "Capture a note on an email using the `org-mode' capture interface.
While viewing emails in gnus, in a summary or artile buffer,
quickly capture an org note capture system. The capture template
will be preselected with the `gnus-mylist-org-capture-key',
unless it is not defined in `org-capture-templates'. The gnus
keywords should be available during template expansion."
  (interactive)
  (unless (memq major-mode '(gnus-summary-mode gnus-article-mode))
    (user-error "Not in a gnus summary or article buffer"))
  (org-capture nil
               (cl-find gnus-mylist-org-capture-key
                        (mapcar #'car org-capture-templates) :test #'equal)))

(defun gnus-mylist-org-get-entry (&optional keep-properties)
  "Get the org entry text.
With the optional KEEP-PROPERTIES non-nil keep the text
properties. By default, text properties are removed."
  (interactive)
  (when (eq major-mode 'org-agenda-mode)
    (org-agenda-goto))
  (if keep-properties
      (org-get-entry)
    (gnus-string-remove-all-properties (org-get-entry))))

(defun gnus-mylist-org-search-string-org-links-gnus (txt)
  "Search text TXT for org-links, having protocol \"gnus:\".
Returns a list of org-links, that point to gnus articles."
  (mapcar 'car
          (s-match-strings-all "\\[\\[gnus:.+\\]\\]"
                               (gnus-string-remove-all-properties txt))))

(defun gnus-mylist-org-get-orgids (txt)
  "Find the org-ids in org entry text TXT."
  (mapcar (lambda (x) (cadr (split-string x ":" t " +")))
          (mapcar 'car
                  (s-match-strings-all "^ +:ID:.+" txt))))

(defun gnus-mylist-org-filter-message-ids-list (id-list)
  "Get the article message-id that have any of the given org-ids.
ID-LIST is a list of org-ids to search in `gnus-mylist--articles-list'.
Returns a combined list of all article message-ids found."
  ;; FIXME: use equal for the test
  (mapcan '(lambda (id) (gnus-mylist-filter-prop 'message-id id #'string=))
          id-list))

(defun gnus-mylist-org-message-add-header (header value)
  "Add a HEADER when composing a new message.
VALUE is the value for the header."
  (when (and value (derived-mode-p 'message-mode 'mail-mode))
    (save-excursion
      (save-restriction
        (message-narrow-to-headers-or-head)
        (open-line 1)
        (message-insert-header header value)))))

(defun gnus-mylist-org-message-add-header-orgid (&optional orgid)
  "Add an X-Org-Id header to an outgoing message.
When the optional argument ORGID is missing, will get the orgid
value from `gnus-mylist-org-get-heading-alist'."
  (gnus-mylist-org-message-add-header
   'X-Org-Id (or orgid
                  (alist-get 'uid gnus-mylist-org--current-heading-alist))))

(defhydra hydra-gnus-org-helm (:columns 4 :exit nil)
  "Persistent actions"
  ("c" (gnus-mylist-kill-new-org-link gnus-mylist-helm-current-data-pa) "Copy Org link")
  ("b" (gnus-mylist-bbdb-display-all  gnus-mylist-helm-current-data-pa) "BBDB entries")
  ("{" helm-enlarge-window "enlarge")
  ("}" helm-narrow-window "narrow")
  (">" helm-toggle-truncate-line "wrap lines")
  ("_" helm-toggle-full-frame "full frame")
  ("Y" helm-yank-selection "yank entry")
  ("U" helm-refresh "update data")
  ("q" nil "quit" :exit t))

(defun gnus-mylist-org-helm-hydra-pa (artdata)
  "Persistent action activates a Hydra.
ARTDATA is the current article in the helm buffer."
  (setq gnus-mylist-helm-current-data-pa artdata)
  (hydra-gnus-org-helm/body))

;; keybindings
(defun gnus-mylist-org-define-key (&optional key)
  "Bind KEY for org integration.
A convenience function to define a single key sequence for
integration with org. By default KEY is set to \"C-c t\"."
  (unless key (setq key "C-c t"))
  (define-key org-mode-map          (kbd key) #'gnus-mylist-org-handle-mail)
  (org-defkey org-agenda-keymap     (kbd key) #'gnus-mylist-org-handle-mail)
  (define-key gnus-summary-mode-map (kbd key) #'gnus-mylist-org-capture-mail)
  (define-key gnus-article-mode-map (kbd key) #'gnus-mylist-org-capture-mail))

;; init actions
(defun gnus-mylist-org-init ()
  "Start-up actions for `gnus-mylist-org'."
  (when gnus-mylist-org-capture-template
    (add-to-list 'org-capture-templates gnus-mylist-org-capture-template ))
  (when gnus-mylist-org--template-context
    (add-to-list 'org-capture-templates-contexts gnus-mylist-org--template-context )))

(provide 'gnus-mylist-org)
;;; gnus-mylist-org.el ends here
