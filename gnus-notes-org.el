;;; gnus-notes-org.el --- Some org-mode integration for gnus-notes  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Deus Max

;; Author: Deus Max <deusmax@gmx.com>
;; URL: https://github.com/deusmax/gnus-notes
;; Version: 0.3.1
;; Keywords: convenience, mail, bbdb, gnus helm, org, hydra

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Keep track of your seen messages, using a helm interface.
;; Gnus-notes provides an interface with minimum configuration.
;; It should "just work".
;; This file provides some integration with Org-mode and TODO headings.
;;
;; For org-mode integration, activate the key bindings:
;;     (gnus-notes-org-define-key)       ; default "C-c t"
;;

;;; Code:

(require 's)
(require 'gnus)
(require 'helm)
(require 'hydra)
(require 'bbdb-mua)
(unless (require 'ol-gnus nil 'noerror)
  (require 'org-gnus))
(require 'org-agenda)
(require 'org-capture)
(require 'async)                        ; required by helm
(require 'lv)                           ; required by hydra
(require 'gnus-notes)

(defvar gnus-notes-helm-map)
(defvar gnus-notes-helm-current-data-pa)

(declare-function gnus-notes-helm-candidates "gnus-notes-helm" (articles-list))

(defgroup gnus-notes-org nil
  "Org integration for gnus-notes"
  :tag "Gnus Notes Org"
  :group 'gnus-notes)

(defcustom gnus-notes-org-capture-key "e"
    "The key used for `gnus-notes-org-capture-template'.
The template key should match with this value."
  :group 'gnus-notes-org
  :type 'string)

(defcustom gnus-notes-org-capture-template
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
  :group 'gnus-notes-org
  :type (second (get 'org-capture-templates 'custom-type)))

(defvar gnus-notes-org--template-context (list gnus-notes-org-capture-key ""
                                                '((in-mode . "article-mode")
                                                  (in-mode . "summary-mode")))
  "Context for when `gnus-notes-org-capture-template' is available.")

(defvar gnus-notes-org--current-org-id nil
  "Internal variable; for temporary holding the current heading org-id.")

(defvar gnus-notes-org--current-heading-alist nil
  "Internal variable; for temporary holding the current heading info.")

(defvar gnus-notes-org--last-window-configuration nil
  "Internal variable; for saving a window configuration.")

;; (defhydra gnus-notes-org-trigger-actions (:color blue :columns 2)
;;   "List of actions on org-headings."
;;   ("t" gnus-notes-org-todo "change Todo")
;;   ("n" gnus-notes-org-note-add "add Note")
;;   ("a" gnus-notes-org-associate  "only Associate")
;;   ("c" gnus-notes-org-capture-child "capture to Child")
;;   ("s" gnus-notes-org-capture-sibling "capture to Sibling")
;;   ("q" nil "Quit, don't associate"))

;; FIXME: here it is hoped the top article-crumb is the top link. Most likely true,
;; but not guaranteed. Crumbs may have been deleted. Need to check and confirm, this
;; to be the case.
(defun gnus-notes-org-handle-mail-top ()
  "Reply to the top email message on the current org headline.
The body of the org heading must have at least one gnus link to
reply to."
  (interactive)
  (let ((art (car-safe (alist-get 'articles-crumbs gnus-notes-org--current-heading-alist))))
    (if art
        (gnus-notes--reply-article-wide-yank art)
      (gnus-message 5 "gnus-notes has lost the article, revisit top article.")
      (gnus-notes-org-clear-heading-alist))))

;; TODO: implement additional nnir engines, currently only IMAP (2020-04-013)
(defun gnus-notes-org-handle-mail-view ()
  "Do a gnus nnir search for the gnus messages on the current org headline.
The messages will be shown in a Gnus ephemeral group using nnir.
Currently, the search is limited to nnimap groups."
  (interactive)
  ;; 1. take the org-link-gnus form the *--current-heading-alist
  ;; 2. split each link to a (group . msgid) pair
  ;; 3. filter to allow only pairs with an nnimap group (current limitation)
  (let ((nnimap-links-split
         (seq-filter
          (lambda (p)
            (string-match-p "^nnimap" (gnus-group-server (car p))))
          (mapcar #'gnus-notes-split-org-link-gnus
                  (alist-get 'org-links-gnus gnus-notes-org--current-heading-alist))))
        groups-list msgids-list)
    (if (eql 0 (length nnimap-links-split))
        (message-box "No Gnus IMAP messages found under current org heading subtree.")
      ;; separate parts and make unique
      (dolist (link nnimap-links-split)
        (cl-pushnew (cdr link) msgids-list :test #'equal)
        (cl-pushnew (car link) groups-list :test #'equal))
      (gnus-notes-org-nnir-search (gnus-notes-org-nnir-query-spec msgids-list)
                                   (gnus-notes-org-nnir-group-spec groups-list)))))

(defun gnus-notes-org-nnir-group-spec (groups)
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

(defun gnus-notes-org-nnir-query-spec (query &optional criteria)
  "Given an IMAP QUERY, format a nnir `query-spec' list.
Default query CRITERIA on article Message-ID. See
`nnir-imap-search-arguments' for available IMAP search items for
use in nnir. Currently, only IMAP search implemented and only for
Message-ID."
  (list (cons 'query (string-join query " OR "))
        (cons 'criteria (or criteria "HEADER \"Message-ID\""))))

(defun gnus-notes-org-nnir-search (query-spec group-spec)
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

(defun gnus-notes-org-get-heading-alist ()
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
           (org-links-gnus (gnus-notes-org-search-string-org-links-gnus hd-txt))
           (articles-msgid (mapcar #'cdr
                                   (mapcar #'gnus-notes-split-org-link-gnus
                                           org-links-gnus)))
           (articles (gnus-notes-org-filter-message-ids-list articles-msgid)))
      (list
       (cons 'uid uid)
       (cons 'org-hd-marker org-hd-marker)
       (cons 'windc gnus-notes-org--last-window-configuration)
       (cons 'entry-text hd-txt)
       (cons 'org-links-gnus org-links-gnus)
       (cons 'orgids (gnus-notes-org-get-orgids hd-txt))
       (cons 'articles-msgid articles-msgid)
       (cons 'articles-crumbs articles)))))

(defun gnus-notes-org-set-heading-alist ()
  "Save the heading info needed to `gnus-notes-org--current-heading-alist'."
  (interactive)
  (setq gnus-notes-org--current-heading-alist (gnus-notes-org-get-heading-alist)))

(defun gnus-notes-org-clear-heading-alist ()
  "Clear all data from variable `gnus-notes-org--current-heading-alist'."
  (interactive)
  (setq gnus-notes-org--current-heading-alist nil))

(defun gnus-notes-org-message-add-hooks ()
  "Add the hooks for an outgoing message."
  (add-hook 'message-sent-hook #'gnus-notes-org-message-sent-actions t)
  (add-hook 'message-cancel-hook #'gnus-notes-org-message-remove-hooks))

(defun gnus-notes-org-message-remove-hooks ()
  "Remove the hooks set by `gnus-notes-org-message-add-hooks'."
  (remove-hook 'message-sent-hook #'gnus-notes-org-message-sent-actions)
  (remove-hook 'message-cancel-hook #'gnus-notes-org-message-remove-hooks))

;; FIXME: Exploratory code, need to handle cancelling and aborting.
;; FIXME: Message-send (C-c C-s) results in empty group field.
(defun gnus-notes-org-message-sent-actions ()
  "Tidy up after an outgoing message is sent.
Add a gnus-link to the org entry as a log-note, then tidy up."
  (when gnus-notes-org--current-heading-alist
    (let* ((artdata-out (car gnus-notes--articles-list))
           (root-marker (alist-get 'org-hd-marker
                                   gnus-notes-org--current-heading-alist))
           (org-link (gnus-notes--create-org-link artdata-out)))
      ;; confirm the last item was an outgoing message
      (when (gnus-notes-outgoing-message-p artdata-out)
        (org-with-point-at root-marker
          (org-add-log-setup 'note nil nil nil org-link))
        (gnus-notes-org-clear-heading-alist)
        (gnus-notes-org-message-remove-hooks)))))

;;; FIXME: use el-patch for this advice
(defun gnus-notes-org-outshine-comment-region-advice (beg end &optional arg)
  "Check the current major mode.
BEG, END and optional ARG are the arguments of the function to be advised."
  (ignore beg end arg)                  ; keep byte compiler quiet
  (eq major-mode 'gnus-summary-mode))

;; don't allow outshine-comment-region to proceed for gnus buffers.
(eval-after-load 'outshine
  (advice-add 'outshine-comment-region
              :before-until
              #'gnus-notes-org-outshine-comment-region-advice))

;; if needed during development.
;; (advice-remove 'outshine-comment-region #'gnus-notes-org-outshine-comment-region-advice)

(defun gnus-notes-org-handle-mail-crumbs ()
  "Show available gnus messages from the current org headline in helm."
  (interactive)
  (helm
   :sources (helm-build-sync-source "Heading articles"
              :keymap gnus-notes-helm-map
              :candidates (lambda ()
                            (gnus-notes-helm-candidates
                             (alist-get 'articles-crumbs
                                        gnus-notes-org--current-heading-alist)))
              :filtered-candidate-transformer  'gnus-notes-helm-candidate-transformer
              :persistent-action 'gnus-notes-org-helm-hydra-pa
              :persistent-help "view hydra"
              :action '(("Open article"               . gnus-notes--open-article)
                        ("Wide reply and yank"        . gnus-notes--reply-article-wide-yank)
                        ("Show thread"                . gnus-notes--show-article-thread)
                        ("Copy org link to kill ring" . gnus-notes-kill-new-org-link)
                        ("Display BBDB entries"       . gnus-notes-bbdb-display-all)))
   :buffer "*helm org heading articles*"
   :truncate-lines t))

(defun gnus-notes-org-handle-mail ()
  "Handle mail in org heading.
First, this function sets the variable
`gnus-notes--current-heading-alist' with the the current heading
info. Second, it activates a hook to run after sending a message,
that will take care of the org stuff. Then it calls a hydra to
select the action on the email articles."
  (interactive)
  (setq gnus-notes-org--last-window-configuration (current-window-configuration))
  (gnus-notes-org-set-heading-alist)
  (if (alist-get 'org-links-gnus gnus-notes-org--current-heading-alist)
      (progn
        (gnus-notes-org-message-add-hooks)
        (hydra-gnus-notes-org-handle-mail/body))
    (gnus-message 5 "No gnus links found in current org entry")
    (gnus-notes-org-clear-heading-alist)))

(defhydra hydra-gnus-notes-org-handle-mail (:color blue :columns 2)
  "Reply to email from current heading"
  ("h" gnus-notes-org-handle-mail-crumbs "View in helm")
  ("t" gnus-notes-org-handle-mail-top "Reply to top")
  ("v" gnus-notes-org-handle-mail-view "Search Gnus (imap)")
  ("q" gnus-notes-org-clear-heading-alist "quit"))

(defun gnus-notes-org-capture-mail ()
  "Capture a note on an email using the `org-mode' capture interface.
While viewing emails in gnus, in a summary or article buffer,
quickly capture an org note capture system. The capture template
will be preselected with the `gnus-notes-org-capture-key',
unless it is not defined in `org-capture-templates'. The gnus
keywords should be available during template expansion."
  (interactive)
  (unless (memq major-mode '(gnus-summary-mode gnus-article-mode))
    (user-error "Not in a gnus summary or article buffer"))
  (org-capture nil
               (cl-find gnus-notes-org-capture-key
                        (mapcar #'car org-capture-templates) :test #'equal)))

(defun gnus-notes-org-get-entry (&optional keep-properties)
  "Get the org entry text.
With the optional KEEP-PROPERTIES non-nil keep the text
properties. By default, text properties are removed."
  (interactive)
  (when (eq major-mode 'org-agenda-mode)
    (org-agenda-goto))
  (if keep-properties
      (org-get-entry)
    (gnus-string-remove-all-properties (org-get-entry))))

(defun gnus-notes-org-search-string-org-links-gnus (txt)
  "Search text TXT for org-links, having protocol \"gnus:\".
Returns a list of org-links, that point to gnus articles."
  (mapcar #'car
          (s-match-strings-all "\\[\\[gnus:.+\\]\\]"
                               (gnus-string-remove-all-properties txt))))

(defun gnus-notes-org-get-orgids (txt)
  "Find the org-ids in org entry text TXT."
  (mapcar (lambda (x) (cadr (split-string x ":" t " +")))
          (mapcar #'car
                  (s-match-strings-all "^ +:ID:.+" txt))))

(defun gnus-notes-org-filter-message-ids-list (id-list)
  "Get the article message-id that have any of the given org-ids.
ID-LIST is a list of org-ids to search in `gnus-notes--articles-list'.
Returns a combined list of all article message-ids found."
  ;; FIXME: use equal for the test
  (mapcan (lambda (id) (gnus-notes-filter-prop 'message-id id #'string=))
          id-list))

(defun gnus-notes-org-message-add-header (header value)
  "Add a HEADER when composing a new message.
VALUE is the value for the header."
  (when (and value (derived-mode-p 'message-mode 'mail-mode))
    (save-excursion
      (save-restriction
        (message-narrow-to-headers-or-head)
        (open-line 1)
        (message-insert-header header value)))))

(defun gnus-notes-org-message-add-header-orgid (&optional orgid)
  "Add an X-Org-Id header to an outgoing message.
When the optional argument ORGID is missing, will get the orgid
value from `gnus-notes-org-get-heading-alist'."
  (gnus-notes-org-message-add-header
   'X-Org-Id (or orgid
                  (alist-get 'uid gnus-notes-org--current-heading-alist))))

(defhydra hydra-gnus-org-helm (:columns 4 :exit nil)
  "Persistent actions"
  ("c" (gnus-notes-kill-new-org-link gnus-notes-helm-current-data-pa) "Copy Org link")
  ("b" (gnus-notes-bbdb-display-all  gnus-notes-helm-current-data-pa) "BBDB entries")
  ("{" helm-enlarge-window "enlarge")
  ("}" helm-narrow-window "narrow")
  (">" helm-toggle-truncate-line "wrap lines")
  ("_" helm-toggle-full-frame "full frame")
  ("Y" helm-yank-selection "yank entry")
  ("U" helm-refresh "update data")
  ("q" nil "quit" :exit t))

(defun gnus-notes-org-helm-hydra-pa (artdata)
  "Persistent action activates a Hydra.
ARTDATA is the current article in the helm buffer."
  (setq gnus-notes-helm-current-data-pa artdata)
  (hydra-gnus-org-helm/body))

;; keybindings
(defun gnus-notes-org-define-key (&optional key)
  "Bind KEY for org integration.
A convenience function to define a single key sequence for
integration with org. By default KEY is set to \"<Control-c t>\"."
  (unless key (setq key "C-c t"))
  (define-key org-mode-map          (kbd key) #'gnus-notes-org-handle-mail)
  (org-defkey org-agenda-keymap     (kbd key) #'gnus-notes-org-handle-mail)
  (define-key gnus-summary-mode-map (kbd key) #'gnus-notes-org-capture-mail)
  (define-key gnus-article-mode-map (kbd key) #'gnus-notes-org-capture-mail))

;; init actions
(defun gnus-notes-org-init ()
  "Start-up actions for `gnus-notes-org'."
  (when gnus-notes-org-capture-template
    (add-to-list 'org-capture-templates gnus-notes-org-capture-template ))
  (when gnus-notes-org--template-context
    (add-to-list 'org-capture-templates-contexts gnus-notes-org--template-context )))

(provide 'gnus-notes-org)
;;; gnus-notes-org.el ends here

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
