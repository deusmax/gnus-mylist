;;; gnus-notes-helm.el --- View gnus-notes with helm -*- lexical-binding: t -*-
;;
;; Copyright (C) 2020 Deus Max

;; Author: Deus Max <deusmax@gmx.com>
;; URL: https://github.com/deusmax/gnus-notes
;; Version: 0.3.1
;; Keywords: convenience, mail, bbdb, gnus, helm, org, hydra

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

;; Keep handy notes of your read Gnus articles with helm and org.
;;
;; Viewing gnus-notes with the helm interface brings powerful
;; search features and all the other goodness provided by helm.
;; This file provides the helm interface for viewing gnus-notes.
;;
;;
;; This package is a fork of gnus-recent with additional inspiration by gnorb.
;;
;;; To use:
;;
;; The helm command 'gnus-notes-helm', is the entry point for  using gnus-notes.
;; For quick access assign to a global key:
;;
;;     (require 'gnus-notes-helm)
;;     (gnus-notes-init)
;;     (global-set-key (kbd "C-c m") #'gnus-notes-helm)
;;
;; Can also add `gnus-notes-helm' as an option to your favorite hydra.
;;
;; For org-mode integration, activate the key bindings:
;;     (gnus-notes-org-define-key)       ; default "C-c t"
;;

;;; Code:

(require 'gnus)
(require 'helm)
(require 'hydra)
(require 'async)
(require 'lv)
(require 'gnus-notes)
(require 'gnus-notes-org)

(defvar gnus-notes-helm-display-extra nil
  "Display extra article info.")

(defvar gnus-notes-helm-display-levels '(To Cc Msgid Orgid nil)
  "Display levels for extra article info.")

(defvar gnus-notes-helm-current-data-pa nil
  "Keeps the article for the persistent action Hydra.")

(defvar gnus-notes-helm-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<up>") 'gnus-notes-helm-display-cycle)
    map)
  "Keymap for a `helm' source.")

(defun gnus-notes-helm-display-cycle ()
  "Cycle the levels of article info to display.
The user interactively changes the article information display
level. Currently only the \"default\", \"To\" and \"Cc\" levels
are implemented. The function will refresh the `helm' buffer to
display the new level."
  (interactive)
  (setq gnus-notes-helm-display-extra (pop gnus-notes-helm-display-levels))
  (nconc gnus-notes-helm-display-levels (list gnus-notes-helm-display-extra))
  (helm-refresh))

(defun gnus-notes-helm-display-select ()
  "Select the level of article info to display.
The user selects the article information display level. Currently only the
  \"default\", \"To\" and \"Cc\" levels are implemented.
The function will refresh the `helm' buffer to display the new level."
  (interactive)
  (let ((display (read-char "Show level: default (d) | To (t) |  Cc (c): ")))
    (cond
     ((eq display ?t) (setq gnus-notes-helm-display-extra 'To))
     ((eq display ?c) (setq gnus-notes-helm-display-extra 'Cc))
     (t (setq gnus-notes-helm-display-extra nil))))
  (helm-refresh))

(defun gnus-notes-helm-candidate-transformer (candidates source)
  "Transform the `helm' data for the display level selected.
This function acts on the filtered data, to show the selected
display information. CANDIDATES is the list of filtered data.
SOURCE is the `helm' source. Both arguments are passed by `helm',
when the function is called. Also the `helm' multi-line feature is
turned on and off, as needed."
  (pcase gnus-notes-helm-display-extra
    ('To (helm-attrset 'multiline nil source)
         (mapcar #'gnus-notes-helm-candidates-display-to candidates))
    ('Cc (helm-attrset 'multiline nil source)
         (mapcar #'gnus-notes-helm-candidates-display-cc candidates))
    ('Msgid (helm-attrset 'multiline nil source)
            (mapcar #'gnus-notes-helm-candidates-display-msgid candidates))
    ('Orgid (helm-attrset 'multiline nil source)
            (mapcar #'gnus-notes-helm-candidates-display-orgid candidates))
    (_ (assq-delete-all 'multiline source)
       candidates)))

;; FIXME: should be using gnus-group-decoded-name
(defun gnus-notes-helm-candidates-display-default (item)
  "The default text to display for each article.
Is the function argument to `mapcar' for establishing the
candidates default text. `Helm' uses this text to filter the
articles list.
For each article data ITEM in `gnus-notes--articles-list' it
returns a cons cell (name . item), where name is the article
display text."
  (cons (concat (car item)
                " ["
                (propertize (gnus-notes-decode-utf8 (alist-get 'group item))
                            'face 'gnus-notes-group-face)
                "]")
        item))

(defun gnus-notes-helm-candidates-display-to (item)
  "Display the To field for each article on a separate line.
ITEM is the article data in `gnus-notes--articles-list'. Used as the function
  argument to `mapcar.'"
  (cons (concat (car item)
                "\n    To: "
                (alist-get 'To  (alist-get 'recipients item)))
        item))

(defun gnus-notes-helm-candidates-display-cc (item)
  "Display the To and Cc fields for each article on separate lines.
ITEM is the article data in `gnus-notes--articles-list'. Used as the function
argument to `mapcar.'"
  (cons (concat (car item)
                "\n    To: "
                (alist-get 'To  (alist-get 'recipients item))
                (helm-aif (alist-get 'Cc  (alist-get 'recipients item))
                    (concat "\n    Cc: " it)))
        item))

(defun gnus-notes-helm-candidates-display-msgid (item)
  "Display the To, Cc and message-id fields for each article on separate lines.
ITEM is the article data in `gnus-notes--articles-list'. Used as the function
argument to `mapcar.'"
  (cons (concat (car item)
                "\n    To: "
                (alist-get 'To  (alist-get 'recipients item))
                (helm-aif (alist-get 'Cc  (alist-get 'recipients item))
                    (concat "\n    Cc: " it))
                "\n    MsgID: "
                (alist-get 'message-id item))
        item))

(defun gnus-notes-helm-candidates-display-orgid (item)
  "Rotate the display of article fields.
The displayed fields are the To, Cc, message-id and org-id for
each article. Each is shown on a separate line. ITEM is the
article data in `gnus-notes--articles-list'. Used as the
function argument to `mapcar.'"
  (cons (concat (car item)
                "\n    To: "
                (alist-get 'To  (alist-get 'recipients item))
                (helm-aif (alist-get 'Cc  (alist-get 'recipients item))
                    (concat "\n    Cc: " it))
                "\n    MsgID: "
                (alist-get 'message-id item)
                (helm-aif (alist-get 'org-id item)
                    (concat "\n    orgid: "
                            (symbol-name it)))) ; FIXME: org-id should be text, not symbols
        item))


(defun gnus-notes-helm-candidates (articles-list)
  "Initialize the `helm' candidates data.
ARTICLES-LIST is the data list of articles for helm."
  (mapcar #'gnus-notes-helm-candidates-display-default articles-list))

(defun gnus-notes-helm-forget (_artlistitem)
  "Remove Gnus articles from `gnus-notes--articles-list' using `helm'.
Helm allows for marked articles or current selection.  See
function `helm-marked-candidates'.  Argument _ARTLISTITEM is not used."
  (let* ((cand (helm-marked-candidates))
         (l1-cand (length cand))
         (l1-gral (length gnus-notes--articles-list)))
    (dolist (article cand)
      (gnus-notes-forget article t))
    (gnus-message 4 "Removed %d of %d article(s) from gnus-notes done."
                  (- l1-gral (length gnus-notes--articles-list))
                  l1-cand)))

(defun gnus-notes-helm-forget-pa (artdata)
  "Forget current or marked articles without quitting `helm'.
This is the persistent action defined for the helm session.
Argument ARTDATA is the article data."
  (gnus-notes-helm-forget artdata)
  (helm-delete-current-selection)
  (helm-refresh))

(defhydra hydra-gnus-notes-helm (:columns 3 :exit nil)
  "Persistent actions"
  ("c" (gnus-notes-kill-new-org-link gnus-notes-helm-current-data-pa) "Copy Org link")
  ("b" (gnus-notes-bbdb-display-all gnus-notes-helm-current-data-pa) "BBDB entries")
  ("K" (gnus-notes-helm-forget-pa gnus-notes-helm-current-data-pa) "Forget current")
  ("{" helm-enlarge-window "helm window enlarge")
  ("}" helm-narrow-window "helm window narrow")
  (">" helm-toggle-truncate-line "helm toggle truncate lines")
  ("_" helm-toggle-full-frame "helm toggle full frame")
  ("Y" helm-yank-selection "helm yank selection")
  ("U" helm-refresh "helm update source")
  ("q" nil "quit" :exit t))

(defun gnus-notes-helm-hydra-pa (artdata)
  "Persistent action activates a Hydra.
ARTDATA is the current article in the helm buffer."
  (setq gnus-notes-helm-current-data-pa artdata)
  (hydra-gnus-notes-helm/body))

;;;###autoload
(defun gnus-notes-helm ()
  "Use `helm' to filter the Gnus notes articles.
Also a number of possible actions are defined."
  (interactive)
  (helm :sources (helm-build-sync-source "Gnus notes articles"
                   :keymap gnus-notes-helm-map
                   :candidates (lambda () (gnus-notes-helm-candidates gnus-notes--articles-list))
                   :filtered-candidate-transformer  'gnus-notes-helm-candidate-transformer
                   :persistent-action 'gnus-notes-helm-hydra-pa
                   :persistent-help "quick actions"
                   :action '(("Open article"               . gnus-notes--open-article)
                             ("Reply article"              . gnus-notes--reply-article-wide-yank)
                             ("Show thread"                . gnus-notes--show-article-thread)
                             ("Copy org link to kill ring" . gnus-notes-kill-new-org-link)
                             ("Insert org link"            . gnus-notes-insert-org-link)
                             ("Insert quick-note"          . gnus-notes-insert-quick-note)
                             ("Remove marked article(s)"   . gnus-notes-helm-forget)
                             ("Display BBDB entries"       . gnus-notes-bbdb-display-all)
                             ("Clear all"                  . gnus-notes-forget-all)))
        :buffer "*helm gnus notes*"
        :truncate-lines t))

(provide 'gnus-notes-helm)
;;; gnus-notes-helm.el ends here

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
