;;; gnus-notes.el --- Keep handy notes of read Gnus articles with helm and org  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Deus Max

;; Author: Deus Max <deusmax@gmx.com>
;; URL: https://github.com/deusmax/gnus-notes
;; Version: 0.3.1
;; Package-Requires: ((emacs "25.1") (bbdb "3.1") (helm "3.1") (hydra "0.13.0") (org "8.3") (s "0.0") (lv "0.0") (async "1.9.1"))
;; Keywords: convenience, mail, bbdb, gnus, helm, org, hydra

;; This file is not part of GNU Emacs.

;;; License

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
;;
;; Keep handy notes of your read Gnus articles with helm and org.
;;
;; This file provides the core setup and data needs of gnus-notes.
;;
;; Keep notes on the gnus articles that are important to me. The
;; rest can simply be removed from gnus-notes, without affecting Gnus.
;; If an article is removed from gnus-notes, by accident or I want it
;; back for whatever reason, no problem. All I have to do is view the
;; article in gnus, and it is back on gnus-notes.
;;
;; Gnus notes works in the background silently, keeping track of the
;; articles read with gnus. When an article is read, it adds a quick
;; note of it to notes. Simply, that's all. It removes notes of
;; deleted articles or the ones expunged by gnus.
;;
;; Gnus-notes is similar to the Gnus registry, but whereas the
;; registry tries to catch everything, gnus-notes is light-weight.
;; It doesn't try to keep everything. Only the articles "I have"
;; read. Its job is much simpler. "My read" articles are the only
;; really important "to me" articles, isn't is so ?
;;
;; This simplicity allows the user to add and remove articles to
;; gnus-notes, stress free.
;;
;; Viewing gnus-notes with the powerful helm interface brings great
;; search capabilities and all the other helm goodness.
;; Gnus-notes has been built around helm.
;;
;; Additional integration provided, or planned, with:
;; - org-mode, built-in
;; - BBDB built-in with gnus (gnus-insinuate 'bbdb 'message)
;; - EBDB (todo)
;;
;; Gnus is not limited to email, that is why gnus uses the term "articles".
;; Gnus-notes follows the Gnus general philosophy, it also uses the term
;; "articles". Most testing has been done on email (and IMAP in particular) and RSS.
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
(require 'gnus-sum)
(unless (require 'ol-gnus nil 'noerror)
  (require 'org-gnus))
(require 'rfc2047)
(require 'subr-x)
(require 'helm-lib)
(require 'bbdb-mua)

(declare-function gnus-notes-org-init "gnus-notes-org" nil)
(declare-function org-strip-quotes "ext:org-macs" (string))
(declare-function org-gnus-follow-link (if (featurep 'ol-gnus) "ext:ol-gnus" "ext:org-gnus") (&optional group article))

(defgroup gnus-notes nil
  "Keep handy notes of gnus articles."
  :tag "Gnus Notes"
  :group 'gnus)

(defcustom gnus-notes-top-dir (concat user-emacs-directory "gnus-notes/")
  "The parent directory for gnus-notes files."
  :group 'gnus-notes
  :type 'directory)

(defcustom gnus-notes-file  (concat gnus-notes-top-dir "articles.el")
  "A file to save the gnus notes articles list data.
Set to nil, for no article tracking between gnus sessions.
Otherwise, best to keep this file under `gnus-notes-top-dir'."
  :group 'gnus-notes
  :type 'file)

(defcustom gnus-notes-breadcrumbs-dir (concat gnus-notes-top-dir "crumbs/")
  "A directory for keeping article breadcrumbs in between saves.
Used only when `gnus-notes-file' is non-nil."
  :group 'gnus-notes
  :type 'directory)

(defcustom gnus-notes-format-time-string "%F %a %T"
  "A string for formatting the article date.
The format is used by `format-time-string'. See its documentation
for details on format specifiers. For example, to produce a full
ISO 8601 format, use \"%FT%T%z\", for org style use \"%F %a %T\".
Changing this variable affects only new entries. Previous entries
keep the old format."
  :group 'gnus-notes
  :type 'string)

(defface gnus-notes-group-face
  '((t . (:inherit font-lock-type-face :foreground "lightblue")))
  "Face used for gnus group in the notes articles list."
  :group 'gnus-notes)

(defface gnus-notes-date-face
  '((t . (:inherit font-lock-type-face)))
  "Face used for dates in the notes articles list."
  :group 'gnus-notes)

(defvar gnus-notes--articles-list nil
  "The list of articles kept by gnus-notes.")

(defvar gnus-notes--temp-message-headers nil
  "Internal variable; temporarily placing header data from an outgoing message.")

(defmacro gnus-notes-rot1 (ilst)
  "Cycle left the list elements, return the first item.
Argument ILST is the list to cycle its items."
  `(let ((x (pop ,ilst)))
     (nconc ,ilst (list x))
     x))

(defmacro gnus-notes-rot1r (ilst)
  "Cycle right the list elements, return the last item.
Argument ILST is the list to cycle its items."
  `(let ((x (car (last ,ilst))))
     (nbutlast ,ilst)
     (push x ,ilst)
     x))

(defun gnus-notes-decode-utf8 (string &optional charset)
  "Decode a gnus-group name.
Replaces `gnus-group-name-decode' for decoding group names. For
gnus group name a utf-8-emacs CHARSET is assumed unless provided
otherwise.
Argument STRING is the gnus-group name."
  (decode-coding-string string (or charset 'utf-8-emacs) t))

(defun gnus-notes-date-format (date)
  "Convert the DATE to string.
Date format specified in `gnus-notes-format-time-string'."
  (condition-case ()
      (format-time-string gnus-notes-format-time-string (gnus-date-get-time date))
    (error "Error in date format conversion")))

(defun gnus-notes-get-email (address &optional unbracket)
  "Get the email portion of a gnus address.
ADDRESS is a gnus sender or recipient address string. When optional
argument UNBRACKET is non-nil, brackets will be trimmed from the
email."
  (car (last (split-string address " " t (and unbracket "<\\|>")))))

(defun gnus-notes-get-email-name (address &optional use-email decode)
  "Get the name portion of a gnus address.
ADDRESS is a gnus sender or recipient address string. If a name
is not found and USE-EMAIL is not nil, use the email address as
default. If DECODE, RFC2047 decoding will be applied to the
display-name."
  (let ((x (split-string-and-unquote address)))
    (if (eql 1 (length x))
        (or (and use-email (car x)) "")
      (let ((name (string-join (butlast x) " ")))
        (if (and decode
                 (string= "=?" (substring name 0 2)))
            (org-strip-quotes (rfc2047-decode-address-string name))
          name)))))

(defun gnus-notes--get-article-data ()
    "Get the article data used for `gnus-notes' based on `gnus-summary-article-header'."
    (let* ((article-header (gnus-summary-article-header))
           (date  (gnus-notes-date-format (mail-header-date article-header)))
           (subject (mail-header-subject article-header))
           (author (mail-header-from article-header))
           (recipients (mail-header-extra article-header)))
      (dolist (r recipients)
        (setcdr r (rfc2047-decode-address-string (cdr r))))
      (list (format "%s%s: %s \t%s"
                    (gnus-notes--article-display-prefix author recipients)
                    (propertize (gnus-notes--article-display-name author recipients) 'face 'bold)
                    subject
                    (propertize date 'face 'gnus-notes-date-face))
            (cons 'group gnus-newsgroup-name)
            (cons 'message-id (mail-header-id article-header))
            (cons 'date date)
            (cons 'subject subject)
            (cons 'sender author)
            (cons 'recipients recipients)
            (cons 'references (mail-header-references article-header)))))

(defun gnus-notes--article-display-prefix (sender &optional recipients)
  "Display the proper article prefix based on article SENDER and RECIPIENTS."
  (if (string-match (gnus-ignored-from-addresses) sender)
      (if (and recipients (alist-get 'Newsgroups recipients))
          gnus-summary-newsgroup-prefix
        gnus-summary-to-prefix)
    ""))

(defun gnus-notes--article-display-name (sender recipients)
  "Compose the display name for the article.
If SENDER matches the variable `gnus-ignored-from-addresses' then
use the first RECIPIENTS name. If the To header has no name, then
fall back to a newsgroup name, if available."
  (when (string-match (gnus-ignored-from-addresses) sender)
    (setq sender (or (car-safe (bbdb-split ","
                                           (or (alist-get 'To recipients)
                                               (alist-get 'Newsgroups recipients)
                                               "")))
                     "")))
  (gnus-notes-get-email-name sender t))

(defun gnus-notes--track-article ()
  "Store this article in the notes article list.
For tracking of backend moves (B-m) see `gnus-notes--track-move-article'."
  (gnus-notes-add-to-list (gnus-notes--get-article-data)))

(defun gnus-notes--track-move-article (action article _from-group to-group _select-method)
  "Track backend move (B-m) of articles.
When ACTION is 'move, will change the group to TO-GROUP for the
article data in `gnus-notes--articles-list', but only if the
moved article was already tracked. ARTICLE is the gnus message
header. For use by `gnus-summary-article-move-hook', so all
arguments are passed by gnus."
  (when (eq action 'move)
    (if to-group
        (gnus-notes-update-message-id (mail-header-id article) to-group)
      (message-box "Move article to EmptyGroup! Probable Error!\n"))))

(defun gnus-notes--track-delete-article (action article _from-group &rest _rest)
  "Track interactive user deletion of articles.
Remove the article data in `gnus-notes--articles-list'. ACTION
should be 'delete. ARTICLE is the gnus message header. For use by
`gnus-summary-article-delete-hook', so all arguments are passed
by gnus."
  (when (eq action 'delete)
    (gnus-notes-forget-message-id (mail-header-id article) t)))

(defun gnus-notes--track-expire-article (action article _from-group to-group _select-method)
  "Track when articles expire.
Handle the article data in `gnus-notes--articles-list',
according to the expiry ACTION. TO-GROUP should have the value of
the expiry-target group if set. ARTICLE is the gnus message
header passed when the hook is run. For use by
`gnus-summary-article-expire-hook'."
  (when (eq action 'delete)
    (if to-group                        ; article moves to the expiry-target group
        (gnus-notes-update-message-id (mail-header-id article) to-group)
      (gnus-notes-forget-message-id (mail-header-id article) t)))) ; article deleted

(defmacro gnus-notes--shift (lst)
  "Put the first element of LST last, then return that element."
  `(let ((top (pop ,lst)))
     (setq ,lst (nconc ,lst (list top)) )
     top))

(defun gnus-notes-goto-previous (&optional no-retry)
  "Go to the top of the notes article list.
Unless NO-RETRY, we try going further back if the top of the
article list is the article we're currently looking at."
  (interactive)
  (if (not gnus-notes--articles-list)
      (message "No notes article to show")
    (gnus-notes--action
     (gnus-notes--shift gnus-notes--articles-list)
     (lambda (message-id group)
       (if (and (not no-retry)
                (equal (current-buffer) gnus-summary-buffer)
                (equal message-id (mail-header-id (gnus-summary-article-header))))
           (gnus-notes-goto-previous 'no-retry)
         (gnus-summary-read-group group 1) ; have to show at least one old one
         (gnus-summary-refer-article message-id))))))

(defun gnus-notes--action (msg func)
  "Find `message-id' and group arguments from MSG, call FUNC on them.
Warn if MSG can't be deconstructed as expected."
  (pcase msg
    (`(,_ . (,message-id ,group . ,_))
     (funcall func message-id group))
    (_
     (gnus-message 3 "Couldn't parse notes message: %S" msg))))

(defun gnus-notes--open-article (artdata)
  "Open ARTDATA gnus article using `org-gnus'."
  (org-gnus-follow-link (alist-get 'group artdata) (alist-get 'message-id artdata)))

(defun gnus-notes--reply-article-wide-yank (artdata)
  "Make a wide reply and yank to the current ARTDATA article."
  ;; TODO: handle the case the article/email doesn't exist any more
  (gnus-notes--open-article artdata)
  (call-interactively #'gnus-summary-wide-reply-with-original))
  ;; (when (fboundp 'gnus-notes-org-message-add-header-orgid)
  ;;   (gnus-notes-org-message-add-header-orgid)))

(defun gnus-notes--show-article-thread (artdata)
  "Show the ARTDATA gnus article thread in a summary buffer."
  (gnus-notes--open-article artdata)
  (gnus-warp-to-article)
  (call-interactively #'gnus-summary-refer-thread)
  (goto-char (point-min)))

(defun gnus-notes--create-org-link (artdata)
  "Return an `org-mode' link to ARTDATA Gnus article."
  (format "[[gnus:%s#%s][Email %s%s]]"
          (alist-get 'group artdata)
          (gnus-notes-string-unbracket (alist-get 'message-id artdata))
          (if (gnus-notes-outgoing-message-p artdata)
              ""
            "from ")
          (bbdb-string-trim
           (replace-regexp-in-string "[][\t]" ""
                                     (substring (car artdata) 0 48)))))

(defun gnus-notes-split-org-link-gnus (link)
  "Split a gnus article org LINK into its parts.
Returns a cons cell as (gnus-group . message-id)."
  (when link
    (let ((s (cl-subseq (split-string (substring-no-properties link 7) "[]#]") 0 2)))
      (cons (car s) (concat "<" (nth 1 s) ">")))))

(defun gnus-notes-string-unbracket (txt)
  "Trim brackets from TXT string."
  (replace-regexp-in-string "^<\\|>$" "" txt))

(defun gnus-notes-kill-new-org-link (artdata)
  "Add to the `kill-ring' an `org-mode' link to ARTDATA Gnus article."
  (kill-new (gnus-notes--create-org-link artdata))
  (gnus-message 5 "Added org-link to kill-ring"))

(defun gnus-notes-insert-org-link (artdata)
  "Insert an `org-mode' link to ARTDATA Gnus article."
  (insert (gnus-notes--create-org-link artdata)))

(defun gnus-notes-update-message-id (message-id to-group &optional no-crumb-save)
  "Update the Gnus article with MESSAGE-ID in `gnus-notes--articles-list'.
The Gnus article has moved to group TO-GROUP.
Set NO-CRUMB-SAVE non-nil to skip saving a crumb."
  (let ((article (gnus-notes-find-message-id message-id)))
    (when article
      (setf (alist-get 'group article) to-group)
      (unless no-crumb-save
        (gnus-notes--crumb-save article 'upd)))))

(defun gnus-notes-update (artdata to-group)
  "Update ARTDATA Gnus article in `gnus-notes--articles-list'.
The Gnus article has moved to group TO-GROUP."
  (gnus-notes-update-message-id (alist-get 'message-id artdata) to-group))

(defun gnus-notes-forget-message-id (message-id &optional print-msg no-crumb-save)
  "Remove the Gnus article with MESSAGE-ID in `gnus-notes--articles-list'.
When PRINT-MSG is non-nil, show a message about it.
Set NO-CRUMB-SAVE non-nil to skip saving a crumb file."
  (let ((l1 (length gnus-notes--articles-list))
        (article (car gnus-notes--articles-list)))
    ;; check for a match on the first article on list
    (if (equal message-id (alist-get 'message-id article))
        (pop gnus-notes--articles-list)
      (setq article (gnus-notes-find-message-id message-id))
      (when article
        (cl-delete article gnus-notes--articles-list :test 'equal :count 1)))
    (when (= 1 (- l1 (length gnus-notes--articles-list)))
      (unless no-crumb-save (gnus-notes--crumb-save article 'del))
      (when print-msg
        (gnus-message 4 "Removed 1 of 1 from gnus-notes articles")
        (gnus-message 4 "Removed item: %s from gnus-notes articles" (car article))))))

(defun gnus-notes-forget (artdata &optional print-msg)
  "Remove ARTDATA Gnus article from `gnus-notes--articles-list'.
When PRINT-MSG is non-nil, show a message about it."
  (gnus-notes-forget-message-id (alist-get 'message-id artdata) print-msg))

(defun gnus-notes-forget-all (&rest _artdata)
  "Clear the gnus-notes articles list."
  (interactive)
  (when (yes-or-no-p "Action can not be undone. Are you sure? ")
    (setq gnus-notes--articles-list nil)
    (gnus-notes--crumbs-clear-all)
    (gnus-message 4 "Cleared all gnus-notes article entries")))

(defun gnus-notes-bbdb-display-all (artdata)
  "Display sender and recipients in BBDB.
Display sender and all recipients in BBDB. Ask to create a BBDB entry, if not in
BBDB. ARTDATA is the gnus-notes data for the selected article."
  (let ((recipients (alist-get 'recipients artdata))
        (search-list '(bbdb-search (bbdb-records))))
    (setq recipients (append (bbdb-split "," (or (alist-get 'sender artdata) ""))
                             (bbdb-split "," (or (alist-get 'To recipients) ""))
                             (bbdb-split "," (or (alist-get 'Cc recipients) ""))))
    (dolist (r recipients)              ; add new entries to BBDB (ask)
      (bbdb-update-records (list (list (gnus-notes-get-email-name r t)
                                       (gnus-notes-get-email r t)))
                           'query t))
    ;; make an array (:mail email1 :mail email2 ...etc)
    (dolist (r recipients search-list)
      (helm-aif (gnus-notes-get-email r t)
          (nconc search-list (list :mail it))))
    (setq search-list (eval search-list))
    (if search-list
        (bbdb-display-records search-list 'multi-line nil)
      (gnus-message 4 "No matching BBDB records found"))))

;; FIXME: this function text is only a placeholder.
;; Now that gnus-notes uses the message-id to handle the articles in its'
;; article list, there should be no duplicates entries. Still, this function will
;; help with checking consistency, removal is just not that critical at the moment.
;; (defun gnus-notes-alist-find-duplicates ()
;;   "Find any duplicate entries in `gnus-notes--articles-list'.
;; Duplicates entries are considered those that have the same
;; message-id, even if some other property may differ such as the
;; group value. It returns a list of message-ids that are found more
;; than once."
;; (cl-find elem1 gnus-notes--articles-list))

(defun gnus-notes-filter-prop (prop value &optional test)
  "Return a list of all articles with PROP equal to VALUE.
Search the `gnus-notes--articles-list' for all elements with
property PROP equal to value.
Optional argument TEST should provide a test function. Default test 'equal'."
  (seq-filter (lambda (item)
                (funcall (or test #'equal) value (alist-get prop item)))
              gnus-notes--articles-list))

(defun gnus-notes-find-prop (prop value)
  "Check for an article with the property value given.
Find in `gnus-notes--articles-list' if there is a property PROP equal to VALUE.
Returns the first article data when a match is found. It does not try
to find any more matches."
  (seq-find (lambda (item)
              (equal value (alist-get prop item)))
            gnus-notes--articles-list))

(defun gnus-notes-find-message-id (message-id)
  "Search the gnus-notes articles data by MESSAGE-ID.
Returns the first article in `gnus-notes--articles-list' that
matches the MESSAGE-ID provided. A convenience wrapper for
`gnus-notes-find-prop'."
  (gnus-notes-find-prop 'message-id  message-id))

(defun gnus-notes-find-message-ids-list (msgids-list)
  "Search gnus-notes articles for MSGIDS-LIST.
Returns the list of articles in `gnus-notes--articles-list' that
match the list of message-id provided. MSGIDS-LIST is a list of
article message-ids."
  (mapcar #'gnus-notes-find-message-id msgids-list))

(defun gnus-notes-find-article (artdata)
  "Search the gnus-notes articles list for ARTDATA article.
Returns the first article in `gnus-notes--articles-list' that
matches the message-id of the ARTDATA article argument."
  (gnus-notes-find-message-id (alist-get 'message-id artdata)))

(defun gnus-notes-add-to-list (artdata &optional no-crumb-save)
  "Add the ARTDATA article data to the articles list.
Ensures the value for messsage-id is unique among all articles
stored in `gnus-notes--articles-list'. When NO-CRUMB-SAVE is
non-nil, will not save the article data to a crumb file. See
`gnus-notes--get-article-data' for the artdata article data
format."
  (when artdata
    (unless (gnus-notes-find-message-id (alist-get 'message-id artdata))
      (push artdata gnus-notes--articles-list)
      (unless no-crumb-save
        (gnus-notes--crumb-save artdata 'new)))))

(defun gnus-notes--crumb-filename (type)
  "Generate a full path filename for an article crumb.
Crumb files are used to store a single article data. They reside
in the `gnus-notes-breadcrumbs-dir' directory. TYPE should
indicate an action type, see `gnus-notes--crumb-save'."
  (format "%s/cr-%s-%s.el"
          (directory-file-name gnus-notes-breadcrumbs-dir)
          (format-time-string "%Y%m%d%H%M%S-%N")
          type))

(defun gnus-notes--crumbs-clear-all ()
  "Clear all crumb files."
  (dolist (crumb (directory-files gnus-notes-breadcrumbs-dir  t "^cr-" t))
    (delete-file crumb)))

(defun gnus-notes--crumbs-load ()
  "Load the article data saved in crumb files to `gnus-notes--articles-list'.
In case something goes wrong, crumb files are used to restore
`gnus-notes--articles-list', as not to lose any previous
actions."
  (let ((icount 0))
    (dolist (crumb (directory-files gnus-notes-breadcrumbs-dir  t "^cr-") icount)
      (cl-incf icount)
      (cond
       ((string-match-p "-new.el$" crumb) (gnus-notes-load-crumb-new crumb))
       ((string-match-p "-upd.el$" crumb) (gnus-notes-load-crumb-upd crumb))
       ((string-match-p "-del.el$" crumb) (gnus-notes-load-crumb-del crumb))
       (t (cl-decf icount)
          (message "Warning: found bad crumb: %s" (file-name-nondirectory crumb))))
      (delete-file crumb))))

(defun gnus-notes-load-crumb-new (crumb-file)
  "Load the elisp data in CRUMB-FILE to `gnus-notes--articles-list'.
CRUMB-FILE is the full file path to a crumb file of type new.
Pass non-nil for the optional argument to
`gnus-notes-add-to-list' no-crumb-save, not to save another
crumb."
  (gnus-notes-add-to-list (gnus-notes--read-file-contents crumb-file) t))

(defun gnus-notes-load-crumb-upd (crumb-file)
  "Use the elisp data in CRUMB-FILE to update `gnus-notes--articles-list'.
CRUMB-FILE is the full file path to a crumb file of type upd.
Pass non-nil for the optional argument to
`gnus-notes-add-to-list' no-crumb-save, not to save another
crumb."
  (let ((article (gnus-notes--read-file-contents crumb-file)))
    (gnus-notes-update-message-id (alist-get 'message-id article)
                                   (alist-get 'group article)
                                   t)))

(defun gnus-notes-load-crumb-del (crumb-file)
  "Use CRUMB-FILE to delete an item in `gnus-notes--articles-list'.
CRUMB-FILE is the full file path to a crumb file of type del.
Pass non-nil for the optional argument to
`gnus-notes-add-to-list' no-crumb-save, not to save another
crumb."
  (gnus-notes-forget-message-id
   (alist-get 'message-id (gnus-notes--read-file-contents crumb-file))
   t t))

(defun gnus-notes--crumb-save (artdata type)
  "Backup single article data until the next save.
TYPE should be one of 'new, 'upd or 'del.
ARTDATA is an alist of the article data."
  (with-temp-file (gnus-notes--crumb-filename type)
    (prin1 artdata (current-buffer))))

(defun gnus-notes--read-file-contents (file)
  "Read the contents of a file.
FILE is the full file path."
  (if (and file (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (read (current-buffer)))
    (error "Can not read file '%s'" file)))

(defun gnus-notes-save ()
  "Save the gnus notes items to file for persistence."
  (interactive)
  (when gnus-notes-file
    (if (file-writable-p gnus-notes-file)
        (progn
          (gnus-message 5 "Saving gnus-notes data to %s." gnus-notes-file)
          (with-temp-file gnus-notes-file
            (let ((print-level nil)
                  (print-length nil))
              (prin1 gnus-notes--articles-list (current-buffer))))
          (gnus-notes--crumbs-clear-all)
          (gnus-message 5 "Saving gnus-notes data (%d items) done."
                        (length gnus-notes--articles-list)))
      (error "Error: can not save gnus-notes data to %s" gnus-notes-file))))

(defun gnus-notes-read ()
  "Read gnus-notes data from a previous session."
  (interactive)
  (when gnus-notes-file
    (if (file-readable-p gnus-notes-file)
        (progn
          (gnus-message 5 "Reading gnus-notes data from %s." gnus-notes-file)
          (setq gnus-notes--articles-list
                (gnus-notes--read-file-contents gnus-notes-file))
          (gnus-message 5 "Read %d item(s) from %s... done."
                        (length gnus-notes--articles-list) gnus-notes-file)
          (let ((numcrumbs (gnus-notes--crumbs-load)))
            (gnus-message 5 "Loaded %d item(s) gnus-notes crumbs found... done" numcrumbs)
            (when (> numcrumbs 0)
              (gnus-notes-save))))
      (error "Error: can not read gnus-notes data from %s" gnus-notes-file))))

(defun gnus-notes-count-saved ()
  "Count the number of articles saved in `gnus-notes-file'."
  (if (and gnus-notes-file (file-readable-p gnus-notes-file))
      (length (read
               (with-temp-buffer
                 (insert-file-contents gnus-notes-file)
                 (buffer-string))))
    nil))

;;
;; Track outgoing messages
;;
(defun gnus-notes--track-message ()
  "Add an newly sent message to the list of tracked articles.
Is run from `message-sent-hook'. A alist of the message header
data should be available on `gnus-notes--temp-message-headers'."
  (interactive)
  (let* ((hdrs gnus-notes--temp-message-headers)
         (date (gnus-notes-date-format (alist-get 'date hdrs)))
         (author (rfc2047-decode-address-string (or (alist-get 'from hdrs) "")))
         (recipients (rassq-delete-all nil
                                       (list (cons 'Newsgroups (alist-get 'newsgroups hdrs))
                                             (cons 'To (alist-get 'to hdrs))
                                             (cons 'Cc (alist-get 'cc hdrs)))))
         (to-first (car (bbdb-split "," (or (alist-get 'To recipients) "")))))
    (dolist (r recipients)
      (setcdr r (rfc2047-decode-address-string (cdr r))))
    ;; This is a new message, can directly add to list... but better be safe.
    (gnus-notes-add-to-list
     (list (format "%s%s: %s \t%s"
                   (if (alist-get 'Newsgroups recipients)
                       gnus-summary-newsgroup-prefix
                     gnus-summary-to-prefix)
                   (propertize (gnus-notes-get-email-name to-first t) 'face 'bold)
                   (alist-get 'subject hdrs)
                   (propertize date 'face 'gnus-notes-date-face))
           (cons 'group (alist-get 'gcc hdrs))
           (cons 'message-id (alist-get 'message-id hdrs))
           (cons 'date  date)
           (cons 'subject (alist-get 'subject  hdrs))
           (cons 'sender author)
           (cons 'recipients recipients)
           (cons 'references (alist-get 'references hdrs))
           (cons 'in-reply-to (alist-get 'in-reply-to hdrs))))))

(defun gnus-notes--get-message-data ()
  "Get the headers from a new outgoing message.
Returns a header alist, see function
`mail-header-extract-no-properties'. Needs to run with the
`message-header-hook' which applies narrowing to the message
headers by default. Saves the header data to variable
`gnus-notes--temp-message-header' so it can get out of the way
as quickly as possible. After the message is sent,
`gnus-notes--track-message' processes the header data and adds
an entry to `gnus-notes--articles-list'.
Note that `mail-header-extract' downcases the property headers."
  (save-restriction
    (goto-char (point-min))
    (setq gnus-notes--temp-message-headers (mail-header-extract-no-properties))))

(defun gnus-notes-outgoing-message-p (artdata)
  "Check the title of a gnus-notes article for the outgoing message prefix.
ARTDATA is the gnus-notes article data."
  (string= gnus-summary-to-prefix
           (substring (car artdata) 0 (length gnus-summary-to-prefix))))

;;
;; Redifining gnus stuff
;;
;; FIXME: should not need to redifine other libraries.
;; (defmacro gnus-group-entry (group)
;;   "Get the newsrc entry for GROUP.
;; Modified not to error when `gnus-newsrc-hashtb' is not assigned."
;;   (when (hash-table-p gnus-newsrc-hashtb) `(gethash ,group gnus-newsrc-hashtb)))

;;
;; starting gnus-notes
;;
(defun gnus-notes-init ()
  "Start Gnus Notes."
  (interactive)
  (gnus-notes-check-files)
  (gnus-message 5 "Starting gnus-notes")
  (gnus-notes-add-hooks)
  (gnus-notes-org-init)
  (gnus-notes-read))

(defun gnus-notes-check-files ()
  "Check for the gnus-notes directories.
If the directories don't exist, create them."
  (dolist (d (list gnus-notes-top-dir gnus-notes-breadcrumbs-dir))
    (unless (file-exists-p d)
      (make-directory d t))))

(defun gnus-notes-add-hooks ()
  "Install the gnus-notes hooks."
  (interactive)
  ;; Activate the hooks  (should be named -functions)
  ;; Note: except for the 1st, the other hooks run using run-hook-with-args
  (add-hook 'gnus-article-prepare-hook        #'gnus-notes--track-article)
  (add-hook 'gnus-summary-article-move-hook   #'gnus-notes--track-move-article)
  (add-hook 'gnus-summary-article-delete-hook #'gnus-notes--track-delete-article)
  (add-hook 'gnus-summary-article-expire-hook #'gnus-notes--track-expire-article)
  ;; hooks for new messages
  (add-hook 'message-header-hook #'gnus-notes--get-message-data)
  ;; TODO: replace this hook call with an async call from gnus-notes--get-message-data.
  ;;       Optimize later.
  (add-hook 'message-sent-hook #'gnus-notes--track-message)

  ;; hooks related to saving the data
  (add-hook 'gnus-save-newsrc-hook #'gnus-notes-save)
  (add-hook 'kill-emacs-hook #'gnus-notes-save))

(defun gnus-notes-remove-hooks ()
  "Remove the gnus-notes hooks."
  (interactive)
  (remove-hook 'gnus-article-prepare-hook        #'gnus-notes--track-article)
  (remove-hook 'gnus-summary-article-move-hook   #'gnus-notes--track-move-article)
  (remove-hook 'gnus-summary-article-delete-hook #'gnus-notes--track-delete-article)
  (remove-hook 'gnus-summary-article-expire-hook #'gnus-notes--track-expire-article)
  ;; hooks for new messages
  (remove-hook 'message-header-hook #'gnus-notes--get-message-data)
  (remove-hook 'message-sent-hook #'gnus-notes--track-message)
  ;; hooks related to saving the data
  (remove-hook 'gnus-save-newsrc-hook #'gnus-notes-save)
  (remove-hook 'kill-emacs-hook #'gnus-notes-save))

(provide 'gnus-notes)
;;; gnus-notes.el ends here

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
