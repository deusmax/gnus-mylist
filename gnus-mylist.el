;;; gnus-mylist.el --- Handy notes of Gnus articles with helm and org  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Deus Max

;; Author: Deus Max <deusmax@gmx.com>
;; URL: https://github.com/deusmax/gnus-mylist
;; Version: 0.3.0
;; Package-Requires: ((emacs "25.1.0") (bbdb "3.1") (helm "3.1") (org "8.3") (s "0.0"))
;; Keywords: convenience, mail, gnus, helm, org

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
;;
;; Keep handy notes of your read Gnus articles with helm and org.
;;
;; This is "my list", so I can keep only the articles that are
;; important to me. The rest I can simply remove from mylist,
;; without affecting Gnus. If an article is removed, by accident or
;; I want it back for whatever reason, no problem. All I have to do
;; is view the article in gnus, and it is back on the list !
;;
;; Gnus mylist works in the background silently, keeping track of the
;; articles read with gnus. When an article is read, it adds a quick
;; note of it to mylist. Simply, that's all. It removes notes of
;; deleted articles or the ones expunged by gnus.
;;
;; Gnus-mylist is similar to the Gnus registry, but whereas the
;; registry tries to catch everything, gnus-mylist is light-weight.
;; It doesn't try to keep everything. Only the articles "I have"
;; read. Its job is much simpler. "My read" articles are the only
;; really important "to me" articles, isn't is so ?
;;
;; This simplicity allows the user to add and remove articles to
;; gnus-mylist, stress free.
;;
;; Viewing gnus-mylist with the powerful helm interface brings great
;; search capabilities and all the other helm goodness.
;; Gnus-mylist has been built around helm.
;;
;; To start using gnus-mylist, use the helm command 'gnus-mylist-helm'.
;; For quick access assign to a global key:
;;
;;     (global-set-key (kbd "C-c m") #'gnus-mylist-helm)
;;
;; Or add an option to your favorite hydra.
;;
;; Additional integration provided, or planned, with:
;; - org-mode, with gnus-mylist-org
;; - BBDB built-in with gnus (gnus-insinuate)
;; - EBDB (todo)
;;
;; Gnus is not limited to email, that is why gnus uses the term "articles".
;; Gnus-mylist follows the Gnus general philosophy, it also uses the term
;; "articles". Most testing has been done on email (and IMAP in particular) and RSS.
;;
;; This package is a fork of gnus-recent with additional inspiration by gnorb.

;;; To use, require:
;;
;; (require 'gnus-mylist)
;;

;;; Code:

(require 'gnus)
(require 'gnus-sum)
(unless (require 'ol-gnus nil 'noerror)
  (require 'org-gnus))
(require 'rfc2047)
(require 'helm-lib)
(require 'bbdb)
(require 'bbdb-mua)
(require 'gnus-mylist-helm)
(require 'gnus-mylist-org)

(defgroup gnus-mylist nil
  "Keep handy notes of gnus articles."
  :tag "Gnus Mylist"
  :group 'gnus)

(defcustom gnus-mylist-top-dir (concat user-emacs-directory "gnus-mylist/")
  "The parent directory for gnus-mylist files."
  :group 'gnus-mylist
  :type 'directory)

(defcustom gnus-mylist-file  (concat gnus-mylist-top-dir "articles.el")
  "A file to save the gnus mylist articles list data.
Set to nil, for no article tracking between gnus sessions.
Otherwise, best to keep this file under `gnus-mylist-top-dir'."
  :group 'gnus-mylist
  :type 'file)

(defcustom gnus-mylist-breadcrumbs-dir (concat gnus-mylist-top-dir "crumbs/")
  "A directory for keeping article breadcrumbs in between saves.
Used only when `gnus-mylist-file' is non-nil."
  :group 'gnus-mylist
  :type 'directory)

(defcustom gnus-mylist-format-time-string "%F %a %T"
  "A string for formating the article date.
The format is used by `format-time-string'. See its documentation
for details on format specifiers. For example, to produce a full
ISO 8601 format, use \"%FT%T%z\", for org style use \"%F %a %T\".
Changing this variable affects only new entries. Previous entries
keep the old format."
  :group 'gnus-mylist
  :type 'string)

(defface gnus-mylist-group-face
  '((t . (:inherit font-lock-type-face :foreground "lightblue")))
  "Face used for gnus group in the mylist articles list."
  :group 'gnus-mylist)

(defface gnus-mylist-date-face
  '((t . (:inherit font-lock-type-face)))
  "Face used for dates in the mylist articles list."
  :group 'gnus-mylist)

(defvar gnus-mylist--articles-list nil
  "The list of articles kept by gnus-mylist.")

(defvar gnus-mylist--temp-message-headers nil
  "Internal variable; temporarily placing header data from an outgoing message.")

(defmacro gnus-mylist-rot1 (ilst)
  "Cycle left the list elements, return the first item.
Argument ILST is the list to cycle its items."
  `(let ((x (pop ,ilst)))
     (nconc ,ilst (list x))
     x))

(defmacro gnus-mylist-rot1r (ilst)
  "Cycle right the list elements, return the last item.
Argument ILST is the list to cycle its items."
  `(let ((x (car (last ,ilst))))
     (nbutlast ,ilst)
     (push x ,ilst)
     x))

(defun gnus-mylist-decode-utf8 (string &optional charset)
  "Decode a gnus-group name.
Replaces `gnus-group-name-decode' for decoding group names. For
gnus group name a utf-8-emacs CHARSET is assumed unless provided
otherwise.
Argument STRING is the gnus-group name."
  (decode-coding-string string (or charset 'utf-8-emacs) t))

(defun gnus-mylist-date-format (date)
  "Convert the DATE to string.
Date format specified in `gnus-mylist-format-time-string'."
  (condition-case ()
      (format-time-string gnus-mylist-format-time-string (gnus-date-get-time date))
    (error "Error in date format conversion")))

(defun gnus-mylist-get-email (address &optional unbracket)
  "Get the email portion of a gnus address.
ADDRESS is a gnus sender or recipient address string. When optional
argument UNBRACKET is non-nil, brackets will be trimmed from the
email."
  (car (last (split-string address " " t (and unbracket "<\\|>")))))

(defun gnus-mylist-get-email-name (address &optional use-email decode)
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

(defun gnus-mylist--get-article-data ()
    "Get the article data used for `gnus-mylist' based on `gnus-summary-article-header'."
    (let* ((article-header (gnus-summary-article-header))
           (date  (gnus-mylist-date-format (mail-header-date article-header)))
           (subject (mail-header-subject article-header))
           (author (mail-header-from article-header))
           (recipients (mail-header-extra article-header)))
      (dolist (r recipients)
        (setcdr r (rfc2047-decode-address-string (cdr r))))
      (list (format "%s%s: %s \t%s"
                    (gnus-mylist--article-display-prefix author recipients)
                    (propertize (gnus-mylist--article-display-name author recipients) 'face 'bold)
                    subject
                    (propertize date 'face 'gnus-mylist-date-face))
            (cons 'group gnus-newsgroup-name)
            (cons 'message-id (mail-header-id article-header))
            (cons 'date date)
            (cons 'subject subject)
            (cons 'sender author)
            (cons 'recipients recipients)
            (cons 'references (mail-header-references article-header)))))

(defun gnus-mylist--article-display-prefix (sender &optional recipients)
  "Display the proper article prefix based on article SENDER and RECIPIENTS."
  (if (string-match (gnus-ignored-from-addresses) sender)
      (if (and recipients (alist-get 'Newsgroups recipients))
          gnus-summary-newsgroup-prefix
        gnus-summary-to-prefix)
    ""))

(defun gnus-mylist--article-display-name (sender recipients)
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
  (gnus-mylist-get-email-name sender t))

(defun gnus-mylist--track-article ()
  "Store this article in the mylist article list.
For tracking of Backend moves (B-m) see `gnus-mylist--track-move-article'."
  (gnus-mylist-add-to-list (gnus-mylist--get-article-data)))

(defun gnus-mylist--track-move-article (action article _from-group to-group _select-method)
  "Track backend move (B-m) of articles.
When ACTION is 'move, will change the group to TO-GROUP for the
article data in `gnus-mylist--articles-list', but only if the
moved article was already tracked. ARTICLE is the gnus message
header. For use by `gnus-summary-article-move-hook', so all
arguments are passed by gnus."
  (when (eq action 'move)
    (if to-group
        (gnus-mylist-update-message-id (mail-header-id article) to-group)
      (message-box "Move article to EmptyGroup! Probable Error!\n"))))

(defun gnus-mylist--track-delete-article (action article _from-group &rest _rest)
  "Track interactive user deletion of articles.
Remove the article data in `gnus-mylist--articles-list'. ACTION
should be 'delete. ARTICLE is the gnus message header. For use by
`gnus-summary-article-delete-hook', so all arguments are passed
by gnus."
  (when (eq action 'delete)
    (gnus-mylist-forget-message-id (mail-header-id article) t)))

(defun gnus-mylist--track-expire-article (action article _from-group to-group _select-method)
  "Track when articles expire.
Handle the article data in `gnus-mylist--articles-list',
according to the expiry ACTION. TO-GROUP should have the value of
the expiry-target group if set. ARTICLE is the gnus message
header passed when the hook is run. For use by
`gnus-summary-article-expire-hook'."
  (when (eq action 'delete)
    (if to-group                        ; article moves to the expiry-target group
        (gnus-mylist-update-message-id (mail-header-id article) to-group)
      (gnus-mylist-forget-message-id (mail-header-id article) t)))) ; article deleted

(defmacro gnus-mylist--shift (lst)
  "Put the first element of LST last, then return that element."
  `(let ((top (pop ,lst)))
     (setq ,lst (nconc ,lst (list top)) )
     top))

(defun gnus-mylist-goto-previous (&optional no-retry)
  "Go to the top of the mylist article list.
Unless NO-RETRY, we try going further back if the top of the
article list is the article we're currently looking at."
  (interactive)
  (if (not gnus-mylist--articles-list)
      (message "No mylist article to show")
    (gnus-mylist--action
     (gnus-mylist--shift gnus-mylist--articles-list)
     (lambda (message-id group)
       (if (and (not no-retry)
                (equal (current-buffer) gnus-summary-buffer)
                (equal message-id (mail-header-id (gnus-summary-article-header))))
           (gnus-mylist-goto-previous 'no-retry)
         (gnus-summary-read-group group 1) ; have to show at least one old one
         (gnus-summary-refer-article message-id))))))

(defun gnus-mylist--action (msg func)
  "Find `message-id' and group arguments from MSG, call FUNC on them.
Warn if MSG can't be deconstructed as expected."
  (pcase msg
    (`(,_ . (,message-id ,group . ,_))
     (funcall func message-id group))
    (_
     (gnus-message 3 "Couldn't parse mylist message: %S" msg))))

(defun gnus-mylist--open-article (artdata)
  "Open ARTDATA gnus article using `org-gnus'."
  (org-gnus-follow-link (alist-get 'group artdata) (alist-get 'message-id artdata)))

(defun gnus-mylist--reply-article-wide-yank (artdata)
  "Make a wide reply and yank to the current ARTDATA article."
  ;; TODO: handle the case the article/email doesn't exist any more
  (gnus-mylist--open-article artdata)
  (call-interactively 'gnus-summary-wide-reply-with-original))
  ;; (when (fboundp 'gnus-mylist-org-message-add-header-orgid)
  ;;   (gnus-mylist-org-message-add-header-orgid)))

(defun gnus-mylist--show-article-thread (artdata)
  "Show the ARTDATA gnus article thread in a summary buffer."
  (gnus-mylist--open-article artdata)
  (gnus-warp-to-article)
  (call-interactively 'gnus-summary-refer-thread)
  (goto-char (point-min)))

(defun gnus-mylist--create-org-link (artdata)
  "Return an `org-mode' link to ARTDATA Gnus article."
  (format "[[gnus:%s#%s][Email %s%s]]"
          (alist-get 'group artdata)
          (gnus-mylist-string-unbracket (alist-get 'message-id artdata))
          (if (gnus-mylist-outgoing-message-p artdata)
              ""
            "from ")
          (bbdb-string-trim
           (replace-regexp-in-string "[][\t]" ""
                                     (substring (car artdata) 0 48)))))

(defun gnus-mylist-split-org-link-gnus (link)
  "Split a gnus article org LINK into its parts.
Returns a cons cell as (gnus-group . message-id)."
  (when link
    (let ((s (cl-subseq (split-string (substring-no-properties link 7) "[]#]") 0 2)))
      (cons (car s) (concat "<" (nth 1 s) ">")))))

(defun gnus-mylist-string-unbracket (txt)
  "Trim brackets from TXT string."
  (replace-regexp-in-string "^<\\|>$" "" txt))

(defun gnus-mylist-kill-new-org-link (artdata)
  "Add to the `kill-ring' an `org-mode' link to ARTDATA Gnus article."
  (kill-new (gnus-mylist--create-org-link artdata))
  (gnus-message 5 "Added org-link to kill-ring"))

(defun gnus-mylist-insert-org-link (artdata)
  "Insert an `org-mode' link to ARTDATA Gnus article."
  (insert (gnus-mylist--create-org-link artdata)))

(defun gnus-mylist-update-message-id (message-id to-group &optional no-crumb-save)
  "Update the Gnus article with MESSAGE-ID in `gnus-mylist--articles-list'.
The Gnus article has moved to group TO-GROUP.
Set NO-CRUMB-SAVE non-nil to skip saving a crumb."
  (let ((article (gnus-mylist-find-message-id message-id)))
    (when article
      (setf (alist-get 'group article) to-group)
      (unless no-crumb-save
        (gnus-mylist--crumb-save article 'upd)))))

(defun gnus-mylist-update (artdata to-group)
  "Update ARTDATA Gnus article in `gnus-mylist--articles-list'.
The Gnus article has moved to group TO-GROUP."
  (gnus-mylist-update-message-id (alist-get 'message-id artdata) to-group))

(defun gnus-mylist-forget-message-id (message-id &optional print-msg no-crumb-save)
  "Remove the Gnus article with MESSAGE-ID in `gnus-mylist--articles-list'.
When PRINT-MSG is non-nil, show a message about it.
Set NO-CRUMB-SAVE non-nil to skip saving a crumb file."
  (let ((l1 (length gnus-mylist--articles-list))
        (article (car gnus-mylist--articles-list)))
    ;; check for a match on the first article on list
    (if (equal message-id (alist-get 'message-id article))
        (pop gnus-mylist--articles-list)
      (setq article (gnus-mylist-find-message-id message-id))
      (when article
        (cl-delete article gnus-mylist--articles-list :test 'equal :count 1)))
    (when (= 1 (- l1 (length gnus-mylist--articles-list)))
      (unless no-crumb-save (gnus-mylist--crumb-save article 'del))
      (when print-msg
        (gnus-message 4 "Removed 1 of 1 from gnus-mylist articles")
        (gnus-message 4 "Removed item: %s from gnus-mylist articles" (car article))))))

(defun gnus-mylist-forget (artdata &optional print-msg)
  "Remove ARTDATA Gnus article from `gnus-mylist--articles-list'.
When PRINT-MSG is non-nil, show a message about it."
  (gnus-mylist-forget-message-id (alist-get 'message-id artdata) print-msg))

(defun gnus-mylist-forget-all (&rest _artdata)
  "Clear the gnus-mylist articles list."
  (interactive)
  (when (yes-or-no-p "Action can not be undone. Are you sure? ")
    (setq gnus-mylist--articles-list nil)
    (gnus-mylist--crumbs-clear-all)
    (gnus-message 4 "Cleared all gnus-mylist article entries")))

(defun gnus-mylist-bbdb-display-all (artdata)
  "Display sender and recipients in BBDB.
Diplay sender and all recipients in BBDB. Ask to create a BBDB entry, if not in
BBDB. ARTDATA is the gnus-mylist data for the selected article."
  (let ((recipients (alist-get 'recipients artdata))
        (search-list '(bbdb-search (bbdb-records))))
    (setq recipients (append (bbdb-split "," (or (alist-get 'sender artdata) ""))
                             (bbdb-split "," (or (alist-get 'To recipients) ""))
                             (bbdb-split "," (or (alist-get 'Cc recipients) ""))))
    (dolist (r recipients)              ; add new entries to BBDB (ask)
      (bbdb-update-records (list (list (gnus-mylist-get-email-name r t)
                                       (gnus-mylist-get-email r t)))
                           'query t))
    ;; make an array (:mail email1 :mail email2 ...etc)
    (dolist (r recipients search-list)
      (helm-aif (gnus-mylist-get-email r t)
          (nconc search-list (list :mail it))))
    (setq search-list (eval search-list))
    (if search-list
        (bbdb-display-records search-list 'multi-line nil)
      (gnus-message 4 "No matching BBDB records found"))))

;; FIXME: this function text is only a placeholder.
;; Now that gnus-mylist uses the message-id to handle the articles in its'
;; article list, there should be no duplicates entries. Still, this function will
;; help with checking consistency, removal is just not that critical at the moment.
;; (defun gnus-mylist-alist-find-duplicates ()
;;   "Find any duplicate entries in `gnus-mylist--articles-list'.
;; Duplicates entries are considered those that have the same
;; message-id, even if some other property may differ such as the
;; group value. It returns a list of message-ids that are found more
;; than once."
;; (cl-find elem1 gnus-mylist--articles-list))

(defun gnus-mylist-filter-prop (prop value &optional test)
  "Return a list of all articles with PROP equal to VALUE.
Search the `gnus-mylist--articles-list' for all elements with
property PROP equal to value.
Optional argument TEST should provide a test function. Default test 'equal'."
  (seq-filter #'(lambda (item)
                  (funcall (or test #'equal) value (alist-get prop item)))
              gnus-mylist--articles-list))

(defun gnus-mylist-find-prop (prop value)
  "Check for an article with the property value given.
Find in `gnus-mylist--articles-list' if there is a property PROP equal to VALUE.
Returns the first article data when a match is found. It does not try
to find any more matches."
  (seq-find #'(lambda (item)
            (equal value (alist-get prop item)))
        gnus-mylist--articles-list))

(defun gnus-mylist-find-message-id (message-id)
  "Search the gnus-mylist articles data by MESSAGE-ID.
Returns the first article in `gnus-mylist--articles-list' that
matches the MESSAGE-ID provided. A convinience wrapper for
`gnus-mylist-find-prop'."
  (gnus-mylist-find-prop 'message-id  message-id))

(defun gnus-mylist-find-message-ids-list (msgids-list)
  "Search gnus-mylist articles for MSGIDS-LIST.
Returns the list of articles in `gnus-mylist--articles-list' that
match the list of message-id provided. MSGIDS-LIST is a list of
article message-ids."
  (mapcar 'gnus-mylist-find-message-id msgids-list))

(defun gnus-mylist-find-article (artdata)
  "Search the gnus-mylist articles list for ARTDATA article.
Returns the first article in `gnus-mylist--articles-list' that
matches the message-id of the ARTDATA article argument."
  (gnus-mylist-find-message-id (alist-get 'message-id artdata)))

(defun gnus-mylist-add-to-list (artdata &optional no-crumb-save)
  "Add the ARTDATA article data to the articles list.
Ensures the value for messsage-id is unique among all articles
stored in `gnus-mylist--articles-list'. When NO-CRUMB-SAVE is
non-nil, will not save the article data to a crumb file. See
`gnus-mylist--get-article-data' for the artdata article data
format."
  (when artdata
    (unless (gnus-mylist-find-message-id (alist-get 'message-id artdata))
      (push artdata gnus-mylist--articles-list)
      (unless no-crumb-save
        (gnus-mylist--crumb-save artdata 'new)))))

(defun gnus-mylist--crumb-filename (type)
  "Generate a full path filename for an article crumb.
Crumb files are used to store a single article data. They reside
in the `gnus-mylist-breadcrumbs-dir' directory. TYPE should
indicate an action type, see `gnus-mylist--crumb-save'."
  (format "%s/cr-%s-%s.el"
          (directory-file-name gnus-mylist-breadcrumbs-dir)
          (format-time-string "%Y%m%d%H%M%S-%N")
          type))

(defun gnus-mylist--crumbs-clear-all ()
  "Clear all crumb files."
  (dolist (crumb (directory-files gnus-mylist-breadcrumbs-dir  t "^cr-" t))
    (delete-file crumb)))

(defun gnus-mylist--crumbs-load ()
  "Load the article data saved in crumb files to `gnus-mylist--articles-list'.
In case something goes wrong, crumb files are used to restore
`gnus-mylist--articles-list', as not to lose any previous
actions."
  (let ((icount 0))
    (dolist (crumb (directory-files gnus-mylist-breadcrumbs-dir  t "^cr-") icount)
      (cl-incf icount)
      (cond
       ((string-match-p "-new.el$" crumb) (gnus-mylist-load-crumb-new crumb))
       ((string-match-p "-upd.el$" crumb) (gnus-mylist-load-crumb-upd crumb))
       ((string-match-p "-del.el$" crumb) (gnus-mylist-load-crumb-del crumb))
       (t (cl-decf icount)
          (message "Warning: found bad crumb: %s" (file-name-nondirectory crumb))))
      (delete-file crumb))))

(defun gnus-mylist-load-crumb-new (crumb-file)
  "Load the elisp data in CRUMB-FILE to `gnus-mylist--articles-list'.
CRUMB-FILE is the full file path to a crumb file of type new.
Pass non-nil for the optional argument to
`gnus-mylist-add-to-list' no-crumb-save, not to save another
crumb."
  (gnus-mylist-add-to-list (gnus-mylist--read-file-contents crumb-file) t))

(defun gnus-mylist-load-crumb-upd (crumb-file)
  "Use the elisp data in CRUMB-FILE to update `gnus-mylist--articles-list'.
CRUMB-FILE is the full file path to a crumb file of type upd.
Pass non-nil for the optional argument to
`gnus-mylist-add-to-list' no-crumb-save, not to save another
crumb."
  (let ((article (gnus-mylist--read-file-contents crumb-file)))
    (gnus-mylist-update-message-id (alist-get 'message-id article)
                                   (alist-get 'group article)
                                   t)))

(defun gnus-mylist-load-crumb-del (crumb-file)
  "Use CRUMB-FILE to delete an item in `gnus-mylist--articles-list'.
CRUMB-FILE is the full file path to a crumb file of type del.
Pass non-nil for the optional argument to
`gnus-mylist-add-to-list' no-crumb-save, not to save another
crumb."
  (gnus-mylist-forget-message-id
   (alist-get 'message-id (gnus-mylist--read-file-contents crumb-file))
   t t))

(defun gnus-mylist--crumb-save (artdata type)
  "Backup single article data until the next save.
TYPE should be one of 'new, 'upd or 'del.
ARTDATA is an alist of the article data."
  (with-temp-file (gnus-mylist--crumb-filename type)
    (prin1 artdata (current-buffer))))

(defun gnus-mylist--read-file-contents (file)
  "Read the contents of a file.
FILE is the full file path."
  (if (and file (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (read (current-buffer)))
    (error "Can not read file '%s'" file)))

(defun gnus-mylist-save ()
  "Save the gnus mylist items to file for persistance."
  (interactive)
  (when gnus-mylist-file
    (if (file-writable-p gnus-mylist-file)
        (progn
          (gnus-message 5 "Saving gnus-mylist data to %s." gnus-mylist-file)
          (with-temp-file gnus-mylist-file
            (let ((print-level nil)
                  (print-length nil))
              (prin1 gnus-mylist--articles-list (current-buffer))))
          (gnus-mylist--crumbs-clear-all)
          (gnus-message 5 "Saving gnus-mylist data (%d items) done."
                        (length gnus-mylist--articles-list)))
      (error "Error: can not save gnus-mylist data to %s" gnus-mylist-file))))

(defun gnus-mylist-read ()
  "Read gnus-mylist data from a previous session."
  (interactive)
  (when gnus-mylist-file
    (if (file-readable-p gnus-mylist-file)
        (progn
          (gnus-message 5 "Reading gnus-mylist data from %s." gnus-mylist-file)
          (setq gnus-mylist--articles-list
                (gnus-mylist--read-file-contents gnus-mylist-file))
          (gnus-message 5 "Read %d item(s) from %s... done."
                        (length gnus-mylist--articles-list) gnus-mylist-file)
          (let ((numcrumbs (gnus-mylist--crumbs-load)))
            (gnus-message 5 "Loaded %d item(s) gnus-mylist crumbs found... done" numcrumbs)
            (when (> numcrumbs 0)
              gnus-mylist-save)))
      (error "Error: can not read gnus-mylist data from %s" gnus-mylist-file))))

(defun gnus-mylist-count-saved ()
  "Count the number of articles saved in `gnus-mylist-file'."
  (if (and gnus-mylist-file (file-readable-p gnus-mylist-file))
      (length (read
               (with-temp-buffer
                 (insert-file-contents gnus-mylist-file)
                 (buffer-string))))
    nil))

;;
;; Track outgoing messages
;;
(defun gnus-mylist--track-message ()
  "Add an newly sent message to the list of tracked articles.
Is run from `message-sent-hook'. A alist of the message header
data should be available on `gnus-mylist--temp-message-headers'."
  (interactive)
  (let* ((hdrs gnus-mylist--temp-message-headers)
         (date (gnus-mylist-date-format (alist-get 'date hdrs)))
         (author (rfc2047-decode-address-string (or (alist-get 'from hdrs) "")))
         (recipients (rassq-delete-all nil
                                       (list (cons 'Newsgroups (alist-get 'newsgroups hdrs))
                                             (cons 'To (alist-get 'to hdrs))
                                             (cons 'Cc (alist-get 'cc hdrs)))))
         (to-first (car (bbdb-split "," (or (alist-get 'To recipients) "")))))
    (dolist (r recipients)
      (setcdr r (rfc2047-decode-address-string (cdr r))))
    ;; This is a new message, can directly add to list... but better be safe.
    (gnus-mylist-add-to-list
     (list (format "%s%s: %s \t%s"
                   (if (alist-get 'Newsgroups recipients)
                       gnus-summary-newsgroup-prefix
                     gnus-summary-to-prefix)
                   (propertize (gnus-mylist-get-email-name to-first t) 'face 'bold)
                   (alist-get 'subject hdrs)
                   (propertize date 'face 'gnus-mylist-date-face))
           (cons 'group (alist-get 'gcc hdrs))
           (cons 'message-id (alist-get 'message-id hdrs))
           (cons 'date  date)
           (cons 'subject (alist-get 'subject  hdrs))
           (cons 'sender author)
           (cons 'recipients recipients)
           (cons 'references (alist-get 'references hdrs))
           (cons 'in-reply-to (alist-get 'in-reply-to hdrs))))))

(defun gnus-mylist--get-message-data ()
  "Get the headers from a new outgoing message.
Returns a header alist, see function
`mail-header-extract-no-properties'. Needs to run with the
`message-header-hook' which applies narrowing to the message
headers by default. Saves the header data to variable
`gnus-mylist--temp-message-header' so it can get out of the way
as quickly as possible. After the message is sent,
`gnus-mylist--track-message' processes the header data and adds
an entry to `gnus-mylist--articles-list'.
Note that `mail-header-extract' downcases the property headers."
  (save-restriction
    (goto-char (point-min))
    (setq gnus-mylist--temp-message-headers (mail-header-extract-no-properties))))

(defun gnus-mylist-outgoing-message-p (artdata)
  "Check the title of a gnus-mylist article for the outgoing message prefix.
ARTDATA is the gnus-mylist article data."
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
;; starting gnus-mylist
;;
(defun gnus-mylist-start ()
  "Start Gnus Mylist."
  (interactive)
  (gnus-mylist-check-files)
  (gnus-message 5 "Starting gnus-mylist")
  (gnus-mylist-add-hooks)
  (gnus-mylist-org-init)
  (gnus-mylist-read))

(defun gnus-mylist-check-files ()
  "Check for the gnus-mylist directories.
If the directories don't exist, create them."
  (dolist (d (list gnus-mylist-top-dir gnus-mylist-breadcrumbs-dir))
    (unless (file-exists-p d)
      (make-directory d t))))

(defun gnus-mylist-add-hooks ()
  "Install the gnus-mylist hooks."
  (interactive)
  ;; Activate the hooks  (should be named -functions)
  ;; Note: except for the 1st, the other hooks run using run-hook-with-args
  (add-hook 'gnus-article-prepare-hook        'gnus-mylist--track-article)
  (add-hook 'gnus-summary-article-move-hook   'gnus-mylist--track-move-article)
  (add-hook 'gnus-summary-article-delete-hook 'gnus-mylist--track-delete-article)
  (add-hook 'gnus-summary-article-expire-hook 'gnus-mylist--track-expire-article)
  ;; hooks for new messages
  (add-hook 'message-header-hook 'gnus-mylist--get-message-data)
  ;; TODO: replace this hook call with an async call from gnus-mylist--get-message-data.
  ;;       Optimize later.
  (add-hook 'message-sent-hook 'gnus-mylist--track-message)

  ;; hooks related to saving the data
  (add-hook 'gnus-save-newsrc-hook 'gnus-mylist-save)
  (add-hook 'kill-emacs-hook 'gnus-mylist-save))

(defun gnus-mylist-remove-hooks ()
  "Remove the gnus-mylist hooks."
  (interactive)
  (remove-hook 'gnus-article-prepare-hook        'gnus-mylist--track-article)
  (remove-hook 'gnus-summary-article-move-hook   'gnus-mylist--track-move-article)
  (remove-hook 'gnus-summary-article-delete-hook 'gnus-mylist--track-delete-article)
  (remove-hook 'gnus-summary-article-expire-hook 'gnus-mylist--track-expire-article)
  ;; hooks for new messages
  (remove-hook 'message-header-hook 'gnus-mylist--get-message-data)
  (remove-hook 'message-sent-hook 'gnus-mylist--track-message)
  ;; hooks related to saving the data
  (remove-hook 'gnus-save-newsrc-hook 'gnus-mylist-save)
  (remove-hook 'kill-emacs-hook 'gnus-mylist-save))

;; start gnus-mylist session
(gnus-mylist-start)

(provide 'gnus-mylist)
;;; gnus-mylist.el ends here
