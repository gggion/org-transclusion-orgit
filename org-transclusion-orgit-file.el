;;; org-transclusion-orgit-file.el --- Orgit file link support for org-transclusion -*- lexical-binding: t; -*-

;; Author: Gino Cornejo
;; Mantainer: Gino Cornejo <gggion123@gmail.com>
;; Homepage: https://github.com/gggion/orgit-file
;; Keywords: convenience, files, hypermedia, outlines, vc

;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (org-transclusion "1.4.0") (orgit-file "1.0.0") (magit "4.0"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This extension adds support for transcluding content from Git
;; repositories using orgit-file: links.  It integrates with the
;; orgit-file package to access historical versions of files through
;; Magit's file viewing capabilities.
;;
;; Transcluded content from git revisions is READ-ONLY.  You cannot
;; use live-sync editing on these transclusions since they represent
;; historical state.
;;
;; Basic usage:
;;
;;     #+transclude: [[orgit-file:~/repo::REV::path/to/file.el]]
;;
;; With search options and thing-at-point:
;;
;;     #+transclude: [[orgit-file:~/repo::REV::file.el::(defun name]] :src elisp :thingatpt sexp
;;
;; The extension supports :src, :lines, :thing-at-point, and :end properties.
;;
;; The orgit-file: link format is:
;;
;;     orgit-file:REPO::REV::FILE or
;;     orgit-file:REPO::REV::FILE::SEARCH
;;
;; Where:
;; - REPO is the repository path or name
;; - REV is any Git revision (commit hash, branch, tag)
;; - FILE is the path to the file within the repository
;; - SEARCH is an optional Org search string (heading, custom ID, text to search)

;;; Code:

(require 'org-transclusion)
(require 'org-element)
(require 'orgit-file)
(require 'magit)

;;;; Customization

(defvar org-transclusion-orgit-file-debug nil
  "When non-nil, print detailed debugging information.")

;;;; Setting up the extension

(add-hook 'org-transclusion-add-functions
          #'org-transclusion-orgit-file-add)

;;;; Core Functions

(defun org-transclusion-orgit-file-add (link plist)
  "Return payload for orgit-file: LINK with properties PLIST.

Use `magit-find-file-noselect' to create a buffer visiting the
Git file at the specified revision, then extract content directly
from that buffer.  The resulting transclusion is read-only and
does not support live-sync editing.

LINK should be an org-element link object with type \"orgit-file\".
The link path format is: REPO::REV::FILE or REPO::REV::FILE::SEARCH

PLIST contains properties from the #+transclude: keyword.

Return a payload plist with :src-content and markers pointing to
the git buffer (for overlay creation), or nil if link type is not
supported."
  (when (string= "orgit-file" (org-element-property :type link))
    (pcase-let* ((path (org-element-property :path link))
                 (`(,repo ,rev ,file-path ,search-option)
                  (org-transclusion-orgit-file--parse-path path))
                 (default-directory
                  (org-transclusion-orgit-file--repository-directory repo)))
      (condition-case err
          (let* ((git-buf (magit-find-file-noselect rev file-path))
                 (result (org-transclusion-orgit-file--extract-content
                          git-buf search-option plist)))
            (when result
              (pcase-let ((`(,content ,beg ,end) result))
                (list :tc-type (or (plist-get plist :src) "orgit")
                      :src-content content
                      :src-buf git-buf
                      :src-beg beg
                      :src-end end))))
        (error
         (message "Error accessing git file %s at %s: %s"
                  file-path rev (error-message-string err))
         nil)))))

(defun org-transclusion-orgit-file--extract-content (buffer search-option plist)
  "Extract content from BUFFER according to SEARCH-OPTION and PLIST.

BUFFER is a magit blob buffer.
SEARCH-OPTION is the optional search string from the link.
PLIST contains properties like :src, :lines, :thing-at-point, :end.

Return (CONTENT BEG-MARKER END-MARKER) where markers point to the
actual region in BUFFER that was extracted."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        
        ;; Handle search option if present
        (when search-option
          (unless (search-forward search-option nil t)
            (user-error "Search string %S not found" search-option))
          (goto-char (match-beginning 0)))
        
        (let* ((start-pos (point))
               (thing (plist-get plist :thing-at-point))
               (end-search (plist-get plist :end))
               (lines (plist-get plist :lines))
               (src-lang (plist-get plist :src))
               (rest (plist-get plist :rest))
               (beg)
               (end)
               (content))
          
          ;; Determine region based on properties
          (cond
           ;; Thing-at-point takes precedence
           (thing
            (let ((thing-sym (intern (replace-regexp-in-string
                                      "^:?\\(thing-at-point\\|thingatpt\\) +"
                                      "" thing))))
              (let ((bounds (bounds-of-thing-at-point thing-sym)))
                (unless bounds
                  (user-error "No %s at point" thing-sym))
                (setq beg (car bounds))
                (setq end (cdr bounds)))))
           
           ;; Lines property
           (lines
            (let* ((range (split-string lines "-"))
                   (line-beg (string-to-number (or (car range) "0")))
                   (line-end (string-to-number (or (cadr range) "0"))))
              (goto-char start-pos)
              (when (> line-beg 0)
                (forward-line (1- line-beg)))
              (setq beg (point))
              (if (zerop line-end)
                  (setq end (point-max))
                (goto-char start-pos)
                (forward-line (1- line-end))
                (end-of-line)
                (setq end (1+ (point))))))
           
           ;; End search property
           (end-search
            (setq beg start-pos)
            (unless (search-forward end-search nil t)
              (user-error "End search string %S not found" end-search))
            (setq end (line-beginning-position)))
           
           ;; Default: entire buffer
           (t
            (setq beg (point-min))
            (setq end (point-max))))
          
          ;; Extract content
          (setq content (buffer-substring-no-properties beg end))
          
          ;; Wrap in src block if :src property present
          (when src-lang
            (setq content
                  (concat
                   (format "#+begin_src %s" src-lang)
                   (when rest (format " %s" rest))
                   "\n"
                   content
                   (unless (string-suffix-p "\n" content) "\n")
                   "#+end_src\n")))
          
          ;; Return content and markers
          (list content
                (copy-marker beg)
                (copy-marker end)))))))

(defun org-transclusion-orgit-file--parse-path (path)
  "Parse orgit-file link PATH.

PATH format: REPO::REV::FILE or REPO::REV::FILE::SEARCH

Return (REPO REV FILE SEARCH) where SEARCH may be nil."
  (let* ((parts (split-string path "::" t))
         (repo (nth 0 parts))
         (rev (nth 1 parts))
         (file (nth 2 parts))
         (search (nth 3 parts)))
    (list repo rev file search)))

(defun org-transclusion-orgit-file--repository-directory (repo)
  "Return directory path for REPO identifier.

REPO can be either a repository name (from
`magit-repository-directories') or a full path.

This function mirrors the logic from orgit-file.el to maintain
consistency with how orgit-file: links are resolved."
  (let ((dir (or (cdr (assoc repo (magit-repos-alist)))
                 (file-name-as-directory (expand-file-name repo)))))
    (cond ((file-exists-p dir) dir)
          ((string-match-p "\\=[./]" repo)
           (user-error "Cannot open link; repository %S does not exist" dir))
          (t
           (user-error "Cannot open link; no entry for %S in =%s'"
                       repo 'magit-repository-directories)))))

(provide 'org-transclusion-orgit-file)

;;; org-transclusion-orgit-file.el ends here
