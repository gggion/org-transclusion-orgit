;;; org-transclusion-orgit-file.el --- Orgit file link support for org-transclusion -*- lexical-binding: t; -*-

;; Author: Gino Cornejo
;; Maintainer: Gino Cornejo <gggion123@gmail.com>
;; Homepage: https://github.com/gggion/org-transclusion-orgit
;; Keywords: convenience, files, hypermedia, outlines, vc

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


;; This file is part of org-transclusion-orgit.

;;; Commentary:

;; This extension adds support for transcluding content from Git
;; repositories using orgit-file: links.
;;
;; Transcluded content from git revisions is READ-ONLY.  You cannot
;; use live-sync editing on these transclusions since they represent
;; historical state.
;;
;; Basic usage:
;;
;;     #+transclude: [[orgit-file:/path/to/repo/::REV::path/to/file.el]]
;;     #+transclude: [[orgit-file:repo-name::REV::path/to/file.el]]
;;
;; The orgit-file: link format supports both:
;;
;;     orgit-file:/absolute/path/to/repo/::REV::FILE::SEARCH
;;     orgit-file:repo-name::REV::FILE::SEARCH
;;
;; Repository names work when `orgit-store-repository-id' is non-nil
;; and the repository is configured in `magit-repository-directories'.
;;
;; Where:
;; - REPO is absolute path or repository name from `magit-repository-directories'
;; - REV is any Git revision (commit hash, branch, tag)
;; - FILE is the path to the file within the repository
;; - SEARCH is an optional Org search string or line number

;;; Code:

(require 'org-transclusion)
(require 'org-element)
(require 'orgit-file)
(require 'orgit)
(require 'magit)

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

Supports both link formats:
  orgit-file:/absolute/path/to/repo/::REV::FILE
  orgit-file:repo-name::REV::FILE

PLIST contains properties from the #+transclude: keyword.

Return a payload plist with :src-content and markers pointing to
the git buffer (for overlay creation), or nil if link type is not
supported."
  (when (string= "orgit-file" (org-element-property :type link))
    (pcase-let* ((path (org-element-property :path link))
                 ;; orgit-file--parse-path returns 6 elements
                 (`(,repo ,rev ,file-path ,search-option ,line-start ,line-end)
                  (orgit-file--parse-path path))
                 ;; Use orgit's resolution logic to handle both formats
                 (default-directory (orgit--repository-directory repo)))
      (condition-case err
          (let* ((git-buf (magit-find-file-noselect rev file-path))
                 (result (org-transclusion-orgit-file--extract-content
                          git-buf search-option line-start line-end plist)))
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

(defun org-transclusion-orgit-file--extract-content
    (buffer search-option line-start line-end plist)
  "Extract content from BUFFER according to search parameters and PLIST.

BUFFER is a magit blob buffer.
SEARCH-OPTION is the optional text search string from link (may be nil).
LINE-START and LINE-END are parsed line numbers (may be nil).
PLIST contains properties like :src, :lines, :thing-at-point, :end.

Line numbers from the link take precedence over :lines property.

Return (CONTENT BEG-MARKER END-MARKER) where markers point to the
actual region in BUFFER that was extracted."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))

        (let* ((start-pos (point-min))
               (thing (plist-get plist :thing-at-point))
               (end-search (plist-get plist :end))
               ;; Only use :lines if link didn't specify line numbers
               (lines (unless (or line-start line-end)
                        (plist-get plist :lines)))
               (src-lang (plist-get plist :src))
               (rest (plist-get plist :rest))
               (beg)
               (end)
               (content))

          ;; Handle line numbers from link (takes precedence)
          (when line-start
            (goto-char (point-min))
            (forward-line (1- line-start))
            (setq beg (point))
            (if line-end
                (progn
                  (goto-char (point-min))
                  (forward-line (1- line-end))
                  (end-of-line)
                  (setq end (min (1+ (point)) (point-max))))
              (setq end (point-max))))

          ;; Handle text search if no line numbers
          (when (and search-option (not line-start))
            (goto-char (point-min))
            (unless (search-forward search-option nil t)
              (user-error "Search string %S not found" search-option))
            (setq start-pos (match-beginning 0)))

          ;; Determine region based on properties (only if not already set)
          (unless beg
            (cond
             ;; Thing-at-point takes precedence
             (thing
              (goto-char start-pos)
              (let* ((thing-sym (intern (replace-regexp-in-string
                                         "^:?\\(thing-at-point\\|thingatpt\\) +"
                                         "" thing)))
                     (bounds (bounds-of-thing-at-point thing-sym)))
                (unless bounds
                  (user-error "No %s at point" thing-sym))
                (setq beg (car bounds))
                (setq end (cdr bounds))))

             ;; Lines property (from :lines keyword)
             (lines
              (let* ((range (split-string lines "-"))
                     (line-beg (string-to-number (or (car range) "0")))
                     (line-end-num (string-to-number (or (cadr range) "0"))))
                (goto-char (point-min))
                (when (> line-beg 0)
                  (forward-line (1- line-beg)))
                (setq beg (point))
                (if (zerop line-end-num)
                    (setq end (point-max))
                  (goto-char (point-min))
                  (forward-line (1- line-end-num))
                  (end-of-line)
                  (setq end (min (1+ (point)) (point-max))))))

             ;; End search property
             (end-search
              (setq beg start-pos)
              (goto-char start-pos)
              (unless (search-forward end-search nil t)
                (user-error "End search string %S not found" end-search))
              (setq end (line-beginning-position)))

             ;; Default: entire buffer
             (t
              (setq beg (point-min))
              (setq end (point-max)))))

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

(provide 'org-transclusion-orgit-file)

;;; org-transclusion-orgit-file.el ends here
