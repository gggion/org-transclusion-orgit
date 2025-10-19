;;; org-transclusion-git.el --- Git link support for org-transclusion -*- lexical-binding: t; -*-

;; Author: gggion <gggion123@gmail.com>
;; Keywords: convenience, files, hypermedia, outlines
;; Package-Requires: ((emacs "27.1") (org-transclusion "1.4.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This extension adds support for transcluding content from git
;; repositories using git: and gitbare: links.  It integrates with
;; ol-git-link.el to access historical versions of files.
;;
;; Basic usage:
;;
;;     #+transclude: [[git:~/path/to/file.el::commit-hash]]
;;
;; With search options and thing-at-point:
;;
;;     #+transclude: [[git:~/path/to/file.el::commit-hash::(defun function-name]] :src elisp :thingatpt sexp
;;
;; The extension supports all standard org-transclusion properties:
;; - :src - Wrap content in a source block
;; - :lines - Transclude specific line ranges
;; - :thing-at-point (or :thingatpt) - Extract specific syntactic elements
;; - :end - Dynamic end search term
;; - :rest - Additional source block properties
;;
;; Requirements:
;; - ol-git-link.el must be loaded
;; - Git must be available in PATH

;;; Code:

(require 'org-transclusion)
(require 'org-element)
(require 'ol-git-link)

;;;; Setting up the extension

;; Add git link handler to transclusion system
(add-hook 'org-transclusion-add-functions
          #'org-transclusion-add-git)

;; Ensure git links work with existing src-lines extension
(add-hook 'org-transclusion-open-source-marker-functions
          #'org-transclusion-open-source-marker-git)

;;;; Core Functions

(defun org-transclusion-add-git (link plist)
  "Return payload for git: or gitbare: LINK with properties PLIST.

This function handles transclusion of content from git repositories.
It supports both user-friendly git: links and bare gitbare: links.

LINK should be an org-element link object with type \"git\" or \"gitbare\".
PLIST contains properties from the #+transclude: keyword.

Return a payload plist with :tc-type, :src-content, :src-buf, :src-beg,
and :src-end, or nil if the link type is not supported.

The function extracts the git object to a temporary buffer and then
applies the same processing as org-transclusion-src-lines, supporting
:src, :lines, :thing-at-point, and other properties."
  (let ((type (org-element-property :type link)))
    (when (or (string= type "git")
              (string= type "gitbare"))
      (let* ((path (org-element-property :path link))
             ;; Parse the path using ol-git-link's parser
             ;; Format: "filepath::commit::search" or "gitdir::object"
             (parts (org-git-split-string path))
             (git-content (org-transclusion--git-get-content type parts)))
        (when git-content
          ;; Create a temporary buffer with the git content
          (let ((temp-buf (generate-new-buffer " *org-transclusion-git*")))
            (with-current-buffer temp-buf
              (insert git-content)
              ;; Set appropriate major mode based on file extension
              (let ((filename (org-transclusion--git-extract-filename parts)))
                (when filename
                  (set-visited-file-name filename t t)
                  (set-auto-mode)))
              
              ;; Now process the content using the same logic as src-lines
              (let* ((search-string (nth 2 parts))  ; Third part is search string
                     (payload (org-transclusion--git-process-content
                               temp-buf search-string plist)))
                ;; Add git-specific metadata
                (plist-put payload :tc-type
                           (or (plist-get plist :src) "git"))
                payload))))))))

(defun org-transclusion--git-get-content (link-type parts)
  "Extract content from git repository.

LINK-TYPE is either \"git\" or \"gitbare\".
PARTS is the result of org-git-split-string on the link path.

For git: links, PARTS is (filepath commit search-string).
For gitbare: links, PARTS is (gitdir object search-string).

Return the file content as a string, or nil if extraction fails."
  (condition-case err
      (cond
       ;; Handle git: links (user-friendly format)
       ((string= link-type "git")
        (org-transclusion--git-get-content-user-friendly parts))
       
       ;; Handle gitbare: links (bare format)
       ((string= link-type "gitbare")
        (org-transclusion--git-get-content-bare parts))
       
       (t nil))
    (error
     (message "Error extracting git content: %s" (error-message-string err))
     nil)))

(defun org-transclusion--git-get-content-user-friendly (parts)
  "Extract content from git: link.

PARTS is (filepath commit search-string) from org-git-split-string.

Return the file content at the specified commit as a string."
  (unless (featurep 'ol-git-link)
    (user-error "ol-git-link package is required for git: links"))
  
  (let* ((filepath (nth 0 parts))
         (commit (nth 1 parts))
         (expanded-path (expand-file-name filepath))
         (dirlist (org-git-find-gitdir (file-truename expanded-path)))
         (gitdir (nth 0 dirlist))
         (relpath (nth 1 dirlist)))
    
    (unless gitdir
      (user-error "File `%s' is not in a git repository" filepath))
    
    (let ((object (concat commit ":" relpath)))
      (with-temp-buffer
        (org-git-show gitdir object (current-buffer))
        (buffer-string)))))

(defun org-transclusion--git-get-content-bare (parts)
  "Extract content from gitbare: link.

PARTS is (gitdir object search-string) from org-git-split-string.

Return the file content as a string."
  (unless (featurep 'ol-git-link)
    (user-error "ol-git-link package is required for gitbare: links"))
  
  (let* ((git-path (nth 0 parts))
         (object (nth 1 parts))
         (gitdir (if (string-suffix-p "/.git" git-path)
                     (expand-file-name git-path)
                   (expand-file-name ".git" git-path))))
    
    (with-temp-buffer
      (org-git-show gitdir object (current-buffer))
      (buffer-string))))

(defun org-transclusion--git-extract-filename (parts)
  "Extract filename from git link PARTS.

PARTS is the result of org-git-split-string.

Return the filename or nil if it cannot be determined."
  (let ((filepath (nth 0 parts)))
    (cond
     ;; For git: links, first part is the file path
     ((and filepath (file-name-nondirectory filepath)))
     
     ;; For gitbare: links, try to extract from object spec
     ((and filepath (string-match ":[^:]+$" filepath))
      (file-name-nondirectory (match-string 0 filepath)))
     
     (t nil))))

(defun org-transclusion--git-process-content (temp-buf search-string plist)
  "Process git content in TEMP-BUF according to PLIST properties.

TEMP-BUF is a temporary buffer containing the git file content.
SEARCH-STRING is the optional search string from the link (third part).
PLIST contains transclusion properties like :src, :lines, :thing-at-point.

Return a payload plist suitable for org-transclusion."
  (with-current-buffer temp-buf
    (let* (;; Find start position based on search string
           (start-pos (if (and search-string (not (string-empty-p search-string)))
                          (save-excursion
                            (goto-char (point-min))
                            (when (search-forward search-string nil t)
                              (line-beginning-position)))
                        (point-min)))
           
           ;; Get thing-at-point if specified
           (thing-at-point (plist-get plist :thing-at-point))
           (thing-symbol (when thing-at-point
                           (make-symbol (cadr (split-string thing-at-point)))))
           
           ;; Calculate bounds for thing-at-point
           (bounds (when (and thing-symbol start-pos)
                     (save-excursion
                       (goto-char start-pos)
                       (back-to-indentation)
                       (bounds-of-thing-at-point thing-symbol))))
           
           ;; Determine beginning and end positions
           (beg (cond
                 ;; Use bounds from thing-at-point if available
                 ((and bounds (car bounds)))
                 
                 ;; Use start-pos with :lines offset
                 (t (let ((lines (plist-get plist :lines)))
                      (if lines
                          (let* ((range (split-string lines "-"))
                                 (lbeg (string-to-number (car range))))
                            (goto-char (or start-pos (point-min)))
                            (when (> lbeg 0)
                              (forward-line (1- lbeg)))
                            (point))
                        (or start-pos (point-min)))))))
           
           (end (cond
                 ;; Use bounds from thing-at-point if available
                 ((and bounds (cdr bounds)))
                 
                 ;; Use :end property if specified
                 ((plist-get plist :end)
                  (let ((end-search (plist-get plist :end)))
                    (save-excursion
                      (goto-char beg)
                      (when (search-forward end-search nil t)
                        (line-beginning-position)))))
                 
                 ;; Use :lines range
                 ((plist-get plist :lines)
                  (let* ((lines (plist-get plist :lines))
                         (range (split-string lines "-"))
                         (lend (string-to-number (cadr range))))
                    (if (zerop lend)
                        (point-max)
                      (save-excursion
                        (goto-char (or start-pos (point-min)))
                        (forward-line (1- lend))
                        (end-of-line)
                        (1+ (point))))))
                 
                 ;; Default to end of buffer
                 (t (point-max))))
           
           (content (buffer-substring-no-properties beg end)))
      
      ;; Wrap in source block if :src is specified
      (when (plist-get plist :src)
        (let ((src-lang (plist-get plist :src))
              (rest (plist-get plist :rest)))
          (setq content
                (concat
                 (format "#+begin_src %s" src-lang)
                 (when rest (format " %s" rest))
                 "\n"
                 (if (string-suffix-p "\n" content)
                     content
                   (concat content "\n"))
                 "#+end_src\n"))))
      
      ;; Return payload
      (list :src-content content
            :src-buf temp-buf
            :src-beg beg
            :src-end end))))

(defun org-transclusion-open-source-marker-git (type)
  "Return marker for git transclusion source.

TYPE should be \"git\" for this function to handle it.

Return nil since git transclusions don't have a persistent source
buffer that can be opened (the content comes from git history)."
  (when (string= type "git")
    ;; Git transclusions are read-only historical snapshots
    ;; There's no meaningful source buffer to open
    nil))

;;;; Helper Functions

(defun org-transclusion-git-link-p (link)
  "Return non-nil if LINK is a git or gitbare link.

LINK should be an org-element link object."
  (let ((type (org-element-property :type link)))
    (or (string= type "git")
        (string= type "gitbare"))))

(provide 'org-transclusion-git)

;;; org-transclusion-git.el ends here
