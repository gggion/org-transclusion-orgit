;;; org-transclusion-olgit.el --- Description -*- lexical-binding: t; -*-
;;
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
;; - :lines - Transclude specific line ranges (e.g., "1-10", "-5", "10-")
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

(add-hook 'org-transclusion-add-functions
          #'org-transclusion-olgit-add)

;;;; Core Functions

(defun org-transclusion-olgit-add (link plist)
  "Return payload for git: or gitbare: LINK with properties PLIST.

Extract git content to a temporary buffer, then delegate to
org-transclusion-src-lines for all property handling (:src,
:lines, :thing-at-point, :end, :rest).

LINK should be an org-element link object with type \"git\" or \"gitbare\".
PLIST contains properties from the #+transclude: keyword.

Return a payload plist or nil if link type is not supported."
  (let ((type (org-element-property :type link)))
    (when (or (string= type "git")
              (string= type "gitbare"))
      (let* ((path (org-element-property :path link))
             (parts (org-git-split-string path))
             (git-content (org-transclusion-olgit--get-content type parts)))
        (when git-content
          ;; Create temporary buffer with git content
          (let* ((temp-buf (generate-new-buffer " *org-transclusion-git*"))
                 (filename (org-transclusion-olgit--extract-filename parts))
                 (search-string (nth 2 parts)))
            (with-current-buffer temp-buf
              (insert git-content)
              ;; Set major mode based on file extension
              (when filename
                (set-visited-file-name filename t t)
                (set-auto-mode)))
            
            ;; Create a fake file link pointing to our temp buffer
            ;; and delegate to org-transclusion-src-lines
            (let* ((fake-link (org-element-create
                               'link
                               (list :type "file"
                                     :path (buffer-file-name temp-buf)
                                     :search-option search-string)))
                   ;; Choose the right function based on :src property
                   (payload (if (plist-get plist :src)
                                (org-transclusion-content-src-lines fake-link plist)
                              (org-transclusion-content-range-of-lines fake-link plist))))
              ;; Add git-specific type
              (when payload
                (plist-put payload :tc-type
                           (or (plist-get plist :src) "git")))
              payload)))))))

(defun org-transclusion-olgit--get-content (link-type parts)
  "Extract content from git repository.

LINK-TYPE is either \"git\" or \"gitbare\".
PARTS is the result of org-git-split-string on the link path.

Return the file content as a string, or nil if extraction fails."
  (condition-case err
      (cond
       ((string= link-type "git")
        (org-transclusion-olgit--get-content-user-friendly parts))
       ((string= link-type "gitbare")
        (org-transclusion-olgit--get-content-bare parts))
       (t nil))
    (error
     (message "Error extracting git content: %s" (error-message-string err))
     nil)))

(defun org-transclusion-olgit--get-content-user-friendly (parts)
  "Extract content from git: link.

PARTS is (filepath commit search-string) from org-git-split-string.

Return the file content at the specified commit as a string."
  (unless (featurep 'ol-git-link)
    (user-error "ERROR: ol-git-link package is required for git: links"))
  
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

(defun org-transclusion-olgit--get-content-bare (parts)
  "Extract content from gitbare: link.

PARTS is (gitdir object search-string) from org-git-split-string.

Return the file content as a string."
  (unless (featurep 'ol-git-link)
    (user-error "ERROR: ol-git-link package is required for gitbare: links"))
  
  (let* ((git-path (nth 0 parts))
         (object (nth 1 parts))
         (gitdir (if (string-suffix-p "/.git" git-path)
                     (expand-file-name git-path)
                   (expand-file-name ".git" git-path))))
    
    (with-temp-buffer
      (org-git-show gitdir object (current-buffer))
      (buffer-string))))

(defun org-transclusion-olgit--extract-filename (parts)
  "Extract filename from git link PARTS.

Return the filename or nil if it cannot be determined."
  (let ((filepath (nth 0 parts)))
    (cond
     ((and filepath (file-name-nondirectory filepath)))
     ((and filepath (string-match ":[^:]+$" filepath))
      (file-name-nondirectory (match-string 0 filepath)))
     (t nil))))

(provide 'org-transclusion-olgit)

;;; org-transclusion-olgit.el ends here
