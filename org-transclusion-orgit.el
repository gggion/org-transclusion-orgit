;;; org-transclusion-orgit.el --- Git integration for org-transclusion -*- lexical-binding: t; -*-

;; Author: Gino Cornejo
;; Maintainer: Gino Cornejo <gggion123@gmail.com>
;; Homepage: https://github.com/gggion/org-transclusion-orgit
;; Keywords: convenience, files, hypermedia, outlines, vc

;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "29.1")
;;                    (org-transclusion "1.4.0")
;;                    (orgit-file "0.3.0")
;;                    (orgit "2.0")
;;                    (magit "4.3"))

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

;; Provides transclusion support for orgit links.
;;
;; Supports:
;; - orgit-file: links (files at specific revisions)
;; 
;; Future support planned:
;; - orgit-rev: links (revision buffers)
;; - orgit-log: links (log buffers)
;;
;; Basic usage:
;;     #+transclude: [[orgit-file:/path/to/repo::REV::file.el]]


;;; Code:

(require 'org-transclusion)
(require 'orgit-file) 

;;;###autoload
(defgroup org-transclusion-orgit nil
  "Orgit link transclusion for org-transclusion."
  :group 'org-transclusion
  :group 'orgit)

(require 'org-transclusion-orgit-file)

(provide 'org-transclusion-orgit)
;;; org-transclusion-orgit.el ends here
