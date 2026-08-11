;; Batch export org-mode files to RST for Sphinx
;; Usage: emacs --batch -l docs/export.el

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'ox-rst)
  (package-refresh-contents)
  (package-install 'ox-rst))

(require 'ox-rst)
(require 'ox-publish)

;; ox-rst 2025-04 needs org-element-type-p (Org 9.7+). Ubuntu emacs-nox is 29/9.6.
(require 'org-element)
(unless (fboundp 'org-element-type-p)
  (defun org-element-type-p (node types)
    (memq (org-element-type node)
          (if (listp types) types (list types)))))

;; Prefer Sphinx-friendly RST (no auto section numbers; titles from #+title)
(setq org-export-with-section-numbers nil)
(setq org-export-with-toc nil)
(setq org-export-with-author nil)
(setq org-export-with-timestamps nil)
(setq org-rst-headline-underline ?-)

(setq org-publish-project-alist
      '(("capnp-fortran-rst"
         :base-directory "./docs/orgmode/"
         :base-extension "org"
         :publishing-directory "./docs/source/"
         :publishing-function org-rst-publish-to-rst
         :recursive t
         :headline-levels 4
         :with-toc nil
         :section-numbers nil
         :with-author nil)))

(org-publish "capnp-fortran-rst" t)
