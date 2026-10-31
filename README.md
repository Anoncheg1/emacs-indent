# emacs-indent
Modified Emacs built-in indent.el that implement Indentation and completion by TAB key.

# files
indent.el - old replace

# Configuration
```elisp
(add-to-list 'load-path "/path/to/folder/emacs-indent")
(require 'indent-advices)
```

For Python mode:
```elisp
(defun my/python-set-indent-region-function ()
  (setq-local indent-region-function
              ;; #'indent-region-line-by-line
              #'indent-region-like-first-line))
(add-hook 'python-mode-hook #'my/python-set-indent-region-function)
```
