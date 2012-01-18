;;; ido-better-flex.el --- A better flex (fuzzy) algorithm for Ido.

;; Copyright 2012 Victor Hugo Borja.
;; Author: Victor Hugo Borja
;; URL: http://github.com/vic/ido-better-flex
;; Version: 0.1
;; Keywords: ido flex fuzzy match algorithm

;; Commentary:
;;  
;; This package implements just another algorithm for fuzzy matching.
;; Require this package and invoke the `ido-better-flex/enable'function.
;;
;; `ido-better-flex' matches the list of candidates against a single
;; abbreviation by using the function `ido-better-flex/match'.
;; basically for each candidate the algorithm calculate an score based
;; on the characters that make the abbreviation and their position in
;; the candidate string.
;;
;; the matching algorithm implemented in this file is not limited to
;; ido, you could easily use it to do fuzzy matching in other packages,
;; the main entry point for that purpose is the `ido-better-flex/score'
;; function. 
;;



(eval-when-compile
  (require 'cl))

(defconst ido-better-flex/NO-MATCH 0.0
  "The score indicating a negative match")
(defconst ido-better-flex/MATCH 1.0
  "The score indicating a full-match.")
(defconst ido-better-flex/EMPTY 0.8
  "The score to return when the abrreviation string is empty.")

;;;###autoload
(defun ido-better-flex/enable nil
  (interactive)
  "Enable the IDO matching with `ido-better-flex'."
  (ad-enable-advice 'ido-set-matches-1 'around 'ido-better-flex-match)
  (ad-activate 'ido-set-matches-1))

;;;###autoload
(defun ido-better-flex/disable nil
  (interactive)
  "Disable the IDO matching with `ido-better-flex'."
  (ad-disable-advice 'ido-set-matches-1 'around 'ido-better-flex-match)
  (ad-activate 'ido-set-matches-1))

;;;###autoload
(defun ido-better-flex/score (string abbreviation)
  "Computes the score of matching string with abbreviation.
   The return value is in the range 0.0 to 1.0 the later being full-match."
  (let ((len (length abbreviation)))
    (cond ((= 0 len) ido-better-flex/EMPTY)
          ((> len (length string)) ido-better-flex/NO-MATCH)
          (t (ido-better-flex/build-score string abbreviation)))))

;;;###autoload
(defun ido-better-flex/match (items)
  "Returns an ordered list (higher score first) of items that match the
   current `ido-text'. Items are included only if their score is greater than zero."
  (let (score matches)
    (mapc (lambda (item)
              (let ((name (ido-name item)) score)
                (if (> (setq score (ido-better-flex/score name ido-text)) 0)
                    (setq matches (cons (cons item score) matches))))) items)
    (mapcar 'car (sort matches (lambda (x y) (> (cdr x) (cdr y)))))))

(defun ido-better-flex/position (av string end)
  "Searchs a character `av' on `string' backwards up until index `end'"
  (if ido-case-fold
      (or (position (upcase av) string :end end :from-end t)
          (position (downcase av) string :end end :from-end t))
    (position av string :end end :from-end t)))

;;;####autoload
(defun ido-better-flex/build-score (string abbreviation)
  "Calculates the fuzzy score of matching `string' with `abbreviation'."
    (let ((length (length string))
          (score 0)
          index av)
      (catch 'failed
        (dotimes (i (length abbreviation))

          (setq av (elt abbreviation i))
          (setq index (ido-better-flex/position av string length))
           
          (while (and index
                  (= 1 (logand 1 (lsh score (* -1 index))))
                  (setq index (ido-better-flex/position av string index))))

          (unless index (throw 'failed ido-better-flex/NO-MATCH))
          
          (setq score (logior score (lsh 1 index))))

          (/ (* score ido-better-flex/MATCH) (- (expt 2 length) 1)))))

;;;###autoload
(defadvice ido-set-matches-1 (around ido-better-flex-match)
  "An advice using `ido-better-flex' for IDO matching."
  (setq ad-return-value (ido-better-flex/match (ad-get-arg 0))))


(provide 'ido-better-flex)

;;; ido-better-flex.el ends here
