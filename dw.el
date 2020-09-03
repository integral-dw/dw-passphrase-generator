;;; dw.el --- Diceware passphrase generation commands and API  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  D. Williams

;; Author: D. Williams <d.williams@posteo.net>
;; Maintainer: D. Williams <d.williams@posteo.net>
;; Keywords: convenience
;; Version: 0.3.0
;; Homepage: https://github.com/integral-dw/dw-passphase-generator
;; Package-Requires: ((emacs "25.1"))

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

;; Apart from the listed requirements, this package requires (ideally)
;; one or more casino-grade dice for true random number generation.

;; Please note that the function dw--read is not my own; it is taken
;; from rejeep’s f.el (https://github.com/rejeep/f.el), and not
;; intended as part of this package's public API.

;;; Code:

(require 'seq)
(require 'wid-edit)

(defgroup dw nil
  "Generate diceware passphrases."
  :group 'convenience)

;;; Package-specific errors

;; Input Errors
(define-error 'dw-bad-roll
  "Invalid die roll"
  'user-error)
(define-error 'dw-incomplete-roll
  "Not enough die rolls for a complete word"
  'dw-bad-roll)
(define-error 'dw-too-short-passphrase
  "Too few words for a secure passphrase"
  'dw-bad-roll)
;; File Errors
(define-error 'dw-bad-wordlist
  "Broken wordlist"
  'user-error)
(define-error 'dw-incomplete-wordlist
  "Broken wordlist, missing entries"
  'dw-bad-wordlist)
(define-error 'dw-overfull-wordlist
  "Broken wordlist, too many entries"
  'dw-bad-wordlist)
(define-error 'dw-missing-entry
  "Broken wordlist, missing entry for dice roll"
  'dw-bad-wordlist)
;; Misc RNG Errors
(define-error 'dw-incomplete-int
  "Not enough die rolls for the given integer range"
  'dw-bad-roll)
(define-error 'dw-overflow
  "Too many consecutive die rolls, not implemented"
  'range-error)

;;; Package-specific warnings

(defun dw--warn-short-words ()
  "Report a warning for passphrases with too many short words."
  (delay-warning '(dw too-many-short-words)
                 (concat "The generated passphrase has many short words. "
                         "Consider discarding it.")))

;;; Constants

(defconst dw--dice-number 5
  "Number of die rolls needed for one word in a passphrase.")

(defconst dw--conversion-limit (floor (log most-positive-fixnum 6))
  "Length of the largest string that allows direct integer conversion.
For the time being, this constant also governs the maximum number
of dice usable to generate a random integer with ‘dw-generate-ranint’.")

(defconst dw--wordlist-length (expt 6 dw--dice-number)
  "Number of entries needed for a valid wordlist.")

;;; User-facing variables
;; Core API

(defcustom dw-ignore-regexp "\\s-+"
  "Regular expression to match a sequence of separator chars.
Essentially, this regexp defines which chars (apart from the
numerals 1 to 6) are valid to appear in dice roll stings.
Allowing separators serves as a convenience for the user to be
able to keep long sequences of dice rolls readable on input."
  :type 'regexp
  :group 'dw)

(defcustom dw-minimum-word-count 5
  "Minimum length of a good passphrase (measured in words rolled).

Generating any passphrase shorter than this value will cause
public parts of the API to signal an error by default."
  :type 'integer
  :group 'dw)

(defcustom dw-minimum-chars-per-word 3.8
  "Minimum number of characters a good passphrase should have per word.

Generating any passphrase shorter than this value (times the
number of words) will cause public parts of the API to report an
appropriate warning."
  :type 'float
  :group 'dw)

;; Extended passphrase generation

;;;###autoload
(put 'dw-extra-char-string 'risky-local-variable t)
(defcustom dw-extra-char-string "945678^~!#$%=&*()-}+[]\\{>:;\"'<3?/012"
  "String of extra characters that can be added to a passphrase.

Every character should be unique.  Additionally, the string
should be no longer than 36 (6^2) characters."
  :type '(string
          :validate dw--validate-extra-char-string)
  :risky
  :group 'dw)

(defun dw--validate-extra-char-string (text-field)
  "Raise an error if TEXT-FIELD’s value is invalid for ‘dw-extra-char-string’.
If the string exceeds the maximal allowed length (or contains
redundant characters), remove excess chars and raise an error."
  (let* ((extra-string (widget-value text-field))
         (minimized-extra-string (concat (seq-uniq extra-string)))
         has-error)
    (unless (string= extra-string minimized-extra-string)
      (setq extra-string minimized-extra-string)
      (widget-put text-field :error "Characters must be unique in string")
      (setq has-error t))
    (unless (< (length extra-string) 36)
      (setq extra-string (substring extra-string 0 36))
      (widget-put text-field :error "String length must not exceed 36")
      (setq has-error t))
    (when has-error
      (widget-value-set text-field extra-string)
      text-field)))

;;;###autoload
(put 'dw-directory 'risky-local-variable t)
(defcustom dw-directory (locate-user-emacs-file "diceware")
  "Default directory for diceware wordlists for interactive functions.
If this directory is not present, it is automatically generated."
  :type 'directory
  :risky
  :group 'dw)
;; TODO: auto-generate when set

;; Quietly generate dir if it is missing.
(condition-case nil
    (make-directory dw-directory)
  (file-already-exists))

(defcustom dw-named-wordlists nil
  "Alist of personal wordlists for interactive use.

Each element is a dotted list of the form
\(NAME FILE . CODING)

where NAME is the wordlist’s name used interactively (a symbol),
FILE is a string containing the actual filename of the wordlist
and CODING is the encoding of the file, nil being equivalent to
`utf-8'.

NAME is what interactive commands will query for to access a
particular wordlist.

FILE, if relative, is relative to ‘dw-directory’."
  :type '(alist
          :key-type (symbol :format "Wordlist name: %v" :value default)
          :value-type
          (cons (string :format "Wordlist file: %v")
                (choice
                 :format "Coding: %[Value Menu%] %v"
                 (const :tag "Default (‘utf-8’)" nil)
                 (coding-system
                  :tag "Other coding system")))) ;; TODO: validate string argument
  :group 'dw)

;;; Internal predicates

(defun dw--valid-rolls-p (string &optional allow-empty)
  "Return t if STRING is a sequence of digits from 1 to 6.
If the optional second argument ALLOW-EMPTY is non-nil, then
treat empty strings as valid."
  (and (stringp string)
       (or allow-empty (not (seq-empty-p string)))
       (not (dw--invalid-chars-list string))))


;;; Internal string processing

(defun dw--strip-separators (string)
  "Remove seperator chars from STRING.
Which chars constitute as such is governed by
‘dw-ignore-regexp’."
  (replace-regexp-in-string
   dw-ignore-regexp "" string))

(defun dw--invalid-chars-list string
  "Return a list of invalid die rolls in STRING.
The resulting list contains all characters that are not digits
from 1 to 6."
  (seq-difference string "123456"))

(defun dw--internalize-rolls (string)
  "Convert a STRING of dice rolls to a base-6 int."
  (string-to-number
   (replace-regexp-in-string "6" "0" string) 6))

;; Sadly, ‘number-to-string’ has no optional BASE argument.
(defun dw--format-dice-roll (int)
  "Convert internally used INT to corresponding dice roll string."
  (when (>= int 0)
    (let (digits)
      (dotimes (_ dw--dice-number)
          (push (% int 6) digits)
        (setq int (/ int 6)))
      (replace-regexp-in-string "0" "6"
                                (mapconcat #'number-to-string digits "")))))

(defun dw--parse-string (user-string &optional noerror)
  "Parse a USER-STRING of dice rolls.

USER-STRING is stripped of junk chars specified by
‘dw-ignore-regexp’ and then converted into a list of keys for an
internalized diceware wordlist (an alist).

If the optional second argument NOERROR is non-nil, then return
nil for invalid strings instead of signaling an error."
  (let* ((string (dw--strip-separators user-string))
         (total-rolls (length string))
         error-data)
    (unless (dw--valid-rolls-p string)
      (if (seq-empty-p string)
          (setq error-data
                `(dw-incomplete-roll 0 ,dw--dice-number))
        (setq error-data
              `(dw-bad-roll
                ,(char-to-string (car (dw--invalid-chars-list string)))))))
    (unless (= (% total-rolls dw--dice-number) 0)
      (setq error-data
            `(dw-incomplete-roll
              ,total-rolls
              ,(+ total-rolls
                  (- dw--dice-number (% total-rolls dw--dice-number))))))
    (cond ((and error-data noerror) nil)
          (error-data
           (signal (car error-data) (cdr error-data)))
          (t (mapcar #'dw--internalize-rolls
            (seq-partition string dw--dice-number))))))


;;; Internal wordlist parsing

(defun dw--read (path &optional coding)
  "Read text file located at PATH, using CODING.
Return the decoded text as multibyte string.

CODING defaults to ‘utf-8’.

This function is taken from rejeep’s f.el."
  (decode-coding-string
   (with-temp-buffer
     (set-buffer-multibyte nil)
     (setq buffer-file-coding-system 'binary)
     (insert-file-contents-literally path)
     (buffer-substring-no-properties (point-min) (point-max)))
   (or coding 'utf-8)))

(defun dw--read-wordlist (path &optional coding)
  "Internalize plain text wordlist at PATH using CODING.
Return the resulting intermediate list for further processing.

CODING defaults to ‘utf-8’.

Each non-empty line of the file should be of the form
  ROLL WORD

where ROLL is a sequence of five digits from 1 to 6, representing
one of the 6^5 (7776) possible dice rolls.  WORD should be a
sequence of (non-whitespace) characters to be used in the
passphrase for that particular ROLL.

Each element of the returned list is a list of strings
corresponding to one (non-empty) line in the file.  Each line is
in return segmented at every space and horizontal tab.

This function is not supposed to be used by itself.  In order to
access a diceware wordlist from a file, see `dw-build-alist'."
  (let ((dice-table (dw--read path coding)))
    (mapcar (lambda (x) (split-string x "[ \t]+" t "[ \t]+"))
            (split-string dice-table "[\f\r\n\v]+" t))))

(defun dw--parse-list (read-list)
  "Parse raw construct READ-LIST, forming a proper diceware alist.
READ-LIST is sanitized before conversion, so that junk entries
are ignored.

This function is not supposed to be used by itself.  In order to
access a diceware wordlist from a file, see `dw-build-alist'."
  (mapcar (lambda (x)
            (cons (dw--internalize-rolls (elt x 0))
                  (elt x 1)))
          (seq-filter
           (lambda (x) (and (dw--valid-rolls-p (car x))
                            (= (length (car x)) dw--dice-number)))
           read-list)))


;;; Checkers
(defun dw--check-passlist (passlist &optional noerror)
  "Check PASSLIST for issues and return it.

If the optional second argument NOERROR is non-nil, then return
nil instead of raising an error for an unusable PASSLIST."
  (let ((word-count (length passlist))
        (pass-length (length (apply #'concat passlist)))
        error-data)
    (cond
     ((< word-count dw-minimum-word-count)
      (setq error-data
            `(dw-too-short-passphrase ,word-count ,dw-minimum-word-count)))
     ((< pass-length (* word-count dw-minimum-chars-per-word))
      (dw--warn-short-words)))
    (cond ((and error-data noerror)
           nil)
          (error-data
           (signal (car error-data) (cdr error-data)))
          (t
           passlist))))

(defun dw--check-wordlist (alist &optional noerror)
  "Check internalized wordlist ALIST for issues and return it.

If the optional second argument NOERROR is non-nil, then return
nil instead of raising an error for an unusable ALIST."
  (let ((wordlist-length (length alist))
        (required-keys (number-sequence 0 (1- dw--wordlist-length)))
        (key-list (mapcar #'car alist))
        missing-keys
        error-data)
    (cond
     ((< wordlist-length dw--wordlist-length)
      (setq error-data `(dw-incomplete-wordlist
                         ,wordlist-length ,dw--wordlist-length)))
     ((> wordlist-length dw--wordlist-length)
      (setq error-data `(dw-overfull-wordlist
                         ,wordlist-length ,dw--wordlist-length)))
     ((not (equal key-list required-keys))
      (setq missing-keys
            (seq-filter #'identity
                        (cl-mapcar (lambda (x y) (and (/= x y) x))
                              required-keys
                              key-list)))
      (setq error-data `(dw-missing-entry
                         ,(dw--format-dice-roll (car missing-keys))))))
    (cond ((and error-data noerror)
           nil)
          (error-data
           (signal (car error-data) (cdr error-data)))
          (t
           alist))))


;;; Basic public API

(defun dw-build-alist (path &optional default-dir coding noerror)
  "Read a plain text wordlist at PATH and convert to an internalized alist.

Each non-empty line of the file should be of the form
  ROLL WORD

where ROLL is a sequence of five digits from 1 to 6, representing
one of the 6^5 (7776) possible dice rolls.  WORD should be a
sequence of (non-whitespace) characters to be used in the
passphrase for that particular ROLL.  It must be separated from
ROLL by at least one space or tab character.  Both may be
preceded/followed by an arbitrary amount of whitespace.

Empty lines (as well as lines with invalid contents) are treated
as junk and ignored.

DEFAULT-DIR, if non-nil, is the directory to start with if PATH
is relative.  It defaults to the current buffer’s value of
‘default-directory’.

CODING, if non-nil, is the coding system used for the wordlist.
It defaults to ‘utf-8’.

If the optional fourth argument NOERROR is non-nil, then return
nil instead of raising an error in case of the wordlist being
either invalid or incomplete."
  (let ((dice-file (expand-file-name path default-dir)))

    (dw--check-wordlist
     (sort (dw--parse-list
            (dw--read-wordlist dice-file coding))
           (lambda (x y) (< (car x) (car y))))
     noerror)))

(defun dw-generate-passlist (string alist &optional noerror)
  "Generate a list of words from a STRING of dice rolls.
ALIST should be an internalized wordlist generated by
‘dw-build-alist’.  The result is a list of words forming the
actual passphrase.

If the optional third argument NOERROR is non-nil, then return
nil instead of raising an error in case of STRING being either
invalid or incomplete."
  (mapcar (lambda (x) (cdr (assq x alist)))
          (dw--check-passlist
           (dw--parse-string string noerror)
           noerror)))

(defun dw-required-dice (n)
  "Minimum number of dice to randomly choose between N possible outcomes."
  (ceiling (log n 6)))

(defun dw-generate-ranint (string maxint &optional noerror)
  "Convert STRING of dicerolls to a random int from 0 to MAXINT.
STRING is expected to be a sequence of dicerolls.
MAXINT is not included in the random number range.
If STRING does not produce a valid value, return -1.

STRING must contain at least log6(MAXINT) die rolls, rounded up.
It may contain more, however (up to a machine-dependent limit).

Unless MAXINT divides a power of 6, there is no way to map all
outcomes N dice can produce evenly and exhaustively at the same
time.  Hence, on occasion random number generation will fail,
producing -1 as an outcome.  Since the failure chance can reach
up to 50%, it is recommended to choose an appropriate MAXINT.

If the optional third argument NOERROR is non-nil, then return
nil instead of raising an error in case of STRING."

  (let* ((string (dw--strip-separators string))
         (dice-num (length string))
         (min-dice (dw-required-dice string))
         (random-int -1)
         error-data)
    (cond ((< dice-num min-dice)
           (setq error-data
                 `(dw-incomplete-int ,dice-num ,min-dice)))
          ((not (dw--valid-rolls-p string))
           (setq error-data
                 `(dw-bad-roll
                   ,(char-to-string (car (dw--invalid-chars-list string))))))
          ;; Does the entire dice string fit into a fixnum int?
          ((< dice-num dw--conversion-limit)
           (setq random-int (dw--internalize-rolls string))
           (unless (< random-int (% (expt 6 dice-num) random-int))
             (setq random-int (% random-int dice-num))))
          ;; With bignums in Emacs 27.1, I could in principle rely on
          ;; arbitrary integer artithmetic.  However, using bignums
          ;; for this is immensely wasteful, especially since this can
          ;; easily be done with fixnums using simple modulo
          ;; arithmetic.  However, it's a feature not worth needlessly
          ;; implementing, especially since this package has a history
          ;; of accumulating needless complexity.  I'll support it
          ;; once someone opens an issue.
          (t
           (setq error-data
                 `(dw-overflow ,dice-num ,dw--conversion-limit))))

    (when (and error-data (not noerror))
      (error-data
       (signal (car error-data) (cdr error-data))))

    random-int))


;;; Interactive commands
;; dw-set-alist

(provide 'dw)
;;; dw.el ends here
