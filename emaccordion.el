;; -*- lexical-binding: t -*-

;;;; Copyright 2017-2018 Janne Nykopp

;;;; emaccordion.el

;;;;    Emaccordion is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    Emaccordion is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with Emaccordion. If not, see <http://www.gnu.org/licenses/>.


;;; Variable `lexical-binding' is set to true in file local variable
;;; settings on the first line; it's required for running timers with
;;; lexical variables.

;;; `cl-lib' is used at runtime in the MIDI parser originating from
;;; Common Lisp program "Notewhacker"
;;; <https://github.com/jnykopp/notewhacker>. Hence required here.
(require 'cl-lib)

;;; For high-level documentation, see the file documentation.org.


;;; ----------------------------------------------------------------------------
;;; Some helper functions for handling MIDI note number (MNN) and
;;; scientific pitch notation (SPN).
(defvar emaccordion--mnnm-to-letter-name-accidental
  '((c . nil) (c . s) (d . nil) (d . s) (e . nil) (f . nil) (f . s)
    (g . nil) (g . s) (a . nil) (a . s) (b . nil))
  "Map a MNN-modulo to letter name and accidental (on C-Major
  scale). Intended for use with `emaccordion--mnn-to-spn' and
  `emaccordion--spn-to-mnn'.")

(defun emaccordion--mnn-to-octave-mnnm (mnn)
  "Convert a MIDI note number MNN to octave and MNN-modulo.
Return values octave, mnnm."
  (cl-multiple-value-bind (oct-plus-1 mnnm) (cl-floor mnn 12)
    (cl-values (1- oct-plus-1) mnnm)))

(defun emaccordion--mnn-to-spn (mnn)
  "Convert a MIDI note number to scientific notation plus
accidental. Return the notation as a string; name, possible
accidental and octave. Note: when MNN could be written in
multiple ways, only outputs the choice with sharp or nil as
accidental."
  (cl-multiple-value-bind (oct mnnm) (emaccordion--mnn-to-octave-mnnm mnn)
    (cl-destructuring-bind (name . accidental)
        (nth mnnm emaccordion--mnnm-to-letter-name-accidental)
      (format "%s%s%s" name (if accidental "#" "") oct))))

(defun emaccordion--spn-to-mnn (spn)
    "Convert scientific pitch notation SPN (string of key, octave
and accidental) to MIDI note number. Note! Only sharp accidentals
are recognized at the moment."
    (if (string-match (rx bol (group (in "a-gA-G")) (group (? (in "#"))) (group digit) eol)
		      spn)
	(let* ((name (intern (match-string 1 spn)))
	       (octave (string-to-number (match-string 3 spn)))
	       (accidental (if (string= "" (match-string 2 spn)) nil 's))
	       (mnnm (cl-position (cons name accidental)
                                  emaccordion--mnnm-to-letter-name-accidental
                                  :test #'cl-equalp)))
	  (if mnnm
	      (+ mnnm (* (1+ octave) 12))
	    (error "Invalid Scientific Pitch Notation: %s" spn)))
      (error "Invalid Scientific Pitch Notation: %s" spn)))

(defun emaccordion--chan-spn/mnn-to-note (chan-spn/mnn)
  "Convert CHAN-SPN/MNN (which is a cons of a channel and either
an SPN if it's a string, or a MNN if it's an integer) to a note."
  (let ((chan (car chan-spn/mnn))
        (spn/mnn (cdr chan-spn/mnn)))
    (cons chan
          (cl-etypecase spn/mnn
            (string (emaccordion--spn-to-mnn spn/mnn))
            (integer spn/mnn)))))

;;; ----------------------------------------------------------------------------
;;; Note and chord handling basics.
(defun emaccordion--<note (x y)
  "See if note X is smaller than note Y."
  (if (= (car x) (car y)) (< (cdr x) (cdr y)) (< (car x) (car y))))

(defun emaccordion--normalize-chord (l)
  "A chord is always sorted. Return a sorted copy of the list L
consisting of notes."
  (sort l #'emaccordion--<note))

(defun emaccordion--find-note-from-mapping (n l)
  "Find note N from list L like
`emaccordion-note-to-modifier'. Returns the mapping, which is a
cons of note and modifier value."
  (cdr (assoc n l)))

(defun emaccordion--find-chord-from-mapping (c l)
  "Find a chord C from a list L like
`emaccordion-chord-actions'. Returns the mapping, which is a cons
of chord and action."
  (cl-find c l :key #'car :test #'cl-equalp))

(defun emaccordion--midi-event-to-note (e)
  "Convert a MIDI event E to a note."
  (cons (emaccordion--midi-event-channel e) (emaccordion--midi-event-key e)))

(defun emaccordion--midi-event-list-to-chord (l)
  "Convert the list of MIDI events L to a chord. Used for finding
a list of pressed keys and the matching action from the
configured key chords."
  (emaccordion--normalize-chord
   (mapcar #'emaccordion--midi-event-to-note l)))

;;; ----------------------------------------------------------------------------
;;; Modifier values etc. Note that these come from C-code. In my
;;; opinion, it would be great to have these available from elisp side
;;; as well by default.
(defvar emaccordion--alt-modifier #x0400000
  "ALT modifier value. See Emacs' source src/lisp.h. Note:
  Usually on a PC keyboard pressing 'Alt' key produces META
  modifier, not ALT modifier!")
(defvar emaccordion--super-modifier #x0800000
  "SUPER modifier value. See Emacs' source src/lisp.h")
(defvar emaccordion--hyper-modifier #x1000000
  "HYPER modifier value. See Emacs' source src/lisp.h")
(defvar emaccordion--shift-modifier #x2000000
  "SHIFT modifier value. See Emacs' source src/lisp.h")
(defvar emaccordion--ctrl-modifier #x4000000
  "CTRL modifier value. See Emacs' source src/lisp.h")
(defvar emaccordion--meta-modifier #x8000000
  "META modifier value. See Emacs' source src/lisp.h. Note:
  Usually on a PC keyboard pressing 'Alt' key produces META
  modifier, not ALT modifier!")

(let ((kwd-to-val-alist
       `((:alt   . ,emaccordion--alt-modifier)
         (:super . ,emaccordion--super-modifier)
         (:hyper . ,emaccordion--hyper-modifier)
         (:shift . ,emaccordion--shift-modifier)
         (:ctrl  . ,emaccordion--ctrl-modifier)
         (:meta  . ,emaccordion--meta-modifier))))
  (defun emaccordion--modkwd-to-val (keyword)
    "Helper function. Convert a modifier KEYWORD to a value. For
ease of use, modifiers can be referred to with KEYWORD symbols
`:alt', `:super', `:hyper', `:shift', `:ctrl', `:meta'.  These
correspond with the value of `emaccordion--alt-modifier' etc."
    (cdr (assoc keyword kwd-to-val-alist)))
  (defun emaccordion--modval-to-kwd (val)
    "Helper function. Convert a modifier VAL back to modifier
keyword."
    (car (cl-find val kwd-to-val-alist :key #'cdr))))

;;; --------------------------------------------------------------------
;;; Configuration helper functions
(defun generate-chan-spn-list (&rest chan-spn-mod-list)
  "Helper function to create a list of mappings from channel and
scientific pitch notation to modifier, used in
`emaccordion-note-to-modifier'."
  (let (result)
    (dolist (chan-spn-mod chan-spn-mod-list (reverse result))
      (cl-destructuring-bind (chan spn mod) chan-spn-mod
	(push (cons (cons chan (emaccordion--spn-to-mnn spn))
                    (emaccordion--modkwd-to-val mod))
              result)))))

(defun emaccordion-generate-modifier-config (&rest note-modifier-configs)
  "Helper function. Generate an `emaccordion-note-to-modifier'
mapping out of given NOTE-MODIFIER-CONFIGS. The syntax of
NOTE-MODIFIER-CONFIGS is (:note (chan . key-as-spn) :modifier
modifier-name). Modifier name is one of the keyword symbols
`:alt', `:super', `:hyper', `:shift', `:ctrl', `:meta'.

A more elaborate description is available in the high-level
documentation."
  (let (note-mod-mapping)
    (dolist (config note-modifier-configs note-mod-mapping)
      (let* ((note-spn (cl-getf config :note))
             (note (emaccordion--chan-spn/mnn-to-note note-spn))
             (modifier-int (emaccordion--modkwd-to-val (cl-getf config :modifier)))
             (prev-entry (emaccordion--find-note-from-mapping note note-mod-mapping)))
        (when prev-entry
          (error "Entry already exists for %s: %s" note-spn (emaccordion--modval-to-kwd prev-entry)))
        (unless (and modifier-int (car note) (cdr note)) (error "Invalid entry %s" config))
        (push (cons note modifier-int) note-mod-mapping)))))

(defun emaccordion-generate-chord-action-config (&rest configs)
  "Helper function. Generate an `emaccordion-chord-actions'
mapping out of given CONFIGS. The syntax of CONFIGS
is (:chord ((chan . key-as-spn) ...) :action the-action)

A more elaborate description is available in the high-level
documentation."
  (cl-flet ((chord-as-proper-list (c) (if (listp (car c)) c (list c))))
    (let ((ch-act-mapping))
      (dolist (config configs ch-act-mapping)
	(let* ((entry-as-list (chord-as-proper-list (cl-getf config :chord)))
	       (chord (emaccordion--normalize-chord (mapcar
                                                     #'emaccordion--chan-spn/mnn-to-note
                                                     entry-as-list)))
	       (action (cl-getf config :action))
	       (prev-entry (cdr (emaccordion--find-chord-from-mapping chord ch-act-mapping))))
	  (when prev-entry (error "Entry already exists for %s" entry-as-list))
	  (unless (and chord action) (error "Invalid entry %s" config))
	  (push (cons chord action) ch-act-mapping))))))

;;; --------------------------------------------------------------------
;;; Configuration
(defvar emaccordion-midi-device-pathname (expand-file-name "/dev/midi1")
  "Read raw MIDI octets from this device.")

(defvar emaccordion-print-buffer
  (get-buffer-create "*emaccordion-events*")
  "Print the actions in this buffer, if
  `emaccordion-print-actions' is on.")

(defvar emaccordion-print-actions t
  "Print the actions in a separate buffer, whose name is
  specified in `emaccordion-print-buffer'.")

(defvar emaccordion-upcase-velocity-threshold 100
  "The MIDI-velocity, which when exceeded by all of the
  keypresses in the chord, the character action will be
  upcased. Range [0-127]. See
  https://en.wikipedia.org/wiki/MIDI#Messages")

(defvar emaccordion-modifier-clear-timeout 30.0
  "How long to wait in seconds for modifier key release event
  before automatically clearing the modifiers that have been
  set. This automatic clearing is provided so that a broken MIDI
  connection won't render Emacs unusable.

  If this is set to NIL, automatic clearing will not be in use.")

;;; Provided as an example.
(defvar emaccordion-note-to-modifier
  (emaccordion-generate-modifier-config
   '(:note (0 . "b3") :modifier :ctrl)
   '(:note (0 . "f3") :modifier :meta)
   '(:note (0 . "a3") :modifier :shift))
  "A list of conses of note and modifier-integer-value.")

(defvar emaccordion-chord-expire-timeout 0.015
  "The expire timeout for `emaccordion--chord-expire-timer' in
  seconds. User input sequences arriving within this time between
  each other are collected into chords (as two or more keypresses
  won't come exactly simultaneously).

  Should be short enough not to induce noticeable lag or
  erroneous chords in typing, but long enough to allow for
  chords.")

;;; Provided as an example of different kinds of chords and actions.
(defvar emaccordion-chord-actions
  (emaccordion-generate-chord-action-config
   '(:chord ((0 . "c5") (0 . "b5")) :action ?a)
   '(:chord (0 . "e4") :action "test")
   '(:chord (0 . "d4") :action [?a ?b ?c ?✈])
   '(:chord (0 . "c4") :action emacs-uptime))
  "Database of actions to take on given chords. A list containing
  lists of [conses of MIDI channel and MIDI note number
  combinations] and the action. The channel-key sequences must be
  kept sorted by channel first, then key, for searchability.")

(defvar emaccordion--repeat-key-initial-timeout 1
  "The initial expire timeout for `emaccordion--repeat-key-timer'
  in seconds. Defines the pause between pressing a key or chord
  `emaccordion--repeat-key-chord' and starting to repeat it. If
  NIL, do not use repeat at all.")

(defvar emaccordion--repeat-key-timeout 0.1
  "The repeat expire timeout for `emaccordion--repeat-key-timer'
  in seconds. Defines the repeat rate (seconds between repeats)
  of a pressed and held key or chord
  `emaccordion--repeat-key-chord'. If NIL, do not use repeat at
  all.")
;;; Configuration ends
;;; --------------------------------------------------------------------

;;; --------------------------------------------------------------------
;;; Simple shorthand functions
(defun emaccordion--midi-event-matching-modval (e)
  "Return the modifier value associated to
emaccordion--midi-event E in `emaccordion-note-to-modifier' or
NIL if not found."
  (emaccordion--find-note-from-mapping
   (emaccordion--midi-event-to-note e)
   emaccordion-note-to-modifier))

;;; ----------------------------------------------------------------------------
;;; Raw MIDI handler - translated to emacs lisp from Common Lisp
;;; program "Notewhacker" <https://github.com/jnykopp/notewhacker>.
;;;
;;; Compared to the original, all functions, macros and structs have
;;; the "emaccordion--" prefix and some Common Lisp calls are prefixed
;;; with "cl-". Special variables' earmuffs were removed as they're
;;; against the emacs lisp coding conventions
;;; (https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html). Unit
;;; tests have been also removed.
(cl-defstruct emaccordion--midi-event
  (timestamp (current-time))    ; Timestamps unused, could be removed?
  channel
  key
  velocity)                ; Velocity of 0 means a MIDI note-off event

(cl-declaim (type (or null (integer #x80 #xef)) emaccordion--midi-running-status))
(defvar emaccordion--midi-running-status nil
  "Voice and Mode Messages set the receiver in Running Status mode,
where status octets are not necessary if the status doesn't
change. This will be set to nil if any non voice or mode message
is received, or any System Exclusive or Common Status message is
received. Real-time messages won't affect running status.")

(defun emaccordion--midi-event-key-chan-eq-p (ev1 ev2)
  "See if two midi-events are equal (channel and key compared
  only)."
  (and (= (emaccordion--midi-event-channel ev1) (emaccordion--midi-event-channel ev2))
       (= (emaccordion--midi-event-key ev1) (emaccordion--midi-event-key ev2))))

(defun emaccordion--midi-status-octet-p (octet)
  "Check if OCTET is a midi status octet."
  (and (> octet #b01111111) octet))

(cl-defmacro emaccordion--with-default-midi-data-checking ((num-of-args raw-data ptr) &body body)
  "Wrap BODY in code that tests that midi data from RAW-DATA starting
  from index PTR seems to be in valid (right number of argument octets
  etc.). If it's not, a suitable value will be returned."
  (let ((num-of-args-gs (cl-gensym "num-of-args"))
        (raw-data-gs (cl-gensym "raw-data"))
        (ptr-gs (cl-gensym "ptr")))
    `(let ((,num-of-args-gs ,num-of-args)
           (,raw-data-gs ,raw-data)
           (,ptr-gs ,ptr))
       (if (< (- (length ,raw-data-gs) ,ptr-gs) ,num-of-args-gs)
           ;; Insufficient data. TODO: Log.
           (cl-values nil ,ptr-gs)
         (let ((err-pos
                (cl-position-if 'emaccordion--midi-status-octet-p ,raw-data-gs
                                :start ,ptr-gs :end (+ ,ptr-gs ,num-of-args-gs))))
           (if err-pos
               ;; Invalid data (status octet instead of data
               ;; octet). TODO: Log.
               (cl-values nil err-pos)
             ;; Data was ok.
             (cl-values ,@body (+ ,ptr-gs ,num-of-args-gs))))))))

(defun emaccordion--gen-chan-handler-ignoring-n-args (num-of-args)
  "Creates a midi octet stream handler that just ignores status
octet and NUM-OF-ARGS octets."
  (lambda (status raw-data ptr)
    (setf emaccordion--midi-running-status status)
    (emaccordion--with-default-midi-data-checking (num-of-args raw-data ptr)
      nil)))

(defun emaccordion--status-octet-channel-number (octet)
  "Get the channel number from a status octet OCTET."
  (logand octet #b00001111))

(defun emaccordion--note-generic-handler (status raw-data ptr note-on-p)
  "Generic handler for note midi message. Create the event from
STATUS (the status octet) and RAW-DATA, offset by PTR. RAD-DATA
at PTR should start the data octets for the note. If NOTE-ON-P is
non-nil and velocity is greater than 0, the instance will be
note-on. (Some midi devices send note-on with velocity of 0
instead of note-off.)

Return values consisting of the instance and number of octets
handled."
  (emaccordion--with-default-midi-data-checking (2 raw-data ptr)
    (let ((channel (emaccordion--status-octet-channel-number status))
          (key (aref raw-data ptr))
          (velocity (aref raw-data (1+ ptr))))
      (declare (type (integer 0 127) key velocity)
               (type (integer 0 15) channel))
      (make-emaccordion--midi-event
       :channel channel :key key :velocity (if note-on-p velocity 0)))))

(defun emaccordion--note-on-handler (status raw-data ptr)
  "Create and return a note-on or note-off event. (Some midi devices
  send note-on with velocity of 0 instead of note-off.)"
  (setf emaccordion--midi-running-status status)
  (emaccordion--note-generic-handler status raw-data ptr t))

(defun emaccordion--note-off-handler (status raw-data ptr)
  "Create and return a note-off event."
  (setf emaccordion--midi-running-status status)
  (emaccordion--note-generic-handler status raw-data ptr nil))

(defun emaccordion--sys-common-handler (raw-data ptr)
  "Handle a system common message. These come in various forms"
  (setf emaccordion--midi-running-status nil)
  (error "Sys-common-handler not yet implemented (status: %s; data: %s %s)"
         status raw-data ptr))

(defun emaccordion--sys-real-time-handler (status raw-data ptr)
  "Handle a system real time message. Right now, they are ignored."
  (ignore status raw-data)
  (cl-values nil (1+ ptr)))

(defvar emaccordion--midi-channel-status-handler-mapper
  `((#b1000 . emaccordion--note-off-handler)                       ; Note off event
    (#b1001 . emaccordion--note-on-handler)                        ; Note on event
    (#b1010 . ,(emaccordion--gen-chan-handler-ignoring-n-args 2))  ; Polyph. Key Pressure
    (#b1011 . ,(emaccordion--gen-chan-handler-ignoring-n-args 2))  ; Control Change
    (#b1100 . ,(emaccordion--gen-chan-handler-ignoring-n-args 1))  ; Program Change
    (#b1101 . ,(emaccordion--gen-chan-handler-ignoring-n-args 1))  ; Channel Pressure
    (#b1110 . ,(emaccordion--gen-chan-handler-ignoring-n-args 2))) ; Pitch Wheel Change
  "Map a status octete of a midi event with channel information by its
  most-significant-nibble to a handler function.")

(defvar emaccordion--midi-system-status-handler-mapper
  '((#b11110 . emaccordion--sys-common-handler)
    (#b11111 . emaccordion--sys-real-time-handler))
  "Map a status octet of a system status midi event by its 5 MSBs to a
  handler function.")

(defun emaccordion--get-handler-for (status-octet)
  "Get a handler according to the STATUS-OCTET."
  (or (cdr (assoc (ash status-octet -4) emaccordion--midi-channel-status-handler-mapper))
      (cdr (assoc (ash status-octet -3) emaccordion--midi-system-status-handler-mapper))))

(defun emaccordion--octets-to-midi-event (status raw-data ptr)
  "Create one midi-event from STATUS octet and RAW-DATA. If the
RAW-DATA is insufficient, return values nil and PTR. If RAW-DATA
is invalid, return values nil and PTR + number of octets so far
handled. If RAW-DATA defines an event, return an event instance
and PTR + number of octets handled. The event instance returned
may be nil, if the event was to be ignored."
  (let ((handler (emaccordion--get-handler-for status)))
    (funcall handler status raw-data ptr)))

(defun emaccordion--create-midi-events-from-octets (raw-data)
  "Create new midi events from raw octet data RAW-DATA (vector or
nil). Raw data might contain incomplete event in the end. Returns
multiple values: a list of events ordered by arrival time,
earlies first, and a vector containing remaining unhandled
handled octets in the case there was an incomplete event at the
end. Malformed data is ignored."
  (let ((ptr 0) (raw-data-len (length raw-data))
        events)
    (cl-loop
     with status = nil
     while (< ptr raw-data-len)
     do
     ;; Seek to next status octet or continue running status
     (cl-loop
      for candidate-status = (emaccordion--midi-status-octet-p (aref raw-data ptr))
      for curr-status = (or candidate-status emaccordion--midi-running-status)
      when (or candidate-status (not curr-status)) do (cl-incf ptr)
      do (setf status curr-status)
      until curr-status)
     ;; Then create an event
     (cl-multiple-value-bind (event new-ptr)
         (emaccordion--octets-to-midi-event status raw-data ptr)
       (when event (push event events))
       (if (= new-ptr ptr)
           ;; The last event was incomplete, nothing more to do.
           (loop-finish)
         ;; Otherwise continue.
         (setf ptr new-ptr))))
    (cl-values (reverse events) (cl-subseq raw-data (min ptr raw-data-len)))))

;;; Notewhacker-loan ends
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Actions-related code
(defvar emaccordion--pressed-and-held-keys
  (list)
  "Keys (instances of emaccordion--midi-event) that are currently
  pressed and held. This is a list of MIDI-events instead of a
  normalized chord, because we can then also use the velocity
  information of a MIDI-event for upcasing.")

(defun emaccordion--inject-char-action (char)
  "Make emacs behave as if a key matching CHAR on a regular PC
keyboard was pressed. (Though CHAR can be any character, not
restricted to the keys found on existing PC keyboards).

Current implementation adds the CHAR to `unread-command-events'
list, maintaining its order. Emacs' command loop will then call
the input actions later."
  (setf unread-command-events
        (nconc unread-command-events (cons char nil))))

(defun emaccordion--decorate-with-modifiers (char modifiers)
  "This function is supposed to be run before
`emaccordion--inject-char-action'. Modify the given CHAR, being a
character like one pressed on keyboard, to equivalent character
for which the MODIFIERS (a list like
`emaccordion--pressed-and-held-modifiers') are applied. Return
the new action character."
  ;; Control and shift are currently the only "special" cases for MIDI
  ;; input.
  ;;
  ;; When MODIFIERS has shift in it, it doesn't affect
  ;; `emaccordion--inject-char-action' so manually upcase any input
  ;; CHAR. This is implemented in `conv-to-shift'.
  ;;
  ;; For control, we replicate the functionality of "make_ctrl_char"
  ;; from src/keyboard.c. This handles ASCII range differently than
  ;; the rest of the range, and certain character range inside the
  ;; ascii ([a-z] and [A-Z]) differently than rest of the others. This
  ;; is implemented in `conv-to-ctrl'. (Would be nice to have
  ;; "make_ctrl_char" exposed as standard emacs lisp function).
  ;;
  ;; Another possibility would be to use (string-to-char (kbd (format
  ;; "C-%s" (char-to-string ...)))) for control-modified characters
  ;; (when they're in the correct range, i.e. ASCII), but it is ugly
  ;; ugly ugly way to create the control sequence. But would it be
  ;; better (portability and maintenance wise)?
  (cl-flet ((conv-to-ctrl (ch)
              (if (> (logand (lognot #o177) ch) 0)
                  ;; non-ASCII, just decorate with CONTROL
                  (logior ch emaccordion--ctrl-modifier)
                ;; ASCII; for both ranges [a-z] and [#o100-#o140] we
                ;; set the ch's all but lowest 5 bits to 0. For the
                ;; range [A-Z] we also need to turn on the
                ;; shift-modifier bit. For anything else starting from
                ;; space, we just set the control modifier. Rest of
                ;; the ASCII table is just returned as-is (I guess you
                ;; can't control-NAK for example).
                (cond
                 ((or (< #o077 ch #o140) (<= ?a ch ?z))
                  (logior (logand ch (lognot #o140))
                          (if (<= ?A ch ?Z)
                              emaccordion--shift-modifier
                            0)))
                 ((>= ch ? ) (logior ch emaccordion--ctrl-modifier))
                 (t ch))))
            (conv-to-shift (ch) (upcase ch)))
    (cl-macrolet ((handle-modifier (mod-val char fun)  ;TODO: UGLY!!
                   "Apply FUN to CHAR, and if FUN returns non-NIL,
remove MOD-VAL from MODIFIERS."
                   `(if (memq ,mod-val modifiers)
                        (let ((v (,fun ,char)))
                          (if v
                              (progn (setf modifiers (delete ,mod-val modifiers)) v)
                            ,char))
                      ,char)))
      (let* ((maybe-s-char (handle-modifier emaccordion--shift-modifier char conv-to-shift))
	     (maybe-s-c-char (handle-modifier emaccordion--ctrl-modifier maybe-s-char conv-to-ctrl)))
	;; Just logior the rest to the possibly ctrl'd or shifted char.
	(apply #'logior maybe-s-c-char modifiers)))))

(defmacro emaccordion--run-when-emacs-idle (&rest body)
  "Run BODY at next time emacs is idle. Purpose is to let command
  loop run the `unread-command-events' first. Idea from
  https://emacs.stackexchange.com/questions/11003/run-a-function-after-control-returns-to-the-command-loop."
  `(run-with-timer 0 nil (lambda () ,@body)))

(defun emaccordion--perform-action (action &rest enhancers)
  "Make emacs perform the ACTION, possibly enhanced with
ENHANCERS (list of keywords; currently only :upcase is
supported. See `emaccordion--perform-action-matching-midi-events' which
is the only place this is used at the moment."
    (cond
     ((characterp action)
      (let ((char (emaccordion--decorate-with-modifiers
                   action
                   emaccordion--pressed-and-held-modifiers)))
        ;; Can't put this within undo boundaries. Trust that the
        ;; emacs' command loop handles undo.
        (emaccordion--inject-char-action (if (cl-find :upcase enhancers) (upcase char) char))))
     ((and (functionp action) (commandp action))
      (emaccordion--run-when-emacs-idle
       (undo-boundary)
       (call-interactively action)
       (undo-boundary)))
     ((commandp action)
      (emaccordion--run-when-emacs-idle
       (undo-boundary)
       (execute-kbd-macro action)
       (undo-boundary)))))

;; TODO: The variables in the closure should be maybe converted to
;; bunch of defcustoms.
(let ((chan-colors (list "#ca3542" "#27647b" "#849fad"))
      (char-color "#57575f")
      (cmd-color "#aec8c9"))
  (with-current-buffer emaccordion-print-buffer
    (font-lock-mode)
    (setf tab-width 20)
    (setf header-line-format (concat " MIDI "
                                     (propertize "ch0 " 'face `(:foreground ,(nth 0 chan-colors)))
                                     (propertize "ch1 " 'face `(:foreground ,(nth 1 chan-colors)))
                                     (propertize "ch2 " 'face `(:foreground ,(nth 2 chan-colors)))
                                     (propertize "ch3..15 " 'face '(:foreground "#000000"))
                                     "\tAction")))
  (defun emaccordion--report-action (chord action &rest enhancers)
    "Report the CHORD and matching ACTION (and ENHANCERS) to a
buffer. For visualization and debugging."
    (when emaccordion-print-actions
      (with-current-buffer emaccordion-print-buffer
        (let ((spn-chrd
               (mapcar (lambda (x)
                         (cons (car x) (emaccordion--mnn-to-spn (cdr x))))
                       chord)))
          (goto-char (point-min))
          (dolist (c spn-chrd)
            (insert (propertize
                     (concat (cdr c) " ") 'font-lock-face
                     `(:foreground ,(or (nth (car c) chan-colors) "#000000")))))
          (insert "\t")
          (if action
              (insert (propertize (format (if (characterp action) "'%c'" "%s") action)
                                  'font-lock-face
                                  `(:foreground ,(if (integerp action) char-color cmd-color))))
            (insert (propertize "NO ACTION" 'font-lock-face '(:foreground "#ffffff" :background "#ff0000")))))
        (when (cl-some 'identity enhancers)
          (insert (format " %s" enhancers)))
        (newline)))))

;; I was thinking here a lot of changing the
;; `emaccordion--pressed-and-held-keys' to a normalized chord with
;; information about the minimum velocity to maybe eliminate some
;; lines of code, but decided to keep it as a list of midi-events,
;; because it's more flexible in the end.
(defun emaccordion--perform-action-matching-midi-events
    (action-list midi-events velocity-threshold)
  "Given MIDI-EVENTS (like `emaccordion--pressed-and-held-keys'),
search for the matching action for it in ACTION-LIST (like
`emaccordion-chord-actions') and perform that action if found.

If not found, message the user that the chord didn't match any
action.

If all note-on events in the MIDI-EVENTS exceed
VELOCITY-THRESHOLD, perform the action with :upcase enhancer."
  (when midi-events
    (let* ((upcase
            (when (cl-every (lambda (e)
                              (> (emaccordion--midi-event-velocity e)
                                 velocity-threshold))
                            midi-events)
              :upcase))
           (pressed-chord (emaccordion--midi-event-list-to-chord midi-events))
           (action (cdr (emaccordion--find-chord-from-mapping pressed-chord action-list))))
      (if action
          (emaccordion--perform-action action upcase)
        (message "Emaccordion: %s is undefined" pressed-chord))
      (emaccordion--report-action pressed-chord action upcase))))

(defun emaccordion--process-k-p-and-clear ()
  "Shorthand for processing and clearing
 `emaccordion--pressed-and-held-keys' against
 `emaccordion-chord-actions'"
  (emaccordion--perform-action-matching-midi-events
   emaccordion-chord-actions
   emaccordion--pressed-and-held-keys
   emaccordion-upcase-velocity-threshold)
  (setf emaccordion--pressed-and-held-keys (list)))

;;; ----------------------------------------------------------------------------
;;; Modifier related code

;; Emacs' modifier keys are defined in src/lisp.h. Alt, super, hyper,
;; and meta are straightforward, but control and shift are not. See
;; src/keyboard.c function read_event_from_main_queue where
;; extra_keyboard_modifiers are handled and function make_ctrl_char.
;;
;; To streamline the handling of modifiers in emaccordion, only the
;; following modifier codes are used:
;;
;; CHAR_ALT   = 0x0400000, (?\A-x)
;; CHAR_SUPER = 0x0800000, (?\s-x)
;; CHAR_HYPER = 0x1000000, (?\H-x)
;; CHAR_SHIFT = 0x2000000, (?\S-x)
;; CHAR_CTL   = 0x4000000, (?\C-x or e.g. ?\C-0)
;; CHAR_META  = 0x8000000, (?\M-x)
;;
;; Emaccordion only unsets modifiers it has itself set (of course it
;; can't know if someone else also wanted to set the exact same code
;; so it'll happily unset the modifier in that case), so any time
;; emaccordion sets e.g. the control modifier it will unset it with
;; the same code 0x4000000 - there might be still other control
;; modifiers in the `extra-keyboard-modifiers'.

(defvar emaccordion--pressed-and-held-modifiers (list)
  "List of pressed and held modifiers. Contains values like
  `emaccordion--alt-modifier' etc.")

(defvar emaccordion--modifier-clear-timer nil
  "A timer for clearing up modifiers, in case a key release event
  never arrives.")

(defun emaccordion--cancel-modifier-timer ()
  "Cancel the modifier clearing timer
`emaccordion--modifier-clear-timer'."
  (when emaccordion--modifier-clear-timer
    (cancel-timer emaccordion--modifier-clear-timer)
    (setf emaccordion--modifier-clear-timer nil)))

(defun emaccordion--restart-modifier-timer ()
  "Restart the timer `emaccordion--modifier-clear-timer'. Means
that the timer will be cancelled and created again."
  (when (and emaccordion--pressed-and-held-modifiers
             emaccordion-modifier-clear-timeout)
    ;; Set timer only if a modifier is pressed (and a timeout is
    ;;  configured).
    (emaccordion--cancel-modifier-timer)
    (setf emaccordion--modifier-clear-timer
          (run-at-time emaccordion-modifier-clear-timeout nil
                       #'emaccordion--clear-all-modifiers))))

(defun emaccordion--clear-modifiers (&rest modifiers)
  "Clear the given MODIFIERS (integer values, not modifier
names). Only clear the ones that were set by Emaccordion."
  (let ((emaccordion-set-to-be-cleared
         (cl-union emaccordion--pressed-and-held-modifiers modifiers)))
    (setf extra-keyboard-modifiers
          (logand extra-keyboard-modifiers
                  (lognot (apply 'logior emaccordion-set-to-be-cleared))))
    (setf emaccordion--pressed-and-held-modifiers
          (cl-set-difference emaccordion--pressed-and-held-modifiers
                             emaccordion-set-to-be-cleared))
    (unless emaccordion--pressed-and-held-modifiers
      ;; All cleared? No need for the timer any more.
      (emaccordion--cancel-modifier-timer))))

(defun emaccordion--clear-all-modifiers ()
  "Clear all of the modifiers set by Emaccordion."
  (emaccordion--clear-modifiers emaccordion--alt-modifier
                                emaccordion--super-modifier
                                emaccordion--hyper-modifier
                                emaccordion--shift-modifier
                                emaccordion--ctrl-modifier
                                emaccordion--meta-modifier))

(defun emaccordion--set-modifier (modifier)
  "Make the MODIFIER active. Only sets modifiers Emaccordion
hasn't already set."
  (unless (cl-find modifier emaccordion--pressed-and-held-modifiers)
    (push modifier emaccordion--pressed-and-held-modifiers)
    (setf extra-keyboard-modifiers (logior extra-keyboard-modifiers modifier))))

;;; ----------------------------------------------------------------------------
;;; Chord expire timer functionality
(defvar emaccordion--chord-expire-timer nil
  "A timer that is set when a (non-modifier) MIDI-key is
  pressed. When the timeout expires, current
  `emaccordion--pressed-and-held-keys' are processed. This timer allows for
  chords to be pressed, as the MIDI-events comprising a chord
  probably won't arrive simultaneously.")

(defun emaccordion--cancel-chord-expire-timer ()
  "Cancel the `emaccordion--chord-expire-timer'."
  (when emaccordion--chord-expire-timer
    (cancel-timer emaccordion--chord-expire-timer)
    (setf emaccordion--chord-expire-timer nil)))

(defun emaccordion--chord-expire-action ()
  "A function that's run when key or chord has been held down for
long enough to be triggered even if the key is not lifted off."
  (when (and emaccordion--repeat-key-initial-timeout emaccordion--repeat-key-timeout)
    (setf emaccordion--repeat-key-chord (cl-copy-list emaccordion--pressed-and-held-keys)))
  (emaccordion--process-k-p-and-clear)
  (emaccordion--restart-repeat-key-timer))

(defun emaccordion--restart-chord-expire-timer ()
  "Restart the timer `emaccordion--chord-expire-timer'. Means
that the timer will be cancelled and created again."
  (emaccordion--cancel-chord-expire-timer)
  (setf emaccordion--chord-expire-timer
	(run-at-time emaccordion-chord-expire-timeout
		     nil 'emaccordion--chord-expire-action)))

;;; ----------------------------------------------------------------------------
;;; Repeat key functionality
(defvar emaccordion--repeat-key-chord (list)
  "A list of keys that the repeat-key-timer will process
  repeatedly while the keys are held pressed. Same format as
  `emaccordion--pressed-and-held-keys'.")

(defvar emaccordion--repeat-key-timer nil
  "A timer that repeats the chord `emaccordion--repeat-key-chord'. When the
  timeout expires, current `emaccordion--repeat-key-chord' is
  processed and the timer is rescheduled.")

(defun emaccordion--cancel-repeat-key-timer ()
  "Cancel the `emaccordion--repeat-key-timer'."
  (when emaccordion--repeat-key-timer
    (cancel-timer emaccordion--repeat-key-timer)
    (setf emaccordion--repeat-key-timer nil)))

(defun emaccordion--process-r-k ()
  "Shorthand for processing `emaccordion--repeat-key-chord'
against `emaccordion-chord-actions'."
  (emaccordion--perform-action-matching-midi-events
   emaccordion-chord-actions
   emaccordion--repeat-key-chord
   emaccordion-upcase-velocity-threshold))

(defun emaccordion--restart-repeat-key-timer ()
  "Restart the timer `emaccordion--repeat-key-timer'. Means that
the timer will be cancelled and created again."
  (emaccordion--cancel-repeat-key-timer)
  (when (and emaccordion--repeat-key-initial-timeout emaccordion--repeat-key-timeout)
    (setf emaccordion--repeat-key-timer
	  (run-at-time emaccordion--repeat-key-initial-timeout
		       emaccordion--repeat-key-timeout
		       #'emaccordion--process-r-k))))

;;; ----------------------------------------------------------------------------
;;; Regular event related code
(defun emaccordion--register-midi-key-pressed (midi-event-inst)
  "Update `emaccordion--pressed-and-held-keys' with
MIDI-EVENT-INST (instance of emaccordion--midi-event)."
  (cl-pushnew midi-event-inst emaccordion--pressed-and-held-keys
              :test #'emaccordion--midi-event-key-chan-eq-p))

(defun emaccordion--press-action-handler (e)
  "Handle a MIDI note-on event E (instance of
emaccordion--midi-event). If the E matches a modifier binding, it
won't be registered for regular event machinery since modifier
keys can't be used in chords. Each new arriving keypress MIDI
event restarts the `emaccordion--modifier-clear-timer' and each
non-modifier MIDI event restarts the
`emaccordion--chord-expire-timer'."
  (let ((modval-bound-to-midi-event
         (emaccordion--midi-event-matching-modval e)))
    (if modval-bound-to-midi-event
        ;; Modifier
        (progn
          (emaccordion--set-modifier modval-bound-to-midi-event)
          (emaccordion--report-action
           (emaccordion--midi-event-list-to-chord (list e))
           (cl-prin1-to-string (emaccordion--modval-to-kwd modval-bound-to-midi-event))
           "modifier"))
      ;; Non-modifier (i.e. "action") keypress.
      (emaccordion--cancel-repeat-key-timer)
      (setf emaccordion--repeat-key-chord nil)
      (emaccordion--register-midi-key-pressed e)
      (emaccordion--restart-chord-expire-timer))
    (emaccordion--restart-modifier-timer)))

(defun emaccordion--depress-action-handler (e)
  "Handle a MIDI note-off event E (instance of
emaccordion--midi-event)."
  (let ((modval-bound-to-midi-event
         (emaccordion--midi-event-matching-modval e)))
    (if modval-bound-to-midi-event
        ;; Modifier
	(emaccordion--clear-modifiers modval-bound-to-midi-event)
      ;; If depress wasn't a modifier, process all pressed and held
      ;; keys (which may have been cleared by previous depress
      ;; already), deal with them as chord. Then clear the pressed and
      ;; held keys list, as they were just processed.
      (emaccordion--cancel-chord-expire-timer)
      (when (cl-find e emaccordion--repeat-key-chord
                     :test #'emaccordion--midi-event-key-chan-eq-p)
	;; If the lifted off key was in repeat, stop the repeat.
	(emaccordion--cancel-repeat-key-timer)
	(setf emaccordion--repeat-key-chord nil))
      (emaccordion--process-k-p-and-clear))))

(defun emaccordion--handle-midi-events (events)
  "Handle the midi events EVENTS, which is a list of
emaccordion--midi-event."
  (dolist (e events)
    (if (> (emaccordion--midi-event-velocity e) 0)
        (emaccordion--press-action-handler e)
      (emaccordion--depress-action-handler e))))

;;; ----------------------------------------------------------------------------
;;; MIDI device handling - process and filter function
(defvar emaccordion--unhandled-octets
  nil
  "The octets that haven't been yet handled. Array.")

(defun emaccordion--midi-filter-function (process midi-data-as-string)
  "Handle incoming MIDI data.

Filters the incoming MIDI-DATA-AS-STRING from PROCESS and
transforms the raw bytes to MIDI events.

Then operates on these events."
  (ignore process)
  (cl-multiple-value-bind (events unhandled)
      (emaccordion--create-midi-events-from-octets
       (vconcat emaccordion--unhandled-octets midi-data-as-string))
    (setf emaccordion--unhandled-octets unhandled)
    (when events
      (emaccordion--handle-midi-events events))))

(defun emaccordion-create-midi-reading-process ()
  "Create and return a process for reading midi character special
device."
  (let ((proc (make-serial-process :port emaccordion-midi-device-pathname
                                   :speed nil
                                   :coding '(raw-text . raw-text))))
    (set-process-filter proc #'emaccordion--midi-filter-function)
    proc))

;;; ----------------------------------------------------------------------
;;; Config Examples
(defun set-mappings-typewriter ()
  "Make a Chromatic Button Accordion's right-hand-side (C-griff)
behave nearly like a typewriter."
  (setf emaccordion-chord-actions
        (emaccordion-generate-chord-action-config
         '(:chord (0 . "a3") :action ? )
         '(:chord (0 . "g#6") :action paredit-backward-delete)
         '(:chord (0 . "a#3") :action ?q)
         '(:chord (0 . "c#4") :action ?w)
         '(:chord (0 . "e4") :action ?e)
         '(:chord (0 . "g4") :action ?r)
         '(:chord (0 . "a#4") :action ?t)
         '(:chord (0 . "c#5") :action ?y)
         '(:chord (0 . "e5") :action ?u)
         '(:chord (0 . "g5") :action ?i)
         '(:chord (0 . "a#5") :action ?o)
         '(:chord (0 . "c#6") :action ?p)
         '(:chord (0 . "e6") :action ?å)
         '(:chord (0 . "g6") :action ?^)
         '(:chord (0 . "a#6") :action newline)
         '(:chord (0 . "c4") :action ?a)
         '(:chord (0 . "d#4") :action ?s)
         '(:chord (0 . "f#4") :action ?d)
         '(:chord (0 . "a4") :action ?f)
         '(:chord (0 . "c5") :action ?g)
         '(:chord (0 . "d#5") :action ?h)
         '(:chord (0 . "f#5") :action ?j)
         '(:chord (0 . "a5") :action ?k)
         '(:chord (0 . "c6") :action ?l)
         '(:chord (0 . "d#6") :action ?ö)
         '(:chord (0 . "f#6") :action ?ä)
         '(:chord (0 . "a6") :action ?')
         '(:chord (0 . "c7") :action newline)
         '(:chord (0 . "d4") :action ?z)
         '(:chord (0 . "f4") :action ?x)
         '(:chord (0 . "g#4") :action ?c)
         '(:chord (0 . "b4") :action ?v)
         '(:chord (0 . "d5") :action ?b)
         '(:chord (0 . "f5") :action ?n)
         '(:chord (0 . "g#5") :action ?m)
         '(:chord (0 . "b5") :action ?,)
         '(:chord (0 . "d6") :action ?.)
         '(:chord (0 . "f6") :action ?-)))
  (setf emaccordion-note-to-modifier
        (emaccordion-generate-modifier-config
         '(:note (0 . "b3") :modifier :shift)
         '(:note (0 . "g#3") :modifier :ctrl))))
