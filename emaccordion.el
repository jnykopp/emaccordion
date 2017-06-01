;;;; Copyright 2017 Janne Nykopp

;;;; emaccordion.el

;;;;    Emaccordion is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    Notewhacker is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with Notewhacker.  If not, see <http://www.gnu.org/licenses/>.


;;; Work in progress...
;;;
;;; See version history at the end of this file.

(require 'cl)

;;; Some helpers for configuration.
(defvar *mknm-to-letter-name-accidental*
  '((c . nil) (c . s) (d . nil) (d . s) (e . nil) (f . nil) (f . s)
    (g . nil) (g . s) (a . nil) (a . s) (b . nil))
  "Map a MKNm to letter name and accidental (on C-Major
  scale). Intended for use with `spn-to-mkn'.")

(defun spn-to-mkn (spn)
    "Convert scientific pitch notation SPN (string of key, octave
and accidental) to midi key number. Note! Only sharp accidentals
are recognized at the moment."
    (if (string-match (rx bol (group (in "a-gA-G")) (group (? (in "#"))) (group digit) eol)
		      spn)
	(let* ((name (intern (match-string 1 spn)))
	       (octave (string-to-int (match-string 3 spn)))
	       (accidental (if (string= "" (match-string 2 spn)) nil 's))
	       (mknm (position (cons name accidental)
			       *mknm-to-letter-name-accidental*
			       :test #'equalp)))
	  (if mknm
	      (+ mknm (* (1+ octave) 12))
	    (error "Invalid Scientific Pitch Notation: %s" spn)))
      (error "Invalid Scientific Pitch Notation: %s" spn)))

(defun compare-chan-mkns (x y)
  "Compare X and Y which are cons of channel and midi-key-number."
  (if (= (car x) (car y)) (< (cdr x) (cdr y)) (< (car x) (car y))))

(defun sort-chan-mkn-list (l)
  "Return a sorted copy of the list L consisting of conses of
  channel and midi-key-number."
  (sort (copy-seq  l) #'compare-chan-mkns))

(defun find-chord-from-chord-mapping-list (c l)
  "Find a chord C (sorted list of conses of channel and
midi-key-number) from a list L like `*chord-actions*'"
  (find c l :key #'car :test #'equalp))

;;; --------------------------------------------------------------------
;;; Configuration

(defun generate-chan-spn-list (&rest chan-spn-mod-list)
  "Helper function to create a list of mappings from channel and
scientific pitch notation to modifier, used in
`*midi-to-mod-map*'."
  (let (result)
    (dolist (chan-spn-mod chan-spn-mod-list (reverse result))
      (destructuring-bind (chan spn mod) chan-spn-mod
	(push (cons (cons chan (spn-to-mkn spn)) (modifier-kwd-to-val mod)) result)))))

(defvar *midi-to-mod-map*
  (generate-chan-spn-list
   '(0 "b3" :ctrl)
   '(0 "f3" :meta)
   '(0 "a3" :shift))
  "A list of `chord-to-action' instances.")

;;; Different actions on midi chords:
;;;
;;; a) mimic `self-insert-command' by pushing to
;;; `unread-command-events', or
;;;
;;; b) execute a command, or
;;;
;;; c) run a keyboard macro
;;;
;;; In case of a) the action in `*chord-actions*' should be something
;;; that satisfies `characterp'. In case of b) the action should be
;;; something that satisfies `commandp'. In case of c) the action
;;; should be a vector or a string - see `execute-kbd-macro'.
(defun generate-chan-chord-action-config (&rest configs)
  "Generate a `*chord-actions*' mapping out of given CONFIGS. The
syntax of CONFIGS is (:chord ((chan . key-as-spn) ...) :action
the-action)"
  (cl-flet ((chord-as-proper-list (c) (if (listp (car c)) c (list c)))
	 (chan-spn-to-chan-mkn (e) (cons (car e) (spn-to-mkn (cdr e)))))
    (let ((ch-act-mapping))
      (dolist (config configs ch-act-mapping)
	(let* ((entry-as-list (chord-as-proper-list (getf config :chord)))
	       (chord (sort-chan-mkn-list (mapcar #'chan-spn-to-chan-mkn entry-as-list)))
	       (action (getf config :action))
	       (prev-entry (cdr (find-chord-from-chord-mapping-list chord ch-act-mapping))))
	  (when prev-entry (error "Entry already exists for %s" entry-as-list))
	  (unless (and chord action) (error "Invalid entry %s" config))
	  (push (cons chord action) ch-act-mapping))))))

;; (defvar *chord-actions*
;;   (generate-chan-chord-action-config
;;    '(:chord ((0 . "c5") (0 . "b5")) :action ?a)
;;    '(:chord (0 . "e4") :action "test")
;;    '(:chord (0 . "d4") :action [?a ?b ?c])
;;    '(:chord (0 . "c4") :action emacs-uptime))
;;   "Database of actions to take on given chords. A list containing
;;   lists of [conses of channel and key combinations] and the
;;   action. The channel-key sequences should be kept sorted by
;;   channel first, then key, for searchability.")

;;; For accordion test
(defvar *chord-actions*
  (generate-chan-chord-action-config
   '(:chord (0 . "c4")  :action ?a)
   '(:chord (0 . "c#4") :action ?b)
   '(:chord (0 . "d4")  :action ?c)
   '(:chord (0 . "d#4") :action ?d)
   '(:chord (0 . "e4")  :action ?e)
   '(:chord (0 . "f4")  :action ?f)
   '(:chord (0 . "f#4") :action ?g)
   '(:chord (0 . "g4")  :action ?h)
   '(:chord (0 . "g#4") :action ?i)
   '(:chord (0 . "a4")  :action ?j)
   '(:chord (0 . "a#4") :action ?k)
   '(:chord (0 . "b4")  :action ?l)
   '(:chord (0 . "c5")  :action ?m)
   '(:chord (0 . "c#5") :action ?n)
   '(:chord (0 . "d5")  :action ?o)
   '(:chord (0 . "d#5") :action ?p)
   '(:chord (0 . "e5")  :action ?q)
   '(:chord (0 . "f5")  :action ?r)
   '(:chord (0 . "f#5") :action ?s)
   '(:chord (0 . "g5")  :action ?t)
   '(:chord (0 . "g#5") :action ?u)
   '(:chord (0 . "a5")  :action ?v)
   '(:chord (0 . "a#5") :action ?w)
   '(:chord (0 . "b5")  :action ?x)
   '(:chord (0 . "c6")  :action ?y)
   '(:chord (0 . "c#6")  :action ?z)
   '(:chord (0 . "d6") :action emacs-uptime)
   '(:chord ((0 . "c4") (0 . "d4")) :action ?✈))
  "Database of actions to take on given chords. A list containing
  lists of [conses of channel and key combinations] and the
  action. The channel-key sequences should be kept sorted by
  channel first, then key, for searchability.")

;;; Configuration ends
;;; --------------------------------------------------------------------


;; (defvar *midi-device-pathname* (expand-file-name "~/test")
;;   "Read raw midi octets from this file.")

;;; For accordion test
(defvar *midi-device-pathname* (expand-file-name "/dev/midi1")
  "Read raw midi octets from this file.")

(defvar *raw-buffer-length* 256
  "How many octets the raw buffer will be long")

;;; Replace midi-octet-buffer with emacs' buffer.

(defalias 'multiple-value-bind 'destructuring-bind)

;; Any midi-event with velocity of 0 is considered a midi-note-off
;; event.
(defstruct midi-event
  (timestamp (current-time))
  channel
  key
  velocity)

(defun midi-status-octet-p (octet)
  "Check if OCTET is a midi status octet."
  (> octet #b1111111))

(defmacro* with-default-midi-data-checking ((num-of-args raw-data ptr) &body body)
  "Wrap BODY in code that tests that midi data from RAW-DATA starting
  from index PTR seems to be in valid (right number of argument octets
  etc.). If it's not, a suitable value will be returned."
  (let ((num-of-args-gs (gensym "num-of-args"))
        (raw-data-gs (gensym "raw-data"))
        (ptr-gs (gensym "ptr")))
    `(let ((,num-of-args-gs ,num-of-args)
           (,raw-data-gs ,raw-data)
           (,ptr-gs ,ptr))
       (if (< (- (length ,raw-data-gs) ,ptr-gs) (1+ ,num-of-args-gs))
           ;; Insufficient data. TODO: Log.
           (values nil ,ptr-gs)
         (let ((err-pos
                (position-if 'midi-status-octet-p ,raw-data-gs :start (1+ ,ptr-gs)
                             :end (+ ,ptr-gs 1 ,num-of-args-gs))))
           (if err-pos
               ;; Invalid data (status octet instead of data
               ;; octet). TODO: Log.
               (values nil err-pos)
             ;; Data was ok.
             (values ,@body (+ ,ptr-gs 1 ,num-of-args-gs))))))))

(defun gen-chan-handler-ignoring-n-args (num-of-args)
  "Creates a midi octet stream handler that just ignores status octet
  and NUM-OF-ARGS octets."
  (lambda (raw-data ptr)
    (with-default-midi-data-checking (num-of-args raw-data ptr)
      nil)))

(defun status-octet-channel-number (octet)
  "Get the channel number from a status octet OCTET."
  (logand octet #b00001111))

(defun note-generic-handler (raw-data ptr note-on-p)
  "Generic handler for note midi message. Create the event from
RAW-DATA, offset by PTR. If NOTE-ON-P is non-nil and velocity is
greater than 0, the instance will be note-on. (Some midi devices
send note-on with velocity of 0 instead of note-off.)

Return values consisting of the instance and number of octets
handled."
  (with-default-midi-data-checking (2 raw-data ptr)
    (let ((channel (status-octet-channel-number (aref raw-data ptr)))
          (key (aref raw-data (+ ptr 1)))
          (velocity (aref raw-data (+ ptr 2))))
      (declare (type (integer 0 127) key velocity)
               (type (integer 0 15) channel))
      (make-midi-event :channel channel :key key :velocity (if note-on-p velocity 0)))))

(defun note-on-handler (raw-data ptr)
  "Create and return a note-on or note-off event. (Some midi devices
  send note-on with velocity of 0 instead of note-off.)"
  (note-generic-handler raw-data ptr t))

(defun note-off-handler (raw-data ptr)
  "Create and return a note-off event."
  (note-generic-handler raw-data ptr nil))

(defun sys-common-handler (raw-data ptr)
  "Handle a system common message. These come in various forms"
  (error "Sys-common-handler not yet implemented (data: ~a ~a)" raw-data ptr))

(defun sys-real-time-handler (raw-data ptr)
  "Handle a system real time message. Right now, they are ignored."
;  (declare (ignore raw-data))
  (values nil (1+ ptr)))

(defvar *midi-channel-status-handler-mapper*
  `((#b1000 . note-off-handler)                      ; Note on event
    (#b1001 . note-on-handler)                       ; Note off event
    (#b1010 . ,(gen-chan-handler-ignoring-n-args 2))  ; Polyph. Key Pressure
    (#b1011 . ,(gen-chan-handler-ignoring-n-args 2))  ; Control Change
    (#b1100 . ,(gen-chan-handler-ignoring-n-args 1))  ; Program Change
    (#b1101 . ,(gen-chan-handler-ignoring-n-args 1))  ; Channel Pressure
    (#b1110 . ,(gen-chan-handler-ignoring-n-args 2))) ; Pitch Wheel Change
  "Map a status octete of a midi event with channel information by its
  most-significant-nibble to a handler function.")

(defvar *midi-system-status-handler-mapper*
  '((#b11110 . sys-common-handler)
    (#b11111 . sys-real-time-handler))
  "Map a status octet of a system status midi event by its 5 MSBs to a
  handler function.")

(defun get-handler-for (status-octet)
  "Get a handler according to the STATUS-OCTET."
  (or (cdr (assoc (ash status-octet -4) *midi-channel-status-handler-mapper*))
      (cdr (assoc (ash status-octet -3) *midi-system-status-handler-mapper*))))

(defun octets-to-midi-event (raw-data ptr)
  "Create one midi-event from RAW-DATA. If the RAW-DATA is
  insufficient, return values nil and PTR. If RAW-DATA is invalid,
  return values nil and PTR + number of octets so far handled. If
  RAW-DATA defines an event, return an event instance and PTR + number
  of octets handled. The event instance returned may be nil, if the
  event was to be ignored."
  (let ((handler (get-handler-for (aref raw-data ptr))))
    ;(message "handler for %s is %s" (aref raw-data ptr) handler)
    (funcall handler raw-data ptr)))

(defun create-midi-events-from-octets (raw-data)
  "Create new midi events from raw octet data RAW-DATA (vector or
nil). Raw data might contain incomplete event in the end. Returns
multiple values: a list of events, ordered by sequence of arrival, and
a vector containing remaining unhandled handled octets in the case
there was an incomplete event at the end. Malformed data is ignored."
  (if (and raw-data (> (length raw-data) 0))
      (let* ((data-ptr
              (position-if 'midi-status-octet-p raw-data)) ; Fast-forward to status octet.
             (data-left (not (null data-ptr)))
             events)
        (loop while (and data-left (< data-ptr (length raw-data)))
           do (multiple-value-bind (event new-ptr)
                   (octets-to-midi-event raw-data data-ptr)
                 (when event
                   (push event events))
                 (if (= new-ptr data-ptr)
                     ;; The event remaining was incomplete.
                     (setf data-left nil)
                     ;; Again fast-forward to next status octet if such is
                     ;; found. Ignore anything else, since it must be
                     ;; rubbish. TODO: Log.
                     (setf data-ptr
                           (or (position-if 'midi-status-octet-p raw-data :start new-ptr)
                               new-ptr)))))
        (if data-ptr
            (values (reverse events) (subseq raw-data data-ptr))
	  (values nil nil)))))


;;; Emacs-specific

(defvar *unhandled-octets*
  nil
  "The octets that haven't been yet handled. Array.")

(defvar *unhandled-midi-events*
  nil
  "Midi events that haven't been yet handled. Array.")

(defvar *pressed-and-held-keys*
  (list)
  "Keys (cons of midi channel and key number) that are currently
  pressed and held.")

(defun add-command-event (event)
  "Add an event) to `unread-command-events' list. Note: the idea
  is that command loop (perhaps read_event_from_main_queue in
  keyboard.c) runs so fast that the order of events in the list
  shouldn't matter."
  (push event unread-command-events))

(defun decorate-with-modifiers (character-action modifiers)
  "Modify the given CHARACTER-ACTION, being a character like one
pressed on keyboard, to equivalent character for which the
MODIFIERS (a list like `*pressed-and-held-modifiers*') are
applied. Return the new action character."
  ;; Control and shift are currently the only "special" cases. When
  ;; `extra-keyboard-modifier' has shift in it, it doesn't affect
  ;; `add-command-event' so manually upcase everything (is this
  ;; right?). For control, see src/keyboard.c. Only do it for certain
  ;; character range; see src/keyboard.c function
  ;; "make_ctrl_char". This (string-to-char (kbd (format "C-%s"
  ;; (char-to-string ...)))) is ugly ugly ugly way to create the
  ;; control sequence, but is there anything better (portability and
  ;; maintenance wise)?!
  (cl-flet ((conv-to-ctrl (ch) (when (or (and (>= ch ?A) (<= ch ?Z)) (and (>= ch ?a) (<= ch ?z)))
                                 (string-to-char (kbd (format "C-%s" (char-to-string ch))))))
            (conv-to-shift (ch) (upcase ch)))
    (macrolet ((handle-modifier (mod char fun)  ;TODO: UGLY!!
	          `(let ((mod-val (modifier-kwd-to-val ,mod))) ;TODO: Unnecessary call if mod vals are improved
		     (if (find mod-val modifiers)
                         (let ((v (,fun ,char)))
                           (if v 
                               (progn (setf modifiers (delete mod-val modifiers)) v)
                             ,char))
		       ,char))))
      (let* ((maybe-s-char (handle-modifier :shift character-action conv-to-shift))
	     (maybe-s-c-char (handle-modifier :ctrl  maybe-s-char conv-to-ctrl)))
	;; Just logior the rest to the possibly ctrl'd or shifted char.
	(apply #'logior maybe-s-c-char modifiers)))))

(defun perform-action (action)
  "Do the ACTION."
  ;; TODO! add-command-event doesn't take into consideration the
  ;; extra-keyboard-modifiers! FINISH decorate-with-modifiers!
  (cond
   ((characterp action) (add-command-event (decorate-with-modifiers action *pressed-and-held-modifiers*)))
   ((and (functionp action) (commandp action)) (call-interactively action))  ;or command-execute?
   ((commandp action) (execute-kbd-macro action))))

(defun perform-actions-matching-keys (action-list pressed-key-list)
  "Process the given PRESSED-KEY-LIST (like `*pressed-and-held-keys*')
and launch specified actions in ACTION-LIST (like
`*chord-actions*') according to that."
  (let ((action (cdr (find-chord-from-chord-mapping-list
		      (sort-chan-mkn-list pressed-key-list) action-list))))
    (if action
	  (perform-action action)
	(ding) (message "Emaccordion: No action for %s" pressed-key-list))))

(defun process-pressed-and-held-keys ()
  "Make emacs jump hoops according to current
`*pressed-and-held-keys*'"
  (when *pressed-and-held-keys*
    ;; Only react if something is "pressed" at the moment (could be,
    ;; that depress cleared all pressed keys already etc.)
    (perform-actions-matching-keys *chord-actions* *pressed-and-held-keys*)))

(defun process-pressed-and-held-keys-and-clear ()
  "Like `process-pressed-and-held-keys' but also clear 
 `*pressed-and-held-keys*'"
  (when *pressed-and-held-keys*
    (process-pressed-and-held-keys)
    (setf *pressed-and-held-keys* (list))))

;; When midi key is pressed, call a press-action-handler, which is
;; responsible for keeping a list of pressed-and-held-midi-keys and
;; pressed-and-held-modifiers. When midi key is depressed, call a
;; depress-action-handler, which is responsibile to clearing those
;; same lists accordingly.
;;
;; In this fashion, it is possible to use the mechanism to mimic
;; modifier keys - see `extra-keyboard-modifiers' (which aren't
;; cleared until the corresponding depress event comes in) and
;; event-triggering chords, after which all chording-related pressed
;; keys should be cleared.
;;
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
;; Using the basic modifier codes for even control and ascii
;; characters should work fine. Emaccordion only unsets modifiers it
;; has itself set (of course it can't know if someone else also wanted
;; to set the exact same code so it'll happily unset the modifier in
;; that case), so any time emaccordion sets e.g. the control modifier
;; it will unset it with the same code 0x4000000 - there might be
;; still other control modifiers in the `extra-keyboard-modifiers'.

(defvar *pressed-and-held-modifiers* (list)
  "List of pressed and held modifiers. Contains the values from
  `*modifier-value-list*'")

;;; TODO: This is stupid. Give values to each modifier directly so no
;;; lookups need to be done in code.
(defvar *modifier-value-list*
  '(:alt #x0400000
    :super #x0800000
    :hyper #x1000000
    :shift #x2000000
    :ctrl  #x4000000
    :meta  #x8000000)
  "List of modifier name and value.")

(defun modifier-kwd-to-val (keyword)
  "Helper function. Convert a modifier keyword to a value."
  (or (getf *modifier-value-list* keyword)
      (error "Not a valid modifier keyword: %s" keyword)))

(defun get-midi-event-bound-modifier (midi-event-inst)
  "Find a matching modifier key for MIDI-EVENT-INST from
`*midi-to-mod-map*'."
  (cdr (assoc (cons (midi-event-channel midi-event-inst)
		    (midi-event-key midi-event-inst))
	      *midi-to-mod-map*)))

(defvar *modifier-clear-timeout* 30.0
  "How long to wait in seconds for modifier key release event
  before automatically clearing the modifiers that have been
  set.")

(defvar *modifier-clear-timer* nil
  "A timer for clearing up modifiers, in case a key release event
  never arrives.")

(defun cancel-modifier-timer ()
  "Cancel the timer."
  (when *modifier-clear-timer*
    (cancel-timer *modifier-clear-timer*)
    (setf *modifier-clear-timer* nil)))

(defun refresh-modifier-timer ()
  "Refresh the timer `*modifier-clear-timer*'"
  (when *pressed-and-held-modifiers*
    ;; Do only when there's a modifier pressed
    (cancel-modifier-timer)
    (setf *modifier-clear-timer* (run-at-time *modifier-clear-timeout* nil 'clear-all-modifiers))))

(defun clear-all-modifiers ()
  "Clear all of the modifiers."
  (apply 'clear-modifiers (loop for x in (cdr *modifier-value-list*) by 'cddr
				collect x)))

(defun clear-modifiers (&rest modifiers)
  "Clear the given MODIFIERS (integer values, not modifier keywords)."
  (setf extra-keyboard-modifiers (logand extra-keyboard-modifiers (lognot (apply 'logior modifiers))))
  (setf *pressed-and-held-modifiers* (set-difference *pressed-and-held-modifiers* modifiers))
  (unless *pressed-and-held-modifiers*
    ;; All cleared? No need for the timer any more.
    (cancel-modifier-timer)))

(defun set-modifier (modifier)
  "Set the modifier in use. This also starts a timer that
eventually cancels the modifiers emaccordion has set (in case the
note off never arrives).

Only sets modifiers that aren't already set by Emaccordion."
  (pushnew modifier *pressed-and-held-modifiers*)
  (setf extra-keyboard-modifiers (logior extra-keyboard-modifiers modifier))
  (refresh-modifier-timer))

(defvar *keypress-expire-timeout* 0.015
  "The expire timeout for `*keypress-expire-timer*' in
  seconds. Should be short enough not to induce noticeable lag or
  erroneous chords in typing, but long enough to allow for
  chords.")

(defvar *keypress-expire-timer* nil
  "A timer that is set when a (non-modifier) midi-key is
  pressed. When the timeout expires, current
  `*pressed-and-held-keys*' are processed. This timer allows for
  chords to be pressed, as the midi-events comprising a chord
  probably won't arrive simultaneously.")

(defun cancel-keypress-expire-timer ()
  "Cancel the `*keypress-expire-timer*'."
  (when *keypress-expire-timer*
    (cancel-timer *keypress-expire-timer*)
    (setf *keypress-expire-timer* nil)))

(defun keypress-expire-action ()
  "A function that's run when key has been held down for long
enough to be triggered even if the key is not lifted off."
  (when (and *repeat-key-initial-timeout* *repeat-key-timeout*)
    (setf *repeat-key-chord* (copy-list *pressed-and-held-keys*)))
  (process-pressed-and-held-keys-and-clear)
  (refresh-repeat-key-timer))

(defun refresh-keypress-expire-timer ()
  "Refresh the timer `*keypress-expire-timer*'"
  (cancel-keypress-expire-timer)
  (setf *keypress-expire-timer*
	(run-at-time *keypress-expire-timeout*
		     nil 'keypress-expire-action)))

(defvar *repeat-key-chord* (list)
  "A list of keys that the repeat-key-timer will process
  repeatedly while the keys are held pressed. Same format as
  `*pressed-and-held-keys*'.")

(defvar *repeat-key-initial-timeout* 1
  "The expire timeout for `*repeat-key-timer*' in
  seconds. Defines the pause between pressing a key or chord
  `*repeat-key-chord*' and starting to repeat it. If nil, do not
  use repeat at all.")

(defvar *repeat-key-timeout* 0.1
  "The expire timeout for `*repeat-key-timer*' in
  seconds. Defines the repeat rate of a pressed and held key or
  chord `*repeat-key-chord*'. If nil, do not use repeat at all.")

(defvar *repeat-key-timer* nil
  "A timer that repeats the chord `*repeat-key-chord*'. When the
  timeout expires, current `*repeat-key-chord*' is
  processed and the timer is rescheduled.")

(defun cancel-repeat-key-timer ()
  "Cancel the `*repeat-key-timer*'."
  (when *repeat-key-timer*
    (cancel-timer *repeat-key-timer*)
    (setf *repeat-key-timer* nil)))

(defun process-repeat-keys ()
  "Repeat `*repeat-key-chord*'"
  (when *repeat-key-chord*
    ;; Only react if something needs to be repeated at the moment (could be,
    ;; that depress cleared this already etc.)
    (perform-actions-matching-keys *chord-actions* *repeat-key-chord*)))

(defun refresh-repeat-key-timer ()
  "Refresh the timer `*repeat-key-timer*'."
  (cancel-repeat-key-timer)
  (when (and *repeat-key-initial-timeout* *repeat-key-timeout*)
    (setf *repeat-key-timer*
	  (run-at-time *repeat-key-initial-timeout*
		       *repeat-key-timeout*
		       'process-repeat-keys))))

(defun midi-event-to-keyentry (midi-event-inst)
  "Convert MIDI-EVENT-INST to the format expected by e.g. `*pressed-and-held-keys*'"
  (cons (midi-event-channel midi-event-inst) (midi-event-key midi-event-inst)))

(defun register-midi-key-pressed (midi-event-inst)
  "Update `*pressed-and-held-keys*' with keypress pertaining to
the MIDI-EVENT-INST."
  (pushnew (midi-event-to-keyentry midi-event-inst)
	   *pressed-and-held-keys* :test #'equalp))

(defun press-action-handler (midi-event-inst)
  "Handle a MIDI-EVENT-INST of pressed key. If the
MIDI-EVENT-INST matches a modifier binding, it won't be
registered otherwise - modifier keys can't be used in
chords. Each new arriving keypress midi event restarts the
`*modifier-clear-timer*' and each non-modifier midi event
restarts the `*keypress-expire-timer*'."
  (let ((modifier-bound-to-midi-event (get-midi-event-bound-modifier midi-event-inst)))
    (if modifier-bound-to-midi-event
        (progn (message "setting modifier %s" modifier-bound-to-midi-event)
               (set-modifier modifier-bound-to-midi-event))
      ;; Non-modifier (i.e. "action") keypress.
      (cancel-repeat-key-timer)
      (setf *repeat-key-chord* nil)
      (register-midi-key-pressed midi-event-inst)
      (refresh-keypress-expire-timer))
    (refresh-modifier-timer)))

(defun depress-action-handler (midi-event-inst)
  "Handle a depress midi KEY coming from a certain midi CHANNEL."
  ;; Handle note off. Clear all pressed and held keys, deal with them
  ;; as chords.
  (let ((modifier-bound-to-midi-event (get-midi-event-bound-modifier midi-event-inst)))
    (if modifier-bound-to-midi-event
	(clear-modifiers modifier-bound-to-midi-event)
      ;; If depress wasn't a modifier, process all pressed and held
      ;; keys (which may have been cleared by previous depress
      ;; already), deal with them as chord. Then clear the pressed and
      ;; held keys list, as they were just processed.
      (cancel-keypress-expire-timer)	;Expire timer is no longer needed.
      (when (find (midi-event-to-keyentry midi-event-inst) *repeat-key-chord*
		  :test 'equal)
	;; If the lifted off key was in repeat, stop the repeat.
	(cancel-repeat-key-timer)
	(setf *repeat-key-chord* nil))
      (process-pressed-and-held-keys-and-clear))))

(defun handle-midi-events (events)
  "Handle the midi events EVENTS."
  (loop for event in events
	do (if (> (midi-event-velocity event) 0)
	       ;; Handle note on.
	       (press-action-handler event)
	     ;; Handle note off.
	     (depress-action-handler event))))

(defun midi-filter-function (process midi-data-as-string)
  "Handle incoming Midi data.
  
  Filters the incoming MIDI-DATA-AS-STRING from PROCESS and
  transforms the raw bytes to Midi events.
  
  Then operates on these events."
  ;(message "data is %s" (vconcat *unhandled-octets* midi-data-as-string))
  (multiple-value-bind (events unhandled)
      (create-midi-events-from-octets
       (vconcat *unhandled-octets* midi-data-as-string))
    (setf *unhandled-octets* unhandled)
    (when events
      (message "got events %s" events)
      (handle-midi-events events))
    ;(setf *unhandled-midi-events* (vconcat *unhandled-midi-events* events))
    ))

(defstruct midi-keypress-status
  "Midi keypress or chord press status. State is either on (still
    (at least one key) pressed) or off (not pressed any
    more). Triggered is a timestamp of last trigger event."
  note-numbers
  triggered)

(defun create-midi-reading-process ()
  "Create and return a process for reading midi character special
device."
  (let ((proc (make-serial-process :port *midi-device-pathname* :speed nil
                                   :coding '(raw-text . raw-text))))
    (get-buffer-create "*cleartext-midi*")
    (set-process-filter proc 'midi-filter-function)
    proc))

;;; Short version history. This version is the "3rd version".

;;; 1st version: No timers.
;;; 
;;; Each midi-off triggers all currently pressed keys as chord,
;;; clearing list of pressed and held keys. Midi-on adds the key to
;;; list of pressed and held keys.

;;; 2nd version: Timer for keypress, but no repeat.
;;; 
;;; Each incoming midi-on triggers/resets timer T1. When timer expires
;;; or midi-off encountered, pressed and held keys are processed and
;;; then cleared and T1 is stopped.

;;; 3rd version: Keypress and repeat timers. More like a real
;;; keyboard?
;;;
;;; Like 2nd version but with additional repetition timer T2. Each
;;; midi-on resets T1 and stops T2. Each midi-off triggers processing
;;; of pressed and held keys IF T1 HAS NOT BEEN RUN FOR THIS SET OF
;;; KEYS and clears the pressed and held keys, and resets T1 and stops
;;; T2. When T1 runs, it marks it's been run FOR THIS SET OF KEYS and
;;; also triggers processing pressed and held keys, but instead of
;;; clearing them it leaves them intact and starts repetition timer
;;; T2. When T2 runs, it processes the pressed and held keys but
;;; doesn't modify them and reschedules itself.
