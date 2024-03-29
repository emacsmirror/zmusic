;;; zmusic.el --- Write repetitive electronic music -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Zachary Kanfer

;; Author: Zachary Kanfer <zkanfer@gmail.com>
;; Version: 0.1
;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;; Homepage: https://hg.sr.ht/~zck/zmusic
;; Keywords: multimedia

;; Package-Requires: ((emacs "26.1"))

;;; Commentary:
;; This library plays repetitive digital music.
;; Whether this is good is up to the viewer.

;;; Code:
(require 'seq)
(require 'cl-lib)
(require 'rx)

;;can specify hex numbers with #x123456789abcdef0, So #x10 is 16

(defconst hex-lookup-table
  (let ((table (make-hash-table :size 16)))
    (puthash ?0 0 table)
    (puthash ?1 1 table)
    (puthash ?2 2 table)
    (puthash ?3 3 table)
    (puthash ?4 4 table)
    (puthash ?5 5 table)
    (puthash ?6 6 table)
    (puthash ?7 7 table)
    (puthash ?8 8 table)
    (puthash ?9 9 table)
    (puthash ?A 10 table)
    (puthash ?B 11 table)
    (puthash ?C 12 table)
    (puthash ?D 13 table)
    (puthash ?E 14 table)
    (puthash ?F 15 table)
    table)
  "A map of hex characters to the integer value.")

(defconst quad-to-hex-table
  (let ((table (make-hash-table :size 16)))
    (puthash 0 ?0 table)
    (puthash 1 ?1 table)
    (puthash 2 ?2 table)
    (puthash 3 ?3 table)
    (puthash 4 ?4 table)
    (puthash 5 ?5 table)
    (puthash 6 ?6 table)
    (puthash 7 ?7 table)
    (puthash 8 ?8 table)
    (puthash 9 ?9 table)
    (puthash 10 ?A table)
    (puthash 11 ?B table)
    (puthash 12 ?C table)
    (puthash 13 ?D table)
    (puthash 14 ?E table)
    (puthash 15 ?F table)
    table)
  "A table that maps the four-bit value to a hex digit.")

(defun hex-digit-to-number (val)
  "Convert VAL from a hex digit (as a character or integer) to a number."
  (gethash (upcase val) hex-lookup-table 0))

(defun hex-digits-to-byte (digit1 digit2)
  "Convert the hex DIGIT1 and DIGIT2 to a single byte value."
  (+ (* 16 (hex-digit-to-number digit1))
     (hex-digit-to-number digit2)))

(defun hex-string-to-number (val)
  "Convert VAL, a string of hex digits, to a number.

\"FF\" becomes 255."

  (cl-reduce
   (lambda (accum next-digit)
     (+ (* accum 16)
        next-digit))
   (mapcar #'hex-digit-to-number val)
   :initial-value 0))

(defun four-bits-to-hex (four-bit-integer)
  "Convert a FOUR-BIT-INTEGER to a hex character."
  (gethash four-bit-integer quad-to-hex-table ?0))

(defun byte-to-hexes (byte)
  "Convert BYTE to a hex string."
  (format "%c%c"
          (four-bits-to-hex (/ byte 16))
          (four-bits-to-hex (mod byte 16))))

;;zck strip spaces? Why shouldn't (value-to-bytes "10 FF") work?
(cl-defun value-to-bytes (value &optional (min-byte-count 1) (big-endian t))
  "Convert a VALUE to a sequence of bytes.

This value is either a hex string, an ASCII string-list, a list of
bytes, or a byte.

If MIN-BYTE-COUNT is given, pad out the value to be that many
bytes.  This only works for a single byte at the moment.  The final
value might have more bytes than this, if the raw value has more bytes
in it than MIN-BYTE-COUNT.

If BIG-ENDIAN is nil, then reverse the bytes of the padded value, so
the most significant byte is last.  Each byte still internally has the
most significant byte first.

\(value-to-bytes \"10FF\") => '(16 255)
\(value-to-bytes '(ASCII \"abc\")) => '(97 98 99)
\(value-to-bytes '(10 00 255)) => '(10 0 255)
\(value-to-bytes 81) => '(81)
\(value-to-bytes 513) => '(2 1)
\(value-to-bytes 81 4) => '(0 0 0 81)
\(value-to-bytes 81 4 nil) => '(81 0 0 0)
\(value-to-bytes '(b 81 4)\) is the same as (value-to-bytes 81 4).
\(value-to-bytes '(b 81 4 nil)\) is the same as (value-to-bytes 81 4 nil)."

  ;;string of hex digits -> two digits represents a byte.
  (cond ((stringp value)
         (cl-loop for index below (length value) by 2
                  collect (hex-digits-to-byte (elt value index)
                                              (elt value (1+ index)))))
        ((listp value)
         (cond ;;ASCII string
               ((equal (car value)
                       'ASCII)
                (append (cadr value) nil))

               ;;b-padded; input is '(b byte-value min-byte-count big-endian)
               ((equal (car value)
                       'b)
                (apply #'value-to-bytes
                       (seq-rest value)))

               ;;list of bytes
               ;;zck should this split into bytes?
               ((listp value)
                value)))

        ;;single value
        (t (setq value (truncate value))
           (let* ((bytes-remaining min-byte-count)
                  (reverse-bytes
                   (cl-loop while (or (> value 0)
                                      (> bytes-remaining 0))
                            collect (mod value 256) into reverse-bytes
                            do (setq value (/ value 256))
                            (cl-decf bytes-remaining)
                            finally return reverse-bytes)))
             (if big-endian
                 (nreverse reverse-bytes)
               reverse-bytes)))))

(defun values-to-bytes (&rest values)
  "Convert a sequence of VALUES to a list of bytes.

Each VALUE is either a hex string, an ASII list-string, a list of
bytes, or a byte.

A hex string is like \"1FB3\", and represents the bytes 1F B3.
An ASCII list-string is like '(ASCII \"a0C!\"), and represents the
bytes '(97 48 67 33), which are the ASCII values for the characters
a 0 C !"
  (seq-mapcat #'value-to-bytes values))

(cl-defun zmusic//wave-header (data-subchunk &optional (big-endian t))
  "Make a wave header, a list of bytes.

The DATA-SUBCHUNK is the actual subchunk containing the data.  It has
to be calculated before we get the header.

BIG-ENDIAN controlls the endianness of the bytes here.

This assumes we're using PCM."
  (values-to-bytes (list 'ASCII (if big-endian
                                    "RIFX"
                                  "RIFF"))

                    ;;size of file after this specific field. The whole file size - 8.
                   `(b ,(+ 4 ;;rest of header
                           24 ;;size of FMT chunk
                           (length data-subchunk))
                       4
                       big-endian)
                   '(ASCII "WAVE")))

(cl-defun zmusic//fmt-subchunk (sample-rate &key (number-of-channels 2) (bytes-per-sample 2) (big-endian t))
  "Make a fmt subchunk.

SAMPLE-RATE is the number of samples per second.

NUMBER-OF-CHANNELS is the number of audio channels; 2 is stereo.

BYTES-PER-SAMPLE is the number of bytes in each sample.

BIG-ENDIAN controlls the endianness.

This assumes PCM."
  (values-to-bytes '(ASCII "fmt ")
                   (list 'b 16 4 big-endian) ;;subchunk size -- PCM means no extra params.
                   (list 'b 1 2 big-endian)  ;;PCM
                   (list 'b number-of-channels 2 big-endian)
                   (list 'b sample-rate 4 big-endian)
                   (list 'b
                         (* sample-rate number-of-channels bytes-per-sample)
                         4
                         big-endian)
                   (list 'b
                         (* number-of-channels bytes-per-sample)
                         2
                         big-endian)
                   (list 'b (* 8 bytes-per-sample) 2 big-endian)))

(defun rescale (value orig-low orig-high new-low new-high)
  "Rescales a VALUE in one range, to the equivalent value in another range.

Specifically, it converts a point in (ORIG-LOW ORIG-HIGH) to being in
\(NEW-LOW and NEW-HIGH).

For example, (rescale 5 0 10 100 200) -> 150.  5 is halfway between 0
and 10, so it should end up as halfway between 100 and 200."
  (+ new-low
     (* (/ (float (- value orig-low))
           (- orig-high orig-low))
        (- new-high new-low))))


(cl-defun zmusic//data-subchunk (raw-samples &optional (big-endian t))
  "Make a data subchunk from RAW-SAMPLES.

BIG-ENDIAN controlls the endianness."
  (values-to-bytes '(ASCII "data")
                   (list 'b (length raw-samples) 4 big-endian)
                   raw-samples))

(defun annotate-wave-data (wave-data)
  "Attempt to annotate the given WAVE-DATA."
  (message (concat (format "header\nRIFF (82 73 70 70): %s\n" (seq-subseq wave-data 0 4))
                   (format "chunkSize: %s\n" (seq-subseq wave-data 4 8))
                   (format "WAVE (87 65 86 69): %s\n\n" (seq-subseq wave-data 8 12))

                   (format "fmt subchunk\nfmt (102 109 116 32): %s\n" (seq-subseq wave-data 12 16))
                   (format "subchunk1 size: %s\n" (seq-subseq wave-data 16 20))
                   (format "linear quantization (1 0): %s\n" (seq-subseq wave-data 20 22))
                   (format "number of channels: %s\n" (seq-subseq wave-data 22 24))
                   (format "sample rate: %s\n" (seq-subseq wave-data 24 28))
                   (format "byte rate: %s\n" (seq-subseq wave-data 28 32))
                   (format "block align: %s\n" (seq-subseq wave-data 32 34))
                   (format "bits per sample: %s\n" (seq-subseq wave-data 34 36)))))

(cl-defun zmusic//make-full-wave-data (sample-rate music-data &key (number-of-channels 2) (bytes-per-sample 2) (big-endian t))
  "Make full wave data from the MUSIC-DATA, which is SAMPLE-RATE.

NUMBER-OF-CHANNELS and BYTES-PER-SAMPLE are optional arguments.

BIG-ENDIAN controlls the endianness.

This assumes PCM.

This returns a vector of bytes."
  (let ((data-subchunk (zmusic//data-subchunk music-data big-endian)))
    (vconcat (zmusic//wave-header data-subchunk big-endian)
             (zmusic//fmt-subchunk sample-rate :number-of-channels number-of-channels :bytes-per-sample bytes-per-sample :big-endian big-endian)
             data-subchunk)))

(defun write-bytes-to-file (bytes filename)
  "Write BYTES to FILENAME."
  ;;zck maybe also needs to bind (coding-system-for-write 'no-conversion)
  ;;zck maybe also needs (set-buffer-file-coding-system 'raw-text)
  (with-temp-buffer
    (toggle-enable-multibyte-characters -1)
    (seq-doseq (byte bytes)
      (insert-byte byte 1))
    (write-region nil nil filename)))

(cl-defun play-wave-data (wave-data)
  "Play WAVE-DATA at VOLUME, asynchronously."
  (let ((temp-file-name (make-temp-file "zmusic" nil ".wav")))
    (write-bytes-to-file wave-data temp-file-name)
    (start-process-shell-command "zmusic" nil (format "%s %s" *zmusic/wave-playing-executable* temp-file-name))))

(cl-defun make-tone (hz duration sample-rate &key (sample-size 1) (big-endian t) (min-sample-value 0) (max-sample-value (1- (expt 2 (* sample-size 8)))))
  "Make samples for a note at frequency HZ.

The note should last DURATION seconds, sampled at SAMPLE-RATE.

BIG-ENDIAN controls the endianness.

This returns a list of raw samples, as bytes.  Each SAMPLE-SIZE bytes
represent a single sample, reversed to be little-endian."
  (seq-mapcat
   (lambda (val) (value-to-bytes val sample-size big-endian))
   (cl-loop for sample-number below (* duration sample-rate)
            collect (rescale (sin (rescale sample-number
                                           0 sample-rate
                                           0 (* 2 float-pi hz)))
                             -1 1
                             min-sample-value max-sample-value))))

(cl-defun zmusic//make-silent-note (duration sample-rate)
  "Render a silent note.  The note lasts DURATION seconds.

Use SAMPLE-RATE, and each sample is one byte.

This is just the raw samples."
  (make-list (truncate (* sample-rate duration)) 127))

(cl-defun make-note (semitones-up duration sample-rate &key (sample-size 1) (min-sample-value 0) (max-sample-value (1- (expt 2 (* sample-size 8)))))
  "Make a note SEMITONES-UP semitones up from concert A (440hz).

This returns a list of raw samples.

The note lasts DURATION seconds.

Use SAMPLE-RATE, and SAMPLE-SIZE."
  (make-tone (frequency semitones-up)
             duration
             sample-rate
             :sample-size sample-size
             :big-endian nil
             :min-sample-value min-sample-value
             :max-sample-value max-sample-value))

(cl-defun frequency (semitones-up &optional (root-frequency *zmusic//root-frequency*))
  "Return the frequency of a note SEMITONES-UP from ROOT-FREQUENCY."
  (* root-frequency
     (expt (expt 2 (/ 12.0))
           semitones-up)))

(defun bytes-to-number (&rest bytes)
  "Convert a sequence of BYTES to a single number."
  (cl-reduce (lambda (acc cur) (+ (* acc 256) cur))
             bytes
             :initial-value 0))

;;;###autoload
(define-derived-mode zmusic-mode special-mode "zmusic mode"
  "A mode to write music in."
  (define-key zmusic-mode-map (kbd "SPC") #'zmusic/toggle-at-point)

  ;;zck p can't be previous-line *and* play. Ugh.
  (define-key zmusic-mode-map (kbd "P") #'zmusic/toggle-play)

  (define-key zmusic-mode-map (kbd "p") #'zmusic/previous-beat)
  (define-key zmusic-mode-map (kbd "n") #'zmusic/next-beat)
  (define-key zmusic-mode-map (kbd "b") #'zmusic/backward-degree)
  (define-key zmusic-mode-map (kbd "f") #'zmusic/forward-degree)
  (define-key zmusic-mode-map (kbd "s") #'zmusic/set-beat)
  (define-key zmusic-mode-map (kbd "c") #'zmusic/count-beat)
  (define-key zmusic-mode-map (kbd "t") #'zmusic/play-this-beat)
  (define-key zmusic-mode-map (kbd "r") #'zmusic/replay-beat)
  (define-key zmusic-mode-map (kbd "k") #'zmusic/kill-beat)
  (define-key zmusic-mode-map (kbd "y") #'zmusic/yank-beat)

  (define-key zmusic-mode-map (kbd "N") #'zmusic/new-zmusic)
  (define-key zmusic-mode-map (kbd "X") #'zmusic/export))

(defvar *zmusic//bpm* 240 "The beats per minute.")
(defvar *zmusic//root-frequency* 440 "The frequency of the root note of the scale.")
(defvar *zmusic//empty-note* ?- "The character printed when there is no note for a cell.")
(defvar *zmusic//note* ?* "The character printed when there is a note for a cell.")

(defvar *zmusic//starting-beats* 16 "The number of beats that a new composition starts with.")

(defvar *zmusic//sheet-music* nil
  "A list containing each beat of music.

Each beat is a list of degrees.  If the degree is t, it's on.
If it's nil, it's off.")

;;zck this should be wiped out whenever we change the tempo.
;;zck fill this async, so rendering doesn't block playing.
(defvar *zmusic//rendered-notes-files* nil
  "A hashtable mapping lists of notes to files with the data for those notes.

The notes are given by a list of semitones-up, in increasing order.")

(defvar *zmusic//current-beat-number* 1
  "The number of the current beat.

This number determines which notes to play, and what row to
highlight.")

(defvar *zmusic//repeat-current-beat-count* 0
  "The number of times to repeat the current beat.")

(defvar *zmusic//beginning-of-music-point* 1
  "The point where the music starts.")

(defvar *zmusic//end-of-music-point* 0
  "The point where the music ends.

This is the last character of the last beat.")

(defface zmusic//beat-face
  '((t (:background "#C0FFFF")))
  "face for the neighbors of point"
  :group 'zmusic/faces)

(defvar *zmusic//beat-overlay*
  (let ((overlay (make-overlay 0 0)))
    (overlay-put overlay 'face 'zmusic//beat-face)
    overlay))

(defvar *zmusic//beat-timer* nil
  "The timer that runs the beat of the music.")

(defvar *zmusic//beat-kill-ring* nil
  "A list of killed beats.")

(defvar *zmusic//beat-kill-ring-yank-pointer* nil
  "The tail of *zmusic//beat-kill-ring* whose car is the last thing yanked.")

(defcustom *zmusic/wave-playing-executable*
  (cl-first (seq-filter #'executable-find
                        (list "aplay" ;;Linux
                              "afplay";;Mac
                              )))
  "An executable that can play wave files.

It is passed the path to a wave file."
  :type 'string
  :group 'zmusic)

(defcustom *zmusic/play-beat-upon-write*
  t
  "When a note is inserted or deleted, play that note's beat, unless the music is playing."
  :type 'boolean
  :group 'zmusic)

(defun zmusic//insert-header ()
  "Insert a zmusic header into the current buffer."
  (goto-char (point-min))
  (let* ((music-width (1- (* 2 (length (zmusic//get-beat 1)))))
         (zmusic-left-padding (+ (/ (- music-width (length "==ZMUSIC==")) 2)
                                 (length "==ZMUSIC==")
                                 1))
         (play/pause-label (if *zmusic//beat-timer* "pause" "play"))
         (play/pause-left-padding (+ (/ (- music-width (length play/pause-label)) 2)
                                     ;;"play" gets indented one more space.
                                     (if *zmusic//beat-timer* 0 1)))
         (bpm-left-padding (+ (/ (- music-width
                                    ;;5 is two spaces plus "bpm"
                                    (+ (length (number-to-string *zmusic//bpm*)) 5))
                                 2)
                              (length (number-to-string *zmusic//bpm*))
                              1)))
    (insert (format (format "\n%%%ss\n\n%%%ss  bpm\n" zmusic-left-padding bpm-left-padding)
                    "==ZMUSIC=="
                    *zmusic//bpm*))
    (insert (make-string  play/pause-left-padding ?\s))
    (insert-text-button play/pause-label
                        'action (lambda (button)
                                  (zmusic/toggle-play))
                        'follow-link t)
    (insert (format "\n\n %s\n\n"
                    (make-string music-width ?-)))))

(defun zmusic//insert-keyboard ()
  "Insert the playable keyboard into the current buffer."
  (let ((number-of-degrees (length (zmusic//get-beat 0))))

    (insert "\n "
            (make-string (1- (* 2 number-of-degrees)) ?-)
            "\n")
    (dotimes (scale-degree number-of-degrees)
      (insert " ")
      (insert-text-button
       "o"
       'action (lambda (button)
                 (let* ((sample-rate 4410)
                        (sample-size 1)
                        (semitones-up (zmusic//scale-degree-to-semitones-up (1+ scale-degree)))
                        (duration (/ 60.0 *zmusic//bpm*))
                        (music-data (make-note semitones-up duration sample-rate))
                        (wave-data (zmusic//make-full-wave-data sample-rate
                                                                music-data
                                                                :number-of-channels 1
                                                                :bytes-per-sample 1
                                                                :big-endian nil)))
                   (play-wave-data wave-data)))
       'follow-link t))))

(defun zmusic//insert-footer ()
  "Insert the zmusic footer into the current buffer."
  (let ((music-width (1- (* 2 (length (zmusic//get-beat 1))))))
    (insert "\n")
    (insert " " (make-string music-width ?-))
    (insert "\n\n")
    (insert "  space: toggle note\n")
    (insert "  P: play/pause music\n")
    (insert "  p/n/b/f: move point")))

(defun zmusic//insert-music ()
  "Insert the music into the current buffer."
  (setq *zmusic//beginning-of-music-point* (point))
  (seq-do-indexed
   (lambda (beat beat-number)
     (insert ?\s)
     (seq-do-indexed
      (lambda (note note-position)
        (insert-text-button (if note *zmusic//note* *zmusic//empty-note*)
                            'action (lambda (button)
                                      (zmusic//toggle
                                       ;;zck make a function for this when I can figure out a decent name
                                       (1+ beat-number)
                                       (zmusic//note-position-to-scale-degree note-position))
                                      (zmusic//print-everything))
                            'face 'default
                            'follow-link t)
        (insert ?\ ))
      beat)
     (delete-char -1)
     (insert ?\n))
   *zmusic//sheet-music*)
  (setq *zmusic//end-of-music-point* (1- (point))))

(defun zmusic//toggle (beat-number scale-degree)
  "Toggle the beat at BEAT-NUMBER, SCALE-DEGREE.

Both of these are one-indexed, as music is."
  (zmusic//set-note beat-number scale-degree
                    (not (zmusic//note-at beat-number scale-degree)))
  (zmusic//render-beat-into-cache beat-number)
  (when (and *zmusic/play-beat-upon-write*
             (not *zmusic//beat-timer*))
    (zmusic//play-beat beat-number)))

(defun zmusic//degrees-in-beat (beat-number)
  "Return a list of degrees in beat BEAT-NUMBER.

BEAT-NUMBER is one-indexed, as are the returned degrees."
  (let ((beat (zmusic//get-beat beat-number)))
    (cl-loop for is-on being the elements of beat
             using (index note-position)
             when is-on
             collect (zmusic//note-position-to-scale-degree note-position))))

;;zck name "intervals-in-beat"?
;;Intervals are normally not a semitone count, though. Ugh.
(defun zmusic//semitones-in-beat (beat-number)
  "Return a list of semitones for beat BEAT-NUMBER.

If a beat has the root note, and a major third, this will return '(0 4).

BEAT-NUMBER is one-indexed."
  (mapcar #'zmusic//scale-degree-to-semitones-up
          (zmusic//degrees-in-beat beat-number)))

(defun zmusic//save-wave-to-file (wave-data)
  "Save WAVE-DATA to a file, and return the file location."
  (let ((temp-file-name (make-temp-file "zmusic" nil ".wav")))
    (write-bytes-to-file wave-data temp-file-name)
    temp-file-name))

(defun zmusic//combine-samples (list-of-samples)
  "Combine LIST-OF-SAMPLES into a single set of samples."
  (apply #'cl-mapcar
         (lambda (&rest samples) (/ (apply #'+ samples) (length samples)))
         list-of-samples))

(defun zmusic//sample-semitones (semitones duration sample-rate)
  "Sample SEMITONES, a list of semitones up from the root.

The samples are taken for DURATION at SAMPLE-RATE."
  (if semitones
      (let ((raw-notes-samples (mapcar (lambda (semitones-up) (make-note semitones-up duration sample-rate :sample-size 1))
                                       semitones)))
        ;;just average them. Probably a better way to combine samples?
        ;;This probably only works for single-byte samples.
        (zmusic//combine-samples raw-notes-samples))
    (zmusic//make-silent-note duration sample-rate)))

(defun zmusic//render-semitones (semitones)
  "Render SEMITONES, a list of semitones up from the root, into full wave data."
  (let* ((sample-rate 4410)
         (averaged-notes (zmusic//sample-semitones semitones (/ 60.0 *zmusic//bpm*) sample-rate))
         (wave-data (zmusic//make-full-wave-data sample-rate
                                                 averaged-notes
                                                 :number-of-channels 1
                                                 :bytes-per-sample 1
                                                 :big-endian nil)))
        wave-data))

(defun zmusic//render-beat-into-cache (beat-number)
  "Render BEAT-NUMBER into *zmusic//rendered-notes-files*."
  (let ((semitones (zmusic//semitones-in-beat beat-number)))
    (when (and semitones
               (not (gethash semitones *zmusic//rendered-notes-files*)))
      (puthash semitones (zmusic//save-wave-to-file (zmusic//render-semitones semitones)) *zmusic//rendered-notes-files*))))

(defun zmusic/count-beat ()
  "Play the next beat.

After it is played, highlight the beat after it."
  (interactive)
  (setq *zmusic//repeat-current-beat-count* 1)
  (zmusic//play-next-beat)
  (zmusic//stop-timer)
  (setq *zmusic//beat-timer*
        (run-with-timer (* 0.99 (/ 60.0 *zmusic//bpm*))
                        nil
                        #'zmusic//move-to-next-beat)))

(defun zmusic//move-to-next-beat ()
  "Move to the next beat, but do not play it."
  (if (> *zmusic//repeat-current-beat-count* 0)
      (cl-decf *zmusic//repeat-current-beat-count*)
    (setq *zmusic//current-beat-number*
          (1+ (mod *zmusic//current-beat-number*
                   (length *zmusic//sheet-music*)))))
  (zmusic//highlight-beat))

(defun zmusic//play-next-beat ()
  "Play the next beat."
  (zmusic//move-to-next-beat)
  (zmusic//play-beat))

(defun zmusic/replay-beat ()
  "Replay the current beat."
  (interactive)
  (zmusic//play-beat))

(defun zmusic/play-this-beat ()
  "Play the beat at point."
  (interactive)
  (zmusic/set-beat)
  (zmusic//play-beat))

(defun zmusic//get-beat (beat-number)
  "Get the BEAT-NUMBERth beat.

This is one-indexed, as music is."
  (seq-elt *zmusic//sheet-music* (1- beat-number)))

(defconst zmusic//major-scale
  (let ((table (make-hash-table)))
    (puthash 1 0 table)
    (puthash 2 2 table)
    (puthash 3 4 table)
    (puthash 4 5 table)
    (puthash 5 7 table)
    (puthash 6 9 table)
    (puthash 7 11 table)
    (puthash 8 12 table)
    (puthash 9 14 table)
    (puthash 10 16 table)
    (puthash 11 17 table)
    (puthash 12 19 table)
    (puthash 13 21 table)
    (puthash 14 23 table)
    (puthash 15 24 table)
    table))

(defconst zmusic//minor-pentatonic-scale
  (let ((table (make-hash-table)))
    (puthash 1 0 table)
    (puthash 2 3 table)
    (puthash 3 5 table)
    (puthash 4 7 table)
    (puthash 5 10 table)
    (puthash 6 12 table)
    (puthash 7 15 table)
    (puthash 8 17 table)
    (puthash 9 19 table)
    (puthash 10 22 table)
    (puthash 11 24 table)
    table)
  "The minor pentatonic scale, mapping the scale degree to the number of semitones up from the root.")

(defvar zmusic//current-scale
  zmusic//minor-pentatonic-scale
  "The current scale used.

A scale is a hash mapping the scale degree to the number of semitones up from the root.  See zmusic//major-scale for an example.")

(defun zmusic//scale-degree-to-semitones-up (scale-degree)
  "Calculate how many semitones up SCALE-DEGREE of the scale is.

For example, in a major scale, the third scale tone is four semitones up."
  (gethash scale-degree zmusic//current-scale))

(defun zmusic//note-position-to-scale-degree (note-position)
  "A NOTE-POSITION is what code thinks the position in the scale a note is.
This is zero-based.

However, a scale is one-based; the first degree of a scale is degree
1.  This function converts from a note position to a scale degree."
  (1+ note-position))

(cl-defun zmusic//play-beat (&optional (beat-number *zmusic//current-beat-number*))
  "Play the BEAT-NUMBERth beat."

  (let ((semitones (zmusic//semitones-in-beat beat-number)))
    (when semitones
      (start-process-shell-command "zmusic"
                                   nil
                                   (format "%s %s"
                                           *zmusic/wave-playing-executable*
                                           (gethash semitones *zmusic//rendered-notes-files*))))))

(defun zmusic//start-timer ()
  "Start playing the music."
  (interactive)
  (setq *zmusic//repeat-current-beat-count* 1)
  (when *zmusic//beat-timer*
    (cancel-timer *zmusic//beat-timer*))
  (setq *zmusic//beat-timer*
        (run-with-timer 0
                        (/ 60.0 *zmusic//bpm*)
                        #'zmusic//play-next-beat)))

(defun zmusic//stop-timer ()
  "Stop stepping forward."
  (when *zmusic//beat-timer*
    (cancel-timer *zmusic//beat-timer*))
  (setq *zmusic//beat-timer* nil))

(defun zmusic/toggle-play ()
  "Play if the music is stopped; stop the music if it's playing."
  (interactive)
  (if *zmusic//beat-timer*
      (progn (zmusic//stop-timer)
             (setq *zmusic//repeat-current-beat-count* 1))
    (zmusic//start-timer))

  ;;mainly for changing the text on the play/pause button
  (zmusic//print-everything))

(defun zmusic/kill-beat ()
  "Kill the current beat -- setting it to all blank, and save it for pasting later."
  (interactive)
  (let* ((beat-number (zmusic//beat-number-at-point))
         (beat (zmusic//get-beat beat-number)))
    (push (seq-copy beat)
          *zmusic//beat-kill-ring*)
    (cl-loop for degree upto (length beat)
             do (zmusic//set-note beat-number degree nil)))
  (zmusic//print-everything))

(defun zmusic/yank-beat ()
  "Yank the last killed beat."
  (interactive)
  (let ((beat-number (zmusic//beat-number-at-point)))
    (seq-do-indexed (lambda (is-on note-position)
                      (zmusic//set-note beat-number
                                        (1+ note-position)
                                        is-on))
                    (car *zmusic//beat-kill-ring*)))
  (zmusic//print-everything))

(defun zmusic/yank-pop-beat ()
  "Like `yank-pop', for beats."
  (interactive))

(defun zmusic//print-everything ()
  "Prints the entire music into the current buffer."
  (let ((inhibit-read-only t)
        ;;;zck what if we're before the music?
        (beat (zmusic//beat-number-at-point))
        (degree (zmusic//degree-in-scale-at-point)))
    (erase-buffer)
    (zmusic//insert-header)
    (zmusic//insert-music)

    ;;zck how can this not sound bad? Maybe need to mix in to currently playing beat? Ugh.
    ;;(zmusic//insert-keyboard)
    (zmusic//insert-footer)
    (goto-char *zmusic//beginning-of-music-point*)
    (forward-line (1- beat))
    (forward-char (1+ (* 2 (1- degree)))))
  (zmusic//highlight-beat *zmusic//current-beat-number*))

(cl-defun zmusic//highlight-beat (&optional (beat-number *zmusic//current-beat-number*))
  "Highlight the BEAT-NUMBERth beat."
  (save-excursion
    (with-current-buffer "zmusic"
      (goto-char *zmusic//beginning-of-music-point*)
      (zmusic/next-beat (1- beat-number))
      (move-overlay *zmusic//beat-overlay*
                    (point)
                    (line-end-position)
                    (get-buffer "zmusic")))))

(defun zmusic//init ()
  "Initialize data for a zmusic composition."
  (when *zmusic//beat-timer*
    (cancel-timer *zmusic//beat-timer*))
  (setq *zmusic//beat-timer* nil)
  (setq *zmusic//current-beat-number* 1)
  (setq *zmusic//repeat-current-beat-count* 1)

  (setq *zmusic//sheet-music* (cl-loop for x below *zmusic//starting-beats* collect (make-list (hash-table-count zmusic//current-scale) nil)))
  (setq *zmusic//rendered-notes-files* (make-hash-table :test #'equal)))

(defun zmusic ()
  "Create a new zmusic."
  (interactive)
  (switch-to-buffer "zmusic")
  (zmusic-mode)
  (zmusic/new-zmusic nil))

(cl-defun zmusic/new-zmusic (&optional (confirm t))
  "Make a new zmusic, erasing anything that is currently there.

Prompt the user for confirmation when CONFIRM is t."
  (interactive)
  (when (or (not confirm)
            (y-or-n-p "Erase current zmusic and start a new one? "))
    (zmusic//init)
    (zmusic//print-everything)
    (goto-char *zmusic//beginning-of-music-point*)
    (forward-char 1)))

(defun zmusic//beat-number-at-point ()
  "Return the beat number at point.

This is one-indexed; as that's how music works."
  (1+ (- (line-number-at-pos)
         (line-number-at-pos *zmusic//beginning-of-music-point*))))

(defun zmusic//degree-in-scale-at-point ()
  "Return the degree in scale at point.

This is one-indexed; as that's how music works."
  (min (1+ (/ (current-column) 2))
       (hash-table-count zmusic//current-scale)))

(defun zmusic//note-at (beat-number scale-degree)
  "Return the note at beat BEAT-NUMBER, degree SCALE-DEGREE.

Both BEAT and DEGREE are one-indexed."
  ;; zck what about beat, degree outside valid values?
  (seq-elt (zmusic//get-beat beat-number)
           (1- scale-degree)))

(defun zmusic//set-note (beat degree value)
  "Set the note at beat BEAT, degree DEGREE to have VALUE.

BEAT and DEGREE are one-indexed."
  (setf (seq-elt (seq-elt *zmusic//sheet-music* (1- beat))
                 (1- degree))
        value))

(defun zmusic/toggle-at-point ()
  "Toggle the note at point."
  (interactive)
  (when (and (looking-at (rx graph))
             t ;;zck, make sure we're in the music.
             )
    ;; both of these are one-indexed,
    ;; because that's how music works
    (let ((beat (zmusic//beat-number-at-point))
          (degree-in-scale (zmusic//degree-in-scale-at-point)))
      (zmusic//toggle beat degree-in-scale))
    (zmusic//print-everything)))

(defun zmusic//at-first-beat ()
  "Return if point is at the first beat."
  (= (line-beginning-position)
     *zmusic//beginning-of-music-point*))

(defun zmusic//at-last-beat ()
  "Return if point is at the last beat."
  (= (line-end-position)
     *zmusic//end-of-music-point*))

(defun zmusic//after-last-beat ()
  "Return if point is at the last beat."
  (> (point)
     *zmusic//end-of-music-point*))

(defun zmusic/previous-beat (lines-to-move)
  "Move to the same degree of the beat LINES-TO-MOVE before point."
  (interactive "p")
  (let ((starting-degree (zmusic//degree-in-scale-at-point)))
    (while (> lines-to-move 0)
      (cl-decf lines-to-move)
      (cond ((< (point)
                *zmusic//beginning-of-music-point*)
             (goto-char *zmusic//beginning-of-music-point*)
             (forward-char 1))
            ((> (point)
                *zmusic//end-of-music-point*)
             (goto-char *zmusic//end-of-music-point*))
            ((zmusic//at-first-beat)
             ;;don't go anywhere
             (setq lines-to-move 0))
            (t (forward-line -1))))
    (beginning-of-line)
    (forward-char (1- (* 2 starting-degree)))))

(defun zmusic/next-beat (lines-to-move)
  "Move to the same degree of the beat LINES-TO-MOVE after point."
  (interactive "p")
  (let ((starting-degree (zmusic//degree-in-scale-at-point)))
    (while (> lines-to-move 0)
      (cl-decf lines-to-move)
      (cond ((< (point)
                *zmusic//beginning-of-music-point*)
             (goto-char *zmusic//beginning-of-music-point*))
            ((zmusic//at-last-beat)
             ;;don't go anywhere
             (setq lines-to-move 0))
            ((zmusic//after-last-beat)
             (goto-char *zmusic//end-of-music-point*)
             (setq lines-to-move 0))
            (t (forward-line))))
    (beginning-of-line)
    (forward-char (1- (* 2 starting-degree)))))

(defun zmusic/forward-degree (degrees-forward)
  "Move to the degree DEGREES-FORWARD after point in the same beat."
  (interactive "p")
  (while (> degrees-forward 0)
    (cl-decf degrees-forward)
    (cond ((looking-at (rx blank))
           (forward-char 1))
          ((looking-at (rx (zero-or-one any) line-end))
           ;;don't move
           (setq degrees-forward 0))
          (t (forward-char 2)))))

(defun zmusic/backward-degree (degrees-backward)
  "Move to the degree DEGREES-BACKWARD before point in the same beat."
  (interactive "p")
  (while (> degrees-backward 0)
    (cl-decf degrees-backward)
    (cond ((bolp)
           ;;move to first degree
           (forward-char 1)
           (setq degrees-backward 0))
          ((= (current-column) 1)
           ;;don't move
           (setq degrees-backward 0))
          ((looking-at (rx (or blank line-end)))
           (backward-char 1))
          (t (backward-char 2)))))

(defun zmusic/set-beat ()
  "Set the beat to the beat number of the cursor."
  (interactive)
  (setq *zmusic//current-beat-number*
        (zmusic//beat-number-at-point))
  (setq *zmusic//repeat-current-beat-count* 1)
  (zmusic//highlight-beat *zmusic//current-beat-number*))

(defun zmusic/export (file-to-export-to)
  "Export the current zmusic to a file.

The file is given by FILE-TO-EXPORT-TO."
  (interactive "FExport to file: ")
  (let* ((beats-as-semitones
          (cl-loop for beat-number
                   from 1
                   to (length *zmusic//sheet-music*)
                   collect (zmusic//semitones-in-beat beat-number)))
         (all-samples
          (seq-mapcat (lambda (semitones)
                        (zmusic//sample-semitones semitones
                                                  (/ 60.0 *zmusic//bpm*)
                                                  4410))
                      beats-as-semitones))
         (wave-data (zmusic//make-full-wave-data 4410 all-samples :number-of-channels 1 :bytes-per-sample 1 :big-endian nil)))
    (write-bytes-to-file wave-data file-to-export-to)))

(provide 'zmusic)
;;; zmusic.el ends here
