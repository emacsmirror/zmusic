;;; zmusic -- write music through Emacs. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'seq)
(require 'cl-lib)
(require 'rx)

;;can specify hex numbers with #x123456789abcdef0, So #x10 is 16

(defconst hex-lookup-table
  (let ((table (make-hash-table :size 22)))
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
    (puthash ?a 10 table)
    (puthash ?B 11 table)
    (puthash ?b 11 table)
    (puthash ?C 12 table)
    (puthash ?c 12 table)
    (puthash ?D 13 table)
    (puthash ?d 13 table)
    (puthash ?E 14 table)
    (puthash ?e 14 table)
    (puthash ?F 15 table)
    (puthash ?f 15 table)
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
  (gethash val hex-lookup-table 0))

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

(ert-deftest value-to-bytes//hexstring ()
  (should (equal '(16 255)
                 (value-to-bytes "10FF"))))
(ert-deftest value-to-bytes//ASCII-string-list ()
  (should (equal '(97 98 99)
                 (value-to-bytes '(ASCII "abc")))))
(ert-deftest value-to-bytes//byte-list ()
  (should (equal '(10 0 255)
                 (value-to-bytes '(10 00 255)))))
(ert-deftest value-to-bytes//single-byte ()
  (should (equal '(81)
                 (value-to-bytes 81))))
(ert-deftest value-to-bytes//single-number-multiple-bytes ()
  (should (equal '(2 1)
                 (value-to-bytes 513))))
(ert-deftest value-to-bytes//single-byte-with-padding ()
  (should (equal '(0 0 0 81)
                 (value-to-bytes 81 4))))
(ert-deftest value-to-bytes//improperly-passed-float ()
  (should (equal '(2 1)
                 (value-to-bytes 513.1))))
(ert-deftest value-to-bytes//byte-with-padding ()
  (should (equal '(0 0 0 1)
                 (value-to-bytes '(b 1 4)))))
(ert-deftest value-to-bytes//little-endian/separate-args ()
  (should (equal '(81 0 0 0)
                 (value-to-bytes 81 4 nil))))
(ert-deftest value-to-bytes//little-endian/single-value ()
  (should (equal '(81 0 0 0)
                 (value-to-bytes '(b 81 4 nil)))))

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

(ert-deftest wave-header ()
  ;;test data from http://soundfile.sapp.org/doc/WaveFormat/
  ;;converted to big-endian (changed to RIFX, reordered size)
  ;;also note that we're not making a header with a 2048-size data chunk
  ;;We're making a header with a data chunk with 2048 bytes of data in it. The header itself has 8 more bytes (it begins with "data", then the size of data.)
  (should (equal (values-to-bytes "524946580000082457415645")
                 (zmusic//wave-header (make-list 2056 0)))))

(defun zmusic//wave-header (data-subchunk)
  "Make a wave header, a list of bytes.

The DATA-SUBCHUNK is the actual subchunk containing the data.  It has
to be calculated before we get the header

This assumes we're using big-endian numbers, plus PCM."
  (values-to-bytes '(ASCII "RIFX")

                    ;;size of file after this specific field. The whole file size - 8.
                   `(b ,(+ 4 ;;rest of header
                           24 ;;size of FMT chunk
                           (length data-subchunk))
                       4)
                   '(ASCII "WAVE")))

(defun zmusic//wave-header-little-endian (data-subchunk)
    "Make a wave header, a list of bytes.

The DATA-SUBCHUNK is the actual subchunk containing the data.  It has
to be calculated before we get the header.

This assumes we're using little-endian numbers, plus PCM."
  (values-to-bytes '(ASCII "RIFF")

                    ;;size of file after this specific field. The whole file size - 8.
                   `(b ,(+ 4 ;;rest of header
                           24 ;;size of FMT chunk
                           (length data-subchunk))
                       4
                       nil)
                   '(ASCII "WAVE")))

(ert-deftest zmusic//sample-fmt ()
  ;;test data from http://soundfile.sapp.org/doc/WaveFormat/
  ;;converted to big-endian
  (should (equal (values-to-bytes "666d74200000001000010002000056220001588800040010")
                 (zmusic//fmt-subchunk 22050))))

(defun zmusic//fmt-subchunk (sample-rate)
  "Make a fmt subchunk.

SAMPLE-RATE is the number of samples per second.

This assumes PCM and stereo, and works with big-endian numbers."
  (let ((number-of-channels 2)
        (bytes-per-sample 2))
    (values-to-bytes '(ASCII "fmt ")
                     '(b 16 4) ;;subchunk size -- PCM means no extra params.
                     '(b 1 2) ;;PCM
                     (list 'b number-of-channels 2)
                     (list 'b sample-rate 4)
                     (list 'b
                           (* sample-rate number-of-channels bytes-per-sample)
                           4)
                     (list 'b
                           (* number-of-channels bytes-per-sample)
                           2)
                     (list 'b (* 8 bytes-per-sample) 2))))

(cl-defun zmusic//fmt-subchunk-little-endian (sample-rate &key (number-of-channels 2) (bytes-per-sample 2))
  "Make a fmt subchunk.

SAMPLE-RATE is the number of samples per second.

NUMBER-OF-CHANNELS is the number of audio channels; 2 is stereo.

BYTES-PER-SAMPLE is the number of bytes in each sample.

This assumes PCM, and works with little-endian numbers."
  (values-to-bytes '(ASCII "fmt ")
                   '(b 16 4 nil) ;;subchunk size -- PCM means no extra params.
                   '(b 1 2 nil) ;;PCM
                   (list 'b number-of-channels 2 nil)
                   (list 'b sample-rate 4 nil)
                   (list 'b
                         (* sample-rate number-of-channels bytes-per-sample)
                         4
                         nil)
                   (list 'b
                         (* number-of-channels bytes-per-sample)
                         2
                         nil)
                   (list 'b (* 8 bytes-per-sample) 2 nil)))

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

(ert-deftest generate-samples/four-samples-per-second-one-hz-five-samples ()
  (should (equal '(127 255  255 255  127 255  0 0  127 255)
                 ;;four samples per second means five samples loops back to 127 255.
                 (zmusic//generate-samples 1 4 5))))

;;zck do we need sixteen-bit values?
(cl-defun zmusic//generate-samples (frequency samples-per-second number-of-samples)
  "Make a sample data subchunk, of a tone FREQUENCY.

The data is NUMBER-OF-SAMPLES data points, sampled from FREQUENCY at
SAMPLES-PER-SECOND.

This returns a list of bytes, where two consecutive bytes is a single sample."
  (cl-loop for sample-number below number-of-samples
           ;;if you imagine a constantly spinning arm in a circle.
           ;;It spins around FREQUENCY times in a second.
           ;;We want to sample the sin of that value SAMPLES-PER-SECOND times.
           ;;so we are (sample-number / samples-per-second) way through the second,
           ;;and the angle of that arm goes to (2 * pi * frequency) radians.
           for raw-value = (truncate (rescale (sin (rescale sample-number
                                                            ;;convert from the number of samples...
                                                            0 samples-per-second

                                                            ;;we loop through 2pi
                                                            ;;how many times? /frequency/ times.
                                                            0 (* 2 float-pi frequency)))
                                              -1 1
                                              0 #xFFFF))
           collect (/ raw-value 256)
           collect (mod raw-value 256)))

(ert-deftest data-subchunk/basic-data ()
  (let ((data '(1 2 3 4 5 6 7 8)))
    (should (equal (values-to-bytes '(ASCII "data")
                                    (list 'b (length data) 4)
                                    data)
                   (zmusic//data-subchunk data)))))

(defun zmusic//data-subchunk (raw-samples)
  "Make a data subchunk from RAW-SAMPLES."
  (values-to-bytes '(ASCII "data")
                   (list 'b (length raw-samples) 4)
                   raw-samples))

(defun zmusic//data-subchunk-little-endian (raw-samples)
  "Make a little-endian data subchunk from RAW-SAMPLES."
  (values-to-bytes '(ASCII "data")
                   (list 'b (length raw-samples) 4 nil)
                   raw-samples))

;;zck should this all be arrays?
(defun zmusic//make-sample-wave-data ()
  "Return a list of wave file data."
  (let* ((sample-rate 100)
         (seconds 1)
         (raw-samples (zmusic//generate-samples 440 sample-rate (* seconds sample-rate)))
         (data-subchunk (zmusic//data-subchunk raw-samples)))
    (append (zmusic//wave-header data-subchunk)
            (zmusic//fmt-subchunk sample-rate)
            data-subchunk)))


(cl-defun zmusic//make-full-wave-data-little-endian (sample-rate music-data &key (number-of-channels 2) (bytes-per-sample 2))
  "Make full wave data from the MUSIC-DATA, which is SAMPLE-RATE.

NUMBER-OF-CHANNELS and BYTES-PER-SAMPLE are optional arguments.

This assumes PCM, and works with little-endian numbers.

This returns a vector of bytes."
  (let ((data-subchunk (zmusic//data-subchunk-little-endian music-data)))
    (vconcat (zmusic//wave-header-little-endian data-subchunk)
             (zmusic//fmt-subchunk-little-endian sample-rate :number-of-channels number-of-channels :bytes-per-sample bytes-per-sample)
             data-subchunk)))

(defun zmusic//make-full-wave-data (sample-rate music-data)
  "Make full wave data from the MUSIC-DATA, which is SAMPLE-RATE.

This assumes PCM, stereo, big-endian"
  (let ((data-subchunk (zmusic//data-subchunk music-data)))
    (append (zmusic//wave-header data-subchunk)
            (zmusic//fmt-subchunk sample-rate)
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

(cl-defun make-tone (hz duration sample-rate &key (sample-size 2))
  "Make samples for a note at frequency HZ.

The note should last DURATION seconds, sampled at SAMPLE-RATE.

Each sample is SAMPLE-SIZE bytes long.

This returns a list of bytes.  Each SAMPLE-SIZE bytes represent a
single sample, reversed to be little-endian."
  (seq-mapcat
   (lambda (val) (value-to-bytes val sample-size nil))
   (cl-loop for sample-number below (* duration sample-rate)
            collect (rescale (sin (rescale sample-number
                                           0 sample-rate
                                           0 (* 2 float-pi hz)))
                             -1 1
                             0 (1- (expt 2 (* sample-size 8)))))))

(cl-defun make-note (semitones-up duration sample-rate &key (sample-size 2))
  "Make a note SEMITONES-UP semitones up from concert A (440hz)

The note lasts DURATION seconds.

Use SAMPLE-RATE, and SAMPLE-SIZE."
  (make-tone (frequency semitones-up)
             duration
             sample-rate
             :sample-size sample-size))

(defun frequency (semitones-up)
  "Return the frequency of a note SEMITONES-UP from concert A (440hz)."
  (* 440 (expt (expt 2 (/ 12.0))
               semitones-up)))

(defun bytes-to-number (&rest bytes)
  "Convert a sequence of BYTES to a single number."
  (cl-reduce (lambda (acc cur) (+ (* acc 256) cur))
             bytes
             :initial-value 0))

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
  (define-key zmusic-mode-map (kbd "c") #'zmusic//count-beat)
  (define-key zmusic-mode-map (kbd "r") #'zmusic/replay-beat)
  (define-key zmusic-mode-map (kbd "k") #'zmusic/kill-beat)
  (define-key zmusic-mode-map (kbd "y") #'zmusic/yank-beat))

(defvar *zmusic//bpm* 240 "The beats per minute.")
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

(defvar *zmusic//beginning-of-music-point* 0
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
  (pcase system-type
    ((or 'gnu/linux 'gnu/kfreebsd) "aplay")
    ('darwin "afplay"))
  "An executable that can play wave files.

It is passed the path to a wave file."
  :type 'string
  :group 'zmusic)

(defun zmusic//insert-header ()
  "Insert a zmusic header into the current buffer."
  (goto-char (point-min))
  (let* ((music-width (1- (* 2 (length (seq-elt *zmusic//sheet-music* 0)))))
         (zmusic-left-padding (+ (/ (- music-width (length "==ZMUSIC==")) 2)
                                 (length "==ZMUSIC==")
                                 1))
         (bpm-left-padding (+ (/ (- music-width
                                    ;;5 is two spaces plus "bpm"
                                    (+ (length (number-to-string *zmusic//bpm*)) 5))
                                 2)
                              (length (number-to-string *zmusic//bpm*))
                              1)))
    (insert (format (format "\n%%%ss\n\n%%%ss  bpm\n\n %%s\n\n" zmusic-left-padding bpm-left-padding)
                    "==ZMUSIC=="
                    *zmusic//bpm*
                    (make-string music-width ?-)))))

(defun zmusic//insert-footer ()
  "Insert the zmusic footer into the current buffer."
  (let ((music-width (1- (* 2 (length (seq-elt *zmusic//sheet-music* 0))))))
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
     ;; (lexical-let ((beat-number beat-number)))
     (insert ?\s)
     (seq-do-indexed
      (lambda (note note-position)
        (insert-text-button (if note *zmusic//note* *zmusic//empty-note*)
                            ;;get beat number, degree number.
                            ;;use it for button action
                            ;;also change the face of the text.
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
  (zmusic//render-beat-into-cache beat-number))

(defun zmusic//note-positions-in-beat (beat-number)
  "Return a list of note positions in beat BEAT-NUMBER.

BEAT-NUMBER is one-indexed, as are the returned note positions."
  (let ((beat (zmusic//get-beat beat-number)))
    (mapcar #'1+
            (-find-indices #'identity beat))))

;;zck name "intervals-in-beat"?
;;Intervals are normally not a semitone count, though. Ugh.
(defun zmusic//semitones-in-beat (beat-number)
  "Return a list of semitones for beat BEAT-NUMBER.

If a beat has the root note, and a major third, this will return '(0 4).

BEAT-NUMBER is one-indexed."
  (mapcar #'zmusic//scale-degree-to-semitones-up
          (zmusic//note-positions-in-beat beat-number)))

(defun zmusic//save-wave-to-file (wave-data)
  "Save WAVE-DATA to a file, and return the file location."
  (let ((temp-file-name (make-temp-file "zmusic" nil ".wav")))
    (write-bytes-to-file wave-data temp-file-name)
    temp-file-name))

(defun zmusic//render-semitones (semitones)
  "Render SEMITONES, a list of semitones up from the root, into full wave data."
  (let* ((sample-rate 4410)
         (notes-data (mapcar (lambda (semitones-up) (make-note semitones-up (/ 60.0 *zmusic//bpm*) sample-rate :sample-size 1))
                             semitones))
         (averaged-notes (apply #'cl-mapcar
                                (lambda (&rest samples) (/ (apply #'+ samples) (length samples)))
                                notes-data))
         (wave-data (zmusic//make-full-wave-data-little-endian sample-rate
                                                               averaged-notes
                                                               :number-of-channels 1
                                                               :bytes-per-sample 1)))
        wave-data))

(defun zmusic//render-beat-into-cache (beat-number)
  "Render BEAT-NUMBER into *zmusic//rendered-notes-files*."
  (let ((semitones (zmusic//semitones-in-beat beat-number)))
    (when (and semitones
               (not (gethash semitones *zmusic//rendered-notes-files*)))
      (puthash semitones (zmusic//save-wave-to-file (zmusic//render-semitones semitones)) *zmusic//rendered-notes-files*))))

;;zck this looks kind of weird. After running this once, the highlighted beat is not the next one played.
(defun zmusic//count-beat ()
  "Play the next beat."
  (interactive)
  (if (> *zmusic//repeat-current-beat-count* 0)
      (cl-decf *zmusic//repeat-current-beat-count*)
    (setq *zmusic//current-beat-number*
          (1+ (mod *zmusic//current-beat-number*
                   (length *zmusic//sheet-music*)))))
  (zmusic//highlight-beat)
  (zmusic//play-beat))

(defun zmusic/replay-beat ()
  "Replay the current beat."
  (interactive)
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
    table))

(defun zmusic//scale-degree-to-semitones-up (scale-degree)
  "Calculate how many semitones up SCALE-DEGREE of the scale is.

For example, in a major scale, the third scale tone is four semitones up."
  (gethash scale-degree zmusic//minor-pentatonic-scale))

(defun zmusic//note-position-to-scale-degree (note-position)
  "A NOTE-POSITION is what code thinks the position in the scale a note is.
This is zero-based.

However, a scale is one-based; the first degree of a scale is degree
1.  This function converts from a note position to a scale degree."
  (1+ note-position))

(cl-defun zmusic//play-beat (&optional (beat-number *zmusic//current-beat-number*))
  "Play the BEAT-NUMBERth beat."

  (let* ((beat (zmusic//get-beat beat-number))
         (note-positions (-find-indices #'identity beat))
         (semitones (zmusic//semitones-in-beat beat-number)))
    (when semitones
      (start-process-shell-command "zmusic"
                                   nil
                                   (format "%s %s"
                                           *zmusic/wave-playing-executable*
                                           (gethash semitones *zmusic//rendered-notes-files*))))))

(defun zmusic//start-timer ()
  "Start stepping forward."
  (interactive)
  (when *zmusic//beat-timer*
    (cancel-timer *zmusic//beat-timer*))
  (setq *zmusic//beat-timer*
        (run-with-timer (/ 60.0 *zmusic//bpm*)
                        (/ 60.0 *zmusic//bpm*)
                        #'zmusic//count-beat)))

(defun zmusic//stop-timer ()
  "Stop stepping forward."
  (cancel-timer *zmusic//beat-timer*)
  (setq *zmusic//beat-timer* nil))

(defun zmusic/toggle-play ()
  "Play if the music is stopped; stop the music if it's playing."
  (interactive)
  (if *zmusic//beat-timer*
      (zmusic//stop-timer)
    (zmusic//start-timer)))

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
      (forward-line (1- beat-number))
      (move-overlay *zmusic//beat-overlay*
                    (point)
                    (line-end-position)
                    (get-buffer "zmusic")))))

(defun zmusic//init ()
  "Initialize data for a zmusic composition."
  (setq *zmusic//current-beat-number* 1)
  (setq *zmusic//repeat-current-beat-count* 1)

  ;;init to degree based on the scale used. Probably want two octaves?
  (setq *zmusic//sheet-music* (cl-loop for x below *zmusic//starting-beats* collect (make-list (hash-table-count zmusic//minor-pentatonic-scale) nil)))
  (setq *zmusic//rendered-notes-files* (make-hash-table :test #'equal)))

(defun zmusic ()
  "Create a new zmusic."
  (interactive)
  (switch-to-buffer "zmusic")
  (zmusic-mode)
  (zmusic//init)
  (zmusic//print-everything))

(defun zmusic//beat-number-at-point ()
  "Return the beat number at point.

This is one-indexed; as that's how music works."
  (1+ (- (line-number-at-pos)
         (line-number-at-pos *zmusic//beginning-of-music-point*))))

(defun zmusic//degree-in-scale-at-point ()
  "Return the degree in scale at point.

This is one-indexed; as that's how music works."
  (1+ (/ (current-column) 2)))

(defun zmusic//note-at (beat-number scale-degree)
  "Return the note at beat BEAT-NUMBER, degree SCALE-DEGREE.

Both BEAT and DEGREE are one-indexed."
  ;; zck what about beat, degree outside valid values?
  (seq-elt (seq-elt *zmusic//sheet-music* (1- beat-number))
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

(defun zmusic/previous-beat (lines-to-move)
  "Move to the same degree of the beat LINES-TO-MOVE before point."
  (interactive "p")
  (while (> lines-to-move 0)
    (cl-decf lines-to-move)
    (cond ((< (point)
              *zmusic//beginning-of-music-point*)
           (goto-char *zmusic//beginning-of-music-point*))
          ((zmusic//at-first-beat)
           ;;don't go anywhere
           (setq lines-to-move 0))
          (t (let ((column (current-column)))
               (forward-line -1)
               (forward-char column)
               (cond ((bolp) (forward-char 1))
                     ((cl-evenp column)
                      (forward-char -1))))))))

(defun zmusic/next-beat (lines-to-move)
  "Move to the same degree of the beat LINES-TO-MOVE after point."
  (interactive "p")
  (while (> lines-to-move 0)
    (cl-decf lines-to-move)
    (cond ((< (point)
              *zmusic//beginning-of-music-point*)
           (goto-char *zmusic//beginning-of-music-point*))
          ((zmusic//at-last-beat)
           ;;don't go anywhere
           (setq lines-to-move 0))
          (t (let ((column (current-column)))
               (forward-line)
               (forward-char column)
               (cond ((bolp) (forward-char 1))
                     ((cl-evenp column)
                      (forward-char -1))))))))

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
  ;;this interacts badly with /replay-beat
  (setq *zmusic//current-beat-number*
        (zmusic//beat-number-at-point))
  (setq *zmusic//repeat-current-beat-count* 1)
  (zmusic//highlight-beat *zmusic//current-beat-number*))

(provide 'zmusic)

;;; zmusic.el ends here
