;;; zmusic-test.el --- Test repetitive electronic music -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Zachary Kanfer

;; Author: Zachary Kanfer <zkanfer@gmail.com>
;; Version: 0.1
;; This file is not part of GNU Emacs.


;; Homepage: https://hg.sr.ht/~zck/zmusic
;; Keywords: multimedia

;; Package-Requires: ((emacs "26.1"))

;;; Commentary:
;; This library plays repetitive digital music.
;; Whether this is good is up to the viewer.

;;; Code:

(require 'zmusic)

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


(ert-deftest wave-header ()
  ;;test data from http://soundfile.sapp.org/doc/WaveFormat/
  ;;converted to big-endian (changed to RIFX, reordered size)
  ;;also note that we're not making a header with a 2048-size data chunk
  ;;We're making a header with a data chunk with 2048 bytes of data in it. The header itself has 8 more bytes (it begins with "data", then the size of data.)
  (should (equal (values-to-bytes "524946580000082457415645")
                 (zmusic//wave-header (make-list 2056 0)))))


(ert-deftest zmusic//sample-fmt ()
  ;;test data from http://soundfile.sapp.org/doc/WaveFormat/
  ;;converted to big-endian
  (should (equal (values-to-bytes "666d74200000001000010002000056220001588800040010")
                 (zmusic//fmt-subchunk 22050))))


(ert-deftest data-subchunk/basic-data ()
  (let ((data '(1 2 3 4 5 6 7 8)))
    (should (equal (values-to-bytes '(ASCII "data")
                                    (list 'b (length data) 4)
                                    data)
                   (zmusic//data-subchunk data)))))




(provide 'zmusic-test)
;;; zmusic-test.el ends here
