;;; cal-china.el --- calendar functions for the Chinese calendar

;; Copyright (C) 1995, 1997, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
;;   2008, 2009, 2010, 2011, 2012  Free Software Foundation, Inc.

;;
;; cal-korean.el (Korean lunar calendar based on cal-china.el)
;; Author : Jaemok Jeong <jmjeong@gmail.com>
;; -----------------------------------------------------------
;;
;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: Korean calendar, calendar, holidays, diary

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See calendar.el.

;; The rules used for the Korean Lunar calendar are those of Baolin Liu
;; (see L. E. Doggett's article "Calendars" in the Explanatory
;; Supplement to the Astronomical Almanac, second edition, 1992) for
;; the calendar as revised at the beginning of the Qing dynasty in
;; 1644.  The nature of the astronomical calculations is such that
;; precise calculations cannot be made without great expense in time,
;; so that the calendars produced may not agree perfectly with
;; published tables--but no two pairs of published tables agree
;; perfectly either!  Liu's rules produce a calendar for 2033 which is
;; not accepted by all authorities.  The date of Korean New Year is
;; correct from 1644-2051.

;; Note to maintainers:
;; Use `korean-year-cache-init' every few years to recenter the default
;; value of `korean-year-cache'.

;;; Code:

(require 'calendar)
(require 'lunar)                        ; lunar-new-moon-on-or-after
;; solar-date-next-longitude brought in by lunar.
;;;(require 'solar)
;; calendar-astro-to-absolute and from-absolute are cal-autoloads.
;;;(require 'cal-julian)


(defgroup calendar-korean nil
  "Korean Lunar calendar support."
  :prefix "calendar-korean-"
  :group 'calendar)

(define-obsolete-variable-alias 'korean-calendar-time-zone
  'calendar-korean-time-zone "23.1")

(defcustom calendar-korean-time-zone
  '(if (< year 1928)
       (+ 465 (/ 40.0 60.0))
     540)
  "Minutes difference between local standard time for Korean Lunar calendar and UTC.
Default is for Beijing.  This is an expression in `year' since it changed at
1928-01-01 00:00:00 from UT+7:45:40 to UT+9."
  :type 'sexp
  :group 'calendar-korean)

;; It gets eval'd.
;;;###autoload
(put 'calendar-korean-time-zone 'risky-local-variable t)
;;;###autoload
(put 'korean-calendar-time-zone 'risky-local-variable t)


(define-obsolete-variable-alias 'korean-calendar-location-name
  'calendar-korean-location-name "23.1")

;; FIXME unused.
(defcustom calendar-korean-location-name "Seoul"
  "Name of location used for calculation of Korean Lunar calendar."
  :type 'string
  :group 'calendar-korean)

(define-obsolete-variable-alias 'korean-calendar-daylight-time-offset
  'calendar-korean-daylight-time-offset "23.1")

(defcustom calendar-korean-daylight-time-offset 0
;; The correct value is as follows, but the Korean calendrical
;; authorities do NOT use DST in determining astronomical events:
;;  60
  "Minutes difference between daylight saving and standard time.
Default is for no daylight saving time."
  :type 'integer
  :group 'calendar-korean)

(define-obsolete-variable-alias 'korean-calendar-standard-time-zone-name
  'calendar-korean-standard-time-zone-name "23.1")

(defcustom calendar-korean-standard-time-zone-name
  '(if (< year 1928)
       "PMT"
     "KST")
  "Abbreviated name of standard time zone used for Korean Lunar calendar.
This is an expression depending on `year' because it changed
at 1928-01-01 00:00:00 from `PMT' to `KST'."
  :type 'sexp
  :group 'calendar-korean)

(define-obsolete-variable-alias 'korean-calendar-daylight-time-zone-name
  'calendar-korean-daylight-time-zone-name "23.1")

(defcustom calendar-korean-daylight-time-zone-name "KDT"
  "Abbreviated name of daylight saving time zone used for Korean Lunar calendar."
  :type 'string
  :group 'calendar-korean)

(define-obsolete-variable-alias 'korean-calendar-daylight-savings-starts
  'calendar-korean-daylight-saving-start "23.1")

(defcustom calendar-korean-daylight-saving-start nil
;; The correct value is as follows, but the Korean Lunar calendrical
;; authorities do NOT use DST in determining astronomical events:
;;  '(cond ((< 1986 year) (calendar-nth-named-day 1 0 4 year 10))
;;         ((= 1986 year) '(5 4 1986))
;;         (t nil))
  "Sexp giving the date on which daylight saving time starts.
Default is for no daylight saving time.  See documentation of
`calendar-daylight-savings-starts'."
  :type 'sexp
  :group 'calendar-korean)

(define-obsolete-variable-alias 'korean-calendar-daylight-savings-ends
  'calendar-korean-daylight-saving-end "23.1")

(defcustom calendar-korean-daylight-saving-end nil
;; The correct value is as follows, but the Korean Lunar calendrical
;; authorities do NOT use DST in determining astronomical events:
;;  '(if (<= 1986 year) (calendar-nth-named-day 1 0 9 year 11))
  "Sexp giving the date on which daylight saving time ends.
Default is for no daylight saving time.  See documentation of
`calendar-daylight-savings-ends'."
  :type 'sexp
  :group 'calendar-korean)

(define-obsolete-variable-alias 'korean-calendar-daylight-savings-starts-time
  'calendar-korean-daylight-saving-start-time "23.1")

(defcustom calendar-korean-daylight-saving-start-time 0
  "Number of minutes after midnight that daylight saving time starts.
Default is for no daylight saving time."
  :type 'integer
  :group 'calendar-korean)

(define-obsolete-variable-alias 'korean-calendar-daylight-savings-ends-time
  'calendar-korean-daylight-saving-end-time "23.1")

(defcustom calendar-korean-daylight-saving-end-time 0
  "Number of minutes after midnight that daylight saving time ends.
Default is for no daylight saving time."
  :type 'integer
  :group 'calendar-korean)

(defcustom calendar-korean-print-long-description nil
  "*If this variable set to t, long description will be used"
  :type 'boolean
  :group 'calendar-korean)

(define-obsolete-variable-alias 'korean-calendar-celestial-stem
  'calendar-korean-celestial-stem "23.1")

(defcustom calendar-korean-celestial-stem
  ["갑" "을" "병" "정" "무" "기" "경" "신" "임" "계"]
  "Prefixes used by `calendar-korean-sexagesimal-name'."
  :group 'calendar-korean
  :type '(vector (string :tag "甲")
                 (string :tag "乙")
                 (string :tag "丙")
                 (string :tag "丁")
                 (string :tag "戊")
                 (string :tag "己")
                 (string :tag "庚")
                 (string :tag "辛")
                 (string :tag "壬")
                 (string :tag "癸")))

(define-obsolete-variable-alias 'korean-calendar-terrestrial-branch
  'calendar-korean-terrestrial-branch "23.1")

(defcustom calendar-korean-terrestrial-branch
  ["자" "축" "인" "묘" "진" "사" "오" "미" "신" "유" "술" "해"]
  "Suffixes used by `calendar-korean-sexagesimal-name'."
  :group 'calendar-korean
  :type '(vector (string :tag "子")
                 (string :tag "丑")
                 (string :tag "寅")
                 (string :tag "卯")
                 (string :tag "辰")
                 (string :tag "巳")
                 (string :tag "午")
                 (string :tag "未")
                 (string :tag "申")
                 (string :tag "酉")
                 (string :tag "戌")
                 (string :tag "亥")))

;;; End of user options.

(defun calendar-korean-sexagesimal-name (n)
  "The N-th name of the Korean Lunar sexagesimal cycle.
N congruent to 1 gives the first name, N congruent to 2 gives the second name,
..., N congruent to 60 gives the sixtieth name."
  (format "%s%s"
          (aref calendar-korean-celestial-stem (% (1- n) 10))
          (aref calendar-korean-terrestrial-branch (% (1- n) 12))))

(defun calendar-korean-zodiac-sign-on-or-after (d)
  "Absolute date of first new Zodiac sign on or after absolute date D.
The Zodiac signs begin when the sun's longitude is a multiple of 30 degrees."
 (let* ((year (calendar-extract-year (calendar-gregorian-from-absolute d)))
         (calendar-time-zone (eval calendar-korean-time-zone)) ; uses year
         (calendar-daylight-time-offset
          calendar-korean-daylight-time-offset)
         (calendar-standard-time-zone-name
          calendar-korean-standard-time-zone-name)
         (calendar-daylight-time-zone-name
          calendar-korean-daylight-time-zone-name)
         (calendar-daylight-savings-starts
          calendar-korean-daylight-saving-start)
         (calendar-daylight-savings-ends
          calendar-korean-daylight-saving-end)
         (calendar-daylight-savings-starts-time
          calendar-korean-daylight-saving-start-time)
         (calendar-daylight-savings-ends-time
          calendar-korean-daylight-saving-end-time))
   (floor
    (calendar-astro-to-absolute
     (solar-date-next-longitude (calendar-astro-from-absolute d) 30)))))

(defun calendar-korean-new-moon-on-or-after (d)
  "Absolute date of first new moon on or after absolute date D."
  (let* ((year (calendar-extract-year (calendar-gregorian-from-absolute d)))
         (calendar-time-zone (eval calendar-korean-time-zone))
         (calendar-daylight-time-offset
          calendar-korean-daylight-time-offset)
         (calendar-standard-time-zone-name
          calendar-korean-standard-time-zone-name)
         (calendar-daylight-time-zone-name
          calendar-korean-daylight-time-zone-name)
         (calendar-daylight-savings-starts
          calendar-korean-daylight-saving-start)
         (calendar-daylight-savings-ends
          calendar-korean-daylight-saving-end)
         (calendar-daylight-savings-starts-time
          calendar-korean-daylight-saving-start-time)
         (calendar-daylight-savings-ends-time
          calendar-korean-daylight-saving-end-time))
    (floor
     (calendar-astro-to-absolute
      (lunar-new-moon-on-or-after (calendar-astro-from-absolute d))))))

(defun calendar-korean-month-list (start end)
  "List of starting dates of Korean Lunar months from START to END."
  (if (<= start end)
      (let ((new-moon (calendar-korean-new-moon-on-or-after start)))
        (if (<= new-moon end)
            (cons new-moon
                  (calendar-korean-month-list (1+ new-moon) end))))))

(defun calendar-korean-number-months (list start)
  "Assign month numbers to the lunar months in LIST, starting with START.
Numbers are assigned sequentially, START, START+1, ..., 11, with
half numbers used for leap months.  First and last months of list
are never leap months."
  (when list
    (cons (list start (car list))       ; first month
          ;; Remaining months.
          (if (zerop (- 12 start (length list)))
              ;; List is too short for a leap month.
              (calendar-korean-number-months (cdr list) (1+ start))
            (if (and (cddr list)        ; at least two more months...
                     (<= (nth 2 list)
                         (calendar-korean-zodiac-sign-on-or-after
                          (cadr list))))
                ;; Next month is a leap month.
                (cons (list (+ start 0.5) (cadr list))
                      (calendar-korean-number-months (cddr list) (1+ start)))
              ;; Next month is not a leap month.
              (calendar-korean-number-months (cdr list) (1+ start)))))))

(defun calendar-korean-compute-year (y)
  "Compute the structure of the Korean Lunar year for Gregorian year Y.
The result is a list of pairs (i d), where month i begins on absolute date d,
of the Korean months from the Korean month following the solstice in
Gregorian year Y-1 to the Korean month of the solstice of Gregorian year Y."
  (let* ((next-solstice (calendar-korean-zodiac-sign-on-or-after
                         (calendar-absolute-from-gregorian
                          (list 12 15 y))))
         (list (calendar-korean-month-list
                (1+ (calendar-korean-zodiac-sign-on-or-after
                     (calendar-absolute-from-gregorian
                      (list 12 15 (1- y)))))
                next-solstice))
         (next-sign (calendar-korean-zodiac-sign-on-or-after (car list))))
    (if (= (length list) 12)
        ;; No room for a leap month, just number them 12, 1, 2, ..., 11.
        (cons (list 12 (car list))
              (calendar-korean-number-months (cdr list) 1))
      ;; Now we can assign numbers to the list for y.
      ;; The first month or two are special.
      (if (or (> (car list) next-sign) (>= next-sign (cadr list)))
          ;; First month on list is a leap month, second is not.
          (append (list (list 11.5 (car list))
                        (list 12 (cadr list)))
                  (calendar-korean-number-months (cddr list) 1))
        ;; First month on list is not a leap month.
        (append (list (list 12 (car list)))
                (if (>= (calendar-korean-zodiac-sign-on-or-after (cadr list))
                        (nth 2 list))
                    ;; Second month on list is a leap month.
                    (cons (list 12.5 (cadr list))
                          (calendar-korean-number-months (cddr list) 1))
                  ;; Second month on list is not a leap month.
                  (calendar-korean-number-months (cdr list) 1)))))))

(defvar calendar-korean-year-cache
  ;; Maintainers: delete existing value, position point at start of
  ;; empty line, then call  M-: (calendar-korean-year-cache-init N)
'((2000 (12 730126) (1 730155) (2 730185) (3 730215) (4 730244) (5 730273)
		(6 730303) (7 730332) (8 730361) (9 730391) (10 730420) (11 730450))
  (2001 (12 730480) (1 730509) (2 730539) (3 730569) (4 730599) (4.5 730628)
		(5 730657) (6 730687) (7 730716) (8 730745) (9 730775) (10 730804)
		(11 730834))
  (2002 (12 730863) (1 730893) (2 730923) (3 730953) (4 730982) (5 731012)
		(6 731041) (7 731071) (8 731100) (9 731129) (10 731159) (11 731188))
  (2003 (12 731218) (1 731247) (2 731277) (3 731307) (4 731336) (5 731366)
		(6 731396) (7 731425) (8 731455) (9 731484) (10 731513) (11 731543))
  (2004 (12 731572) (1 731602) (2 731631) (2.5 731661) (3 731690) (4 731720)
		(5 731750) (6 731779) (7 731809) (8 731838) (9 731868) (10 731897)
		(11 731927))
  (2005 (12 731956) (1 731986) (2 732015) (3 732045) (4 732074) (5 732104)
		(6 732133) (7 732163) (8 732193) (9 732222) (10 732252) (11 732282))
  (2006 (12 732311) (1 732340) (2 732370) (3 732399) (4 732429) (5 732458)
		(6 732488) (7 732517) (7.5 732547) (8 732576) (9 732606) (10 732636)
		(11 732665))
  (2007 (12 732695) (1 732725) (2 732754) (3 732783) (4 732813) (5 732842)
		(6 732871) (7 732901) (8 732930) (9 732960) (10 732990) (11 733020))
  (2008 (12 733049) (1 733079) (2 733109) (3 733138) (4 733167) (5 733197)
		(6 733226) (7 733255) (8 733285) (9 733314) (10 733344) (11 733374))
  (2009 (12 733403) (1 733433) (2 733463) (3 733493) (4 733522) (5 733551)
		(5.5 733581) (6 733610) (7 733639) (8 733669) (9 733698) (10 733728)
		(11 733757))
  (2010 (12 733787) (1 733817) (2 733847) (3 733876) (4 733906) (5 733935)
		(6 733965) (7 733994) (8 734023) (9 734053) (10 734082) (11 734112))
  (2011 (12 734141) (1 734171) (2 734201) (3 734230) (4 734260) (5 734290)
		(6 734319) (7 734349) (8 734378) (9 734407) (10 734437) (11 734466))
  (2012 (12 734496) (1 734525) (2 734555) (3 734584) (3.5 734614) (4 734644)
		(5 734674) (6 734703) (7 734733) (8 734762) (9 734791) (10 734821)
		(11 734850))
  (2013 (12 734880) (1 734909) (2 734939) (3 734968) (4 734998) (5 735028)
		(6 735057) (7 735087) (8 735116) (9 735146) (10 735175) (11 735205))
  (2014 (12 735234) (1 735264) (2 735293) (3 735323) (4 735352) (5 735382)
		(6 735411) (7 735441) (8 735470) (9 735500) (9.5 735530) (10 735559)
		(11 735589))
  (2015 (12 735618) (1 735648) (2 735677) (3 735707) (4 735736) (5 735765)
		(6 735795) (7 735824) (8 735854) (9 735884) (10 735914) (11 735943))
  (2016 (12 735973) (1 736002) (2 736032) (3 736061) (4 736091) (5 736120)
		(6 736149) (7 736179) (8 736208) (9 736238) (10 736268) (11 736297))
  (2017 (12 736327) (1 736357) (2 736386) (3 736416) (4 736445) (5 736475)
		(5.5 736504) (6 736533) (7 736563) (8 736592) (9 736622) (10 736651)
		(11 736681))
  (2018 (12 736711) (1 736741) (2 736770) (3 736800) (4 736829) (5 736859)
		(6 736888) (7 736917) (8 736947) (9 736976) (10 737006) (11 737035))
  (2019 (12 737065) (1 737095) (2 737125) (3 737154) (4 737184) (5 737213)
		(6 737243) (7 737272) (8 737301) (9 737331) (10 737360) (11 737390))
  (2020 (12 737419) (1 737449) (2 737479) (3 737508) (4 737538) (4.5 737568)
		(5 737597) (6 737627) (7 737656) (8 737685) (9 737715) (10 737744)
		(11 737774)))
  "Alist of Korean year structures as determined by `korean-year'.
The default can be nil, but some values are precomputed for efficiency.")

(defun calendar-korean-year (y)
  "The structure of the Korean year for Gregorian year Y.
The result is a list of pairs (i d), where month i begins on absolute date d,
of the Korean months from the Korean month following the solstice in
Gregorian year Y-1 to the Korean month of the solstice of Gregorian year Y.
The list is cached in `calendar-korean-year-cache' for further use."
  (let ((list (cdr (assoc y calendar-korean-year-cache))))
    (or list
        (setq list (calendar-korean-compute-year y)
              calendar-korean-year-cache (append calendar-korean-year-cache
                                         (list (cons y list)))))
    list))

;; Maintainer use.
(defun calendar-korean-year-cache-init (year)
  "Insert an initialization value for `calendar-korean-year-cache' after point.
Computes values for 10 years either side of YEAR."
  (setq year (- year 10))
  (let (calendar-korean-year-cache end)
    (save-excursion
      (insert "'(")
      (dotimes (n 21)
        (princ (cons year (calendar-korean-compute-year year))
               (current-buffer))
        (insert (if (= n 20) ")" "\n"))
        (setq year (1+ year)))
      (setq end (point)))
    (save-excursion
      ;; fill-column -/+ 5.
      (while (and (< (point) end)
                  (re-search-forward "^.\\{65,75\\})" end t))
        (delete-char 1)
        (insert "\n")))
    (indent-region (point) end)))

(defun calendar-korean-to-absolute (date)
  "The number of days elapsed between the Gregorian date 12/31/1 BC and DATE.
DATE is a Korean Lunar date (cycle year month day).  The Gregorian date
Sunday, December 31, 1 BC is imaginary."
  (let* ((cycle (car date))
         (year (cadr date))
         (month (nth 2 date))
         (day (nth 3 date))
         (g-year (+ (* (1- cycle) 60)  ; years in prior cycles
                    (1- year)          ; prior years this cycle
                    -2636)))           ; years before absolute date 0
    (+ (1- day)                        ; prior days this month
       (cadr                    ; absolute date of start of this month
        (assoc month (append (memq (assoc 1 (calendar-korean-year g-year))
                                   (calendar-korean-year g-year))
                             (calendar-korean-year (1+ g-year))))))))

(define-obsolete-function-alias 'calendar-absolute-from-korean
  'calendar-korean-to-absolute "23.1")

(defun calendar-korean-from-absolute (date)
  "Compute Korean Lunar date (cycle year month day) corresponding to absolute DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let* ((g-year (calendar-extract-year
                  (calendar-gregorian-from-absolute date)))
         (c-year (+ g-year 2695))
         (list (append (calendar-korean-year (1- g-year))
                       (calendar-korean-year g-year)
                       (calendar-korean-year (1+ g-year)))))
    (while (<= (cadr (cadr list)) date)
      ;; The first month on the list is in Korean year c-year.
      ;; Date is on or after start of second month on list...
      (if (= 1 (caar (cdr list)))
          ;; Second month on list is a new Korean year...
          (setq c-year (1+ c-year)))
      ;; ...so first month on list is of no interest.
      (setq list (cdr list)))
    (list (/ (1- c-year) 60)
          ;; Remainder of c-year/60 with 60 instead of 0.
          (1+ (mod (1- c-year) 60))
          (caar list)
          (1+ (- date (cadr (car list)))))))

;; Bound in calendar-generate.
(defvar displayed-month)
(defvar displayed-year)

;;;###holiday-autoload
(defun holiday-korean-new-year ()
  "Date of Korean New Year, if visible in calendar.
Returns (((MONTH DAY YEAR) TEXT)), where the date is Gregorian."
  (let ((m displayed-month)
        (y displayed-year)
        korean-new-year)
    ;; In the Gregorian calendar, CNY falls between Jan 21 and Feb 20.
    ;; Jan is visible if displayed-month = 12, 1, 2; Feb if d-m = 1, 2, 3.
    ;; If we shift the calendar forward one month, we can do a
    ;; one-sided test, namely: d-m <= 4 means CNY might be visible.
    (calendar-increment-month m y 1)    ; shift forward a month
    (and (< m 5)
         (calendar-date-is-visible-p
          (setq korean-new-year
                (calendar-gregorian-from-absolute
                 (cadr (assoc 1 (calendar-korean-year y))))))
         (list
          (list korean-new-year
                (format "Korean New Year (%s)"
                        (calendar-korean-sexagesimal-name (+ y 57))))))))

;;;###holiday-autoload
(defun holiday-korean-winter-solstice ()
  "Date of Korean winter solstice, if visible in calendar.
Returns (((MONTH DAY YEAR) TEXT)), where the date is Gregorian."
  (when (memq displayed-month '(11 12 1)) ; is December visible?
    (list (list (calendar-gregorian-from-absolute
                 (calendar-korean-zodiac-sign-on-or-after
                  (calendar-absolute-from-gregorian
                   (list 12 15 (if (eq displayed-month 1)
                                   (1- displayed-year)
                                 displayed-year)))))
                "Winter Solstice Festival"))))

;;;###holiday-autoload
(defun holiday-korean (month day string)
  "Holiday on Korean MONTH, DAY called STRING.
If MONTH, DAY (Korean) is visible, returns the corresponding
Gregorian date as the list (((month day year) STRING)).
Returns nil if it is not visible in the current calendar window."
  ;; This is calendar-nongregorian-visible-p adapted for the form of
  ;; korean dates: (cycle year month day) as opposed to (month day year).
  (let* ((m1 displayed-month)
         (y1 displayed-year)
         (m2 displayed-month)
         (y2 displayed-year)
         ;; Absolute date of first/last dates in calendar window.
         (start-date (progn
                       (calendar-increment-month m1 y1 -1)
                       (calendar-absolute-from-gregorian (list m1 1 y1))))
         (end-date (progn
                     (calendar-increment-month m2 y2 1)
                     (calendar-absolute-from-gregorian
                      (list m2 (calendar-last-day-of-month m2 y2) y2))))
         ;; Local date of first/last date in calendar window.
         (local-start (calendar-korean-from-absolute start-date))
         ;; A basic optimization.  We only care about the year part,
         ;; and the Korean year can only change if Jan or Feb are
         ;; visible.  FIXME can we do more?
         (local-end (if (memq displayed-month '(12 1 2 3))
                        (calendar-korean-from-absolute end-date)
                      local-start))
         ;; When Korean New Year is visible on the far right of the
         ;; calendar, what is the earliest Korean month in the
         ;; previous year that might still visible?  This test doesn't
         ;; have to be precise.
         (local (if (< month 10) local-end local-start))
         (cycle (car local))
         (year (cadr local))
         (date (calendar-gregorian-from-absolute
                (calendar-korean-to-absolute (list cycle year month day)))))
    (if (calendar-date-is-visible-p date)
        (list (list date string)))))

;;;###cal-autoload
(defun calendar-korean-date-string (&optional date)
  "String of Korean Lunar date of Gregorian DATE.
Defaults to today's date if DATE is not given."
  (let* ((a-date (calendar-absolute-from-gregorian
                  (or date (calendar-current-date))))
         (c-date (calendar-korean-from-absolute a-date))
         (cycle (car c-date))
         (year (cadr c-date))
         (month (nth 2 c-date))
         (day (nth 3 c-date))
         (this-month (calendar-korean-to-absolute
                      (list cycle year month 1)))
         (next-month (calendar-korean-to-absolute
                      (list (if (= year 60) (1+ cycle) cycle)
                            (if (= (floor month) 12) (1+ year) year)
                            ;; Remainder of (1+(floor month))/12, with
                            ;; 12 instead of 0.
                            (1+ (mod (floor month) 12))
                            1)))
         (m-cycle (% (+ (* year 5) (floor month)) 60)))
	(if calendar-korean-print-long-description
		(format "%s년 %02d월 %02d일 (%s) %s년 %s %s일"
				(+ (* (1- cycle) 60) ;; years in prior cycles
				   (1- year)		 ;; prior years this cycle
				   -2636)			 ;; years before absolute date 0
				(floor month)
				day
				(if (not (integerp month))
					"윤달"
				  "평달")
				(calendar-korean-sexagesimal-name year)
				(if (integerp month)
					(format "%s월" (calendar-korean-sexagesimal-name
									(+ (* 12 year) month 50)))
				  "")
				(calendar-korean-sexagesimal-name (+ a-date 15)))
	  (format "%s%s/%02d/%02d"
			  (if (not (integerp month))
				  "윤) "
				"")
			  (+ (* (1- cycle) 60)	 ;; years in prior cycles
				 (1- year)			 ;; prior years this cycle
				 -2636)				 ;; years before absolute date 0
			  (floor month)
			  day ))))

;;;###cal-autoload
(defun calendar-korean-print-date ()
  "Show the Korean Lunar date equivalents of date."
  (interactive)
  (message "Computing Korean Lunar date...")
  (message "Korean Lunar date: %s"
           (calendar-korean-date-string (calendar-cursor-to-date t))))

(defun calendar-mod (m n)
  "Non-negative reminder of M/N with N instead of 0."
  (1+ (mod (1- m) n)))

(define-obsolete-function-alias 'calendar-print-korean-date
  'calendar-korean-print-date "23.1")

(defun calendar-korean-months-to-alist (l)
  "Make list of months L into an assoc list."
  (and l (car l)
       (if (and (cdr l) (cadr l))
           (if (= (car l) (floor (cadr l)))
               (append
                (list (cons (format "%s (평달)" (car l)) (car l))
                      (cons (format "%s (윤달)" (car l)) (cadr l)))
                (calendar-korean-months-to-alist (cddr l)))
             (append
              (list (cons (number-to-string (car l)) (car l)))
              (calendar-korean-months-to-alist (cdr l))))
         (list (cons (number-to-string (car l)) (car l))))))

(defun calendar-korean-months (c y)
  "A list of the months in cycle C, year Y of the Korean calendar."
  (memq 1 (append
           (mapcar (lambda (x)
                     (car x))
                   (calendar-korean-year (calendar-extract-year
                                           (calendar-gregorian-from-absolute
                                            (calendar-korean-to-absolute
                                             (list c y 1 1))))))
           (mapcar (lambda (x)
                     (if (> (car x) 11) (car x)))
                   (calendar-korean-year (calendar-extract-year
                                           (calendar-gregorian-from-absolute
                                            (calendar-korean-to-absolute
                                             (list (if (= y 60) (1+ c) c)
                                                   (if (= y 60) 1 y)
                                                   1 1)))))))))

;;;###cal-autoload
(defun calendar-korean-goto-date (date &optional noecho)
  "Move cursor to Korean date DATE.
Echo Korean date unless NOECHO is t."
  (interactive
   (let* ((c (jm-calendar-korean-from-absolute
              (calendar-absolute-from-gregorian
               (calendar-current-date))))
          (input-year (calendar-read
					   "Year in Korean (>0): "
					   '(lambda (x) (< 0 x) )
					   (int-to-string (extract-calendar-year c))))
		  (c-year (+ input-year 2636))
		  (cycle (1+ (/ c-year 60)))
		  (year (calendar-mod (1+ c-year) 60))
		  (month-list (make-korean-month-assoc-list
					   (calendar-korean-months cycle year)))
		  (month (cdr (assoc
					   (completing-read "Korean calendar month: "
										month-list nil t)
					   month-list)))
		  (last (if (= month
					   (car (cdr (cdr
								  (calendar-korean-from-absolute
								   (+ 29
									  (calendar-absolute-from-korean
									   (list cycle year month 1))))))))
					30
				  29))
		  (day (calendar-read
				(format "Korean calendar day (1-%d): " last)
				'(lambda (x) (and (<= 1 x) (<= x last))))))
	 (list (list cycle year month day))))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-absolute-from-korean date)))
  (or noecho (calendar-print-korean-date)))


(defun make-korean-month-assoc-list (l)
  "Make list of months L into an assoc list."
  (if (and l (car l))
      (if (and (cdr l) (car (cdr l)))
          (if (= (car l) (floor (car (cdr l))))
              (append
               (list (cons (format "%s (평달)" (car l)) (car l))
                     (cons (format "%s (윤달)" (car l)) (car (cdr l))))
               (make-korean-month-assoc-list (cdr (cdr l))))
            (append
             (list (cons (int-to-string (car l)) (car l)))
             (make-korean-month-assoc-list (cdr l))))
        (list (cons (int-to-string (car l)) (car l))))))


(defun jm-calendar-absolute-from-korean (date)
  "The number of days elapsed between the Gregorian date 12/31/1 BC and DATE.
The Gregorian date Sunday, December 31, 1 BC is imaginary."
  (let* ((month (car date))
         (day (car (cdr date)))
         (year (car (cdr (cdr date))))
         (g-year year))	;; years before absolute date 0
    (+ (1- day)			;; prior days this month
       (car
        (cdr ;; absolute date of start of this month
         (assoc month (append (memq (assoc 1 (calendar-korean-year g-year))
                                    (calendar-korean-year g-year))
                              (calendar-korean-year (1+ g-year)))))))))


(defun jm-calendar-korean-from-absolute (l-date)
  "Compute Korean Lunar date (cycle year month day) corresponding to absolute DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let* ((g-year (extract-calendar-year
                  (calendar-gregorian-from-absolute l-date)))
         (c-year (+ g-year 2695))
         (list (append (calendar-korean-year (1- g-year))
                       (calendar-korean-year g-year)
                       (calendar-korean-year (1+ g-year))))
		 cycle mod-year)
    (while (<= (car (cdr (car (cdr list)))) l-date)
      ;; the first month on the list is in Korean Lunar year c-year
      ;; date is on or after start of second month on list...
      (if (= 1 (car (car (cdr list))))
          ;; second month on list is a new Korean Lunar year
          (setq c-year (1+ c-year)))
      ;; ...so first month on list is of no interest
      (setq list (cdr list)))
	(setq cycle (/ (1- c-year) 60))
	(setq mod-year (calendar-mod c-year 60))
    (list
	 (car (car list))
	 (1+ (- l-date (car (cdr (car list)))))
	 (+ (* (1- cycle) 60)		 ;; years in prior cycles
		(1- mod-year)			 ;; prior years this cycle
		-2636)					 ;; years before absolute date 0
	 )))

(define-obsolete-function-alias 'calendar-goto-korean-date
  'calendar-korean-goto-date "23.1")

(defvar date)

;; To be called from diary-list-sexp-entries, where DATE is bound.
;;;###diary-autoload
(defun diary-korean-date ()
  "Korean calendar equivalent of date diary entry."
  (format "음력: %s" (calendar-korean-date-string date)))


(defun diary-lunar-date (month day year &optional leap mark)
  "Specific date(s) diary entry.
Entry applies if date is MONTH, DAY, YEAR if `european-calendar-style' is nil,
and DAY, MONTH, YEAR if `european-calendar-style' is t.  DAY, MONTH, and YEAR
can be lists of integers, the constant t, or an integer.  The constant t means
all values.

An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar."
  (let* ((dd (if european-calendar-style
				 month
			   day))
		 (mm (if european-calendar-style
				 day
			   month))
		 (mm (+ mm (if leap 0.5 0)))
		 (c-date (calendar-absolute-from-gregorian
				  (or date (calendar-current-date))))
		 (a-date (jm-calendar-korean-from-absolute c-date))
		 
		 (m (extract-calendar-month a-date))
		 (y (extract-calendar-year a-date))
		 (d (extract-calendar-day a-date)))
	(if (and
		 (or (and (listp dd) (memq d dd))
			 (equal d dd)
			 (eq dd t))
		 (or (and (listp mm) (memq m mm))
			 (equal m mm)
			 (eq mm t))
 		 (or (and (listp year) (memq y year))
 			 (equal y year)
 			 (eq year t)))
        (cons mark (format entry (format "음력 %d월 %d일" m d))))))


(defun diary-lunar-anniversary (month day &optional year leap mark)
  "Anniversary diary entry.
Entry applies if date is the anniversary of MONTH, DAY, YEAR if
`european-calendar-style' is nil, and DAY, MONTH, YEAR if
`european-calendar-style' is t.  Diary entry can contain `%s' or `%s%d'; the
%d will be replaced by the number of years since the MONTH DAY, YEAR and the
%s will be replaced by lunar date.

An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar."
  (let* ((d (if european-calendar-style
                month
              day))
         (m (if european-calendar-style
                day
              month))
		 (m (+ m (if leap 0.5 0)))
		 (c-date (calendar-absolute-from-gregorian
				  (or date (calendar-current-date))))
		 (a-date (jm-calendar-korean-from-absolute c-date))
		 (mm (extract-calendar-month a-date))
		 (yy (extract-calendar-year a-date))
		 (dd (extract-calendar-day a-date))
         (diff (if (not (booleanp year)) (- yy year) 100)))
	(if (and (> diff 0) (calendar-date-equal (list m d yy) (list mm dd yy)))
		(cons mark (format entry (format "음력 %d월 %d일" m d) diff)))))


(eval-after-load "calendar"
  '(progn
	 (define-key calendar-mode-map "pl" 'calendar-korean-print-date)
	 (define-key calendar-mode-map "gl" 'calendar-korean-goto-date)
	 ))

(provide 'cal-korean)

;; arch-tag: 7e5b7e0d-676c-47e3-8696-93e7ea0ab644
;;; cal-korean.el ends here
