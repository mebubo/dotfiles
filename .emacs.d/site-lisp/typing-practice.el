;;; typing-practice.el --- Typing practice

;; Copyright (C) 2006 Alfred J. Fazio

;; Author: Alfred J. Fazio
;; URL: https://raw.github.com/mebubo/dotfiles/master/.emacs.d/site-lisp/typing-practice.el
;; Version: 0.1
;; Maintainer: Alfred J. Fazio <alfred dot fazio at gmail dot com>
;; Keywords: games practice typing
;; Created: 2006-01-10
;; X-URL:   http://alfredfazio.ws/

;; Thanks to Quentin Carbonneaux for valuable input.

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Usage

;; Rename this file to typing-practice.el if it isn't already then place it as
;; well as words.el in your Emacs lisp path (eg. site-lisp) and add to your
;; .emacs file:
;;   (load-library "typing-practice")

;; Some options are available under Help:Customize Options that are specific to
;; typing-practice in Applications/Games/Typing Practice

;; To start the program, type 'M-x practice-words ENTER'

;; -----------------------
;; Sorted by rank of usage

(setq top-1000-words-english '("the" "of" "to" "and" "a" "in" "is" "it" "you" "that" 
"he" "was" "for" "on" "are" "with" "as" "I" "his" "they" "be" "at" "one" "have" "this" 
"from" "or" "had" "by" "hot" "word" "but" "what" "some" "we" "can" "out" "other" "were" 
"all" "there" "when" "up" "use" "your" "how" "said" "an" "each" "she" "which" "do" 
"their" "time" "if" "will" "way" "about" "many" "then" "them" "write" "would" "like" 
"so" "these" "her" "long" "make" "thing" "see" "him" "two" "has" "look" "more" "day" 
"could" "go" "come" "did" "number" "sound" "no" "most" "people" "my" "over" "know" 
"water" "than" "call" "first" "who" "may" "down" "side" "been" "now" "find" "any" 
"new" "work" "part" "take" "get" "place" "made" "live" "where" "after" "back" "little" 
"only" "round" "man" "year" "came" "show" "every" "good" "me" "give" "our" "under" 
"name" "very" "through" "just" "form" "sentence" "great" "think" "say" "help" "low" 
"line" "differ" "turn" "cause" "much" "mean" "before" "move" "right" "boy" "old" "too" 
"same" "tell" "does" "set" "three" "want" "air" "well" "also" "play" "small" "end" 
"put" "home" "read" "hand" "port" "large" "spell" "add" "even" "land" "here" "must" 
"big" "high" "such" "follow" "act" "why" "ask" "men" "change" "went" "light" "kind" 
"off" "need" "house" "picture" "try" "us" "again" "animal" "point" "mother" "world" 
"near" "build" "self" "earth" "father" "head" "stand" "own" "page" "should" "country" 
"found" "answer" "school" "grow" "study" "still" "learn" "plant" "cover" "food" "sun" 
"four" "between" "state" "keep" "eye" "never" "last" "let" "thought" "city" "tree" 
"cross" "farm" "hard" "start" "might" "story" "saw" "far" "sea" "draw" "left" "late" 
"run" "don't" "while" "press" "close" "night" "real" "life" "few" "north" "open" 
"seem" "together" "next" "white" "children" "begin" "got" "walk" "example" "ease" 
"paper" "group" "always" "music" "those" "both" "mark" "often" "letter" "until" 
"mile" "river" "car" "feet" "care" "second" "book" "carry" "took" "science" "eat" 
"room" "friend" "began" "idea" "fish" "mountain" "stop" "once" "base" "hear" "horse" 
"cut" "sure" "watch" "color" "face" "wood" "main" "enough" "plain" "girl" "usual" 
"young" "ready" "above" "ever" "red" "list" "though" "feel" "talk" "bird" "soon" 
"body" "dog" "family" "direct" "pose" "leave" "song" "measure" "door" "product" 
"black" "short" "numeral" "class" "wind" "question" "happen" "complete" "ship" "area" 
"half" "rock" "order" "fire" "south" "problem" "piece" "told" "knew" "pass" "since" 
"top" "whole" "king" "space" "heard" "best" "hour" "better" "true" "during" "hundred" 
"five" "remember" "step" "early" "hold" "west" "ground" "interest" "reach" "fast" 
"verb" "sing" "listen" "six" "table" "travel" "less" "morning" "ten" "simple" "several" 
"vowel" "toward" "war" "lay" "against" "pattern" "slow" "center" "love" "person" "money" 
"serve" "appear" "road" "map" "rain" "rule" "govern" "pull" "cold" "notice" "voice" 
"unit" "power" "town" "fine" "certain" "fly" "fall" "lead" "cry" "dark" "machine" "note" 
"wait" "plan" "figure" "star" "box" "noun" "field" "rest" "correct" "able" "pound" 
"done" "beauty" "drive" "stood" "contain" "front" "teach" "week" "final" "gave" "green" 
"oh" "quick" "develop" "ocean" "warm" "free" "minute" "strong" "special" "mind" "behind" 
"clear" "tail" "produce" "fact" "street" "inch" "multiply" "nothing" "course" "stay" 
"wheel" "full" "force" "blue" "object" "decide" "surface" "deep" "moon" "island" "foot" 
"system" "busy" "test" "record" "boat" "common" "gold" "possible" "plane" "stead" "dry" 
"wonder" "laugh" "480" "ago" "ran" "check" "game" "shape" "equate" "hot" "miss" 
"brought" "heat" "snow" "tire" "bring" "yes" "distant" "fill" "east" "paint" "language" 
"among" "open" "seem" "together" "next" "white" "children" "begin" "got" "walk" 
"example" "ease" "paper" "group" "always" "music" "those" "both" "mark" "often" "letter" 
"until" "mile" "river" "car" "feet" "care" "second" "book" "carry" "took" "science" 
"eat" "room" "friend" "began" "idea" "fish" "mountain" "stop" "once" "base" "hear" 
"horse" "cut" "sure" "watch" "color" "face" "wood" "main" "enough" "plain" "girl" 
"usual" "young" "ready" "above" "ever" "red" "list" "though" "feel" "talk" "bird" 
"soon" "body" "dog" "family" "direct" "pose" "leave" "song" "measure" "door" "product" 
"black" "short" "numeral" "class" "wind" "question" "happen" "complete" "ship" "area" 
"half" "rock" "order" "fire" "south" "problem" "piece" "told" "knew" "pass" "since" 
"top" "whole" "king" "space" "heard" "best" "hour" "better" "true" "during" "hundred" 
"five" "remember" "step" "early" "hold" "west" "ground" "interest" "reach" "fast" "verb" 
"sing" "listen" "six" "table" "travel" "less" "morning" "ten" "simple" "several" 
"vowel" "toward" "war" "lay" "against" "pattern" "slow" "center" "love" "person" 
"money" "serve" "appear" "road" "map" "rain" "rule" "govern" "pull" "cold" "notice" 
"voice" "unit" "power" "town" "fine" "certain" "fly" "fall" "lead" "cry" "dark" 
"machine" "note" "wait" "plan" "figure" "star" "box" "noun" "field" "rest" "correct" 
"able" "pound" "done" "beauty" "drive" "stood" "contain" "front" "teach" "week" "final" 
"gave" "green" "oh" "quick" "develop" "ocean" "warm" "free" "minute" "strong" "special" 
"mind" "behind" "clear" "tail" "produce" "fact" "street" "inch" "multiply" "nothing" 
"course" "stay" "wheel" "full" "force" "blue" "object" "decide" "surface" "deep" "moon" 
"island" "foot" "system" "busy" "test" "record" "boat" "common" "gold" "possible" 
"plane" "stead" "dry" "wonder" "laugh" "480" "ago" "ran" "check" "game" "shape" 
"equate" "hot" "miss" "brought" "heat" "snow" "tire" "bring" "yes" "distant" "fill" 
"east" "paint" "language" "among" "open" "seem" "together" "next" "white" "children" 
"begin" "got" "walk" "example" "ease" "paper" "group" "always" "music" "those" "both" 
"mark" "often" "letter" "until" "mile" "river" "car" "feet" "care" "second" "book" 
"carry" "took" "science" "eat" "room" "friend" "began" "idea" "fish" "mountain" 
"stop" "once" "base" "hear" "horse" "cut" "sure" "watch" "color" "face" "wood" "main" 
"enough" "plain" "girl" "usual" "young" "ready" "above" "ever" "red" "list" "though" 
"feel" "talk" "bird" "soon" "body" "dog" "family" "direct" "pose" "leave" "song" 
"measure" "door" "product" "black" "short" "numeral" "class" "wind" "question" 
"happen" "complete" "ship" "area" "half" "rock" "order" "fire" "south" "problem" 
"piece" "told" "knew" "pass" "since" "top" "whole" "king" "space" "heard" "best" 
"hour" "better" "true" "during" "hundred" "five" "remember" "step" "early" "hold" 
"west" "ground" "interest" "reach" "fast" "verb" "sing" "listen" "six" "table" 
"travel" "less" "morning" "ten" "simple" "several" "vowel" "toward" "war" "lay" 
"against" "pattern" "slow" "center" "love" "person" "money" "serve" "appear" "road" 
"map" "rain" "rule" "govern" "pull" "cold" "notice" "voice" "unit" "power" "town" 
"fine" "certain" "fly" "fall" "lead" "cry" "dark" "machine" "note" "wait" "plan" 
"figure" "star" "box" "noun" "field" "rest" "correct" "able" "pound" "done" "beauty" 
"drive" "stood" "contain" "front" "teach" "week" "final" "gave" "green" "oh" "quick" 
"develop" "ocean" "warm" "free" "minute" "strong" "special" "mind" "behind" "clear" 
"tail" "produce" "fact" "street" "inch" "multiply" "nothing" "course" "stay" "wheel" 
"full" "force" "blue" "object" "decide" "surface" "deep" "moon" "island" "foot" 
"system" "busy" "test" "record" "boat" "common" "gold" "possible" "plane" "stead" "dry" 
"wonder" "laugh" "480" "ago" "ran" "check" "game" "shape" "equate" "hot" "miss" 
"brought" "heat" "snow" "tire" "bring" "yes" "distant" "fill" "east" "paint" "language" 
"among"))

(setq typing-practice-html-code '("<html>" "</html>" "<head>" "</head>" "<body>" 
"</body>" "<p>" "</p>" "<div>" "</div>" "<table>" "<tr>" "<td>" "</table>" "</td>" 
"</tr>" "<style>" "</style>" "<script>" "</script>" "<a href=\"\">" "</a>" 
"<img src=\"\">" "<input>" "<select>" "<textbox>" "</select>" "</textbox>" "<h1>"
"<h2>" "</h1>" "<input type=\"text\">" "<input type=\"checkbox\">" "<p style=\"\">"
"<a href=\"\" onclick=\"\">" "<select name=\"\" id=\"\">" 
"<input type=\"text\" name=\"\" id=\"\">" "<form action=\"\" method=\"post\">"
"<table cellspacing=\"0\" cellpadding=\"0\">" "<title>" "</title>"
"<style type=\"text/css\">" "<script language=\"javascript\">"))

(setq typing-practice-css-code '("font-family:" "font-size:" "font-weight:"
"text-decoration:" "font-family: Arial;" "font-size: 14px;" "font-weight: 600;"
"text-decoration: none;" "border: 1px solid black;" "margin-left:" "margin-right:"
"font-size: 16px;" "cursor: pointer;" "display: none;"))

(setq typing-practice-javascript-code '("function" "{" "}" ">=" "<=" "for" "while"
"(" ")" ";" "return true;" "return false;" "if (!i)" "for (i = 0; i < a.length; i++)" 
"function checkForm () { return true; }" "var i = new Array();" "var t = 0;"
"if (i && a.length <= 100)" "document" "window.location" "document.getElementById('')"
"var el = document.getElementById('element');" "document.onkeydown = function () {}"
"switch (i) {" "case 0: break;" "window.location.href='';" "e.style.display='none';"
"e.style.fontSize='12px';" "if (j < 5 || a == 'something')" "} else {"))

(setq typing-practice-shell-code '("ls" "cd" "grep" "sed" "nano" "sudo" "emacs" "screen"
"~" "ls /tmp" "ls /etc" "sudo nano /etc" "cat" "|" ">" "echo" "awk"
"if [ -z $A ]; then" "nano -w" "ls -l" "ps aux" "grep -R ''" "ln -sf" "rm -rf" "mkdir"
"rmdir" "./" "make" "make install" "./configure" "ssh-agent" "tmux attach"
"sudo /etc/init.d/" "screen -RD" "screen -wipe" "startx" "ssh-add"
"scp" "find . -name '*s'" "for i in 1 2 3; do" "done"))

(setq typing-practice-php-code '("<?" "?>" "<?=" "$" "$i" "foreach ($someArray as $a)"
"$i = 0;" "$a = Array();" "function fun($a, $b) {" "$_GET['']" "$_POST['']"
"$_SERVER['PHP_SELF']" "if (!$i)" "echo $i.\"<br>\";"))

(setq typing-practice-java-code '("while" "void" "true" "toString()" "throws" "throw"
"sysout" "static" "return" "public" "private" "null" "new" "main" "long" "interface"
"int" "implements" "if" "for" "float" "finally" "final" "false" "extends" "class"
"char" "catch" "byte" "boolean" "abstract" "Thread" "String" "Runnable" "Object"
"Map" "Long" "List<String>()" "LinkedList" "Integer" "HashMap" "Float" "Exception"
"Double" "Collection" "Arrays" "ArrayList"))

;; --------------------------


(random t)

(defgroup typing-practice '() 
  "Typing Practice" 
  :group 'games)

(defcustom typing-practice-wordlist 
  top-1000-words-english
  "* Wordlist to use for typing practice."
  :tag "Typing practice wordlist"
  :group 'typing-practice
  :type 'variable
  :options '(top-1000-words-english))

(defcustom typing-practice-consult-threshold
  0
  "* A misspelled word should be consulted at threshold"
  :tag "Consulting threshold"
  :group 'typing-practice
  :type 'integer
  :options '(0 1 5 10))

(defcustom typing-practice-time-threshold
  2.5
  "* Threshold number of seconds for words.  If when making an entry this threshold
is reached, the entry is considered wrong, and as a result will be practiced again
soon thereafter"
  :tag "Time threshold"
  :group 'typing-practice
  :type 'float)

;; Buffer of words typed wrong
(setq wrong-word-list '())

(setq current-word-list '())
(setq current-word-list-length 0)
(setq choice-count 0)

(defun time-seconds (time)
  (+ (* (car time) 65536) (cadr time)))

(when (not (functionp 'caddr))
  (defun caddr (list)
    (car (cdr (cdr list)))))

(defun choose-word ()
  (setq choice-count (1+ choice-count))
  (if (and (>= choice-count typing-practice-consult-threshold) (> (length wrong-word-list) 0))
      (progn
	(setq choice-count 0)
	(pop wrong-word-list)
	)
    (nth (random current-word-list-length) current-word-list))
  )

(defun practice-words (&optional number-words starting-point)
  (interactive)
  (set-list typing-practice-wordlist number-words starting-point)
  (practice-typing nil)
  )

(defun set-word-list (wordlist)
  (setq current-word-list wordlist)
  (setq current-word-list-length (length current-word-list))
  )

(defun practice-php-code (&optional number-words starting-point)
  (interactive)
  (set-list typing-practice-php-code number-words starting-point)
  (practice-typing t)
  )

(defun practice-html-code (&optional number-words starting-point)
  (interactive)
  (set-list typing-practice-html-code number-words starting-point)
  (practice-typing t)
  )

(defun practice-css-code (&optional number-words starting-point)
  (interactive)
  (set-list typing-practice-css-code number-words starting-point)
  (practice-typing t)
  )

(defun practice-javascript-code (&optional number-words starting-point)
  (interactive)
  (set-list typing-practice-javascript-code number-words starting-point)
  (practice-typing)
  )

(defun practice-shell-code (&optional number-words starting-point)
  (interactive)
  (set-list typing-practice-shell-code number-words starting-point)
  (practice-typing t)
  )

(defun practice-java-code (&optional number-words starting-point)
  (interactive)
  (set-list typing-practice-java-code number-words starting-point)
  (practice-typing nil)
  )

(defun set-list (list &optional number-words starting-point)
  (let ((list-length (length list)))
    (if (not number-words) 
	(setq number-words (string-to-number (read-string (concat "How many words? [" (int-to-string list-length) "]: ") nil nil (int-to-string list-length)))))
    (if (not starting-point)
	(setq starting-point (string-to-number (read-string "Start at [1]: " nil nil "1"))))
    (set-word-list (butlast 
		    (reverse 
		     (butlast (reverse list) 
			      (1- starting-point))) 
		    (- (- list-length (1- starting-point)) number-words)))
    )
  )

(defun practice-typing (uses-spaces)
  (let ((word nil)
	(correct nil))
    (while (= 1 1) ;; Keep going until they press C-g.
      (let* ((old-time (current-time))
	     (old-seconds (time-seconds old-time))
	     (old-micro   (/ (caddr old-time) 1000000.0)))
	(setq word (choose-word))
	(if uses-spaces
	    (setq correct (string= word (read-string (concat word "\n"))))
	  (setq correct (string= word (read-no-blanks-input (concat word "\n")))))
	(let* ((new-time (current-time))
	       (new-seconds (time-seconds new-time))
	       (new-micro   (/ (caddr new-time) 1000000.0)))
	  (if (and correct (>= (- (+ new-seconds new-micro)
				  (+ old-seconds old-micro))
			       typing-practice-time-threshold))
	      (too-long word))
	  )
	(unless correct (wrong-answer word))
	)
      )
    )
  )

(defun wrong-answer (wrong-entry)
  (push wrong-entry wrong-word-list)
  (message "WRONG!!")
  (sleep-for 1)
  )

(defun too-long (entry)
  (push entry wrong-word-list)
  )

(provide 'typing-practice)
;;; typing-practice.el ends here
