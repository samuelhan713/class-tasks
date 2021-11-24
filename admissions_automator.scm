;Samuel Han
;samuel.han@stonybrook.edu
#lang racket


;a)
(define *student-records* '()) ; global variable
 
(define (read-applications file-name)
  (define input (open-input-file file-name))
  (set! *student-records* (read input)))

(read-applications "a1.txt")

;b)

(define (add-to-list a aList)
  (append aList (list a)))

; returns a list of all scholarship recipients' full names
(define (find-scholarship-recipients student-records)
  (define (find-scholarship-recipients-aux student-rec ret)
    (if (null? student-rec)
        ret
        (if (equal? (is-scholarship-recipient (car student-rec) 0) #t)
            (find-scholarship-recipients-aux (cdr student-rec) (add-to-list (caar student-rec) ret))
            (find-scholarship-recipients-aux (cdr student-rec) ret)))) 
  (find-scholarship-recipients-aux *student-records* '())) 
          
 ; determines if a student is a scholarship recipient
(define (is-scholarship-recipient student-record n)
    (cond ((= n 3) ;state
           (if (not (or (equal? (car student-record) '(Montana)) (equal? (car student-record) '(Idaho))))
               #f
               (is-scholarship-recipient (cdr student-record) (+ n 1))))
           ((= n 7) ;major
            (if (not (or (equal? (car student-record) '(Philosophy)) (equal? (car student-record) '(History)) (equal? (car student-record) '(Sociology))))
                #f
                (is-scholarship-recipient (cdr student-record) (+ n 1))))
           ((= n 8) ; gpa
            (if (not (> (car student-record) 3.8))
                #f
                (is-scholarship-recipient (cdr student-record) (+ n 1))))
           ((= n 9) ; scholarship
            (if (not (equal? (car student-record) 'Applying4Scholarship))
                #f
                #t))
           (else (is-scholarship-recipient (cdr student-record) (+ n 1)))))

; Test case
(find-scholarship-recipients *student-records*) 




;c)
; calculates the number of students in the file
(define (num-of-students student-records state) 
  (define (num-of-students-aux student-records num)
    (if (null? student-records)
        num
        (if (matches-state (car student-records) state 0)
            (num-of-students-aux (cdr student-records) (+ 1 num))
            (num-of-students-aux (cdr student-records) num))))        
  (num-of-students-aux student-records 0))


; checks whether a student is from a given state
(define (matches-state student-record state n) 
  (cond ((null? student-record) #f)
        ((and (= n 3) (equal? (car student-record) state)) #t)
        (else (matches-state (cdr student-record) state (+ n 1)))))                


 ; returns the gpa of a student that matches the given state
(define (total-gpa-aux student-record n)
  (if (= n 8)
      (car student-record)
      (total-gpa-aux (cdr student-record) (+ 1 n)))) 
      

; calculates the total gpa of a student body
(define (total-gpa student-records sum state) 
  (cond ((null? student-records) sum)
        ((matches-state (car student-records) state 0) (total-gpa (cdr student-records) (+ sum (total-gpa-aux (car student-records) 0)) state)) 
        (else (total-gpa (cdr student-records) sum state))))  


; returns the average gpa of the student body
(define (average-gpa state student-records)
  (/ (total-gpa student-records 0 state) (num-of-students student-records state)))

; Test case
(average-gpa '(California) *student-records*)





;d)
; finds the state from a student's profile
(define (find-state student-record n) 
  (if (= n 3)
      (car student-record)
      (find-state (cdr student-record) (+ n 1))))


 ; returns the highest gpa in a given state
(define (get-highest-gpa state student-records)
  (define (get-highest-gpa-aux state student-records highest)
    (cond ((null? student-records) highest)
          ((not (matches-state (car student-records) state 0)) (get-highest-gpa-aux state (cdr student-records) highest))
          ((> (find-gpa (car student-records) 0) highest) (get-highest-gpa-aux state (cdr student-records) (find-gpa (car student-records) 0)))
          (else (get-highest-gpa-aux state (cdr student-records) highest))))
  (get-highest-gpa-aux state student-records 0))

; returns the gpa of a student
(define (find-gpa student-record n) 
  (if (= n 8)
      (car student-record)
      (find-gpa (cdr student-record) (+ n 1))))


; returns the name of the student that matches the state and gpa
(define (match-gpa-state student-records state gpa) 
  (if (equal? (find-state (car student-records) 0) state)
      (if (equal? (find-gpa (car student-records) 0) gpa)
          (car (car student-records))
          (match-gpa-state (cdr student-records) state gpa))
      (match-gpa-state (cdr student-records) state gpa)))
      

; returns the highest gpa from a given state
(define (highest-gpa state student-records)
  (list (match-gpa-state student-records state (get-highest-gpa state student-records)) (get-highest-gpa state student-records)))


; Test case
(highest-gpa '(Washington) *student-records*)
    


;e)
;reverses a list
(define (reverse-aux u v)
  (cond ((null? u) v)
        ((reverse-aux (cdr u) (cons (car u) v)))))

;gets rid of unnecessary elements from a list
(define (rid-unnecessary alist)
  (define (rid-unnecessary-aux alist)
    (cond ((not (equal? (caaar alist) 'z)) alist)
          (else (rid-unnecessary-aux (cdr alist)))))
  (rid-unnecessary-aux (reverse alist)))
  

; finds the smallest element from a list
(define (find-smallest alist blist)
  (define (find-smallest-aux alist smallest blist)
    (cond ((null? alist) smallest)
          ((occurs? (caaar alist) blist) (find-smallest-aux (cdr alist) smallest blist))
          ((string<? (symbol->string (caaar alist)) (symbol->string (caar smallest))) (find-smallest-aux (cdr alist) (car alist) blist))
          (else (find-smallest-aux (cdr alist) smallest blist))))  
  (find-smallest-aux alist '((z)) blist))


; sorts the list of states in alphabetical order
(define (sort alist ret)
  (define (sort-aux alist blist ret)
    (cond ((null? alist) ret)
          (else (sort-aux (cdr alist) blist (add-to-list (find-smallest blist ret) ret))))) 
  (sort-aux alist alist ret)) 


; creates the histogram
(define (create-histogram student-records)
  (define (create-histogram-aux student-records list1 list2)
    (cond ((null? student-records) list1)
          ((occurs? (find-state (car student-records) 0) list2) (create-histogram-aux (cdr student-records) list1 list2))
          (else (add-to-list (list (find-state (car student-records) 0) (num-of-students student-records (find-state (car student-records) 0)))
                             (create-histogram-aux (cdr student-records) list1 (add-to-list (find-state (car student-records) 0) list2))))))
  (create-histogram-aux student-records '() '()))


; checks if 'a' occurs in aList
(define (occurs? a aList) 
  (cond ((null? aList) #f)
        ((not (pair? aList)) (eqv? a aList))
        ((equal? a aList))
        (else (or (occurs? a (car aList))
                   (occurs? a (cdr aList))))))

(define (histogram student-records)
  (reverse (rid-unnecessary (sort (create-histogram student-records) '()))))

; Test case
(histogram *student-records*)



