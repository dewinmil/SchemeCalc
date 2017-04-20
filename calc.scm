; scheme calculator by Miles De Wind - 4/18/2017
; utilized ideas from GoZoner @ stack overflow for
; my arithmatic function

(define (make-list)
    (let ((val (read)))
      (let ((return (list)))
        (set! return (append return (list val)))
          ;useless conditionals / only way i knew to make it loop
          (let loop ((times 0)) 
            ;allows large formula's to be placed on multiple lines 
            (cond ((not (char=? #\newline (peek-char console-i/o-port)))
              (let ((val (read))) 
                (set! return (append return (list val)))
                (loop (+ times 1))))
              (else
                ;arithmatic makes it a list instead of 
                ;a list of a list / cant return plain return
                (arithmatic (list return))))))))   


(define (arithmatic val)
  (cond ((null? val) 0)
    ((null? (cdr val)) (car val))
    (else (let ((input1 (car val)) (input2 (cadr val)) (input3 (cddr val)))
      ((case input2 ((*) *) ((/) /) ((%) modulo) ((+) +) ((-) -))
        input1 (arithmatic input3))))))


(define (calc)
  (display #\newline)
  (display "Note that the calculator will compute right to left:")
  (display #\newline)
  (display "1 + 2 * 5 - 6 % 14 ==> (1 + (2 * (5 - (6 % 14)))):")
  (display #\newline)
  (display #\newline)
  (display "Note that you must also hit enter twice after your input")
  (display #\newline)
  (display "which allows you to place large formula's onto multiple")
  (display #\newline)
  (display "lines")
  (display #\newline)

  (let loop ((times 0))
      (let ((input (make-list)))
        (cond ((not (equal? input '(end)))
          (let ((val (arithmatic input)))
          (display val)
          (display #\newline)
          (loop (+ times 1))))
          (else "done")))))
     
