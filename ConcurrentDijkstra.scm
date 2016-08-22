

(module concurrent (wait make-synchronizer)
  (import chicken scheme)
  (use srfi-18)
  ; helper procedure to make a thread wait
  (define (wait name dur)
    (if (eq? (thread-name (current-thread)) name)
        (thread-sleep! dur)))

  ; a factory of synchronizers
  ; a synchronizer is an operation that wraps a procedure as a critical
  ; section; all procedures wrapped by the synchronizer will use the same
  ; lock, consequently, all  those procedures will be mutually exclusive between
  ; each other
  (define (make-synchronizer mx)
    (lambda (f)
      (define (synchronized-f . args)
        (mutex-lock! mx)
        (let ((result (apply f args)))
          (mutex-unlock! mx)
          result))
      synchronized-f))  )

(define G1 '(
             (a . ((b . 7) (f . 9) (c . 14)))
             (b . ((a . 7) (d . 15) (f . 10)))
             (c . ((a . 14) (f . 2) (e . 9)))
             (d . ((b . 15) (f . 11) (e . 6)))
             (e . ((c . 9) (d . 6)))  
             (f . ((c . 2) (d . 11) (b . 10) (a . 9)))))

(define (vertices graph)
(map (lambda (n) (car n)) graph))

;Deleta um atomo de uma lista
(define (delete item list)
  (cond
    ((null? list) list)
    ((equal? item (car list)) (cdr list))
    (else (cons (car list) (delete item (cdr list))))))

;Devolve a aridade de uma lista
(define (count-list list num)
  (cond
    ((null? list)num)
    (else (count-list (cdr list) (+ 1 num)))))
    
;;Define um resp com distancia infinita até todos os vertices
(define (passo1 list2 list1)
  (cond
    ((null? list1)(reverse list2))
    (else (passo1 (cons (cons (car list1) +inf.0)list2) (cdr list1)))))

;Define como distancia zero o caminho até a origem
(define (set-origem temp resp origem)
  (cond
    ((null? resp) (reverse temp))
     ((equal? (car(car resp)) origem) (set-origem (cons (cons origem 0)temp) (cdr resp) origem))
     (else (set-origem (cons (car resp) temp) (cdr resp) origem))))

;;;; As funções abaixo deverão ser executadas pelas threads produtoras

;;;; Funçao abaixo deverá ser executado pela thread consumidora principal

;Analisa um LIST de um vertice analisado e compara com as distancias do resp, escolhendo as distancias minimas
  
(define (main temp resp LIST)
		  (cond
		  	((null? LIST) resp)
		  	((null? resp) (main '() temp (cdr LIST)))
		  	((equal? (car(car LIST)) (car (car resp))) (main (cons (cons (car(car LIST)) (min (cdr(car resp))(cdr(car LIST))))temp) (cdr resp) LIST))
		  	(else (main (cons (car resp)temp) (cdr resp) LIST)))) 	

;Devolve o vetor dos vizinhos de um vertice (dados retirados dos grafo)
(define (vert-vizinhos vert graph)
  (cond
    ((equal? vert (car(car graph)))(cdr(car graph)))
     (else (vert-vizinhos vert (cdr graph)))))                              

;Busca no resp (vetor com os menores caminhos até um vertice) o peso do vertice que está sendo analisado   
(define (achar-peso vert resp)
	(cond ((equal? vert (car(car resp)))(cdr(car resp)))
	(else (achar-peso vert (cdr resp)))))

;Processa a lista de vizinhos junto com o peso do vertice e devolve uma lista da distancia real obtida na analise	
(define (soma-peso peso LIST vizinhos)
	(cond 
	((null? vizinhos) LIST)
	(else (soma-peso peso (cons(cons (car(car vizinhos)) (+ peso (cdr(car vizinhos)))) LIST) (cdr vizinhos)))))   

;Teste se tudo está funcionando por hora

(define (pertence a b)
	(cond 
  	((equal? a (car b))#t)
    ((null? b)#f)
    (else (pertence a (cdr b)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FRONTIER;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (no-path temp resp)
	(cond
		((equal? +inf.0 (cdr(car resp)))(no-path (cons (car resp) temp)(cdr resp)))
		((< (cdr(car resp)) +inf.0)(no-path temp (cdr resp)))
		((null? resp)temp)))


(define (delete-unexplored temp unexplored)
	(cond
		((null? temp) unexplored)
		(else (delete-unexplored (cdr temp) (delete (car temp) unexplored)))))
		 

(define make-frontier
    (lambda (frontier)
    (define update-frontier
        (lambda (resp unexplored) 
		   (set! frontier (delete-unexplored(no-path '() resp) unexplored))
            frontier))

    (lambda (m)
    (cond
          ((eq? m 'update)update-frontier)
          (else (error "Metodo desconhecido"))))))		 
		  
		  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;UNEXPLORED;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Lista os vertices do grafo

(define (make-unexplored)
  (lambda (unexplored)
   
    (define def-unexplored
        (lambda (graph)
        	(display unexplored)
            (set! unexplored (vertices graph))
            unexplored))
           
    (define update-unexplored
    	(lambda(vertice)
          (set! unexplored (delete vertice unexplored))
           unexplored))
	(define get
          (lambda ()
            unexplored))
    (lambda (m)
    (cond
          ((eq? m 'def)def-unexplored)
          ((eq? m 'update)update-unexplored)
          ((eq? m 'get) get)
          (else (error "Metodo desconhecido"))))))
		  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;RESP;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-resp)
    (lambda (resp)
    (define def-resp
        (lambda (unexplored source)
            (set! resp (set-origem '() (passo1 '() unexplored) source))
            resp))

    (define consume
    	(lambda(LIST)
        (set! resp (main '() resp LIST))))
                      
    (lambda (m)
    (cond
          ((eq? m 'def)def-resp)
          ((eq? m 'consume)consume)
          (else (error "Metodo desconhecido"))))))
		  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MONITOR;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-buffer-monitor)
  (lambda (frontier) 
  (define count 0)
  (define buffer '())
  (define mx (make-mutex))
  (define synchronizer (make-synchronizer mx))
  (define cv-c (make-condition-variable))
  (define cv-p (make-condition-variable))
  
  (define (put value)
    (let while ()
      (if (full?)
        (begin
          (wait cv-p)
          (while))))
    (set! count (+ 1 count))
    (set! buffer (cons value buffer))
    (notify cv-c)))
    
  (define (get)
    (let while ()
      (if (empty?)
          (begin
            (wait cv-c)
            (while))))
		(let* ((temp (buffer)))		
		(cond ((null? buffer)(set! count 0))
		(set! buffer (cdr buffer))
		(notify cv-p)
    (car temp))))
  
  (define (full?)
    (= count (count-list frontier 0)))
  
  (define (empty?)
    (= count 0))
  
  (define (wait cv)
    (mutex-unlock! mx cv))
  
  (define (notify cv)
    (condition-variable-signal! cv))
  
  (define (notify-all cv)
    (condition-variable-broadcast! cv))
  
  (lambda (m)
    (cond ((eq? m 'put) (synchronizer put))
          ((eq? m 'get) (synchronizer get))
          ((eq? m 'full?) full?)
          ((eq? m 'empty?) empty?))))
		  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PRODUTOR E CONSUMIDOR;;;;;;;;;;
		  
(define (consumer resp buffer frontier unexplored)
  (let* ((LIST (buffer 'get)))
  	(cond ((not (null? LIST)))
    	(begin
      	(print ((resp  'consume)LIST))
      	(consumer resp buffer ((frontier 'update)resp unexplored) ((unexplored 'get)))))))
      
(define (producer graph vertice buffer unexplored resp frontier)
  (let loop ()
  	(cond ((pertence vertice frontier)(
  	(let* ((LIST (soma-peso (achar-peso vertice resp) '() (vert-vizinhos vertice graph))))
   		(cond ((not (null? LIST))
        (begin
        	((unexplored 'update)vert)
          (set! frontier (cdr frontier))
        	((buffer 'put) LIST)))))))
        (else (loop)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (threads thread-list vertices-list)
	(cond
  	((null? vertices-list)((cons (make-thread (lambda () (consumer resp buffer frontier unexplored)) (car vertices-list) thread-list) thread-list)))
    (else (cons (make-thread (lambda () (producer graph (car frontier) buffer unexplored resp frontier)) (car vertices-list) thread-list) (cdr vertices-list)))))

(define (CDijkstra source graph)
  (define frontier (make-frontier '()))
  (define buffer ((make-buffer-monitor) frontier))
  (define resp ((make-resp) '()))
  (define unexplored ((make-unexplored) '()))
  ((unexplored 'def) graph)
  ((resp 'def) unexplored source)
  ((frontier 'update) resp unexplored)
  (map thread-join! (map thread-start! thread-list) (list 5 5 5)))
  
  (display (CDijkstra 'f G1))
