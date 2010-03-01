(require :cffi)

;; parameters for test
(defparameter *in-fn* "/Users/younwook/freetype/tutorial/in3.png")
(defparameter *out-fn* "/Users/younwook/freetype/tutorial/tt1.png")
(defparameter *hist-r-fn* "/Users/younwook/freetype/tutorial/hist-r.png")
(defparameter *hist-g-fn* "/Users/younwook/freetype/tutorial/hist-g.png")
(defparameter *hist-b-fn* "/Users/younwook/freetype/tutorial/hist-b.png")

(cffi:define-foreign-library fttutlib
  (t (:default "/Users/younwook/freetype/tutorial/libfttut")))

(cffi:use-foreign-library fttutlib)

(cffi:defcstruct draw-buffer 
  (width :int)
  (height :int)
  (data :pointer))

(cffi:defcfun ("read_png" read-png) :pointer
  (fn :string))

(cffi:defcfun ("make_new_draw_buffer" make-draw-buffer) :pointer
  (width :int)
  (height :int))

(cffi:defcfun ("delete_draw_buffer" delete-draw-buffer) :int
  (db :pointer))

(cffi:defcfun ("write_png" write-png) :int
  (fn :string)
  (db :pointer))

;; (defun make-draw-buffer (w h) 
;;   (let* ((db (cffi:foreign-alloc :unsigned-char 
;; 				 :count (cffi:foreign-type-size 'draw-buffer))))
;;     (progn
;;       (setf (cffi:foreign-slot-value db 'draw-buffer 'width) w
;; 	    (cffi:foreign-slot-value db 'draw-buffer 'height) h)
;;       (setf (cffi:foreign-slot-value db 'draw-buffer 'data)
;; 	    (cffi:foreign-alloc :unsigned-char :count (* 3 w h)))
;;       (loop for i from 0 to (- (* 3 w h)) do 
;; 	   (setf (cffi:mem-ref (cffi:foreign-slot-value db 'draw-buffer 'data) :unsigned-char i) 0))
;;       db)))

;; (defun delete-draw-buffer (db)
;;   (let ((data (cffi:foreign-slot-value db 'draw-buffer 'data)))
;;     (progn
;;       (cffi:foreign-free data)
;;       (cffi:foreign-free db))))

(defun clone-draw-buffer (db)
  (make-draw-buffer 
   (cffi:foreign-slot-value db 'draw-buffer 'width)
   (cffi:foreign-slot-value db 'draw-buffer 'height)))

(defun print-image-size (db)
  (format t "height is ~s ~%" 
	  (cffi:translate-from-foreign 
	   (cffi:foreign-slot-value db 'draw-buffer 'height) :int)))

(defmacro assign-draw-buffer-parameter (prefix db-name member)
  (let ((aa (format nil "~s-~s" prefix member )))
  `( ,aa (cffi:foreign-slot-value ,db-name 'draw-buffer ',member))))

;; 코드 최적화 전의 put-pixel. get-pixel 코드이다. 이 함수들의 최적화를 통하여
;; 20% 이상의 성능 향상이 발생하였다.
;; 
;; (defun put-pixel (db x y color-code)
;;   (let* ((data-width (cffi:foreign-slot-value db 'draw-buffer 'width))
;; 	 (data-height (cffi:foreign-slot-value db 'draw-buffer 'height))
;; 	 (data-ptr (cffi:foreign-slot-value db 'draw-buffer 'data)))
;; 	 (progn
;; 	   (cffi:incf-pointer data-ptr 
;; 			      (* (+ (* y data-width) x) (list-length color-code)))
;; 	   (loop for c in color-code do
;; 		(progn 
;; 		  (setf (cffi:mem-ref data-ptr :unsigned-char) c)
;; 		  (cffi:incf-pointer data-ptr))))))
;;
;; (defun get-pixel (db x y color-code-length)
;;   (let* ((data-width (cffi:foreign-slot-value db 'draw-buffer 'width))
;; 	 (data-height (cffi:foreign-slot-value db 'draw-buffer 'height))
;; 	 (data-ptr (cffi:foreign-slot-value db 'draw-buffer 'data)))
;; 	 (progn
;; 	   (cffi:incf-pointer data-ptr
;; 			      (* (+ (* y data-width) x) color-code-length))
;; 	   (loop for i from 0 to (- color-code-length 1) collect
;; 		(cffi:mem-ref data-ptr :unsigned-char i)))))

(defun put-pixel (db x y color-code)
  (let* ((data-width (cffi:foreign-slot-value db 'draw-buffer 'width))
	 (data-ptr (cffi:foreign-slot-value db 'draw-buffer 'data))
	 (data-offset (* (+ (* y data-width) x) 3)))
    (progn
      (setf (cffi:mem-ref data-ptr :unsigned-char data-offset) (first color-code))
      (setf (cffi:mem-ref data-ptr :unsigned-char (+ data-offset 1)) (second color-code))
      (setf (cffi:mem-ref data-ptr :unsigned-char (+ data-offset 2)) (third color-code)))))


(defun get-pixel (db x y color-code-length)
  (let* ((data-width (cffi:foreign-slot-value db 'draw-buffer 'width))
	 (data-ptr (cffi:foreign-slot-value db 'draw-buffer 'data))
	 (data-offset (* (+ (* y data-width) x) color-code-length)))
    (list (cffi:mem-ref data-ptr :unsigned-char data-offset)
	  (cffi:mem-ref data-ptr :unsigned-char (+ data-offset 1))
	  (cffi:mem-ref data-ptr :unsigned-char (+ data-offset 2)))))

(defun make-vline (db x y height color-code)
  (loop for i from y to (+ y height) do
       (put-pixel db x i color-code)))

(defun make-hline (db x y width color-code)
  (loop for i from x to (+ x width) do
       (put-pixel db i y color-code)))

(defun make-box (db x y w h color-rgb)
  (let* ((data-ptr (cffi:foreign-slot-value db 'draw-buffer 'data))
	 (data-width (cffi:foreign-slot-value db 'draw-buffer 'width))
	 (data-height (cffi:foreign-slot-value db 'draw-buffer 'height)))
    (progn
      (loop for ix from x to (min (+ x w) data-width) do
	   (loop for iy from y to (min (+ y h) data-height) do
		(progn
		  (put-pixel db ix iy color-rgb))))
      (format t "~%ptr:~s width:~s height:~s~%" data-ptr data-width data-height))))


;; destructuring-bind 같은 명령어(?)를 사용하는 것도 코드를 쉽게 만들 수 있는 좋은
;; 아이디어를 제공한다. 
;; 
;; (destructuring-bind (data-ptr data-width data-height)     
;;     (mapcar #'(lambda (arg)   
;; 		(cffi:foreign-slot-value *test-db* 'draw-buffer arg))             
;; 	    '(data width height)))

;; 이웃 화소 목록 구하기
(defun 4-neighbors (p)
  (list (list (+ (car p) 1) (cadr p))
  	(list (- (car p) 1) (cadr p))
  	(list (car p) (+ (cadr p) 1))
  	(list (car p) (- (cadr p) 1))))

(defun 4-diagonal-neighbors (p)
  (list (list (+ (car p) 1) (+ (cadr p) 1))
  	(list (- (car p) 1) (+ (cadr p) 1))
  	(list (+ (car p) 1) (- (cadr p) 1))
  	(list (- (car p) 1) (- (cadr p) 1))))

(defun 8-neighbors (p)
  (append (4-neighbors p)
	  (4-diagonal-neighbors p)))

;;
;; 어떤 값이 (a b) 로 표현되는 범위 안에 있는지 나타낸다.
;; (in-rangep 3 (2 5)) => T
;; (in-rangep 5 (1 4)) => NIL
(defun in-rangep (a V)
  (and (<= a (max (car V) (cadr V)))
       (>= a (min (car V) (cadr V)))))

;;
;; 어떤 점의 명암도를 계산하는 함수
(defun grayscale (db p)
  (let ((color-code (get-pixel db (car p) (cadr p) 3)))
    (floor (loop for i in color-code sum i) (length color-code))))

;; 인접성 테스트... 두 개의 점이 인접하는지 테스트한다.
;; 그나저나 이런 함수가 ㅆㅡㄹ모가 있을지는...?
(defun 4-adjacency-p (db p q V)
  (let ((grayscale-p (grayscale db p))
	(grayscale-q (grayscale db q)))
    (and (in-rangep grayscale-p V)
	 (in-rangep grayscale-q V)
	 (member-if #'(lambda (a) (equal a q)) 
		    (4-neighbors p)))))

(defun 8-adjacency-p (db p q V)
  (let ((grayscale-p (grayscale db p))
	(grayscale-q (grayscale db q)))
    (and (in-rangep grayscale-p V)
	 (in-rangep grayscale-q V)
	 (member-if #'(lambda (a) (equal a q)) 
		    (8-neighbors p)))))

;; 이 혼합 인접성 함수에 대해서는 "디지탈 영상처리" p67 참조
;; 약간 복잡한 감이 있으며, 향후 검증 필요. 
(defun m-adjacency-p (db p q V)
  (let ((grayscale-p (grayscale db p))
	(grayscale-q (grayscale db q)))
    (and (in-rangep grayscale-p V)
	 (in-rangep grayscale-q V)
	 (or (member-if #'(lambda (a) (equal a q)) 
			(4-neighbors p))
	     (and (member-if #'(lambda (a) (equal a q))
			     (4-diagonal-neighbors p))
		  (not (member-if #'(lambda (a) (in-rangep (grayscale db a) V))
				 (intersection (4-neighbors p)
					       (4-neighbors q)
					       :test #'equal))))))))

;; 강제로 범위 안으로 숫자를 맞추는 함수. 비율에 맞추는 것이 아니라 그냥 잘라 버린다.
;; 함수로 하면 느려질까봐 매크로로 일단 처리함. thres-apply 는 람다형태로 사용할 수 없음. 
(defmacro thres-apply (a V)
  `(max (min (car ,V) (cadr ,V)) (min (max (car ,V) (cadr ,V)) ,a)))

;; image 상에서 사용할 수 있을법한 mapcar.... 라고나 할까...
;; 실수를 정수로 normalization 하고 범위 내에 맞추는 역할은
;; 이 함수 내에서 자체적으로 처리하도록 하자. 
(defun mapcar-image (db-source db-target transform-function)
  (let* ((data-width (cffi:foreign-slot-value db-source 'draw-buffer 'width))
	 (data-height (cffi:foreign-slot-value db-source 'draw-buffer 'height)))
    (loop for y from 0 to (- data-height 1) do
	 (loop for x from 0 to (- data-width 1) do
	      (put-pixel db-target x y
			 (mapcar
			  (lambda (a) 
			    (floor (thres-apply (funcall transform-function a) '(0 255))))
			  (get-pixel db-source x y 3)))))))

;; 이런저런 변환 함수들 (기초적인)
(defun make-log-tf-func (c b)
  (lambda (a)
    (* c (log (+ 1 a) b))))
(defun make-power-tf-func (c gamma)
  (lambda (a)
    (* c (expt a gamma))))

;; 히스토그램 데이터 생성기

;; 이미지 버퍼 내의 모든 픽셀에 대해서 해당 함수를 실행하는 traverse 함수
;; proc 에 전달되는 함수들은 (lambda (db color x y) 의 형태를 ㄸㅏ른다. 
(defun traverse-draw-buffer (db proc)
  (let* ((data-width (cffi:foreign-slot-value db 'draw-buffer 'width))
	 (data-height (cffi:foreign-slot-value db 'draw-buffer 'height)))
    (loop for y from 0 to (- data-height 1) do
	 (loop for x from 0 to (- data-width 1) do
	      (funcall proc db (get-pixel db x y 3) x y)))))

;; 히스토그램 생성을 위한 함수들. make-hist-proc-red, make-hist-proc-blue,
;; make-hist-proc-green 등은 편의상 만들어 놓은 것들이며, 실제로 사용되는 것은
;; make-hist-proc, make-hist, make-histogram-image 와 같은 것들이다. 
;; make-hist 와 같은 함수는 array 크기를 임의대로 256 으로 세팅하는 등,
;; 범용성 면에서 수정하여야 할 것들이 많다. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-hist-proc (hist-array proc)
  (lambda (db color x y)
    (let ((a (aref hist-array (funcall proc color))))
      (setf (aref hist-array (funcall proc color)) (+ a 1)))))

;;
;; make-proc 은 make-hist-proc 을 통상적으로 사용하며, 
;; proc 은 3개의 색 코드 중 하나를 선택하는 데 사용되는데,
;; proc 에는 first, second, third 와 같은 함수들을 지정하면 된다. 
(defun make-hist (db make-proc select-proc)
  (let* ((hist-array (make-array 256 :initial-element 0))
	 (hist-proc (funcall make-proc hist-array select-proc)))
    (progn
      (traverse-draw-buffer db hist-proc)
      hist-array)))

(defun make-histogram-image (hist-array)
  (let* ((max-num (loop for i across hist-array maximize i))
	 (min-num (loop for i across hist-array minimize i))
	 (hist-db (make-draw-buffer 265 110)))
    (progn
      (loop for i from 0 to 255 do
	   (let* ((x (+ i 5))
		  (height (* (/ (aref hist-array i) max-num) 100))
		  (y (- 105 height)))
	     (make-vline hist-db (floor x) (floor y) (floor height) '(255 255 255))))
      hist-db)))

(defun make-hist-proc-red (hist-array)
  (lambda (db color x y)
    (let ((a (aref hist-array (car color))))
      (setf (aref hist-array (car color)) (+ a 1)))))

(defun make-hist-proc-green (hist-array)
  (lambda (db color x y)
    (let ((a (aref hist-array (second color))))
      (setf (aref hist-array (second color)) (+ a 1)))))

(defun make-hist-proc-blue (hist-array)
  (lambda (db color x y)
    (let ((a (aref hist-array (third color))))
      (setf (aref hist-array (third color)) (+ a 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 거리 측정 : 두 좌표를 받아서 그 거리를 리턴한다. 
;; 유클리드 거리와 도시 구획형 거리
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun euclidean-distance (p q)
  (sqrt (+ (expt (- (car p) (car q)) 2)
	   (expt (- (cadr p) (cadr q)) 2))))

(defun 4-distance (p q)
  (+ (abs (- (car p) (car q)))
     (abs (- (cadr p) (cadr q)))))

(defun 8-distance (p q)
  (max (abs (- (car p) (car q)))
       (abs (- (cadr p) (cadr q)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun tt ()
  (let* ((draw-buffer (read-png *in-fn*))
	 (ar-hist-red (make-hist draw-buffer #'make-hist-proc #'first))
	 (ar-hist-green (make-hist draw-buffer #'make-hist-proc #'second))
	 (ar-hist-blue (make-hist draw-buffer #'make-hist-proc #'third))
	 (db-hist-red (make-histogram-image ar-hist-red))
	 (db-hist-green (make-histogram-image ar-hist-green))
	 (db-hist-blue (make-histogram-image ar-hist-blue)))
    (progn
      (write-png *out-fn* draw-buffer)
      (write-png *hist-r-fn* db-hist-red)
      (write-png *hist-g-fn* db-hist-green)
      (write-png *hist-b-fn* db-hist-blue)
      (delete-draw-buffer db-hist-red)
      (delete-draw-buffer db-hist-green)
      (delete-draw-buffer db-hist-blue)
      (delete-draw-buffer draw-buffer))))

;; 히스토그램 균일화 화잘 향상 방법
;; 
(defun make-hist-equalize-function (ar-hist)
  (let* ((num-pixel (loop for i across ar-hist sum i)))
    (lambda (k)
      (floor (* (/ (loop for n from 0 to k sum (aref ar-hist n)) num-pixel) 255)))))

(defun make-transform-func-graph (proc fn)
  (let* ((db (make-draw-buffer 270 270))
	 )
    (progn
      (loop for i from 0 to 255 do
	   (put-pixel db (+ i 10) (- 255 (funcall proc i)) '(255 255 255)))
      (write-png fn db))))

;; 어떠한 이산 함수를 받아서, 이를 테이블로 미리 계산한 후, 나중에 레퍼해서 리턴하는
;; 함수를 생성해 낸다. 
(defun make-precalc-discrete-func (proc low hi)
  (let* ((ar-result (make-array (+ (- hi low) 1))))
    (progn
      (loop for i from low to hi do
	   (setf (aref ar-result i) (funcall proc i)))
      (lambda (a) (aref ar-result a)))))

;; 이산 변환을 수행하는 mapcar-image 함수 2호. 1호는 범용성이 너무 낮다. 
(defun mapcar-image-d-2 (db-source db-target transform-function)
  (let* ((thres-width (- (cffi:foreign-slot-value db-source 'draw-buffer 'width) 1))
	 (thres-height (- (cffi:foreign-slot-value db-source 'draw-buffer 'height) 1)))
    (loop for y from 0 to thres-height do
	 (loop for x from 0 to thres-width do
	      (put-pixel db-target x y
			 (funcall transform-function db-source
				  x y 
				  (get-pixel db-source x y 3)))))))

(defun hist-equalize (db)
  (let* ((db-tgt (clone-draw-buffer db))
	 (hist-r (make-hist db #'make-hist-proc #'first))
	 (hist-g (make-hist db #'make-hist-proc #'second))
	 (hist-b (make-hist db #'make-hist-proc #'third))
	 (eqlize-proc (lambda (db x y c)
			(list
			 (funcall (make-hist-equalize-function hist-r) (first c))
			 (funcall (make-hist-equalize-function hist-g) (second c))
			 (funcall (make-hist-equalize-function hist-b) (third c))))))
    (progn
      (mapcar-image-d-2 db db-tgt eqlize-proc)
      db-tgt)))

(defun hist-equalize-2 (db)
  (let* ((db-tgt (clone-draw-buffer db))
	 (hist-r (make-hist db #'make-hist-proc #'first))
	 (hist-g (make-hist db #'make-hist-proc #'second))
	 (hist-b (make-hist db #'make-hist-proc #'third))
	 (f1 (make-hist-equalize-function hist-r))
	 (f2 (make-hist-equalize-function hist-g))
	 (f3 (make-hist-equalize-function hist-b))
	 (eqlize-proc (lambda (db x y c)
			(list
			 (funcall f1 (first c))
			 (funcall f2 (second c))
			 (funcall f3 (third c))))))
    (progn
      (mapcar-image-d-2 db db-tgt eqlize-proc)
      db-tgt)))

(defun hist-equalize-fast (db)
  (let* ((db-tgt (clone-draw-buffer db))
	 (hist-r (make-hist db #'make-hist-proc #'first))
	 (hist-g (make-hist db #'make-hist-proc #'second))
	 (hist-b (make-hist db #'make-hist-proc #'third))
	 (f1 (make-precalc-discrete-func (make-hist-equalize-function hist-r) 0 255))
	 (f2 (make-precalc-discrete-func (make-hist-equalize-function hist-g) 0 255))
	 (f3 (make-precalc-discrete-func (make-hist-equalize-function hist-b) 0 255))
	 (eqlize-proc (lambda (db x y c)
			(list
			 (funcall f1 (first c))
			 (funcall f2 (second c))
			 (funcall f3 (third c))))))
    (progn
      (mapcar-image-d-2 db db-tgt eqlize-proc)
      db-tgt)))
			 

(defmacro run-time (bl)
  `(block nil
    (let* ((start-time (get-internal-real-time)))
      (return (values ,bl
		      (/ (- (get-internal-real-time) start-time) 
			 internal-time-units-per-second))))))

;; (run-time (tt))

;; 영역에 ㄸㅏ른 영상 향상 수행을 위한 함수들
(defun make-image-sum-recursive (n db x y w h proc)
  (if (>= y h)
      n
      (if (< x w)
	  (make-image-sum-recursive (+ n (funcall proc (get-pixel db x y 3)))
				    db
				    (+ x 1)
				    y w h proc)
	  (make-image-sum-recursive n db 0 (+ y 1) w h proc))))

(defun make-image-sum-recursive-2 (n db x y w h proc)
  (if (>= y h)
      n
      (if (< x w)
	  (make-image-sum-recursive (+ n (funcall proc (get-pixel db x y 3)))
				    db
				    (+ x 1)
				    y w h proc)
	  (make-image-sum-recursive n db 0 (+ y 1) w h proc))))


(defun make-image-sum-loop (db w h proc)
  (loop for y from 0 to (- h 1) sum
       (loop for x from 0 to (- w 1) sum
	    (funcall proc (get-pixel db x y 3))))))

(defun image-average (db proc)
  (let ((db-width (cffi:foreign-slot-value db 'draw-buffer 'width))
	(db-height (cffi:foreign-slot-value db 'draw-buffer 'height)))
      (floor (loop for y from 0 to (- db-height 1) sum 
		  (loop for x from 0 to (- db-width 1) sum 
		       (funcall proc (get-pixel db x y 3)))) (* db-width db-height))))

(defun image-variance-1 (db proc)
  (let* ((db-width (cffi:foreign-slot-value db 'draw-buffer 'width))
	 (db-height (cffi:foreign-slot-value db 'draw-buffer 'height))
	 (num-pixel (* db-width db-height))
	 (db-average (image-average db proc))
	 (ar-hist (make-hist db #'make-hist-proc proc))
	 (j -1))
    (floor (loop for i across ar-hist sum
		(* (expt (- (incf j) db-average) 2) (/ i num-pixel))))))
	 
(defun image-variance-2 (db proc)
  (let* ((db-width (cffi:foreign-slot-value db 'draw-buffer 'width))
	 (db-height (cffi:foreign-slot-value db 'draw-buffer 'height))
	 (num-pixel (* db-width db-height))
	 (db-average (image-average db proc))
	 (j 0))
    (floor (loop for x from 0 to (- db-width 1) sum
		(loop for y from 0 to (- db-height 1) sum
		     (expt (- db-average (funcall proc (get-pixel db x y 3))) 2)))
	   num-pixel)))

(defun image-area-average (db proc x y size)
  (let* ((db-width (cffi:foreign-slot-value db 'draw-buffer 'width))
	 (db-height (cffi:foreign-slot-value db 'draw-buffer 'height))
	 (start-x (max (- x size) 0))
	 (start-y (max (- y size) 0))
	 (end-x (min (+ x size) (- db-width 1)))
	 (end-y (min (+ y size) (- db-height 1)))
	 (num-pixel (* (- end-x start-x) (- end-y start-y))))
    (floor (loop for y from start-y to end-y sum 
		(loop for x from start-x to end-x sum 
		       (funcall proc (get-pixel db x y 3)))) num-pixel)))

(defun image-area-variance (db proc x y size)
  (let* ((db-width (cffi:foreign-slot-value db 'draw-buffer 'width))
	 (db-height (cffi:foreign-slot-value db 'draw-buffer 'height))
	 (start-x (max (- x size) 0))
	 (start-y (max (- y size) 0))
	 (end-x (min (+ x size) (- db-width 1)))
	 (end-y (min (+ y size) (- db-height 1)))
	 (num-pixel (* (- end-x start-x) (- end-y start-y))))
    (floor (loop for y from start-y to end-y sum 
		(loop for x from start-x to end-x sum 
		       (expt (- (image-area-average db proc x y size) 
				(funcall proc (get-pixel db x y 3))) 2))) num-pixel)))

(defun image-improve-by-area (db proc E k0 k1 k2)
  (let* ((db-width (cffi:foreign-slot-value db 'draw-buffer 'width))
	 (db-height (cffi:foreign-slot-value db 'draw-buffer 'height))
	 (target-db (clone-draw-buffer db))
	 (db-var (image-variance-2 db proc))
	 (thres-avg (* (image-average db proc) k0))
	 (thres-var-low (* db-var k1))
	 (thres-var-hi (* db-var k2))
	 (improve-proc (lambda (db x y color)
			 (let ((c (funcall proc color))
			       (m (image-area-average db proc x y 1))
			       (s (image-area-variance db proc x y 1)))
			   (if (or (< m thres-avg)
				   (and (<= s thres-var-hi)
					(>= s thres-var-low)))
			       color
			       (list (thres-apply (floor (* E (first color))) '(0 255))
				     (thres-apply (floor (* E (second color))) '(0 255))
				     (thres-apply (floor (* E (third color))) '(0 255))))))))
    (progn
      (mapcar-image-d-2 db target-db improve-proc)
      target-db)))

(defun image-improve-by-area-1 (db proc E k0 k1 k2)
  (let* ((db-width (cffi:foreign-slot-value db 'draw-buffer 'width))
	 (db-height (cffi:foreign-slot-value db 'draw-buffer 'height))
	 (target-db (clone-draw-buffer db))
	 (db-var (image-variance-2 db proc))
	 (thres-avg (floor (* (image-average db proc) k0)))
	 (thres-var-low (* db-var k1))
	 (thres-var-hi (* db-var k2))
	 (improve-proc (lambda (db x y color)
			 (let ((c (funcall proc color))
			       (m (image-area-average db proc x y 1))
			       (s (image-area-variance db proc x y 1)))
			   (if (or (< m thres-avg)
				   (and (<= s thres-var-hi)
					(>= s thres-var-low)))
			       (list (thres-apply (floor (* E (first color))) '(0 255))
				     (thres-apply (floor (* E (second color))) '(0 255))
				     (thres-apply (floor (* E (third color))) '(0 255)))
			       color)))))
    (progn
      (mapcar-image-d-2 db target-db improve-proc)
      target-db)))

