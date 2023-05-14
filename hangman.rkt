#lang racket
;todo o código, exceto a leitura de arquivos, foi feito usando racket puro
(require racket/file)

;abre o arquivo, caso queira trocar o "tema" do jogo, mude o nome do arquivo
;vale lembrar que o caminho atual está apontado para o repositório em que o jogo está salvo
(define arquivo (open-input-file "cores.txt"))
(define lista (list))
;lê linha por linha jogando cada palavra em uma lista
(let loop ()
  (let ((linha (read-line arquivo)))
    ;checa se o arquivo está no fim
    (if (not (eof-object? linha))
        (begin
          ;joga as palavras para a lista sem o marcador de final de linha
          (set! lista (cons (string-trim linha "\r") lista))
          (loop))
        (void))))
;fecha o arquivo
(close-input-port arquivo)

;desenha a forca de acordo com o número de tentativas restantes
(define (desenha-forca tries)
  (cond
    [(= tries 6) (displayln " _________")
                 (displayln "|         |")
                 (displayln "|")
                 (displayln "|")
                 (displayln "|")
                 (displayln "|")
                 (displayln "|")]
    [(= tries 5) (displayln " _________")
                 (displayln "|         |")
                 (displayln "|         O")
                 (displayln "|")
                 (displayln "|")
                 (displayln "|")
                 (displayln "|")]
    [(= tries 4) (displayln " _________")
                 (displayln "|         |")
                 (displayln "|         O")
                 (displayln "|         |")
                 (displayln "|")
                 (displayln "|")
                 (displayln "|")]
    [(= tries 3) (displayln " _________")
                 (displayln "|         |")
                 (displayln "|         O")
                 (displayln "|        /|")
                 (displayln "|")
                 (displayln "|")
                 (displayln "|")]
    [(= tries 2) (displayln " _________")
                 (displayln "|         |")
                 (displayln "|         O")
                 (displayln "|        /|\\")
                 (displayln "|")
                 (displayln "|")
                 (displayln "|")]
    [(= tries 1) (displayln " _________")
                 (displayln "|         |")
                 (displayln "|         O")
                 (displayln "|        /|\\")
                 (displayln "|        /")
                 (displayln "|")
                 (displayln "|")]
    [(= tries 0) (displayln " _________")
                 (displayln "|         |")
                 (displayln "|         O")
                 (displayln "|        /|\\")
                 (displayln "|        / \\")
                 (displayln "|")
                 (displayln "|")
                 ]))

;função que atualiza a palavra a cada acerto
(define (atualiza-palavra chute)
  (map (lambda (l i)
         ;caso a letra l seja igual ao chute, atualiza a lista da palavra-escondida no indice em especifico
         (if (equal? l chute)
             chute
             (list-ref palavra-escondida i)))
       letras
       (range (length letras))))

;função para o computador jogar sozinho
(define (random-chute)
  ;esse if divide o retono em duas possibilidades
   (if (> (random 100) 60)
       ;40% de chance de retornar uma consoante aleatória
      (list-ref consoantes (random 21))
      ;60% de chance de retornar uma vogal aleatória
      (list-ref vogais (random 5))
      ))

;lista de vogais
(define vogais (list "a" "e" "i" "o" "u"))
;lista de consoantes
(define consoantes '( "b" "c" "d" "f" "g" "h" "j" "k" "l" "m" "n" "p" "q" "r" "s" "t" "v" "w" "x" "y" "z"))
;a palavra a ser acertada é definida pela função random aplicada na lista que é lida do arquivo
(define palavra (list-ref lista (random (length lista))))
;a partir da palavra, é gerada uma lista com cada letra separada
(define letras (map string (string->list palavra)))
;a palavra escondida começa com '_' que se repetem para cada valor dentro da lista letras, ou seja, um underline para cada letra 
(define palavra-escondida (map (lambda (x) "_") letras))
;listagem de erros que começa vazia
(define erros '())

;printa a palavra escondida separada por espaços
(define (print-palavra-escondida)
  (for ([i palavra-escondida]) (display i) (display " "))
  (displayln ""))

;printa todas as tentativas fracassadas separadas por espaços
(define (print-erros)
  (display "Letras erradas: ")
  (for ([i erros]) (display i) (display " "))
  (displayln ""))

;starta o jogo
(define (play)
  ;loop do jogo
  (let loop ([tries 6])
    ;desenha a forca de acordo com o número de tentativas restantes
    (desenha-forca (- tries (length erros)))
    ;printa a palavra escondida
    (print-palavra-escondida)
    ;printa os erros
    (print-erros)
    ;aguarda uma entrada pelo teclado para a varíavel chute
    
    (let ([chute (read-line)])
      (cond
        ;caso o chute já esteja na palavra escondida ou dentro dos erros, o loop retorna e nenhuma tentativa é retirada
        [(or (member chute palavra-escondida) (member chute erros))
         (displayln "Você já digitou essa letra antes, digite uma letra diferente.\n")
         (loop tries)]
        ;caso o chute faça parte da lista letras (palavra correta) atualiza a palavra escondida
        [(member chute letras)
         ;set! serve para alterar toda a lista
         (set! palavra-escondida (atualiza-palavra chute))
         ;caso o underline não seja mais mebro da palavra escondida, quer dizer que o usuário (ou máquina) ganhou
         (if (not (member "_" palavra-escondida))
             ;o begin fez possível executar mais de duas instrução dentro de uma cláusula if
             (begin
             (desenha-forca (- tries (length erros)))
             (displayln (string-append "\nA palavra era: [" palavra "]\nParabéns, você ganhou!")))
             (loop tries))]
        ;caso o underline ainda esteja presente, o jogo continua ou o usuárop/máquina perdeu
        [else
         ;atualiza a lista de erros
         (set! erros (cons chute erros))
         (displayln (string-append "Chute errado! Restam " (number->string (- tries (length erros))) " tentativas restantes."))
                      ;caso o tamanho dos erros seja maior que as tentativas (6) o usuário/máquina perdeu
                      (if (>= (length erros) tries)
                          (begin
                          (desenha-forca (- tries (length erros)))
                          (displayln (string-append "Você perdeu!\nA palavra certa era : [" palavra"]")))
                          (loop tries))]))))

(define (random-play)
  (let loop ([tries 6])
    (desenha-forca (- tries (length erros)))
    (print-palavra-escondida)
    (print-erros)
    ;a única diferença dessa função para a função play é que a chute é definida a partir de uma função externa que escolhe entre consoantes e vogais
    (let ([chute (random-chute)])
      (cond
        [(or (member chute palavra-escondida) (member chute erros))
         (displayln "Você já digitou essa letra antes, digite uma letra diferente.\n")
         (sleep 2)
         (loop tries)]
        [(member chute letras)
         (set! palavra-escondida (atualiza-palavra chute))
         (if (not (member "_" palavra-escondida))
             (begin
             (desenha-forca (- tries (length erros)))
             (displayln (string-append "\nA palavra era: [" palavra "]\nParabéns, você ganhou!")))
             (loop tries))]  
        [else
         (set! erros (cons chute erros))
         (displayln (string-append "\nChute errado! Restam " (number->string (- tries (length erros))) " tentativas restantes."))
         (sleep 2)
                      (if (>= (length erros) tries)
                          (begin
                          (desenha-forca (- tries (length erros)))
                          (displayln (string-append "Você perdeu!\nA palavra certa era : [" palavra"]")))
                          (loop tries))]))))


(play)
;(random-play)

; Foram utiizados conceitos vistos em aula como listas, condicionais e estruturas comparativas
; Porém, utilizamos de conceitos como loops for para garantir o funcionamento do jogo
; Utilizamos dessas estruturas com base em exemplos na internet e julgando que seu comportamento seja intuitivamente parecido com outras linguagens
; Além disso, precisamos utilizar da estrutura "Begin" para garantir mais de dois comandos dentro de cláusulas if

