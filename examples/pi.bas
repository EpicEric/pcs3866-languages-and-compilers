5 REM Imprimir os primeiros digitos de pi
10 READ Q, R, T, K, N, L, I
20 DATA 1, 0, 1, 1, 3, 3, 0
30 LET A = 4 * Q + R - T
40 LET B = N * T
50 IF A >= B THEN 300

60 REM Encontrou digito
65 PRINT INT(N),
70 IF I <> 0 THEN 80
72 PRINT ".",
80 LET I = I + 1
84 REM Parar apos imprimir nove digitos
85 IF I >= 9 THEN 900
90 LET N0 = 10 * (R - N * T)
100 LET X0 = 10 * (3 * Q + R)
110 LET X1 = T
120 GOSUB 600
130 LET N = X2 - 10 * N
140 LET Q = Q * 10
150 LET R = N0
160 GOTO 30

300 REM Ainda nao encontrou digito
305 LET N0 = (2 * Q + R) * L
310 LET X0 = (Q * (7 * K) + 2 + (R * L))
320 LET X1 = (T * L)
330 GOSUB 600
340 LET Q = Q * K
350 LET T = T * L
360 LET L = L + 2
370 LET K = K + 1
380 LET N = X2
390 LET R = N0
400 GOTO 30

600 REM Resultado da divisao inteira X2 = X0 / X1
605 LET X2 = 0
610 IF X0 >= X1 THEN 620
615 RETURN
620 LET X2 = X2 + 1
630 LET X0 = X0 - X1
640 GO TO 610

900 PRINT ""
905 END
